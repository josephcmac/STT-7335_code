# Load required packages
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidytext)

evaluate_emotion_in_poems <- function(
    poem_text,
    dictionary,
    emotions = NULL,
    analyze_by = c("line", "stanza"),
    preserve_punct = FALSE,
    remove_stopwords = FALSE,
    use_stemming = TRUE,
    handle_negation = TRUE,
    negation_words = c("not", "no", "never", "none", "cannot"),
    neg_window = 2,
    negation_weighting = c("flip", "distance"),
    handle_intensifiers = TRUE,
    intensifier_words = c("very", "extremely", "really", "so"),
    int_window = 2,
    int_multiplier = 1.5
) {
  analyze_by <- match.arg(analyze_by)
  negation_weighting <- match.arg(negation_weighting)
  
  # --------------------------
  # 1. Validate Inputs
  # --------------------------
  if (!is.character(poem_text) || length(poem_text) != 1) {
    stop("'poem_text' must be a single character string (the entire poem).")
  }
  if (!is.list(dictionary) || is.null(names(dictionary))) {
    stop("'dictionary' must be a named list of vectors/data frames, each for an emotion.")
  }
  if (is.null(emotions)) {
    # default to "all" known emotions
    emotions <- names(dictionary)
  } else {
    # check validity
    missing_em <- setdiff(emotions, names(dictionary))
    if (length(missing_em) > 0) {
      stop(sprintf("Some requested emotions not in dictionary: %s", 
                   paste(missing_em, collapse = ", ")))
    }
  }
  
  # --------------------------
  # 2. Split Poem Into Segments
  # --------------------------
  if (analyze_by == "line") {
    segments <- unlist(strsplit(poem_text, "\n", fixed = TRUE))
  } else {
    # stanza-splitting on blank lines (like in your original code)
    segments <- unlist(strsplit(poem_text, "\n[ \t]*\n+", perl = TRUE))
  }
  segments <- trimws(segments)
  segments <- segments[nzchar(segments)]  # remove empty
  if (length(segments) == 0) {
    df_empty <- data.frame(
      index = integer(0),
      text_segment = character(0)
    )
    for (em in emotions) {
      df_empty[[paste0("score_", em)]] <- numeric(0)
    }
    # attach average_score attributes
    for (em in emotions) {
      attr(df_empty, paste0("avg_score_", em)) <- 0
    }
    return(df_empty)
  }
  
  # --------------------------
  # 3. Prepare Dictionary
  # --------------------------
  # We will create a named list of data frames for uniform handling
  # (word, weight).
  dict_list <- list()
  for (em in emotions) {
    dict_data <- dictionary[[em]]
    if (is.vector(dict_data)) {
      # no weights, just create weight=1
      dict_data <- data.frame(word = dict_data, weight = 1, stringsAsFactors = FALSE)
    }
    dict_data$word <- trimws(tolower(dict_data$word))
    dict_data <- dict_data[nzchar(dict_data$word), , drop = FALSE]
    
    if (use_stemming) {
      dict_data$word <- quanteda::char_wordstem(dict_data$word)
    }
    dict_data <- dict_data[!duplicated(dict_data$word), ]
    dict_list[[em]] <- dict_data
  }
  
  # --------------------------
  # 4. Batch Tokenization
  # --------------------------
  library(quanteda)
  corpus_segs <- corpus(segments)
  toks <- tokens(
    corpus_segs,
    remove_punct   = !preserve_punct,
    remove_numbers = TRUE
  )
  if (remove_stopwords) {
    toks <- tokens_remove(toks, stopwords("en"))
  }
  if (use_stemming) {
    toks <- tokens_wordstem(toks)
  }
  
  toks_list <- lapply(toks, as.character)
  
  # --------------------------
  # 5. Negation & Intensifier Setup
  # --------------------------
  neg_words_lc <- tolower(negation_words)
  int_words_lc <- tolower(intensifier_words)
  
  # --------------------------
  # 6. Compute Scores for Each Segment and Each Emotion
  # --------------------------
  n_segments <- length(toks_list)
  # We store scores in a matrix: [segment, emotion]
  em_matrix <- matrix(0, nrow = n_segments, ncol = length(emotions))
  colnames(em_matrix) <- emotions
  
  for (i in seq_len(n_segments)) {
    token_vec <- toks_list[[i]]
    n_tokens <- length(token_vec)
    
    if (n_tokens == 0) {
      # all zero scores, skip
      next
    }
    
    # Polarity vector: starts at +1 for each token
    token_polarity <- rep(1, n_tokens)
    
    # ------------------------
    # 6.1. Negation Handling
    # ------------------------
    if (handle_negation && neg_window > 0 && length(neg_words_lc) > 0) {
      neg_positions <- which(token_vec %in% neg_words_lc)
      if (length(neg_positions)) {
        for (pos in neg_positions) {
          start_idx <- pos + 1
          end_idx   <- min(pos + neg_window, n_tokens)
          if (start_idx <= n_tokens) {
            if (negation_weighting == "flip") {
              token_polarity[start_idx:end_idx] <-
                token_polarity[start_idx:end_idx] * -1
            } else if (negation_weighting == "distance") {
              rng <- seq.int(start_idx, end_idx)
              dist <- rng - pos
              weights <- 1 - (dist / neg_window)
              # e.g., if dist=1, weights=1 - (1/2)=0.5 => half polarity
              # if dist=2, weights=1 - (2/2)=0   => no polarity
              token_polarity[rng] <- token_polarity[rng] * (1 - weights)
            }
          }
        }
      }
    }
    
    # ------------------------
    # 6.2. Intensifier Handling
    # ------------------------
    if (handle_intensifiers && int_window > 0 && length(int_words_lc) > 0) {
      int_positions <- which(token_vec %in% int_words_lc)
      if (length(int_positions)) {
        for (pos in int_positions) {
          start_idx <- pos + 1
          end_idx   <- min(pos + int_window, n_tokens)
          if (start_idx <= n_tokens) {
            # multiply polarity by int_multiplier
            token_polarity[start_idx:end_idx] <-
              token_polarity[start_idx:end_idx] * int_multiplier
          }
        }
      }
    }
    
    # For each emotion, match dictionary words and sum
    for (em_idx in seq_along(emotions)) {
      em <- emotions[em_idx]
      dict_df <- dict_list[[em]]
      
      match_mask <- token_vec %in% dict_df$word
      if (!any(match_mask)) {
        em_matrix[i, em_idx] <- 0
      } else {
        # sum of (polarity * weight) for matched tokens
        # We'll join by matching token -> dictionary row
        token_inds <- which(match_mask)
        # for speed, we can do a small merge in R, or a loop
        # a loop approach:
        sum_val <- 0
        for (ti in token_inds) {
          tword <- token_vec[ti]
          # get weight from dict
          w <- dict_df$weight[dict_df$word == tword]
          sum_val <- sum_val + (token_polarity[ti] * w)
        }
        # normalize by number of tokens or your chosen baseline
        raw_score <- sum_val / n_tokens
        # clamp [0,1]
        score_clamped <- max(0, min(raw_score, 1))
        em_matrix[i, em_idx] <- score_clamped
      }
    }
  }
  
  # --------------------------
  # 7. Build and Return Output
  # --------------------------
  df_out <- data.frame(
    index = seq_along(segments),
    text_segment = segments,
    stringsAsFactors = FALSE
  )
  # Add columns for each emotion
  for (em_idx in seq_along(emotions)) {
    em <- emotions[em_idx]
    df_out[[paste0("score_", em)]] <- em_matrix[, em_idx]
  }
  
  # Attach average score attributes
  for (em_idx in seq_along(emotions)) {
    em <- emotions[em_idx]
    avg_score <- mean(em_matrix[, em_idx])
    attr(df_out, paste0("avg_score_", em)) <- avg_score
  }
  
  return(df_out)
}


emotion_dictionary <- list(
  sadness = data.frame(
    word = c(
      "sad", "sadness", "unhappy", "sorrow", "sorrowful", "gloom", "gloomy",
      "tear", "cry", "lonely", "lonesome", "mournful", "depressed", 
      "heartbroken", "forlorn", "melancholy", "woeful", "grief", "lament", 
      "despair", "desolate", "blue", "morose", "wistful", "woe", "bereft",
      "downcast", "dreary", "despondent"
    ),
    weight = c(
      1.0,   # "sad"
      0.95,  # "sadness" - frequently used, slightly lower
      1.0,   # "unhappy"
      1.1,   # "sorrow"
      1.1,   # "sorrowful"
      0.9,   # "gloom"
      1.0,   # "gloomy"
      0.7,   # "tear" - can be sadness but also other emotions
      1.1,   # "cry"
      1.0,   # "lonely"
      1.0,   # "lonesome"
      1.2,   # "mournful"
      1.4,   # "depressed"
      1.5,   # "heartbroken"
      1.4,   # "forlorn"
      1.2,   # "melancholy"
      1.1,   # "woeful"
      1.5,   # "grief" - very deep sadness
      1.3,   # "lament"
      1.6,   # "despair" - extremely strong
      1.3,   # "desolate"
      0.7,   # "blue" - often used casually
      1.3,   # "morose"
      0.8,   # "wistful" - a bit of longing, mild sadness
      1.2,   # "woe"
      1.3,   # "bereft"
      1.0,   # "downcast"
      1.0,   # "dreary"
      1.4    # "despondent"
    )
  ),
  happiness = data.frame(
    word = c(
      "happy", "joy", "delight", "smile", "cheer", "glad", "positive", 
      "bliss", "content", "pleased", "joyful", "exultant", "blissful", 
      "euphoric", "elated", "radiant", "ecstatic", "jubilant", "upbeat", 
      "rapture", "glee", "lighthearted", "thrilled", "wonderful", "awesome"
    ),
    weight = c(
      1.0,  # "happy"
      1.1,  # "joy"
      1.0,  # "delight"
      0.8,  # "smile" - positive but mild
      0.9,  # "cheer"
      0.9,  # "glad"
      1.0,  # "positive"
      1.3,  # "bliss"
      0.7,  # "content" - moderate
      0.8,  # "pleased"
      1.2,  # "joyful"
      1.3,  # "exultant"
      1.4,  # "blissful"
      1.5,  # "euphoric"
      1.4,  # "elated"
      1.1,  # "radiant"
      1.6,  # "ecstatic" - extremely strong
      1.4,  # "jubilant"
      1.0,  # "upbeat"
      1.3,  # "rapture"
      1.0,  # "glee"
      0.8,  # "lighthearted"
      1.2,  # "thrilled"
      1.1,  # "wonderful"
      1.1   # "awesome"
    )
  ),
  confusion = data.frame(
    word = c(
      "confused", "puzzled", "perplexed", "baffled", "uncertain", "doubt",
      "ambiguous", "unclear", "bewildered", "mystified", "perplexing", 
      "nonplussed", "flustered", "disoriented", "confusion", "lost", "iffy"
    ),
    weight = c(
      1.0,  # "confused"
      0.9,  # "puzzled"
      1.1,  # "perplexed"
      1.3,  # "baffled"
      0.8,  # "uncertain"
      0.8,  # "doubt"
      0.7,  # "ambiguous"
      0.7,  # "unclear"
      1.0,  # "bewildered"
      1.2,  # "mystified"
      1.1,  # "perplexing"
      1.3,  # "nonplussed"
      1.0,  # "flustered"
      1.4,  # "disoriented" - strong confusion
      1.0,  # "confusion"
      1.2,  # "lost"
      0.6   # "iffy" - mild or informal
    )
  ),
  anger = data.frame(
    word = c(
      "angry", "rage", "furious", "hate", "irate", "mad", "hostile",
      "vengeful", "livid", "outraged", "enraged", "offended", "irritable",
      "resentful", "aggressive", "cross", "incensed", "infuriated", 
      "annoyed", "indignant", "fuming"
    ),
    weight = c(
      1.0,  # "angry"
      1.2,  # "rage"
      1.5,  # "furious" - very strong
      1.4,  # "hate"
      1.0,  # "irate"
      0.9,  # "mad"
      1.1,  # "hostile"
      1.3,  # "vengeful"
      1.4,  # "livid"
      1.5,  # "outraged"
      1.5,  # "enraged"
      0.8,  # "offended"
      0.8,  # "irritable"
      1.0,  # "resentful"
      1.1,  # "aggressive"
      0.7,  # "cross" - mild anger
      1.3,  # "incensed"
      1.6,  # "infuriated" - extremely strong
      0.6,  # "annoyed" - mild
      1.0,  # "indignant"
      1.3   # "fuming"
    )
  ),
  fear = data.frame(
    word = c(
      "afraid", "fear", "scared", "terrified", "anxious", "nervous",
      "uneasy", "frightened", "horrified", "apprehensive", "dread", 
      "alarmed", "petrified", "panicked", "trembling", "unsettled", 
      "intimidated", "threatened", "spooked"
    ),
    weight = c(
      1.0,  # "afraid"
      1.1,  # "fear"
      1.2,  # "scared"
      1.4,  # "terrified"
      1.0,  # "anxious"
      0.9,  # "nervous"
      0.8,  # "uneasy"
      1.2,  # "frightened"
      1.5,  # "horrified"
      1.0,  # "apprehensive"
      1.3,  # "dread"
      1.1,  # "alarmed"
      1.6,  # "petrified" - extremely intense
      1.4,  # "panicked"
      1.2,  # "trembling"
      0.7,  # "unsettled"
      1.0,  # "intimidated"
      1.3,  # "threatened"
      0.6   # "spooked" - more casual usage
    )
  ),
  surprise = data.frame(
    word = c(
      "surprised", "astonished", "startled", "amazed", "shocked",
      "stunned", "astounded", "speechless", "incredulous", "wow", "whoa"
    ),
    weight = c(
      1.0,  # "surprised"
      1.2,  # "astonished"
      1.1,  # "startled"
      1.3,  # "amazed"
      1.3,  # "shocked"
      1.2,  # "stunned"
      1.4,  # "astounded"
      1.0,  # "speechless"
      1.0,  # "incredulous"
      0.9,  # "wow" - informal expression
      0.8   # "whoa" - often mild or playful
    )
  ),
  disgust = data.frame(
    word = c(
      "disgusted", "repulsed", "revolted", "nauseated", "sickened",
      "grossed", "appalled", "abhorrent", "vile", "loathsome", "repugnant",
      "repellent", "yuck", "gross", "off-putting", "ick"
    ),
    weight = c(
      1.0,  # "disgusted"
      1.1,  # "repulsed"
      1.2,  # "revolted"
      1.2,  # "nauseated"
      1.1,  # "sickened"
      0.7,  # "grossed" - casual usage
      1.3,  # "appalled"
      1.4,  # "abhorrent" - strongly negative
      1.3,  # "vile"
      1.4,  # "loathsome"
      1.3,  # "repugnant"
      1.2,  # "repellent"
      0.6,  # "yuck" - informal
      0.7,  # "gross" - casual
      0.8,  # "off-putting"
      0.5   # "ick" - mild, informal
    )
  ),
  hope = data.frame(
    word = c(
      "hope", "optimistic", "aspire", "wishful", "future", "bright",
      "faith", "yearn", "yearning", "anticipation", "sanguine", "confidence",
      "assured", "aspiration", "encouragement", "promising", "rosy", 
      "faithful", "uplifting"
    ),
    weight = c(
      1.0,  # "hope"
      1.1,  # "optimistic"
      1.0,  # "aspire"
      1.0,  # "wishful"
      0.8,  # "future" - generic, somewhat less direct
      0.9,  # "bright"
      1.1,  # "faith"
      1.0,  # "yearn"
      1.1,  # "yearning"
      1.2,  # "anticipation"
      1.3,  # "sanguine"
      1.1,  # "confidence"
      1.0,  # "assured"
      1.2,  # "aspiration"
      1.1,  # "encouragement"
      1.1,  # "promising"
      0.8,  # "rosy" - mild optimism
      1.1,  # "faithful"
      1.2   # "uplifting"
    )
  )
)



#' Compute sentiment using AFINN lexicon
#'
#' @param poem_text A character string representing a poem.
#' @return A numeric sentiment score in the range [-1, 1].
get_sentiment_afinn <- function(poem_text) {
  
  # Convert the poem into a tibble of words
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  
  # Get AFINN lexicon from tidytext
  afinn <- get_sentiments("afinn")
  
  # Join poem words with AFINN
  scored_words <- words %>%
    inner_join(afinn, by = "word")
  
  # If no words are matched in the lexicon, return 0
  if (nrow(scored_words) == 0) {
    return(0)
  }
  
  # Sum all AFINN scores in the poem
  total_score <- sum(scored_words$value, na.rm = TRUE)
  
  # Normalize the score to [-1, 1].
  # AFINN values range from -5 to 5. 
  # For a rough normalization, divide by 5 * (number of matched words)
  # Alternatively, you could divide by total word count for average.
  
  max_afinn_val <- 5
  normalized_score <- total_score / (max_afinn_val * nrow(scored_words))
  
  return(normalized_score)
}

#' Compute sentiment using Bing lexicon
#'
#' @param poem_text A character string representing a poem.
#' @return A numeric sentiment score in the range [-1, 1].
get_sentiment_bing <- function(poem_text) {
  
  # Tokenize poem
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  
  # Get Bing lexicon from tidytext
  bing <- get_sentiments("bing")
  
  # Join poem words with Bing
  scored_words <- words %>%
    inner_join(bing, by = "word")
  
  # If no words are matched, return 0
  if (nrow(scored_words) == 0) {
    return(0)
  }
  
  # Count how many are positive vs negative
  sentiment_counts <- scored_words %>%
    count(sentiment)
  
  positives <- sentiment_counts$n[sentiment_counts$sentiment == "positive"]
  negatives <- sentiment_counts$n[sentiment_counts$sentiment == "negative"]
  
  # If no positives or negatives, set them to 0
  if (length(positives) == 0) positives <- 0
  if (length(negatives) == 0) negatives <- 0
  
  net_sentiment <- positives - negatives
  total_matched <- positives + negatives
  
  # Normalize to [-1, 1]
  normalized_score <- net_sentiment / total_matched
  
  return(normalized_score)
}

#' Compute NRC-based negative-to-positive ratio
#'
#' @param poem_text A character string representing a poem.
#' @return A numeric sentiment score in the range [-1, 1].
get_sentiment_nrc_ratio <- function(poem_text) {
  
  # Tokenize poem
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  
  # Get NRC sentiment lexicon
  nrc <- get_sentiments("nrc") %>%
    filter(sentiment %in% c("negative", "positive"))
  
  # Join poem words with NRC
  scored_words <- words %>%
    inner_join(nrc, by = "word")
  
  # If no words are matched, return 0
  if (nrow(scored_words) == 0) {
    return(0)
  }
  
  # Count negative vs positive
  sentiment_counts <- scored_words %>%
    count(sentiment)
  
  negatives <- sentiment_counts$n[sentiment_counts$sentiment == "negative"]
  positives <- sentiment_counts$n[sentiment_counts$sentiment == "positive"]
  
  if (length(negatives) == 0) negatives <- 0
  if (length(positives) == 0) positives <- 0
  
  # Compute ratio of (positives - negatives) / total
  net_sentiment <- (positives - negatives)
  total_matched <- positives + negatives
  
  # Normalize to [-1, 1]
  normalized_score <- net_sentiment / total_matched
  
  return(normalized_score)
}

#' @param poem_text A character string representing a poem.
#' @return A numeric score in [0, 1], representing fraction of words that match.
get_suicidal_lexicon_score <- function(poem_text) {
  suicidal_lexicon <- c("death", "die", "dying", "grave", "suicide", 
                        "despair", "hopeless", "end", "darkness")
  
  # Tokenize poem
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  
  total_words <- nrow(words)
  
  # If poem is empty or no words
  if (total_words == 0) {
    return(0)
  }
  
  # Count how many words match the suicidal lexicon
  matched_count <- words %>%
    filter(word %in% suicidal_lexicon) %>%
    nrow()
  
  # Fraction of suicidal-lexicon words
  fraction_suicidal <- matched_count / total_words
  
  # Optionally, transform the fraction to [-1, 1]. 
  # For example, you might interpret higher fraction as more "negative" 
  # and do: 2*fraction - 1
  
  # fraction_suicidal_scaled <- 2 * fraction_suicidal - 1
  # return(fraction_suicidal_scaled)
  
  return(fraction_suicidal)  # remains in [0, 1]
# Compute the mean word length for a given string.
# This function tokenizes the input using stringr::str_split,
# removes non-alphanumeric characters, and computes the mean token length.
}
mean_length <- function(mystring) {
  words <- unlist(str_split(mystring, "\\s+"))
  words_clean <- str_replace_all(words, "[^[:alnum:]]", "")
  words_clean <- words_clean[words_clean != ""]
  mean(str_length(words_clean))
}

# Read the dataset and filter out any rows with empty poem_text
df <- read_csv("../data_entry/case-control-long.csv", show_col_types = FALSE) %>% 
  filter(poem_text != "")


# Calculate the mean word length for each poem
df <- df %>% 
  mutate(mean_length = map_dbl(poem_text, mean_length))

# Define the list of emotions to analyze and a lookup for the use_stemming parameter.
# (For example, you want stemming for most emotions except for "confusion" and "surprise".)
emotions <- c("sadness", "happiness", "confusion", "anger", "fear", "surprise", "disgust", "hope")
stem_lookup <- c(
  sadness   = TRUE,
  happiness = TRUE,
  confusion = FALSE,
  anger     = TRUE,
  fear      = TRUE,
  surprise  = FALSE,
  disgust   = TRUE,
  hope      = TRUE
)

# For each emotion, compute the average emotion score per poem.
# The evaluate_emotion_in_poems() function is assumed to return a data frame 
# with a "score" column (one score per line or stanza).
# Here, we use purrr::map_dbl() to return a numeric vector for each emotion.
for (em in emotions) {
  df[[em]] <- map_dbl(df$poem_text, function(poem) {
    result <- evaluate_emotion_in_poems(
      poem_text         = poem,
      emotion           = em,
      dictionary        = emotion_dictionary,
      analyze_by        = "line",
      preserve_punct    = FALSE,
      remove_stopwords  = TRUE,
      use_stemming      = stem_lookup[em],
      handle_negation   = TRUE,
      negation_weighting = "flip"
    )
    mean(result$score)
  })
}

df$afinn <- sapply(df$poem_text, function(poem) get_sentiment_afinn(poem))
df$bing <- sapply(df$poem_text, function(poem) get_sentiment_bing(poem))
df$nrc_ratio <- sapply(df$poem_text, function(poem) get_sentiment_nrc_ratio(poem))
df$lexicon_score <- sapply(df$poem_text, function(poem) get_suicidal_lexicon_score(poem))

# Remove the original poem_text column and write the final data to CSV.
df %>% 
  select(-poem_text) %>%
  write_csv("case-control-clean.csv")

