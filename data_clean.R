# Load required packages
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidytext)
library(quanteda)

# -----------------------
# 1) Define the function  (only once, and properly closed!)
# -----------------------
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
    # stanza-splitting on blank lines
    segments <- unlist(strsplit(poem_text, "\n[ \t]*\n+", perl = TRUE))
  }
  segments <- trimws(segments)
  segments <- segments[nzchar(segments)]  # remove any empty ones
  
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
  em_matrix <- matrix(0, nrow = n_segments, ncol = length(emotions))
  colnames(em_matrix) <- emotions
  
  for (i in seq_len(n_segments)) {
    token_vec <- toks_list[[i]]
    n_tokens <- length(token_vec)
    
    if (n_tokens == 0) {
      next
    }
    
    # Polarity vector: starts at +1 for each token
    token_polarity <- rep(1, n_tokens)
    
    # 6.1. Negation Handling
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
              token_polarity[rng] <- token_polarity[rng] * (1 - weights)
            }
          }
        }
      }
    }
    
    # 6.2. Intensifier Handling
    if (handle_intensifiers && int_window > 0 && length(int_words_lc) > 0) {
      int_positions <- which(token_vec %in% int_words_lc)
      if (length(int_positions)) {
        for (pos in int_positions) {
          start_idx <- pos + 1
          end_idx   <- min(pos + int_window, n_tokens)
          if (start_idx <= n_tokens) {
            token_polarity[start_idx:end_idx] <- 
              token_polarity[start_idx:end_idx] * int_multiplier
          }
        }
      }
    }
    
    # Compute sums per emotion
    for (em_idx in seq_along(emotions)) {
      em <- emotions[em_idx]
      dict_df <- dict_list[[em]]
      
      match_mask <- token_vec %in% dict_df$word
      if (!any(match_mask)) {
        em_matrix[i, em_idx] <- 0
      } else {
        sum_val <- 0
        token_inds <- which(match_mask)
        for (ti in token_inds) {
          tword <- token_vec[ti]
          w <- dict_df$weight[dict_df$word == tword]
          sum_val <- sum_val + (token_polarity[ti] * w)
        }
        raw_score <- sum_val / n_tokens
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
  for (em_idx in seq_along(emotions)) {
    em <- emotions[em_idx]
    df_out[[paste0("score_", em)]] <- em_matrix[, em_idx]
  }
  
  for (em_idx in seq_along(emotions)) {
    em <- emotions[em_idx]
    avg_score <- mean(em_matrix[, em_idx])
    attr(df_out, paste0("avg_score_", em)) <- avg_score
  }
  
  return(df_out)
}

# -----------------------
# 2) Example dictionaries
# -----------------------
emotion_dictionary <- list(
  sadness = data.frame(
    word = c("sad", "sadness", "unhappy", "sorrow", "sorrowful", 
             "gloom", "gloomy","tear", "cry", "lonely", "lonesome", 
             "mournful", "depressed", "heartbroken", "forlorn", 
             "melancholy", "woeful", "grief", "lament", "despair", 
             "desolate", "blue", "morose", "wistful", "woe", 
             "bereft","downcast", "dreary", "despondent"),
    weight = c(
      1.0, 0.95, 1.0, 1.1, 1.1, 0.9, 1.0, 0.7, 1.1, 1.0, 
      1.0, 1.2, 1.4, 1.5, 1.4, 1.2, 1.1, 1.5, 1.3, 1.6, 
      1.3, 0.7, 1.3, 0.8, 1.2, 1.3, 1.0, 1.0, 1.4
    )
  ),
  happiness = data.frame(
    word = c("happy", "joy", "delight", "smile", "cheer", "glad", 
             "positive", "bliss", "content", "pleased", "joyful", 
             "exultant", "blissful","euphoric", "elated", "radiant", 
             "ecstatic", "jubilant", "upbeat", "rapture", "glee", 
             "lighthearted", "thrilled", "wonderful", "awesome"),
    weight = c(
      1.0, 1.1, 1.0, 0.8, 0.9, 0.9, 1.0, 1.3, 0.7, 0.8, 
      1.2, 1.3, 1.4, 1.5, 1.4, 1.1, 1.6, 1.4, 1.0, 1.3, 
      1.0, 0.8, 1.2, 1.1, 1.1
    )
  )
  # (Similarly define other emotions in the dictionary)
)

# -----------------------
# 3) Helper sentiment functions
# -----------------------
get_sentiment_afinn <- function(poem_text) {
  # Convert poem to tibble of words
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  # Get AFINN lexicon
  afinn <- get_sentiments("afinn")
  # Join poem words with AFINN
  scored_words <- words %>% inner_join(afinn, by = "word")
  # If none matched, return 0
  if (nrow(scored_words) == 0) return(0)
  # Sum up
  total_score <- sum(scored_words$value, na.rm = TRUE)
  max_afinn_val <- 5
  normalized_score <- total_score / (max_afinn_val * nrow(scored_words))
  return(normalized_score)
}

get_sentiment_bing <- function(poem_text) {
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  bing <- get_sentiments("bing")
  scored_words <- words %>% inner_join(bing, by = "word")
  if (nrow(scored_words) == 0) return(0)
  
  sentiment_counts <- scored_words %>%
    count(sentiment)
  
  positives <- sentiment_counts$n[sentiment_counts$sentiment == "positive"]
  negatives <- sentiment_counts$n[sentiment_counts$sentiment == "negative"]
  if (length(positives) == 0) positives <- 0
  if (length(negatives) == 0) negatives <- 0
  
  net_sentiment <- positives - negatives
  total_matched <- positives + negatives
  normalized_score <- net_sentiment / total_matched
  return(normalized_score)
}

get_sentiment_nrc_ratio <- function(poem_text) {
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  
  nrc <- get_sentiments("nrc") %>%
    filter(sentiment %in% c("negative", "positive"))
  
  scored_words <- words %>% inner_join(nrc, by = "word")
  if (nrow(scored_words) == 0) return(0)
  
  sentiment_counts <- scored_words %>%
    count(sentiment)
  
  negatives <- sentiment_counts$n[sentiment_counts$sentiment == "negative"]
  positives <- sentiment_counts$n[sentiment_counts$sentiment == "positive"]
  if (length(negatives) == 0) negatives <- 0
  if (length(positives) == 0) positives <- 0
  
  net_sentiment <- (positives - negatives)
  total_matched <- positives + negatives
  normalized_score <- net_sentiment / total_matched
  return(normalized_score)
}

get_suicidal_lexicon_score <- function(poem_text) {
  suicidal_lexicon <- c("death", "die", "dying", "grave", 
                        "suicide", "despair", "hopeless", 
                        "end", "darkness")
  
  words <- tibble(text = poem_text) %>%
    unnest_tokens(word, text)
  
  total_words <- nrow(words)
  if (total_words == 0) return(0)
  
  matched_count <- words %>%
    filter(word %in% suicidal_lexicon) %>%
    nrow()
  
  fraction_suicidal <- matched_count / total_words
  return(fraction_suicidal)
}

mean_length <- function(mystring) {
  words <- unlist(str_split(mystring, "\\s+"))
  words_clean <- str_replace_all(words, "[^[:alnum:]]", "")
  words_clean <- words_clean[words_clean != ""]
  mean(str_length(words_clean))
}

# -----------------------
# 4) Main Workflow
# -----------------------

# 1. Read the dataset and filter out rows with empty poem_text
df <- read_csv("../data_entry/case-control-long.csv", show_col_types = FALSE) %>% 
  filter(poem_text != "")

# 2. Calculate the mean word length for each poem
df <- df %>% 
  mutate(mean_length = map_dbl(poem_text, mean_length))

# 3. Define the list of emotions and whether we want stemming for each
emotions <- c("sadness", "happiness", "confusion", "anger", 
              "fear", "surprise", "disgust", "hope")

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

# 4. For each emotion, compute the average emotion score per poem
for (em in emotions) {
  df[[em]] <- map_dbl(df$poem_text, function(poem) {
    result <- evaluate_emotion_in_poems(
      poem_text         = poem,
      dictionary        = emotion_dictionary,
      emotions          = em,
      analyze_by        = "line",
      preserve_punct    = FALSE,
      remove_stopwords  = TRUE,
      use_stemming      = stem_lookup[em],
      handle_negation   = TRUE,
      negation_weighting = "flip"
    )
    
    # The relevant column is 'score_<em>'
    score_col <- paste0("score_", em)
    mean(result[[score_col]])
  })
}

# 5. Compute various sentiment scores
df$afinn        <- sapply(df$poem_text, get_sentiment_afinn)
df$bing         <- sapply(df$poem_text, get_sentiment_bing)
df$nrc_ratio    <- sapply(df$poem_text, get_sentiment_nrc_ratio)
df$lexicon_score <- sapply(df$poem_text, get_suicidal_lexicon_score)

# 6. Remove the original poem_text column and write final data to CSV
df %>%
  select(-poem_text) %>%
  write_csv("case-control-clean.csv")
