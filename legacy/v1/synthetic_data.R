# ----------------------------------------------------------
# Synthetic Poetry Dataset Generator (Improved & More Realistic)
# ----------------------------------------------------------
# This script generates a dataset of 'n' poems. Each poem has:
#   1) Demographic data about the poet (gender, birth year, etc.)
#   2) Whether the poet is suicidal (Yes/No), and year of suicide if Yes
#   3) Basic poem metadata (titles, publisher, period, etc.)
#   4) Numeric linguistic/sentiment features (word_count, sentiment scores, etc.)
# 
# "Year of publication" has been removed to align with the 
# user's request. Instead, we rely on "period" for a simple 
# career stage classification.
# ----------------------------------------------------------

generate_synthetic_poetry_dataset <- function(n = 50, seed = 123) {
  set.seed(seed)
  
  # Example lists for random sampling
  poet_names     <- c("Poet A", "Poet B", "Poet C", "Poet D",
                      "Poet E", "Poet F", "Poet G")
  poem_titles    <- c("Nightfall", "Twilight Moon", "Ode to Summer",
                      "Whispering Winds", "Eternal Gloom", "Flicker of Hope")
  book_titles    <- c("Collected Poems", "New Horizons",
                      "Random Reflections", "Shadows & Light")
  publishers     <- c("Fiction House", "Sunlit Press", "Morning Star", "Deep Ink")
  languages      <- c("English", "French", "Spanish", "German")
  period_levels  <- c("early", "middle", "late")
  
  poet_genders   <- c("M", "F", "Nonbinary")
  
  # Pre-allocate vectors
  poem_id                    <- integer(n)
  poet_id                    <- integer(n)
  poet_name                  <- character(n)
  poet_gender                <- character(n)
  poet_year_of_birth         <- integer(n)
  year_of_suicide            <- integer(n)  # 0 => not applicable
  suicidal                   <- character(n) # "Yes"/"No"
  
  poem_title                 <- character(n)
  book_title                 <- character(n)
  publisher                  <- character(n)
  period                     <- character(n)
  language                   <- character(n)
  
  word_count                 <- integer(n)
  line_count                 <- integer(n)
  avg_line_length            <- numeric(n)
  readability_score          <- numeric(n)
  avg_syllables_per_word     <- numeric(n)
  lexical_diversity_index    <- numeric(n)
  punctuation_rate           <- numeric(n)
  
  # Sentiment / Emotion columns
  embedding_sentiment_score  <- numeric(n)
  sadness_score              <- numeric(n)
  anger_score                <- numeric(n)
  joy_score                  <- numeric(n)
  fear_score                 <- numeric(n)
  surprise_score             <- numeric(n)
  disgust_score              <- numeric(n)
  dominant_emotion           <- character(n)
  embedding_emotion_entropy  <- numeric(n)
  
  mean_line_sentiment        <- numeric(n)
  std_line_sentiment         <- numeric(n)
  max_line_sentiment         <- numeric(n)
  min_line_sentiment         <- numeric(n)
  sentiment_slope            <- numeric(n)
  line_negemo_proportion     <- numeric(n)
  line_posemo_proportion     <- numeric(n)
  emotional_arc_type         <- character(n)
  
  # ----------------------------------------------------------
  # Poet-level data (demographics and suicidality)
  # We'll store poet-level data in a small lookup table so
  # each poet has consistent demographic info across poems.
  # ----------------------------------------------------------
  # Number of unique poets
  n_poets <- length(poet_names)
  
  # Create a small table for poet demographics
  # We'll assume each poet in poet_names has a random gender & birth year,
  # and a 40% chance to be suicidal. If suicidal, pick a random year_of_suicide
  # 30-70 years after birth.
  
  poet_demographics <- data.frame(
    poet_id = seq_len(n_poets),
    poet_name = poet_names,
    poet_gender = sample(poet_genders, n_poets, replace = TRUE),
    poet_year_of_birth = sample(1900:1960, n_poets, replace = TRUE),
    suicidal = character(n_poets),
    year_of_suicide = integer(n_poets),
    stringsAsFactors = FALSE
  )
  
  # Assign suicidal or not for each poet, plus year_of_suicide if suicidal
  for (k in seq_len(n_poets)) {
    if (runif(1) < 0.4) {
      poet_demographics$suicidal[k] <- "Yes"
      birth_year <- poet_demographics$poet_year_of_birth[k]
      poet_demographics$year_of_suicide[k] <- birth_year + sample(30:70, 1)
    } else {
      poet_demographics$suicidal[k] <- "No"
      poet_demographics$year_of_suicide[k] <- 0
    }
  }
  
  # ----------------------------------------------------------
  # Generate poem-level data
  # ----------------------------------------------------------
  for (i in seq_len(n)) {
    poem_id[i]  <- i
    
    # Randomly select a poet
    current_poet <- sample(seq_len(n_poets), 1)
    
    # Fill in poet fields from the demographics table
    poet_id[i]            <- current_poet
    poet_name[i]          <- poet_demographics$poet_name[current_poet]
    poet_gender[i]        <- poet_demographics$poet_gender[current_poet]
    poet_year_of_birth[i] <- poet_demographics$poet_year_of_birth[current_poet]
    suicidal[i]           <- poet_demographics$suicidal[current_poet]
    year_of_suicide[i]    <- poet_demographics$year_of_suicide[current_poet]
    
    # Metadata about the poem
    poem_title[i]  <- sample(poem_titles, 1)
    book_title[i]  <- sample(book_titles, 1)
    publisher[i]   <- sample(publishers, 1)
    language[i]    <- sample(languages, 1)
    period[i]      <- sample(period_levels, 1)
    
    # Basic poem stats
    word_count[i]  <- sample(80:300, 1)
    line_count[i]  <- sample(10:40, 1)
    avg_line_length[i]     <- word_count[i] / line_count[i]
    readability_score[i]   <- round(runif(1, 50, 80), 1)
    avg_syllables_per_word[i]  <- runif(1, 1.2, 2.0)
    lexical_diversity_index[i] <- runif(1, 0.5, 0.9)
    punctuation_rate[i]        <- runif(1, 0.01, 0.05)
    
    # Adjust sentiment based on suicidality
    is_suicidal <- (suicidal[i] == "Yes")
    base_sentiment <- rnorm(1, mean = ifelse(is_suicidal, -0.5, 0.3), sd = 0.2)
    # clamp to [-1,1]
    embedding_sentiment_score[i] <- max(min(base_sentiment, 1), -1)
    
    # Discrete emotions: random draws + "softmax" style normalization
    raw_sadness  <- rnorm(1, mean = ifelse(is_suicidal, 0.7, 0.3), sd = 0.2)
    raw_anger    <- rnorm(1, mean = ifelse(is_suicidal, 0.5, 0.2), sd = 0.2)
    raw_joy      <- rnorm(1, mean = ifelse(is_suicidal, 0.2, 0.7), sd = 0.2)
    raw_fear     <- rnorm(1, mean = ifelse(is_suicidal, 0.4, 0.1), sd = 0.2)
    raw_surprise <- rnorm(1, mean = 0.3, sd = 0.2)
    raw_disgust  <- rnorm(1, mean = ifelse(is_suicidal, 0.3, 0.1), sd = 0.2)
    
    # Make sure values are non-negative
    raw_vec <- pmax(c(raw_sadness, raw_anger, raw_joy, raw_fear, raw_surprise, raw_disgust), 0)
    
    if (sum(raw_vec) == 0) {
      # Avoid division by zero
      raw_vec <- runif(6, 0.01, 0.02)
    }
    emotion_probs <- raw_vec / sum(raw_vec)
    
    sadness_score[i]  <- emotion_probs[1]
    anger_score[i]    <- emotion_probs[2]
    joy_score[i]      <- emotion_probs[3]
    fear_score[i]     <- emotion_probs[4]
    surprise_score[i] <- emotion_probs[5]
    disgust_score[i]  <- emotion_probs[6]
    
    # Dominant emotion
    emotion_labels       <- c("sadness","anger","joy","fear","surprise","disgust")
    dominant_emotion[i]  <- emotion_labels[which.max(emotion_probs)]
    
    # Emotion entropy: measure how "mixed" the distribution is
    H_raw <- -sum(emotion_probs * log(emotion_probs + 1e-9))
    embedding_emotion_entropy[i] <- H_raw / log(length(emotion_probs))
    
    # Simulate line-level sentiment variation
    mean_line_sentiment[i] <- embedding_sentiment_score[i] + rnorm(1, sd = 0.05)
    std_line_sentiment_val <- abs(rnorm(1, mean = 0.15, sd = 0.05))
    std_line_sentiment[i]  <- std_line_sentiment_val
    
    # min / max line sentiments
    max_line_sentiment[i] <- min(mean_line_sentiment[i] + runif(1, 0, 0.5), 1)
    min_line_sentiment[i] <- max(mean_line_sentiment[i] - runif(1, 0, 0.5), -1)
    
    # Slope (more negative on average for suicidal)
    if (is_suicidal) {
      sentiment_slope[i] <- runif(1, -0.7, -0.1)
    } else {
      sentiment_slope[i] <- runif(1, -0.1, 0.5)
    }
    
    # Proportion of lines with negative or positive sentiment
    if (mean_line_sentiment[i] < 0) {
      line_negemo_proportion[i] <- runif(1, 0.4, 0.8)
      line_posemo_proportion[i] <- runif(1, 0.0, 0.3)
    } else {
      line_negemo_proportion[i] <- runif(1, 0.0, 0.3)
      line_posemo_proportion[i] <- runif(1, 0.4, 0.8)
    }
    
    # Arc type classification
    slope_val <- sentiment_slope[i]
    if (slope_val < -0.3) {
      emotional_arc_type[i] <- sample(c("fall","fall-rise-fall"), 1)
    } else if (slope_val > 0.3) {
      emotional_arc_type[i] <- sample(c("rise","rise-fall-rise"), 1)
    } else {
      emotional_arc_type[i] <- "steady"
    }
  }
  
  # Construct final data frame
  synthetic_data <- data.frame(
    # Poet demographics
    poem_id = poem_id,
    poet_id = poet_id,
    poet_name = poet_name,
    poet_gender = poet_gender,
    poet_year_of_birth = poet_year_of_birth,
    suicidal = suicidal,
    year_of_suicide = year_of_suicide,
    
    # Poem metadata
    poem_title = poem_title,
    book_title = book_title,
    publisher = publisher,
    period = period,
    language = language,
    
    # Basic stats
    word_count = word_count,
    line_count = line_count,
    avg_line_length = round(avg_line_length, 2),
    readability_score = readability_score,
    avg_syllables_per_word = round(avg_syllables_per_word, 2),
    lexical_diversity_index = round(lexical_diversity_index, 2),
    punctuation_rate = round(punctuation_rate, 4),
    
    # Sentiment & emotion
    embedding_sentiment_score = round(embedding_sentiment_score, 2),
    sadness_score = round(sadness_score, 2),
    anger_score = round(anger_score, 2),
    joy_score = round(joy_score, 2),
    fear_score = round(fear_score, 2),
    surprise_score = round(surprise_score, 2),
    disgust_score = round(disgust_score, 2),
    dominant_emotion = dominant_emotion,
    embedding_emotion_entropy = round(embedding_emotion_entropy, 2),
    
    mean_line_sentiment = round(mean_line_sentiment, 2),
    std_line_sentiment = round(std_line_sentiment, 2),
    max_line_sentiment = round(max_line_sentiment, 2),
    min_line_sentiment = round(min_line_sentiment, 2),
    sentiment_slope = round(sentiment_slope, 2),
    line_negemo_proportion = round(line_negemo_proportion, 2),
    line_posemo_proportion = round(line_posemo_proportion, 2),
    emotional_arc_type = emotional_arc_type,
    
    stringsAsFactors = FALSE
  )
  
  return(synthetic_data)
}


# ----------------------------------------------------------
# Example usage
# ----------------------------------------------------------
# Generate a synthetic dataset of 100 poems:
synthetic_data <- generate_synthetic_poetry_dataset(n = 100, seed = 999)
write.csv(synthetic_data, file="data/synthetic_data.csv", row.names = FALSE)


