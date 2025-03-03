library(dplyr)
library(readr)
library(stringr)
library(syuzhet)

# 1. Read the data
df <- read_csv("./clean_data/clean_data_1.csv", show_col_types = FALSE) %>%
  # 2. Add a column that counts the number of words in 'verse'
  mutate(n_words = str_count(verse, pattern = "\\S+")) %>%
  # 3. Bind the NRC sentiment columns to 'df'
  bind_cols(get_nrc_sentiment(.$verse,
                              language = "english",
                              lowercase = FALSE,
                              lexicon = NULL)) %>%
  # 4. Remove the original 'verse' text column
  select(-verse) %>%
  # 5. Convert several columns to factors in one step
  mutate(
    across(
      c(suicidal, sex, poet, poem_title, period),
      as.factor
    )
  )

# 6. Define v and w, then create a new numeric 'verse' column
v <- 3 * (df$poet_id - 1) + sapply(as.numeric(df$period), function(u) {
  if (u == 1) 0 else if (u == 2) 2 else 1
})

v <- c((1:length(v))[diff(v) == 1], length(v))
v <- c(dplyr::first(v), diff(v))

w <- unlist(sapply(v, function(r) rep(r, r)))
v <- unlist(sapply(v, function(r) 1:r)) - 1

df$verse <- v / w

# 7. Write the final data frame to a CSV file
write_csv(df, "./clean_data/clean_data_2.csv")
