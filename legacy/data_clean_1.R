library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)

df_verses <- read_csv("./raw_data/raw_data.csv", show_col_types = FALSE) %>%
  # Remove the 'source' column (if it exists)
  select(-source) %>%
  # Create file paths and read lines
  mutate(
    filepath = file.path("./raw_data/authors", poet, paste0(poem_title, ".txt")),
    verse = map(filepath, read_lines)
  ) %>%
  select(-filepath) %>%
  # Unnest so each line (verse) is its own row
  unnest(cols = verse) %>%
  # Trim leading/trailing whitespace
  mutate(verse = str_trim(verse)) %>%
  # Filter out verses that are empty or only spaces
  filter(verse != "")

write.csv(df_verses, file="./clean_data/clean_data_1.csv", row.names = FALSE)
