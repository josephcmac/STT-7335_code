library(dplyr)
library(readr)
library(syuzhet) # https://www.rdocumentation.org/packages/syuzhet/versions/1.0.7

df <- read_csv("./clean_data/clean_data_1.csv", show_col_types = FALSE)

df <- cbind(df, get_nrc_sentiment(df$verse,
                            language = "english",
                            lowercase = FALSE,
                            lexicon = NULL)) %>% 
  select(-"verse")

write.csv(df, file="./clean_data/clean_data_2.csv", row.names = FALSE)
