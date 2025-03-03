library(dplyr)
library(tidyr)

# Read in your original wide data (case vs. control per row).
df <- read.csv("../datasource1/case-control-original.csv")

df_long <- df %>%
  # Assign a unique ID to each row
  mutate(pair_id = row_number()) %>%
  # Pivot from wide to long to separate case vs control
  pivot_longer(
    cols      = c("case", "control"),
    names_to  = "type",
    values_to = "poet"
  ) %>%
  # Create a boolean for suicidal (TRUE for case, FALSE for control)
  mutate(suicidal = (type == "case")) %>%
  # Expand each row for the three time periods you want to analyze
  crossing(period = c("Early", "Middle", "Later")) %>%
  # Convert period to a factor with custom order
  mutate(period = factor(period, levels = c("Early", "Middle", "Later"))) %>%
  # Add empty (placeholder) columns that may be relevant for study
  mutate(
    # Demographic columns
    sex                = "",  # male/female
    heterosexual       = "",  # TRUE/FALSE
    date_of_birth      = "",  # e.g. "1830-12-10"
    date_of_death      = "",  # e.g. "1886-05-15"
    country_of_birth   = "",  # e.g. "USA", "UK"
    maternal_language  = "",  # e.g. "English", "Spanish"
    
    # Variables about the poem itself
    poem_language      = "",  # e.g. "English", "German"
    poem_title         = "",  # title of the poem
    poem_text          = ""   # full text or excerpt of the poem if it is too long
  ) %>%
  # Order the rows:
  # 1. by pair_id (ascending),
  # 2. by suicidal (descending),
  # 3. by period (Early < Middle < Later)
  arrange(pair_id, desc(suicidal), period)

# Write out the reshaped dataset
write.csv(df_long, "case-control-long.csv", row.names = FALSE)
