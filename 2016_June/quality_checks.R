# We only need one library for these checks.
library("dplyr")

# Load the data.
df <- read.csv(
    file.path("data", "endorsements.csv"),
    stringsAsFactors = FALSE
)

# Check that we only use "Yes", "No", "Abstain" and NA.
unique(df$endorsement)

# Check how many endorsements we have per category. All of these should be
# multiples of the number of endorsers.
df %>%
    group_by(category) %>%
    summarize(n = n()) %>%
    as.data.frame

# Check how many endorsements we have per candidate. All of these should be
# multiples of the number of endorsers.
df %>%
    group_by(candidate) %>%
    summarize(n = n()) %>%
    as.data.frame

# Check how many endorsements we have per candidate. All of these should be
# exactly equal to the number of endorsers.
df %>%
    group_by(category, candidate) %>%
    summarize(n = n()) %>%
    as.data.frame

# Check how many endorsements we have per endorser. All of these should be
# exactly equal to the number of candidates, but the number of missing values
# can vary a lot across endorsers.
df %>%
    group_by(endorser) %>%
    summarize(
        n = n(),
        n_null = sum(is.na(endorsement))
    ) %>%
    as.data.frame
