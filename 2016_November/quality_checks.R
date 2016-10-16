# We only need one library for these checks.
library("dplyr")

# Load the data.
df <- read.csv(
    file.path("data", "endorsements.csv"),
    stringsAsFactors = FALSE
)

# Check that we only use "Yes", "No", "Abstain" and NA.
is_correct_coding_used <- identical(
    sort(unique(df$endorsement), na.last = TRUE),
    c("Abstain", "No", "Yes", NA)
)

if (!is_correct_coding_used) {
    warning(
        paste0(
            "Invalid coding: (",
            paste0(unique(df$endorsement), collapse = ", "),
            ")",
            collapse = ""
        )
    )
}

# Determine the core quantities.
n_endorsers <- length(unique(df$endorser))
n_categories <- length(unique(df$category))
n_candidates <- length(unique(df$candidate))

# Check if the number of rows data matches our expectation, which is one row
# for every (endorser, candidate) pair.
if (nrow(df) != n_endorsers * n_candidates) {
    warning(
        paste0(
            "Number of rows does not match expectations\n",
            "Number of rows: ", nrow(df), "\n",
            "Number of endorsers: ", n_endorsers, "\n",
            "Number of candidates: ", n_candidates, "\n",
            "Expected rows: ", n_endorsers * n_candidates, "\n"
        )
    )
}

# Check how many endorsements we have per category, candidate pair. All of
# these should be exactly equal to the number of endorsers.
invariant_per_candidate_endorsements <- with(
    df %>%
        group_by(category, candidate) %>%
        summarize(n = n()),
    all(n == n_endorsers)
)

if (!invariant_per_candidate_endorsements) {
    warning(
        paste0(
            "At least one (category, candidate) pair has the wrong number of ",
            "endorsements"
        )
    )
}

# Check how many endorsements we have per endorser. All of these should be
# exactly equal to the number of candidates, but the number of missing values
# can vary a lot across endorsers.
# TODO: Restore this
df %>%
    group_by(endorser) %>%
    summarize(
        n = n(),
        n_null = sum(is.na(endorsement))
    )
