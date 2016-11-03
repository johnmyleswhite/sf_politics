###############################################################################
#
# Set up everything for ideal point analysis.
#
###############################################################################

# We use the following packages.
library("ggplot2")
library("dplyr")
library("reshape2")
library("stringr")
library("rstan")
library("extrafont")

# Optimize some Stan configuration settings.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set a flag to determine whether we perform computationally intensive and
# exact calculations or use faster approaches during interactive development.
interactive_mode <- FALSE

# In interactive development, we do very light MCMC computations even though
# the resulting estimates are quite bad. For production results, we do much
# more computation.
if (interactive_mode) {
    mcmc_iter <- 2500
    mcmc_thin <- 1
} else {
    mcmc_iter <- 25000
    mcmc_thin <- 5
}

# Distinguish propositions from people by category.
props <- c(
    "Regional Proposition",
    "SF Proposition",
    "State Proposition"
)

# We'll need monospace fonts for plotting things with formatted labels.
font_import("mono")

# Load the raw data in long form.
endorsements <- read.csv(
    file.path("data", "endorsements.csv"),
    stringsAsFactors = FALSE
)

# Replace verbal endorsement labels with numbers.
endorsements <- transform(
    endorsements,
    endorsement = ifelse(
        endorsement == "Yes",
        1,
        ifelse(
            endorsement == "No",
            0,
            NA
        )
    )
)

# Calculate pure summary statistics.
summary_statistics <- endorsements %>%
    group_by(category, candidate) %>%
    summarize(
        p_non_missing = mean(!is.na(endorsement)),
        p_yes = mean(endorsement, na.rm = TRUE),
        n = sum(!is.na(endorsement))
    )

p_yes <- summary_statistics %>%
    mutate(
        se = (p_yes * (1 - p_yes)) / sqrt(n),
        lower = p_yes - 2 * se,
        upper = p_yes + 2 * se
    )

# Plot probabilities of endorsement for propositions.
p <- ggplot(
    p_yes %>% filter(category %in% props),
    aes(x = reorder(candidate, p_yes), y = p_yes)
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0.5, alpha = 0.3) +
    xlab("") +
    ylab("Probability of Positive Endorsement") +
    coord_flip() +
    theme_bw() +
    theme(text = element_text(family = "mono"))

# Save the plot to a PNG file.
ggsave(
    file.path("ideal_points", "props_endorsements.png"),
    height = 10,
    width = 14
)

# Plot probabilities of endorsement for people.
p <- ggplot(
    p_yes %>% filter(!(category %in% props)),
    aes(x = reorder(candidate, p_yes), y = p_yes)
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0.5, alpha = 0.3) +
    xlab("") +
    ylab("Probability of Positive Endorsement") +
    coord_flip() +
    theme_bw() +
    theme(text = element_text(family = "mono"))

# Save the plot to a PNG file.
ggsave(
    file.path("ideal_points", "people_endorsements.png"),
    height = 10,
    width = 14
)

# Translate the long-form data set into a matrix where endorsers are rows and
# candidates are columns. We incporate category labels into the column names to
# distinguish repeated candidates such as Scott Wiener and Jane Kim, who are up
# for election to multiple offices.
wide_endorsements <- dcast(
    endorsements,
    endorser ~ category + candidate,
    value.var = "endorsement"
)

# Store these constants for use downstream.
n_endorsers <- nrow(wide_endorsements)
n_candidates <- ncol(wide_endorsements) - 1

###############################################################################
#
# Compute ideal points using Stan.
#
###############################################################################

# Convert the endorsements data.frame into a matrix after removing the names of
# the endorsers.
M <- as.matrix(wide_endorsements[, (1 + 1):(n_candidates + 1)])

# Convert the standard dense matrix ito a COO format using three vectors.
i <- rep(1:n_endorsers, times = n_candidates)
j <- rep(1:n_candidates, each = n_endorsers)
v <- as.vector(M)

# Stan does not support NA's in its input data, so we drop those entries.
non_missing_inds <- which(!is.na(v))
i <- i[non_missing_inds]
j <- j[non_missing_inds]
v <- v[non_missing_inds]

# We'll use these functions to initialize the intercept-like parameters.
logit <- function (p) {log(p / (1 - p))}
bound_probs <- function (p) {
    n <- length(p)
    return(pmin(pmax(p, rep(0.001, n)), rep(0.999, n)))
}

# Compute 1-dimensional ideal points using Stan.
res <- stan(
    file = file.path("stan_code", "ideal_points.stan"),
    data = list(
        n_rows = n_endorsers,
        n_cols = n_candidates,
        n_obs = length(v),
        i = i,
        j = j,
        v = v
    ),
    iter = mcmc_iter,
    warmup = 100 * mcmc_thin,
    chains = 1,
    thin = mcmc_thin,
    init = list(
        list(
            a = logit(bound_probs(rowMeans(M, na.rm = TRUE))),
            x = rnorm(n_endorsers, 0, 1),
            b = logit(bound_probs(colMeans(M, na.rm = TRUE))),
            y = rnorm(n_candidates, 0, 1)
        )
    )
)

# Extract parameters from the Stan results.
params <- summary(res)$summary

# The params data.frame has idiosyncratic row names we'll use for indexing.
a_inds <- paste0("a[", 1:n_endorsers, "]")
b_inds <- paste0("b[", 1:n_candidates, "]")
x_inds <- paste0("x[", 1:n_endorsers, "]")
y_inds <- paste0("y[", 1:n_candidates, "]")

# Store ideal points for endorsers.
ideal_points_endorsers <- data.frame(
    endorser = wide_endorsements[, 1],
    mean = params[x_inds, 1],
    lower = params[x_inds, 4],
    upper = params[x_inds, 8],
    stringsAsFactors = FALSE
)

# Save the ideal points to a CSV file.
write.csv(
    ideal_points_endorsers %>% arrange(mean),
    file = file.path("ideal_points", "endorsers.csv"),
    row.names = FALSE
)

# Plot ideal points for endorsers.
p <- ggplot(
    ideal_points_endorsers,
    aes(x = reorder(endorser, mean), y = mean)
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, alpha = 0.3) +
    xlab("") +
    ylab("Ideal Point") +
    coord_flip() +
    theme_bw() +
    theme(text = element_text(family = "mono"))

# Save the plot to a PNG file.
ggsave(
    file.path("ideal_points", "endorsers.png"),
    height = 14,
    width = 10
)

# TODO: Do this separately for different categories of candidates.
# Thenames of the wide_endorsements columns are a mixture of category
# information and candidate names, so we split them apart again in their
# official order.
candidate_names <- names(wide_endorsements)[(1 + 1):(n_candidates + 1)]
candidate_categories <- sapply(
    strsplit(candidate_names, "_"),
    function (pair) {pair[1]}
)
candidate_names <- sapply(
    strsplit(candidate_names, "_"),
    function (pair) {pair[2]}
)

# Store ideal points for candidates.
ideal_points_candidates <- data.frame(
    category = candidate_categories,
    candidate = candidate_names,
    mean = params[y_inds, 1],
    lower = params[y_inds, 4],
    upper = params[y_inds, 8],
    stringsAsFactors = FALSE
)

# Save the ideal points to a CSV file.
write.csv(
    ideal_points_candidates %>% arrange(mean),
    file = file.path("ideal_points", "candidates.csv"),
    row.names = FALSE
)

# Make pretty-printable names by padding strings before concatenating them.
longest_category_name <-  with(
    ideal_points_candidates,
    max(str_length(category))
)

longest_candidate_name <-  with(
    ideal_points_candidates,
    max(str_length(candidate))
)

ideal_points_candidates <- transform(
    ideal_points_candidates,
    full_name = paste(
        str_pad(
            category,
            longest_category_name,
            side = "right",
            pad = " "
        ),
        str_pad(
            candidate,
            longest_candidate_name,
            side = "left",
            pad = " "
        ),
        sep = " "
    )
)

# Plot ideal points for candidates.
p <- ggplot(
    ideal_points_candidates %>% filter(!(category %in% props)),
    aes(
        x = reorder(full_name, mean),
        y = mean
    )
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, alpha = 0.3) +
    xlab("") +
    ylab("Ideal Point") +
    coord_flip() +
    theme_bw() +
    theme(text = element_text(family = "mono")) +
    scale_color_manual(values = c("#7570b3", "#1b9e77", "#000000"))

# Save the plot to a PNG file.
ggsave(
    file.path("ideal_points", "people.png"),
    height = 14,
    width = 14
)

# Plot ideal points for props.
p <- ggplot(
    ideal_points_candidates %>% filter(category %in% props),
    aes(
        x = reorder(candidate, mean),
        y = mean
    )
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, alpha = 0.3) +
    xlab("") +
    ylab("Ideal Point") +
    coord_flip() +
    theme_bw() +
    theme(text = element_text(family = "mono"))

# Save the plot to a PNG file.
ggsave(
    file.path("ideal_points", "props.png"),
    height = 10,
    width = 14
)

# Binarize for simplicity
ideal_points_endorsers <- transform(
    ideal_points_endorsers,
    side = ifelse(
        mean < median(mean),
        "Faction 1",
        "Faction 2"
    )
)

tmp <- inner_join(
    ideal_points_endorsers %>% select(endorser, side),
    endorsements,
    by = "endorser"
)

tmp <- tmp %>%
    group_by(category, candidate) %>%
    summarize(
        p_yes = round(100 * mean(endorsement, na.rm = TRUE)),
        p_yes_faction_1 = round(100 * mean(
            ifelse(side == "Faction 1", endorsement, NA),
            na.rm = TRUE
        )),
        p_yes_faction_2 = round(100 * mean(
            ifelse(side == "Faction 2", endorsement, NA),
            na.rm = TRUE
        ))
    ) %>%
    ungroup() %>%
    select(-category) %>%
    as.data.frame

write.csv(
    tmp,
    file = file.path("ideal_points", "results.csv"),
    row.names = FALSE
)
