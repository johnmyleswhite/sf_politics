###############################################################################
#
# Set up everything for ideal point analysis.
#
###############################################################################

# We use the following packages.
library("ggplot2")
library("dplyr")
library("reshape2")
library("pscl")
library("stringr")

# Set a flag to determine whether we perform computationally intensive and
# exact calculations or use faster approaches during interactive development.
interactive_mode <- TRUE

# In interactive development, we do very light MCMC computations even though
# the resulting estimates are quite bad. For production results, we do much
# more computation.
if (interactive_mode) {
    mcmc_iter <- 10000
    mcmc_thin <- 100
} else {
    mcmc_iter <- 10000000
    mcmc_thin <- 1000
}

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
        ifelse(endorsement == "No", 0, NA)
    )
)

# Translate the long-form data set into a matrix where endorsers are rows and
# candidates are columns. We use categories to distinguish repeated candidates
# such as Scott Wiener and Jane Kim, who are up for election to multiple
# offices.
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
# Compute ideal points.
#
###############################################################################

# Convert the endorsements data.frame into a matrix after removing the names of
# the endorsers.
M <- as.matrix(wide_endorsements[, (1 + 1):(n_candidates + 1)])

# Compute 1-dimensional ideal points.
res <- ideal(
    rollcall(M),
    d = 1,
    impute = TRUE,
    normalize = TRUE,
    store.item = TRUE,
    maxiter = mcmc_iter,
    thin = mcmc_thin
)

# Unanimous candidates don't get ideal points, so we find the subset of names
# that will have ideal points by looking for candidates that had variance
# in the endorsements they elicited.
has_variance <- apply(M, 2, function (col) {var(col, na.rm = TRUE)}) > 0
candidate_names <- names(wide_endorsements)[(1 + 1):(n_candidates + 1)]
valid_names <- candidate_names[has_variance]

# The names we're using are a mixture of category information and genuine
# names, so we split them apart again.
candidate_categories <- sapply(
    strsplit(valid_names, "_"),
    function (pair) {pair[1]}
)
candidate_names <- sapply(
    strsplit(valid_names, "_"),
    function (pair) {pair[2]}
)

# Store ideal points for endorsers.
ideal_points_endorsers <- data.frame(
    endorser = wide_endorsements$endorser,
    lower = apply(res$x[, , 1], 2, function (v) {quantile(v, 0.025)}),
    mean = apply(res$x[, , 1], 2, mean),
    upper = apply(res$x[, , 1], 2, function (v) {quantile(v, 1 - 0.025)})
)

# Save the ideal points to a CSV file.
write.csv(
    ideal_points_endorsers,
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
    geom_hline(xintercept = 0, alpha = 0.3) +
    xlab("") +
    ylab("Ideal Point") +
    coord_flip() +
    theme_bw()

# Save the plot to a PNG file.
ggsave(
    file.path("ideal_points", "endorsers.png"),
    height = 14,
    width = 10
)

# Store ideal points for candidates.
ideal_points_candidates <- data.frame(
    category = candidate_categories,
    name = candidate_names,
    lower = apply(res$beta[, , 1], 2, function (v) {quantile(v, 0.025)}),
    mean = apply(res$beta[, , 1], 2, mean),
    upper = apply(res$beta[, , 1], 2, function (v) {quantile(v, 1 - 0.025)})
)

# Save the ideal points to a CSV file.
write.csv(
    ideal_points_candidates,
    file = file.path("ideal_points", "candidates.csv"),
    row.names = FALSE
)

# Plot ideal points for candidates.
p <- ggplot(
    ideal_points_candidates,
    aes(
        x = reorder(paste(category, name, sep = " - "), mean),
        y = mean
    )
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_hline(xintercept = 0, alpha = 0.3) +
    xlab("") +
    ylab("Ideal Point") +
    coord_flip() +
    theme_bw()

# Save the plot to a PNG file.
ggsave(
    file.path("ideal_points", "candidates.png"),
    height = 14,
    width = 10
)

###############################################################################
#
# Use k-means clustering to think about the number of distinct slate cards.
#
###############################################################################

# Make a copy of the data before we mutate it.
M2 <- M

# Impute missing values so that k-means will work better.
M2[is.na(M2)] <- mean(M2, na.rm = TRUE)

# Use 8 clusters
k <- 8

# Display the clusters that each endorser is assigned to.
data.frame(
    endorser = wide_endorsements$endorser,
    cluster = kmeans(M2, k)$cluster
) %>%
    arrange(cluster)
