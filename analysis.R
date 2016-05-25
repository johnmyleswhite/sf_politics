###############################################################################
#
# Set up everything for analysis.
#
###############################################################################

# We use the following packages:
library("ggplot2")
library("dplyr")
library("reshape2")
library("pscl")

# Load the raw data in long form.
endorsements <- read.csv("endorsements.csv", stringsAsFactors = FALSE)

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

###############################################################################
#
# Compute ideal points.
#
###############################################################################

# Convert the endorsements data.frame into a matrix after removing the names of
# the endorsers.
M <- as.matrix(wide_endorsements[, 2:ncol(wide_endorsements)])

# Compute 1-dimensional ideal points.
res <- ideal(
    rollcall(M),
    d = 1,
    dropList = list(
        codes = "notInLegis",
        lop = 0
    ),
    impute = TRUE,
    normalize = TRUE,
    maxiter = 250000,
    store.item = TRUE,
    verbose = FALSE
)

# Unanimous candidates don't get ideal points, so we find the subset of names
# that will have ideal points.
# TODO: Remove hardcoded 70.
# TODO: Prettify names.
valid_names <- names(wide_endorsements)[2:70][
    apply(M, 2, function (col) {var(col, na.rm = TRUE)}) > 0
]

# Store ideal points for endorsers.
ideal_points <- data.frame(
    endorser = wide_endorsements$endorser,
    lower = apply(res$x[, , 1], 2, function (v) {quantile(v, 0.025)}),
    mean = apply(res$x[, , 1], 2, mean),
    upper = apply(res$x[, , 1], 2, function (v) {quantile(v, 1 - 0.025)})
)

# Plot ideal points for endorsers.
ggplot(
    ideal_points,
    aes(x = reorder(endorser, mean), y = mean)
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    xlab("") +
    ylab("Ideal Point") +
    coord_flip() +
    theme_bw()
ggsave("endorsers.png", height = 14, width = 10)

# Store ideal points for candidates.
ideal_points_v2 <- data.frame(
    endorser = valid_names,
    lower = apply(res$beta[, , 1], 2, function (v) {quantile(v, 0.025)}),
    mean = apply(res$beta[, , 1], 2, mean),
    upper = apply(res$beta[, , 1], 2, function (v) {quantile(v, 1 - 0.025)})
)

# Plot ideal points for candidates.
ggplot(
    ideal_points_v2,
    aes(x = reorder(endorser, mean), y = mean)
) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    xlab("") +
    ylab("Ideal Point") +
    coord_flip() +
    theme_bw()
ggsave("candidates.png", height = 14, width = 10)

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
