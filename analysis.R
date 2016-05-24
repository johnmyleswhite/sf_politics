###############################################################################
#
# Setup everything for analysis.
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

# The pscl package requires that we place our matrix into a rollcall object.
rcall <- rollcall(M)

# Compute 1-dimensional ideal points.
res <- ideal(rcall, d = 1, impute = TRUE, normalize = TRUE)

# Display the ideal points for endorsers.
data.frame(
    endorser = wide_endorsements$endorser,
    ideal_point = res$xbar[, 1]
) %>%
    arrange(ideal_point)

# Perform a similar analysis but focus attention on the candidates.
rcall <- rollcall(t(M))

# Again, compute 1-dimensional ideal points.
res <- ideal(rcall, d = 1, impute = TRUE, normalize = TRUE)

# Display the ideal points for candidates.
data.frame(
    candidate = row.names(t(M)),
    ideal_point = res$xbar[, 1]
) %>%
    arrange(ideal_point)

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
