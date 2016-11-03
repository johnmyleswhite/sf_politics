setwd("~/Tsne_stuff/sf_politics/2016_November/data")
require(dplyr)
require(Matrix)

df <- read.csv("endorsements.csv", stringsAsFactors=FALSE)

df$endorse_numeric <- ifelse(df$endorsement == 'Yes', 1, 0)

endorser_index <- df %>%
  group_by(endorser) %>%
  summarise(cnt=n()) %>%
  ungroup()

endorser_index$x_index <- c(1:nrow(endorser_index))

candidate_index <- df %>%
  group_by(candidate) %>%
  summarise(cnt2=n()) %>%
  ungroup()

candidate_index$y_index <- c(1:nrow(candidate_index))

df.full <- df %>% inner_join(endorser_index) %>% inner_join(candidate_index)

the_matrix <- sparseMatrix(i = df.full$x_index, j = df.full$y_index, x=df.full$endorse_numeric)
the_matrix[is.na(the_matrix)] <- 0
the_matrix <- as.matrix(the_matrix)

rownames(the_matrix) <- endorser_index$endorser
colnames(the_matrix) <- candidate_index$candidate

#Check dimensionality
require(bcv)
cv <- cv.svd.gabriel(the_matrix)
cv.wold <- cv.svd.wold(the_matrix)

require(tsne)
endorser_tsne <- tsne(the_matrix, perplexity=6, max_iter=2000, initial_dim=5)
qplot(x=endorser_tsne[,1],
      y=endorser_tsne[,2],
      geom='text',
      label=rownames(the_matrix))


candidate_tsne <- tsne(t(the_matrix), perplexity=20, max_iter=3000, initial_dim=5)
qplot(x=candidate_tsne[,1],
      y=candidate_tsne[,2],
      geom='text',
      label=colnames(the_matrix))