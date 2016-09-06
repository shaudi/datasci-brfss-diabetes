# preliminaries
library(dplyr)
library(cluster)
library(ggplot2)
library(Rtsne)
library(maps)

# for reproducibility
set.seed(1981)

# prepare data for cluster analysis
load("../data/brfssdata.rda")
glimpse(brfssdata)

# focus on random subset of people with diabetes (total = ~180,000 people)
nsamples <- 5000
diabetic_data <- brfssdata[brfssdata$DIABETE3 == 1,]
diabetic_sample <- diabetic_data[sample(dim(diabetic_data)[1], nsamples),]

# compute distance metric for mixed data types (i.e., Gower distance)
distance <- daisy(diabetic_sample[,3:18], metric = "gower", type = list(asymm=c("CVDINFR4","CVDSTRK3")))
distance <- as.matrix(distance)

# check everythin is working: examine the nearest and furthest pair
diabetic_sample[which(distance == min(distance[distance != min(distance)]), arr.ind=TRUE)[1,],]
diabetic_sample[which(distance == max(distance[distance != max(distance)]), arr.ind=TRUE)[1,],]

# find best no. of clusters
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(distance,diss=TRUE,k=i)
  sil_width[i] <- pam_fit$silinfo$avg.width
  }
plot(1:10, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:10, sil_width)
pam_fit <- pam(distance, diss = TRUE, k = 3)

# print summary of each cluster
pam_results <- diabetic_sample %>%
  dplyr::select(-X_STATE) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# plot clusters in lower dimensional space
tsne_obj <- Rtsne(distance, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering), name = diabetic_sample$X_STATE)
png("../figures/figure3_cluster_analysis.png", height=500, width=600)
ggplot(aes(x=X, y=Y), data = tsne_data) + geom_point(aes(color=cluster), cex = 1.3)
dev.off()

# save to file
save(tsne_data, distance, diabetic_sample, file = "../data/brfss_clusterdata.rda")
