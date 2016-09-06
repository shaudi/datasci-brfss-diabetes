# preliminaries
library(ROCR)
library(glmnet)
library(caret)

# for reproducibility
set.seed(1981)

# load the preprocessed BRFSS data
load("../data/brfssdata.rda")

# prepare data for use with glmnet (i.e., create dummy variables where appropriate)
f = as.formula(paste("~", paste(vlist[3:18], collapse = "+")))
X <- model.matrix(f, brfssdata) #predictor matrix
y <- brfssdata$DIABETE3 #outcome
Xy <- cbind(X,y)

# check that dummy encoding performed appropriately
head(Xy) 
dim(Xy)

# split into training/test sets
split <- .65
randInd = runif(nrow(Xy))
train <- Xy[(randInd <= split), ]
test <- Xy[(randInd > split), ]

# prepare variables for logistic regression
k <- 10 #no. cross-validation folds
lseq <- c(.0001, .001, .01, .1, 1, 10) #regularization weights to search through

# fit standard/extended models to training dataset using k-fold cross-validation procedure
smodel <- cv.glmnet(x = train[,1:5], y = train[,35], alpha = 1, lambda = lseq, nfolds = k, 
                         family = 'binomial', type.measure = 'auc')
emodel <- cv.glmnet(x = train[,1:34], y = train[,35], alpha = 1, lambda = lseq, nfolds = k,
                         family = 'binomial', type.measure = 'auc')
#check lambda min.....

# select model with best lambda and apply to test dataset
smodel.prob <- predict(smodel,type="response", newx = test[,1:5], s = 0.001)#'lambda.min')
emodel.prob <- predict(emodel,type="response", newx = test[,1:34], s = 0.001)# 'lambda.min')

# calculate true/false positive probabilities and AUC
smodel.pred <- prediction(smodel.prob, test[,35])
emodel.pred <- prediction(emodel.prob, test[,35])
smodel.perf <- performance(smodel.pred, "tpr", "fpr")
emodel.perf <- performance(emodel.pred, "tpr", "fpr")
performance(smodel.pred, "auc")
performance(emodel.pred, "auc")

# plot ROC curve
png("../figures/figure2_model_fitting.png", height=450, width=450)
par(pty="s")
plot(smodel.perf,colorize=FALSE, col="steelblue", lwd = 3)
legend(0.35, 0.35, c("Standard", "Extended"), lty=1, col=c("steelblue","green4"),
       bty='n', cex=1)
par(new=TRUE)
plot(emodel.perf,colorize=FALSE, col="green4", lwd = 3)
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
par(new=FALSE)
dev.off()

# print model pars
coef(smodel)
coef(emodel)

# save to file
save(smodel, emodel, file = "../data/brfss_modeldata.rda")
