rm(list = ls())

# 1 divide train and test sets ####
# set seed for reproductivity
set.seed(1234) 
plots <- read.csv(file="./plots.csv", head=T)
# remove possible outliners(6 plots removed using 500 as threshold)
summary(plots$AveAGB)
sort(plots$AveAGB)
plots <- plots[plots$AveAGB<300,]
# sample data by "FIDs" so the index could be applied to different files
ind <- sample(2, nrow(plots), replace=TRUE, prob=c(0.7, 0.3))
trainIDs <- plots[ind==1,]$FIDs
testIDs <- plots[ind==2,]$FIDs

# 2 select features ####
library(VSURF)
## 2.2 landsat test####
# get merged sample data
l8 <- read.csv(file="./l8.csv", head=T)
l8_all <- merge(plots,l8,by="FIDs")
# get train and test sets
l8_train <- l8_all[l8_all$FIDs%in%trainIDs,]
l8_test <- l8_all[l8_all$FIDs%in%testIDs,]
# remove variables not used in selection
colnames(l8_all)
colnames(l8_all[c(1:12,14:16,103:105)])
l8_train[c(1:12,14:16,103:105)] <- NULL;colnames(l8_train)
l8_test[c(1:12,14:16,103:105)] <- NULL;colnames(l8_test)
# quick summary
summary(l8_all$AveAGB)
summary(l8_train$AveAGB)
summary(l8_test$AveAGB)
# invoke VSURF
xs <- l8_train[2:ncol(l8_train)]
y <- l8_train[[1]]
l8.vsurf <- VSURF(xs,y,parallel=TRUE)
# quick summary
summary(l8.vsurf)
# quick plot
plot(l8.vsurf,var.names = TRUE)
# get variables selected in 3rd step(pred)
l8.vsurf$varselect.pred
names(xs[l8.vsurf$varselect.pred])
l8_selected_train <- l8_train[c("AveAGB",names(xs[l8.vsurf$varselect.pred]))]
l8_selected_test <- l8_test[c("AveAGB",names(xs[l8.vsurf$varselect.pred]))]
# save results
save(l8.vsurf, file="l8.vsurf.Rdata")
save(l8_selected_train, file="l8_selected_train.Rdata")
save(l8_selected_test, file="l8_selected_test.Rdata")

## 2.3 sentinel-1 test####
# get merged sample data
s1 <- read.csv(file="./s1.csv", head=T)
s1_all <- merge(plots,s1,by="FIDs")
# get train and test sets
s1_train <- s1_all[s1_all$FIDs%in%trainIDs,]
s1_test <- s1_all[s1_all$FIDs%in%testIDs,]
# remove variables not used in selection
colnames(s1_all)
colnames(s1_all[c(1:12,14:16)])
# s1_train$AveAGB <- s1_train$AveHt.1
# s1_test$AveAGB <- s1_test$AveHt.1 fake AGB test
s1_train[c(1:12,14:16)] <- NULL;colnames(s1_train)
s1_test[c(1:12,14:16)] <- NULL;colnames(s1_test)
# quick summary
summary(s1_all$AveAGB)
summary(s1_train$AveAGB)
summary(s1_test$AveAGB)
# invoke VSURF
xs <- s1_train[2:ncol(s1_train)]
y <- s1_train[[1]]
s1.vsurf <- VSURF(xs,y,parallel=TRUE) 
# quick summary
summary(s1.vsurf)
# quick plot
plot(s1.vsurf,var.names = TRUE)
# get variables selected in 3rd step(pred)
s1.vsurf$varselect.pred
names(xs[s1.vsurf$varselect.pred])
s1_selected_train <- s1_train[c("AveAGB",names(xs[s1.vsurf$varselect.pred]))]
s1_selected_test <- s1_test[c("AveAGB",names(xs[s1.vsurf$varselect.pred]))]
# save results
save(s1.vsurf, file="s1.vsurf.Rdata")
save(s1_selected_train, file="s1_selected_train.Rdata")
save(s1_selected_test, file="s1_selected_test.Rdata")


## 2.4 palsar-2 test####
# get merged sample data
p2 <- read.csv(file="./p2.csv", head=T)
p2_all <- merge(plots,p2,by="FIDs")
# get train and test sets
p2_train <- p2_all[p2_all$FIDs%in%trainIDs,]
p2_test <- p2_all[p2_all$FIDs%in%testIDs,]
# remove variables not used in selection
colnames(p2_all)
colnames(p2_all[c(1:12,14:16)])
p2_train[c(1:12,14:16)] <- NULL;colnames(p2_train)
p2_test[c(1:12,14:16)] <- NULL;colnames(p2_test)
# quick summary
summary(p2_all$AveAGB)
summary(p2_train$AveAGB)
summary(p2_test$AveAGB)
# invoke VSURF
xs <- p2_train[2:ncol(p2_train)]
y <- p2_train[[1]]
p2.vsurf <- VSURF(xs,y,parallel=TRUE)
# quick summary
summary(p2.vsurf)
# quick plot
plot(p2.vsurf,var.names = TRUE)
# get variables selected in 3rd step(pred)
p2.vsurf$varselect.pred
names(xs[p2.vsurf$varselect.pred])
p2_selected_train <- p2_train[c("AveAGB",names(xs[p2.vsurf$varselect.pred]))]
p2_selected_test <- p2_test[c("AveAGB",names(xs[p2.vsurf$varselect.pred]))]
# save results
save(p2.vsurf, file="p2.vsurf.Rdata")
save(p2_selected_train, file="p2_selected_train.Rdata")
save(p2_selected_test, file="p2_selected_test.Rdata")

