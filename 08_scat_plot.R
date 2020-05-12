# plot scatter points(pred vs reference value)

rm(list=ls())

library(ggplot2)

rmse <- function(x,y)
{
  sqrt(mean((x-y)^2))
}

r2 <- function(x,y)
{
  cor(x,y)^2
}

## func_eq_r2(m) - Return R2 equations, cut to 2-digt ####
func_eq_r2 = function(m){
  r2 = sprintf('%.2f',summary(m)$r.squared)  
  eq <- substitute(italic(R)^2 == r2)
  as.character(as.expression(eq)) 
}

### get_scatplot(x,y) - scatter plot using ggplot2 and showing rmse, r2, n####
# returned: ggplot object
#   show: N, R2, RMSE, MBE
get_scatplot <- function(x,y,x_label,y_label,x_max,y_max,p_title,theme_type,text_pos)  {
  p_data <- data.frame(x,y)
  diff = p_data$y - p_data$x
  p_RMSE=round(sqrt(mean(diff^2)),2)
  p_nS=dim(p_data)[1]
  # x_mean = mean(p_data$x,na.rm=T)
  # p_CV_RMSD = p_RMSE/x_mean
  # p_MBE = (sum(diff,na.rm=T))/p_nS 
  
  p_model=lm(p_data$y~p_data$x)
  p_R2=p_model$r.squared  
  
  x_lim=c(0,x_max)
  y_lim=c(0,y_max)
  y_step=y_max/10
  p2 <- ggplot(aes(y=y,x=x),data=p_data) +
    xlab(x_label) +  ylab(y_label) +
    xlim(x_lim) + ylim(y_lim)	+
    ggtitle(p_title) + 
    theme(text=element_text(size=20))   
  
  if(theme_type=='bk'){   
    p2 <- p2 + geom_point(alpha=0.5,size=2,show.legend=NA) + theme_bw() + geom_blank() 
  }else{    p2 <- p2 + geom_point(alpha=0.5,size=2)  }
  
  if(text_pos=='upleft'){ ## text output on the upper-left
    p2 <- p2 +	
      geom_text(x=0,y=y_max-y_step,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
      geom_text(x=0,y=y_max-y_step*2,label=func_eq_r2(p_model),hjust=0,parse=TRUE,size=5) + # R2
      geom_text(x=0,y=y_max-y_step*3,label=paste('RMSE = ',sprintf('%.1f',p_RMSE),sep=''),hjust=0,size=5)   # RMSE
      # geom_text(x=0,y=y_max-y_step*4,label=paste('MBE = ',sprintf('%.1f',p_MBE),sep=''),hjust=0,size=5)     # MB
  }else if(text_pos=='upright'){ ## text output on the upper-right
    p2 <- p2 +	
      geom_text(x=x_max*0.75,y=y_max-y_step,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
      geom_text(x=x_max*0.75,y=y_max-y_step*2,label=func_eq_r2(p_model),hjust=0,parse=TRUE,size=5) + # R2
      geom_text(x=x_max*0.75,y=y_max-y_step*3,label=paste('RMSE = ',sprintf('%.1f',p_RMSE),sep=''),hjust=0,size=5)  # RMSE
      # geom_text(x=x_max*0.75,y=y_max-y_step*4,label=paste('MBE = ',sprintf('%.1f',p_MBE),sep=''),hjust=0,size=5)     # MB    
  }
  
  p2 <- p2 + 
    geom_abline(intercept=0,colour='red',size=1) 	 # 1:1 line	
  # + geom_smooth(data=p_data, method='rlm',colour='blue',linetype='dashed',size=1,se=FALSE,fullrange=T)
  
  return(p2)
}


### 0 prep data
data <- read.csv(file="./merged_data.csv", head=T)
data <- na.omit(data)
data <- data[data$AveAGB<250,]

set.seed(1)
ind <- sample(2, nrow(data), replace=T,prob = c(0.7,0.3))
data.train <- data[ind==1,]
data.test <- data[ind==2,]

path.plots = 'C:/MyFiles/Research/GraduationDesign/Model/AGBProj/Plots'

### 1.LM
### 1.1 train
load("sel.lm.Rdata")
model.lm<-lm(AveAGB~.,data = data.train[c("AveAGB",sel.lm)])
pred.train <- predict(model.lm,data.train)
field.train <- data.train$AveAGB

x_label="参考值（Mg/ha）"
y_label="估测值（Mg/ha）"
x_max=250; y_max=250
p_title="线性回归"
theme_type='bk'
text_pos="upleft"

p_lm_tr <- get_scatplot(field.train,pred.train,x_label,y_label,x_max,y_max,p_title,theme_type,text_pos)
p_lm_tr
#### 1.2 test
pred.test <- predict(model.lm,data.test)
field.test <- data.test$AveAGB


p_lm_ts <- get_scatplot(field.test,pred.test,x_label,y_label,x_max,y_max,p_title,theme_type,text_pos)
p_lm_ts

ggsave(p_lm_tr,file=paste(path.plots,'lm_tr.jpg',sep='/'),width=5,height=5,units='in')
ggsave(p_lm_ts,file=paste(path.plots,'lm_ts.jpg',sep='/'),width=5,height=5,units='in')


### 2.RF
### 2.1 train
load("sel.rf.Rdata")
library("randomForest")
model.rf<-randomForest(AveAGB~.,data = data.train[c("AveAGB",sel.rf)],ntree=500,mtry=9)
pred.train <- predict(model.rf,data.train)
field.train <- data.train$AveAGB

x_label="参考值（Mg/ha）"
y_label="估测值（Mg/ha）"
x_max=250; y_max=250
p_title="随机森林"
theme_type='bk'
text_pos="upleft"

p_rf_tr <- get_scatplot(field.train,pred.train,x_label,y_label,x_max,y_max,p_title,theme_type,text_pos)
p_rf_tr
#### 2.2 test
pred.test <- predict(model.rf,data.test)
field.test <- data.test$AveAGB

p_rf_ts <- get_scatplot(field.test,pred.test,x_label,y_label,x_max,y_max,p_title,theme_type,text_pos)
p_rf_ts

ggsave(p_rf_tr,file=paste(path.plots,'rf_tr.jpg',sep='/'),width=5,height=5,units='in')
ggsave(p_rf_ts,file=paste(path.plots,'rf_ts.jpg',sep='/'),width=5,height=5,units='in')


### 3.SVM
### 3.1 train
library("e1071")
model.svm<-svm(AveAGB~.,data = data.train[c("AveAGB",sel.rf)],epsilon=0.61,cost=4)
pred.train <- predict(model.svm,data.train)
field.train <- data.train$AveAGB

x_label="参考值（Mg/ha）"
y_label="估测值（Mg/ha）"
x_max=250; y_max=250
p_title="支持向量回归"
theme_type='bk'
text_pos="upleft"

p_svm_tr <- get_scatplot(field.train,pred.train,x_label,y_label,x_max,y_max,p_title,theme_type,text_pos)
p_svm_tr
#### 3.2 test
pred.test <- predict(model.svm,data.test)
field.test <- data.test$AveAGB

p_svm_ts <- get_scatplot(field.test,pred.test,x_label,y_label,x_max,y_max,p_title,theme_type,text_pos)
p_svm_ts

ggsave(p_svm_tr,file=paste(path.plots,'svm_tr.jpg',sep='/'),width=5,height=5,units='in')
ggsave(p_svm_ts,file=paste(path.plots,'svm_ts.jpg',sep='/'),width=5,height=5,units='in')

