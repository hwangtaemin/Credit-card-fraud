library(ggplot2)
#kaggle - credit card fraud
credit <- read.csv("creditcard.csv")

# Performance Evaluation Function
perf_eval2 <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Initialize the performance matrix
perf_mat <- matrix(0, 1, 6)
colnames(perf_mat) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression"

input_idx <- c(1:30)
target_idx <- 31
credit_input <- credit[,input_idx]
credit_target <- credit[,target_idx]

set.seed(12345)
trn_idx <- sample(1:nrow(credit), round(0.7*nrow(credit)))
credit_trn <- credit[trn_idx,]
credit_tst <- credit[-trn_idx,]

full_lr <- glm(Class ~ ., family=binomial, credit_trn)
summary(full_lr)

lr_response <- predict(full_lr, type = "response", newdata = credit_tst)
lr_target <- credit_tst$Class
response_table <- data.frame(lr_response,lr_target)

lr_predicted1 <- rep(0, length(lr_target))
lr_predicted1[which(lr_response >= 0.3)] <- 1
cm_full1 <- table(lr_target, lr_predicted1)
cm_full1

perf_mat[1,] <- perf_eval2(cm_full1)
perf_mat

ROC <- function(prob_table){
  prob_table <- prob_table[order(-prob_table[,1]),]
  ROC_TPR <- rep(0,nrow(prob_table))
  ROC_FPR <- rep(0,nrow(prob_table))
  ROC_actualNG <- length(which(prob_table[,2]==1))
  ROC_actualG <- length(which(prob_table[,2]==0))
  NG_count <- 0
  G_count <- 0
  ROC_table <- data.frame(prob_table, ROC_TPR, ROC_FPR)
  for(i in (1:nrow(ROC_table))){
    if(ROC_table[i,2]==1){
      NG_count = NG_count + 1
      ROC_table[i,3] = NG_count/ROC_actualNG
    }
    else
      NG_count = NG_count
    ROC_table[i,3] = NG_count/ROC_actualNG
  }
  for(i in (1:nrow(ROC_table))){
    if(ROC_table[i,2]==0){
      G_count = G_count + 1
      ROC_table[i,4] = G_count/ROC_actualG
    }
    else
      G_count = G_count
    ROC_table[i,4] = G_count/ROC_actualG
  }
  return(ROC_table)
}
AUROC <-function(roc_table){
  ROC_actualG <- length(which(roc_table[,2]==0))
  AUROC <- 0
  for(i in (1:(nrow(roc_table)-1)))
    if(roc_table[i,4]!=roc_table[i+1,4])
      AUROC = AUROC + roc_table[i,3]*(1/ROC_acturealG)
  else
    AUROC = AUROC
  return(AUROC)
}
roc_table <- ROC(response_table)
AUROC(roc_table)

ggplot(data=roc_table,aes(x=roc_table[,4],y=roc_table[,3]))+geom_path()+
  labs(x="1-specificity",y="sensitivity",title="ROC curve of count")
