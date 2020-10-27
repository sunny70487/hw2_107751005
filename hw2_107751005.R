args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target bad|good --badthre <bad-threthold> --input method1.csv method2.csv ... methodn.csv --output out.csv", call.=FALSE)
}
# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--badthre"){
    thre<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))
#set null list
d <- list()
s <- 0
j <- list()
h <- list()
l <- list()
pred_label <- list()
cf <- list()
tp <- list()
tn <- list()
fp <- list()
fn <- list()
precision <- list()
sensitivity <- list()
specificity <- list()
f1_score <- list()
logli_model <- list()
pNull <- list()
logli_nullModel <- list()
pseudo_R_squared <- list()
mydata <- list()
df <- data.frame()
for(i in files){
  h[[i]]<-gsub(".csv", "", basename(i)) #for index name
  d[[i]] <- read.csv(i,header=T) #read specific file
  j[[i]] <- ifelse(d[[i]]$reference==query_m,1,0) #transformation reference for numeric
  pred_label[[i]] <- as.integer(d[[i]]$pred.score >thre) #set threthold
  cf[[i]] <- table(pred_label[[i]], j[[i]]) #confusion matrix
  tp[[i]] <- cf[[i]][2, 2]
  tn[[i]] <- cf[[i]][1, 1]
  fp[[i]] <- cf[[i]][2, 1]
  fn[[i]] <- cf[[i]][1, 2]
  precision[[i]] <- (tp[[i]] )/(tp[[i]] + fp[[i]]) #precision
  sensitivity[[i]] <- tp[[i]]/(tp[[i]] + fn[[i]]) #sensitivity(recall)
  specificity[[i]] <- tn[[i]]/(tn[[i]] + fp[[i]]) #specificity
  f1_score[[i]] <- (2*precision[[i]]*sensitivity[[i]])/(precision[[i]]+sensitivity[[i]]) #F1_score
  logli_model[[i]] <- sum(ifelse(d[[i]]$reference==query_m,log(d[[i]]$pred.score),log(1-d[[i]]$pred.score))) #loglikelihod
  pNull[[i]] <- sum(j[[i]])/dim(d[[i]])[[1]] #caculate null model probability
  logli_nullModel[[i]] <- sum(j[[i]])*log(pNull[[i]]) + sum(ifelse(d[[i]]$reference==query_m,0,1))*log(1-pNull[[i]]) #null model likelihood
  pseudo_R_squared[[i]] <- 1-((-2*(logli_model[[i]]-s))/(-2*(logli_nullModel[[i]]-s))) #pseudo_R_squared
  mydata[[i]] <- data.frame(h[[i]],round(sensitivity[[i]],2),round(specificity[[i]],2),round(f1_score[[i]],2),round(logli_model[[i]],2),round(pseudo_R_squared[[i]],2)) #set dataframe input above value
  names(mydata[[i]]) <- c("method","sensitivity","specificity","F1","Loglikelihood","pseudoR2") #rename dataframe index
  df <- rbind(df, mydata[[i]]) #combine each other
}
#compare each column which is better method
for(s in 2:length(df)){
  l[[s]] <- df[which.max(df[[s]]),][1]
}
k <- data.frame(c("max",l[[2]],l[[3]],l[[4]],l[[5]],l[[6]]))
names(k) <- c("method","sensitivity","specificity","F1","Loglikelihood","pseudoR2")
df <- rbind(df,k)
write.csv(df,file=out_f,row.names=FALSE) #write output file
read.csv(out_f, header=T) #show result
