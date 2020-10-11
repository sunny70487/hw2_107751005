args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target male|female --input file1 file2 ... filen --output out.csv", call.=FALSE)
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

dir = file.path(getwd(),"data","/")
dir_o=file.path(getwd(),"eval","/")
d <- list()
S <- 0
j <- list()
h <- list()
s <- list()
pred_label <- list()
cf <- list()
tp <- list()
tn <- list()
fp <- list()
fn <- list()
accuracy <- list()
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
  h[[i]]<-gsub(".csv", "", basename(i))
  d[[i]] <- read.csv(paste(dir,i,sep=""))
  j[[i]] <- ifelse(d[[i]]$reference==query_m,1,0)
  pred_label[[i]] <- as.integer(d[[i]]$pred.score >thre)
  cf[[i]] <- table(pred_label[[i]], j[[i]])
  tp[[i]] <- cf[[i]][2, 2]
  tn[[i]] <- cf[[i]][1, 1]
  fp[[i]] <- cf[[i]][2, 1]
  fn[[i]] <- cf[[i]][1, 2]
  accuracy[[i]] <- (tp[[i]] + tn[[i]])/(tp[[i]] + tn[[i]] + fp[[i]] + fn[[i]])
  sensitivity[[i]] <- tp[[i]]/(tp[[i]] + fn[[i]])
  specificity[[i]] <- tn[[i]]/(tn[[i]] + fp[[i]])
  f1_score[[i]] <- (2*accuracy[[i]]*sensitivity[[i]])/(accuracy[[i]]+sensitivity[[i]])
  logli_model[[i]] <- sum(ifelse(d[[i]]$reference==query_m,log(d[[i]]$pred.score),log(1-d[[i]]$pred.score)))
  pNull[[i]] <- sum(j[[i]])/dim(d[[i]])[[1]]
  logli_nullModel[[i]] <- sum(j[[i]])*log(pNull[[i]]) + sum(ifelse(d[[i]]$reference==query_m,0,1))*log(1-pNull[[i]])
  pseudo_R_squared[[i]] <- 1-(-2*(logli_model[[i]]-S))/(-2*(logli_nullModel[[i]]-S))
  mydata[[i]] <- data.frame(h[[i]],sensitivity[[i]],specificity[[i]],f1_score[[i]],logli_model[[i]],pseudo_R_squared[[i]])
  names(mydata[[i]]) <- c("method","sensitivity","specificity","F1","Loglikelihood","pseudoR2")
  df <- rbind(df, mydata[[i]])
}
l <- list()
for(s in 2:length(df)){
  l[[s]] <- df[which.max(df[[s]]),][1]
}
k <- data.frame(c("max",l[[2]],l[[3]],l[[4]],l[[5]],l[[6]]))
names(k) <- c("method","sensitivity","specificity","F1","Loglikelihood","pseudoR2")
df <- rbind(df,k)
write.csv(df,file=paste(dir_o,out_f,sep=""),row.names=FALSE)
read.csv(paste(dir_o,out_f, sep=""), header=T)
