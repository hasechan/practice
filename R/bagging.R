heart <- read.csv("Heart.csv",header=T,row.names=1)
heart <- heart[is.na(heart[,"Ca"])==F,]
heart <- heart[is.na(heart[,"Thal"])==F,]
#lengthは基本的に列で対応している。
n<-length(heart[,1])
oob<-matrix(0,nrow=n,ncol=100)
#とりあえず一回回す
tmp<-sort(sample(1:length(heart),length(heart),replace=T))
b.sample<-heart[tmp,]
oob.sample<-heart[-tmp,]
#ランダムフォレスト:説明変数は14種類。
result <- rpart(AHD~.,data=b.sample,
                control=rpart.control(minsplit=10,cp=0.01))
#type=classで二値で返してくれる。as.numericで数値化する。
pre<-as.numeric(predict(result,newdata=oob.sample,type="class"))
oob[-tmt,1]<-pre
#----ここまでがベースライン(B=100)
for(i in 1:100){
  tmt<-sort(sample(1:length(heart[,1]),length(heart[,1]),replace=T))
  b.sample<-heart[tmt,]
  oob.sample<-heart[-tmt,]
  result <- rpart(AHD~.,data=b.sample,
                  control=rpart.control(minsplit=10,cp=0.01))
  #type=classで二値で返してくれる。
  #as.numericで数値化してくれる。
  pre<-as.numeric(predict(result,newdata=oob.sample,type="class"))
  oob[-tmt,i]<-pre
}
oob
#最頻値関数
mymode<-function(vec){
  as.integer(names(which.max(table(vec))))
}
oob[1,][oob[1,]!=0]
#mymode((oob[1,][oob[1,]>=1]))
#sapply,1~100までに以下のファンクションを適応させる。
fin.pre<-sapply(1:length(heart[,1]),function(x){mymode(oob[x,][oob[x,]!=0])})
fin.pre
res<-as.numeric(heart[,"AHD"])
res
oob.error<-sum(as.numeric(fin.pre!=res))/n
#最終的なアウトプット
oob.error
