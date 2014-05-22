#read columns names
col.names<-read.table(file="UCI HAR Dataset\\features.txt",header=FALSE,sep=" ")

#read the data
df.test<-read.table(file="UCI HAR Dataset\\test\\X_test.txt",header=FALSE,col.names=col.names[,2])
df.train<-read.table(file="UCI HAR Dataset\\train\\X_train.txt",header=FALSE,col.names=col.names[,2])

df.act.test<-read.table(file="UCI HAR Dataset\\test\\Y_test.txt",header=FALSE)
df.act.train<-read.table(file="UCI HAR Dataset\\train\\Y_train.txt",header=FALSE)

df.subj.test<-read.table(file="UCI HAR Dataset\\test\\subject_test.txt",header=FALSE)
df.subj.train<-read.table(file="UCI HAR Dataset\\train\\subject_train.txt",header=FALSE)

#add activities and subjects columns
df.test["activity"]<-df.act.test
df.train["activity"]<-df.act.train

df.test["subject"]<-df.subj.test
df.train["subject"]<-df.subj.train

#read labels of activities
df.labels.act<-read.table(file="UCI HAR Dataset\\activity_labels.txt",header=FALSE)

#order dataframe
df.total<-rbind(df.test,df.train)

df.total<-df.total[,c(563,562,1:561)]

df.total <- df.total[order(df.total$subject, df.total$activity), ]

row.names(df.total)<-NULL

#calculate means and standars desviations
means<-as.matrix(apply(df.total[,3:563],2,mean))
sd<-as.matrix(apply(df.total[,3:563],2,sd))

#calculate means by subject and activity
df.averages.all<-aggregate(df.total[,3],by=list(df.total$subject,df.total$activity), mean)

for (j in 4:563){
fsdfd434<-aggregate(df.total[,j],by=list(df.total$subject,df.total$activity), mean)
df.averages.all<-cbind(df.averages.all,fsdfd434[,3])
}

#set the labels
df.averages.all <- df.averages.all[order(df.averages.all[,1], df.averages.all[,2]), ]

row.names(df.averages.all)<-NULL

df.averages.all[,2]<-factor(df.averages.all[,2],levels=c(1,2,3,4,5,6),labels=df.labels.act[,2])

col.names.all<-rbind(matrix(c("Subject","Activity"),nrow=2),as.matrix(col.names[,2]))

colnames(df.averages.all)<-col.names.all

#create the .txt file
write.table(df.averages.all, file = "tidy.data.txt", sep=" ", row.names = FALSE)
