##problem 0##
XiaoxiSashaZhuAssignment2 <- list(
  firstName<- "Xiaoxi Sasha",
  lastName <- "Zhu",
  email<-"xzhu25@ucsc.edu",
  studentID<- 1505138
)








##problem 1##
install.packages("RCurl")
library(RCurl)
diamonds <- getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
diamonds <- read.csv(text = diamonds)

XiaoxiSashaZhuAssignment2$s1a <- nrow(diamonds)
XiaoxiSashaZhuAssignment2$s1b <- ncol(diamonds)
XiaoxiSashaZhuAssignment2$s1c <- names(diamonds)
XiaoxiSashaZhuAssignment2$s1d <- summary(diamonds$price)

##problem 2##
install.packages("repmis")
library("repmis")
diamonds2<-source_data("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_TSV.txt",header= T)

nrow(diamonds2)

XiaoxiSashaZhuAssignment2$s2a <- nrow(diamonds2)
XiaoxiSashaZhuAssignment2$s2b <- ncol(diamonds2)
XiaoxiSashaZhuAssignment2$s2c <- names(diamonds2)
XiaoxiSashaZhuAssignment2$s2d <- mean(diamonds2$weight)
XiaoxiSashaZhuAssignment2$s2e <- median(diamonds2$weight)
hist(diamonds2$weight)
table(diamonds2$weight)
diamonds2$weight2 <- ifelse(test = diamonds2$weight < 996 | diamonds2$weight > 999,
                            yes = diamonds2$weight,
                            no = NA )                            
hist(diamonds2$weight2)
XiaoxiSashaZhuAssignment2$s2f<- mean(diamonds2$weight2,na.rm=TRUE)
XiaoxiSashaZhuAssignment2$s2g <- median(diamonds2$weight2,na.rm=TRUE)
diamonds2male<-subset(diamonds2,(SEX==1))
XiaoxiSashaZhuAssignment2$s2h<- summary(diamonds2male$weight2)
diamonds2female<-subset(diamonds2,(SEX==2))
XiaoxiSashaZhuAssignment2$s2i<- summary(diamonds2female$weight2)

##problem 3##

vec<-c(letters,LETTERS)
length(vec)
XiaoxiSashaZhuAssignment2$s3a <- vec[seq(2,52,2)]
XiaoxiSashaZhuAssignment2$s3b <- vec[c(50,9,1)]


arr <- array(c(letters,LETTERS),dim = c(3,3,3))
XiaoxiSashaZhuAssignment2$s3c <- arr[,1,2]
arr1<-arr[2,,]
XiaoxiSashaZhuAssignment2$s3d <- arr1[2,]
XiaoxiSashaZhuAssignment2$s3e<-paste(arr[3,2,3],arr[3,3,1],arr[1,1,1],sep="")

save(XiaoxiSashaZhuAssignment2, 
     file = "Desktop/XiaoxiSashaZhuAssignment.RData")
      



