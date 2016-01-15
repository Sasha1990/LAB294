problem 0#
firstName<- "Xiaoxi Sasha"
lastName <- "Zhu"
print(
  paste(firstName,
        lastName
  )
)

studentID<- "1505138"
print(studentID)

#problem 1#
library(foreign)
df.dta <-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv <-read.csv("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td <- read.table("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
df.rdata<-load("/Users/sashazhu/Downloads/NHIS_2007_RData-2.RData")

#problem 2#
##NHIS_2007_CSV.csv is 139KB##
##NHIS_2007_RData.RData is 45.3KB##
##NHIS_2007_TSV.txt is 139KB##
##NHIS_2007_dta.dta is 188KB##
##NHIS_2007_RData.RData is the smallest file##

#problem 3#
typeof(df.rdata)
class(df.rdata)
length(df.rdata)
dim(df.rdata)
nrow(df.rdata)
ncol(df.rdata)
summary(df.rdata)

#problem 4#
df.dta <-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(df.dta)
##1119754 observations   30 variables##
summary(df.dta$rw)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's ##
## 1.8    10.7    15.9    19.8    24.4   354.8  521279  ##

#problem 5#
v<-c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)

length(v)
##Length is 9,because NULL is not counted in it, "NULL" in here just means empty.##

summary(v)
##The result shows that the mean of v is 4 without considering NULL##

#problem 6#
x <- matrix( 
  c(1, 4, 7, 2, 5, 8, 3, 6, 9), 
  nrow=3, 
  ncol=3) 

t(x)
##simply use the command t(x) will help us get the result of matrix transpose##

eigen(x)
##Run this command will show us the eigenvalues and eigenvactors##

y <- matrix( 
  c(1, 3, 2, 2, 2, 3, 3, 1, 0), 
  nrow=3, 
  ncol=3) 

z<-solve(y)
##simply use the command solve(y) will help us get the inverse of matrix##

y%*%z
##The new matrix I get with this command is identity matrix##

#problem 7#
carat = c(5, 2, 0.5, 1.5, 5, NA, 3) 
cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair") 
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", "NA" )
price = c(850, 450, 450, 0, 750, 980, 420)
diamonds <- data.frame(carat, cut, clarity, price)

print(diamonds)
#(a)#
summary(diamonds)
##It shows that the mean value of price is 557.1##

#(b)#
diamondscut<-subset(diamonds,(cut=="fair"))
summary(diamondscut)
##It shows that the mean value of price when we just considering the individuals whose cut="fair" is 557.1##

#(c)#
diamondscut1<-subset(diamonds,(cut=="good"|cut=="very good"|cut=="Ideal"))
summary(diamondscut1)
##It shows that the mean value of price when we just considering the individuals whose cut="good" and "very good" and "Ideal"is 470.0##

#(d)#
diamondscut2<-subset(diamonds,(cut=="very good"|cut=="Ideal"|carat>"2"))
summary(diamondscut2)
##It shows that the mean value of price when we just considering the individuals whose cut="very good" and "Ideal" and carat greater than 2 is 690##
##and median is 750##

