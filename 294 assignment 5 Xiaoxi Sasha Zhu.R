##Assignment 5##
#problem 0#
print("Xiaoxi Sasha Zhu")
print(1505138)
print("xzhu25@ucsc.edu")

#problem 1#
#a#
require(dplyr)
library(ggplot2)
diamonds<-diamonds%>%mutate(volume = x*y*z)%>%tbl_df()
d<-ggplot(diamonds,aes(x=volume,y=price))
d + geom_point(aes(color=clarity,size=carat),alpha = 0.1)+scale_x_log10()+scale_y_log10()

#b#
p <- ggplot(diamonds, aes(carat,..density..))
p + geom_histogram(aes(fill = clarity),bins=25)+ facet_grid(cut ~ .)

#c#
a <- ggplot(diamonds, aes(cut,price))
a + geom_violin()+ geom_jitter(alpha=0.03)

#problem 3#
#a#
library(foreign)
org<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

##uae summarise to get the data I need.##
org_new <- org %>% dplyr::group_by(year,month)%>%
  dplyr::summarise(
    rw_quantile1st = quantile(rw, .1, na.rm = T),
    rw_quantile9st = quantile(rw, .9, na.rm = T),
    rw_quantile1 = quantile(rw, .25, na.rm = T),
    rw_quantile3 = quantile(rw, .75, na.rm = T),
    Median.RW = median(rw, na.rm = T),
    count = n())

org_new<-org_new %>% mutate(date=paste(year,month,"01", sep="-"),
                            date=as.Date(date,format="%Y-%m-%d"))
##It's important to know how to combine the month and year together as date.##

b <- ggplot(org_new, aes(x=date, y=Median.RW))
b + geom_ribbon(aes(ymin=rw_quantile1, ymax=rw_quantile3),alpha=0.6) + geom_ribbon(aes(ymin=rw_quantile1st, ymax=rw_quantile9st),alpha=0.2) + geom_line(aes(y=Median.RW))+lims(y=c(0,50))

#b#
org_new_b <- org %>% dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(
    Median.RW = median(rw, na.rm = T),
    count = n())

org_new_b<-org_new_b %>% mutate(date=paste(year,month,"01", sep="-"),
                                date=as.Date(date,format="%Y-%m-%d"))

e <- ggplot(org_new_b, aes(x=date, y=Median.RW,group=educ))
e + geom_line(aes(color=educ))
