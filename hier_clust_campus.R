#  hierarchal clustering to sort campus data
## builds a cluster from ground up
## 1. put each data point in its own cluster
## 2. group with next nearest cluster

# read data
#set wd
setwd("~/Documents/R-Projects/campus-commuting")
# read csv file
survey <- read.csv("data/jshe_survey.csv")
# drop NAs
complete<- complete.cases(survey)
survey<=survey[complete,]
# change column names
colnames(survey)<-c("respondent","modeone","modetwo","quarters",
                    "weeklyone","weeklytwo","onewaydist","mpg","hybrid")
## if mpg is zero make 20
zeros<-survey$mpg<2
survey$mpg[zeros]<-20

# change name of factors
levels(survey$modeone)<-c("Bike/Walk","Bus","Carpool","Bike/Walk","Drive","Drive")
levels(survey$modetwo)<-c("Bike/Walk","Bus","Carpool","None","None","Bike/Walk","Drive","Drive")

#if modetwo factor is "None" same as modeone

# calculate and add column for carbon emissions
for (i in 1:nrow(survey)) {
  commutes = 0
  if (survey$modeone[i] == 'Drive'|| survey$modeone[i] == 'Bus') {
    commutes=survey$weeklyone[i]
  }
  if (survey$modetwo[i]=='Drive' || survey$modetwo[i]=='Bus') {
    commutes=commutes+survey$weeklytwo[i]
  }
  survey$commutes[i]<-commutes
  survey$co2kg[i] = 19.54*0.120388*survey$quarters[i]*12*commutes*survey$onewaydist[i]*2/survey$mpg[i]
  # add column for share of primary vehicle
  survey$vehshare[i] = commutes/(survey$weeklyone[i] + survey$weeklytwo[i])
  
}



# delete hybrid column 
ss<-survey[,c(1:11)]


# can use hierarchical clustering to categorize based on onewaydist, co2kg or vehshare
clusters <- hclust(dist(survey[, c(7,11)]))
plot(clusters)

# cut off tree at desired number of clusters n=3
clusterCut <- cutree(clusters, 4)

# compare results with original species
table(clusterCut, survey$respondent)

## use mean linkage method
clusters <- hclust(dist(survey[, c(7,11)]), method = 'average')
plot(clusters)

# use cut tree 
clusterCut <- cutree(clusters, 5)
table(clusterCut, survey$modeone)


## plot to compare with original data
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))

## where inner color does not match it was clustered incorrectly