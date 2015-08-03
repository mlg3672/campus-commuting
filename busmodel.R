library(ggmap)
library(stringr)
## next steps July 9
#  find better model fit e.g. tree, non-linear 2nd deg polynomial
#  try more points e.g. N= 2000
#  try larger box e.g. use Victor as bottomright

# find square lat lon box coordinates for rochester
topleft <- geocode("Parma,NY") 
bottomright<-geocode("Perinton, NY") 

# generate 300 random lat, long points
set.seed(11)
meanY<-mean(as.numeric(c(topleft['lat'],bottomright['lat'])))
meanX<-mean(as.numeric(c(topleft['lon'],bottomright['lon'])))
sdY <- abs(as.numeric(topleft['lat'] - bottomright['lat']))
sdX<- abs(as.numeric(topleft['lon'] - bottomright['lon']))
rangeX <- meanX + rnorm(300)*(sdX/2)
rangeY <- meanY + rnorm(300)*(sdY/2)
mean(rangeX)
sd(rangeX)

# only grab points within range

insideX<- as.numeric(topleft['lon']) < rangeX & rangeX < as.numeric(bottomright['lon'])
insideY<- as.numeric(topleft['lat']) > rangeY & rangeY > as.numeric(bottomright['lat'])

XYpairs <- data.frame(lon=sample(rangeX[insideX],200),lat=sample(rangeY[insideY],200))
plot(XYpairs)
dim(XYpairs)

# get directions of random locations to RIT by bus
# get distance in km, and time in minutes, wait time in minutes, # of transfers
to <- as.numeric(geocode("RIT, Rochester, NY"))
from<-as.numeric(geocode("YMCA Rochester, NY"))
# add new cols
busmi<-busleg<-drivemin<-busmin<-drivemiles<-rep(0,dim(XYpairs)[1])
address<-city<-zip<-rep("NA",dim(XYpairs)[1])
gbus<- cbind(XYpairs,drivemiles,drivemin,busmin,busmi,busleg,address)

for (x in 173:dim(XYpairs)[1]) {
  from=as.numeric(XYpairs[x,])
  distance <- mapdist(from, to, mode="driving", output="simple")
  gbus$drivemiles[x]<-distance$miles
  gbus$drivemin[x]<-distance$minutes
  gbus$address[x]<-distance$from
  busroute<-route(from,to,mode = "transit",output="simple") #error at 131
  gbus$busmin[x]<-sum(busroute$minutes)
  gbus$busmi[x]<-sum(busroute$miles)
  gbus$busleg[x]<-max(busroute$leg)
}

 # extract city and zip data from address field
for (x in 1:dim(XYpairs)[1]){
  gbus$city[x]<-str_trim(strsplit(gbus$address[x],",")[[1]])[2]
  gbus$zip[x]<-strsplit(str_trim(strsplit(gbus$address[x],",")[[1]])[3]," ")[[1]][2]
}

# write data to csv
write.csv(x=gbus,file="bustimes.csv")
#plot RIT on graph with points
plot(gbus$lon,gbus$lat,col="black")
points(gbus[c(131,172,200),1:2],col="red") #error points
points(to[1],to[2],col="green")

# find relationship between distance in km and bus travel time
qplot(drivemin,busmin,data=gbus)
qplot(drivemiles,busmi,data=gbus)
qplot(drivemiles,drivemin,data=gbus) # very tight
qplot(drivemiles,busmin,data=gbus)
# data splitting
library(caret)
library(kernlab)
inTrain<-createDataPartition(y=gbus$busmin,p=0.75,list=FALSE)
training<-gbus[inTrain,]
testing<-gbus[-inTrain,]
dim(training)

# fit general linear model to data
set.seed(32343)
modelFit<-train(busmin ~drivemin+busleg+busmi, data = training, method = "lm")
modelFit
modelFit$finalModel
predictions<-predict(modelFit,newdata=testing)
summary(predictions-testing$busmin)
points(testing$drivemin,predictions,col="blue")
points(testing$drivemin,testing$busmin,col="green")
# linear model
lm1<-lm(busmin~drivemin, data = training)
plot(training$drivemin,training$busmin, pch=19,cex = 0.5)
points(training$drivemin, predict(lm1, newdata = training), col = "red", pch = 19, cex = 0.5)
