#TFI Restaurant Revenue Prediction
#Ver. 0.0.2 #Additional Data included

#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("ggplot2")
require("h2o")

#Read Settings file
directories <- fromJSON(file = "SETTINGS.json")

#Set Directories
workingDirectory <- directories$workingDirectory
setwd(workingDirectory)
dataDirectory <- directories$dataDirectory
#h2o location
h2o.jarLoc <- directories$h2o.jarLoc

#Detect available cores
numCores <- detectCores()

#Source additional scripts
source(file.path(workingDirectory, "multiplot.R"))

#Load Raw Data----------------------
train <- fread(file.path(dataDirectory, directories$trainFile), verbose = TRUE)
test <- fread(file.path(dataDirectory, directories$testFile), verbose = TRUE)

setnames(train, 2, "OpenDate")
setnames(train, 4, "CityGroup")
setnames(test, 2, "OpenDate")
setnames(test, 4, "CityGroup")

#Transform dates
train$OpenDate <- log(as.numeric(as.Date(train$OpenDate, format = "%m/%d/%Y")))
test$OpenDate <- log(as.numeric(as.Date(test$OpenDate, format = "%m/%d/%Y")))

#Add duplicate P columns to transform to factors
duplicatePsTrain <- as.data.frame(train)[, c(6, 10:17, 19:30, 35:42)]
names(duplicatePsTrain) <- paste0("FactorP", c(1, 5:12, 14:25, 30:37))
duplicatePsTest <- as.data.frame(test)[, c(6, 10:17, 19:30, 35:42)]
names(duplicatePsTest) <- paste0("FactorP", c(1, 5:12, 14:25, 30:37))

train <- cbind(train, as.data.table(duplicatePsTrain))
test <- cbind(test, as.data.table(duplicatePsTest))

rm(duplicatePsTrain, duplicatePsTest)

#EDA----------------------
#Prepare data for visualization
ggplotDataFrame <- as.data.frame(rbind(train[, -which(names(train) == "revenue"), with = FALSE], test))

#EDA 1 P1-P37 Categorical or not
uniqueValuesInColumns <- apply(ggplotDataFrame, 2, unique)

#Analize distributions - spread to spot non-normal data (possibly categorical)
OpenDatePlot <- ggplot(data = ggplotDataFrame, aes(x = OpenDate)) + geom_bar() 
CityPlot <- ggplot(data = ggplotDataFrame, aes(x = City)) + geom_bar(colour="blue") 
CityGroupPlot <- ggplot(data = ggplotDataFrame, aes(x = CityGroup)) + geom_bar(colour="blue") 
TypePlot <- ggplot(data = ggplotDataFrame, aes(x = Type)) + geom_bar(colour="blue") 
P1Plot <- ggplot(data = ggplotDataFrame, aes(x = P1)) + geom_bar() 
P2Plot <- ggplot(data = ggplotDataFrame, aes(x = P2)) + geom_bar(fill="magenta") 
P3Plot <- ggplot(data = ggplotDataFrame, aes(x = P3)) + geom_bar(fill="magenta") 
P4Plot <- ggplot(data = ggplotDataFrame, aes(x = P4)) + geom_bar(fill="magenta") 
P5Plot <- ggplot(data = ggplotDataFrame, aes(x = P5)) + geom_bar() 
P6Plot <- ggplot(data = ggplotDataFrame, aes(x = P6)) + geom_bar() 
P7Plot <- ggplot(data = ggplotDataFrame, aes(x = P7)) + geom_bar() 
P8Plot <- ggplot(data = ggplotDataFrame, aes(x = P8)) + geom_bar() 
P9Plot <- ggplot(data = ggplotDataFrame, aes(x = P9)) + geom_bar() 
P10Plot <- ggplot(data = ggplotDataFrame, aes(x = P10)) + geom_bar() 
P11Plot <- ggplot(data = ggplotDataFrame, aes(x = P11)) + geom_bar() 
P12Plot <- ggplot(data = ggplotDataFrame, aes(x = P12)) + geom_bar() 
P13Plot <- ggplot(data = ggplotDataFrame, aes(x = P13)) + geom_bar(fill="magenta") 
P14Plot <- ggplot(data = ggplotDataFrame, aes(x = P14)) + geom_bar() 
P15Plot <- ggplot(data = ggplotDataFrame, aes(x = P15)) + geom_bar() 
P16Plot <- ggplot(data = ggplotDataFrame, aes(x = P16)) + geom_bar() 
P17Plot <- ggplot(data = ggplotDataFrame, aes(x = P17)) + geom_bar() 
P18Plot <- ggplot(data = ggplotDataFrame, aes(x = P18)) + geom_bar() 
P19Plot <- ggplot(data = ggplotDataFrame, aes(x = P19)) + geom_bar() 
P20Plot <- ggplot(data = ggplotDataFrame, aes(x = P20)) + geom_bar() 
P21Plot <- ggplot(data = ggplotDataFrame, aes(x = P21)) + geom_bar()
P22Plot <- ggplot(data = ggplotDataFrame, aes(x = P22)) + geom_bar()
P23Plot <- ggplot(data = ggplotDataFrame, aes(x = P23)) + geom_bar()
P24Plot <- ggplot(data = ggplotDataFrame, aes(x = P24)) + geom_bar()
P25Plot <- ggplot(data = ggplotDataFrame, aes(x = P25)) + geom_bar()
P26Plot <- ggplot(data = ggplotDataFrame, aes(x = P26)) + geom_bar(fill="magenta")
P27Plot <- ggplot(data = ggplotDataFrame, aes(x = P27)) + geom_bar(fill="magenta")
P28Plot <- ggplot(data = ggplotDataFrame, aes(x = P28)) + geom_bar()
P29Plot <- ggplot(data = ggplotDataFrame, aes(x = P29)) + geom_bar(fill="magenta")
P30Plot <- ggplot(data = ggplotDataFrame, aes(x = P30)) + geom_bar()
P31Plot <- ggplot(data = ggplotDataFrame, aes(x = P31)) + geom_bar()
P32Plot <- ggplot(data = ggplotDataFrame, aes(x = P32)) + geom_bar()
P33Plot <- ggplot(data = ggplotDataFrame, aes(x = P33)) + geom_bar()
P34Plot <- ggplot(data = ggplotDataFrame, aes(x = P34)) + geom_bar()
P35Plot <- ggplot(data = ggplotDataFrame, aes(x = P35)) + geom_bar()
P36Plot <- ggplot(data = ggplotDataFrame, aes(x = P36)) + geom_bar()
P37Plot <- ggplot(data = ggplotDataFrame, aes(x = P37)) + geom_bar()

multiplot(OpenDatePlot, CityPlot, CityGroupPlot, TypePlot, P1Plot, P2Plot, P3Plot, P4Plot, 
          P5Plot, P6Plot, P7Plot, P8Plot, P9Plot, P10Plot, P11Plot, P12Plot, P13Plot, P14Plot,
          P15Plot, P16Plot, P17Plot, P18Plot, P19Plot, P20Plot, P21Plot, P22Plot, P23Plot, 
          P24Plot, P25Plot, P26Plot, P27Plot, P28Plot, P29Plot, P30Plot, P31Plot, 
          P32Plot, P33Plot, P34Plot, P35Plot, P36Plot, P37Plot, cols = 3)
#Small pause
Sys.sleep(20)
#Save Plot
dev.print(file = "VariableHistogram", device = png, width = 1600)
#Free some memory
rm(ggplotDataFrame)

#Model Comparison-----------------------------------
#5-fold cross validation

#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54344 -name TFI &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54344, nthreads = -1)

#Load Data into server
h2oTFITrain <- as.h2o(h2oServer, train)
h2oTFITest <- as.h2o(h2oServer, test)

#Transform revenue to log revenue
h2oTFITrain$revenue <- log(h2oTFITrain$revenue)

#Transform to factors
#Factor columns as h2o factors
for (columnName in paste0("FactorP", c(1, 5:12, 14:25, 30:37))){
  h2oTFITrain[, columnName] <- as.factor(h2oTFITrain[, columnName])
  h2oTFITrain[, columnName] <- as.factor(h2oTFITrain[, columnName])
  h2oTFITest[, columnName] <- as.factor(h2oTFITest[, columnName])
  h2oTFITest[, columnName] <- as.factor(h2oTFITest[, columnName])
}

#h2o.ai Cross Validation
TFIModelCV <- h2o.glm(x = names(h2oTFITrain)[c(-1, -which(names(h2oTFITrain) == "revenue"))],
                      y = "revenue",
                      data = h2oTFITrain,
                      nfolds = 5,
                      family = "gaussian", 
                      use_all_factor_levels = TRUE,
                      alpha = c(0, 0.01, 0.1),
                      lambda_search = TRUE,
                      nlambda = 40,
                      variable_importances = TRUE,
                      higher_accuracy = TRUE)

print(paste0("There is an error of: ", TFIModelCV@model[[1]]@model$deviance))
bestAlpha <- TFIModelCV@model[[1]]@model$params$alpha
bestLambda <- TFIModelCV@model[[1]]@model$params$lambda_best

#Full Model Training---------------------
TFIModelFull <- h2o.glm(x = names(h2oTFITrain)[c(-1, -which(names(h2oTFITrain) == c("revenue")))],
                        y = "revenue",
                        data = h2oTFITrain,
                        family = "gaussian", 
                        alpha = bestAlpha,
                        lambda = bestLambda,
                        higher_accuracy = TRUE)

#Revenue Prediciton
TFIPrediction <- exp(as.data.frame(h2o.predict(TFIModelFull, newdata = h2oTFITest))[, 1])

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#Write a prediciton file----------------------
sampleSubmission <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)
sampleSubmission$Prediction <- TFIPrediction
write.csv(sampleSubmission, file = "GLMIV.csv", row.names = FALSE)
system('zip GLMIV.zip GLMIV.csv')

#One without negative values
TFIPrediction[TFIPrediction < 0] <- 0
sampleSubmission$Prediction <- TFIPrediction
write.csv(sampleSubmission, file = "GLMNoZerosIV.csv", row.names = FALSE)
system('zip GLMNoZerosIV.zip GLMNoZerosIV.csv')

