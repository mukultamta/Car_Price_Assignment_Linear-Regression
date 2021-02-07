############################################Linear Regression - Assignment######################################
library(stringr)
library(tidyr)
library(car)
library(MASS)

#Importing dataset
car_chinese <- read.csv("CarPrice_Assignment.csv")

#structure of data shows all the data types
str(car_chinese)

#Check for duplicate values
sum(duplicated(car_chinese$Car_ID))

#Remove Car_ID column since it is a unique id of each observation
car_chinese<-car_chinese[,-1]

#Check for NA values
sapply(car_chinese,function(x) sum(is.na(x)))
sum(is.na(car_chinese))


#Removing 'car model' name from CarName and only considering 'car company' for model
car_chinese<- separate(car_chinese,CarName,into=c("car_company","car_model"),sep=" ",remove=TRUE)
car_chinese<-car_chinese[,-3]

# To find the unique names of car company
unique(car_chinese$car_company)

#To replace car company name with valid car names 
car_chinese$car_company <- gsub("maxda","mazda",car_chinese$car_company)
car_chinese$car_company <- gsub("porcshce","porsche",car_chinese$car_company)
car_chinese$car_company <- gsub("Nissan","nissan",car_chinese$car_company)
car_chinese$car_company <- gsub("toyouta","toyota",car_chinese$car_company)
car_chinese$car_company <- gsub("vokswagen","volkswagen",car_chinese$car_company)
car_chinese$car_company <- gsub("vw","volkswagen",car_chinese$car_company)

unique(car_chinese$car_company)

#########################Outliers Treatment#########################

#wheelbase
quantile(car_chinese$wheelbase,seq(0,1,0.01))
#There is a jump between 99% and 100% .So capped all values above 115.544 to 115.544
car_chinese$wheelbase[which(car_chinese$wheelbase>115.544)]<-115.544


#carlength
quantile(car_chinese$carlength,seq(0,1,0.01))
#There is a jump between 0% and 3% & 99% and 100% .So capped all values below 155.900 to 155.900 and above 202.480 to 202.480
car_chinese$carlength[which(car_chinese$carlength<155.900)]<-155.900
car_chinese$carlength[which(car_chinese$carlength>202.480)]<-202.480

#carwidth
quantile(car_chinese$carwidth,seq(0,1,0.01))
#There is a jump between 0% and 1%.So capped all values below 62.356 to 62.356
car_chinese$carwidth[which(car_chinese$carwidth<62.356)]<-62.356

#carheight
quantile(car_chinese$carheight,seq(0,1,0.01))
# There is no significant jump observed.

#curbweight
quantile(car_chinese$curbweight,seq(0,1,0.01))
#There is a jump between 0% and 1%.So capped all values below 1819.72 to 1819.72
car_chinese$curbweight[which(car_chinese$curbweight<1819.72)]<-1819.72

#enginesize
quantile(car_chinese$enginesize,seq(0,1,0.01))
#There is a jump between 96% and 100%.So capped all values above 209.00 to 209.00
car_chinese$enginesize[which(car_chinese$enginesize>209.00)]<-209.00

#boreratio
quantile(car_chinese$boreratio,seq(0,1,0.01))
#There is a jump between 0% and 1% & 99% and 100%.So capped all values below 2.9100 to 2.9100 and above 3.8000 to 3.8000
car_chinese$boreratio[which(car_chinese$boreratio<2.9100)]<-2.9100
car_chinese$boreratio[which(car_chinese$boreratio>3.8000)]<-3.8000

#stroke
quantile(car_chinese$stroke,seq(0,1,0.01))
#There is a jump between 0% and 2% & 96% and 100%.So capped all values below 2.6400 to 2.6400 and above 3.6400 to 3.6400
car_chinese$stroke[which(car_chinese$stroke<2.6400)]<-2.6400
car_chinese$stroke[which(car_chinese$stroke>3.6400)]<-3.6400

#compressionratio
quantile(car_chinese$compressionratio,seq(0,1,0.01))
#There is a jump between 90% and 100%.So capped all values above 10.9400 to 10.9400
car_chinese$compressionratio[which(car_chinese$compressionratio>10.9400)]<-10.9400

#horsepower
quantile(car_chinese$horsepower,seq(0,1,0.01))
#There is a jump between 97% and 100%.So capped all values above 184.00 to 184.00
car_chinese$horsepower[which(car_chinese$horsepower>184.00)]<-184.00

#peakrpm
quantile(car_chinese$peakrpm,seq(0,1,0.01))
#There is a jump between 99% and 100%.So capped all values above 6000 to 6000
car_chinese$peakrpm[which(car_chinese$peakrpm>6000)]<-6000

#citympg
quantile(car_chinese$citympg,seq(0,1,0.01))
#There is a jump between 98% and 100%.So capped all values above 38.00 to 38.00
car_chinese$citympg[which(car_chinese$citympg>38.00)]<-38.00

#highwaympg
quantile(car_chinese$highwaympg,seq(0,1,0.01))
#There is a jump between 98% and 100%.So capped all values above 46.92 to 46.92
car_chinese$highwaympg[which(car_chinese$highwaympg>46.92)]<-46.92


##############################END OF DATA CLEANING#################################################

#Checking structure of car_chinese
str(car_chinese)

#converting 'symboling' to factor type as it is a categorical variable(Given in data dictionary)
car_chinese$symboling <- as.factor(car_chinese$symboling)

#converting 'car_company' to factor type as it is a categorical variable
car_chinese$car_company <- as.factor(car_chinese$car_company)

#Checking how many variables are of factor type
which(sapply(car_chinese, is.factor))

#CREATING DUMMY VARIABLES

# car_company
summary(factor(car_chinese$car_company))
dummy_1<- data.frame(model.matrix(~car_company,data=car_chinese))
dummy_1 <- dummy_1[,-1]
car_chinese <- cbind(car_chinese[,-2],dummy_1)

# fueltype
summary(factor(car_chinese$fueltype))
levels(car_chinese$fueltype) <- c(1,0)
car_chinese$fueltype <- as.numeric(levels(car_chinese$fueltype))[car_chinese$fueltype]

# aspiration
summary(factor(car_chinese$aspiration))
levels(car_chinese$aspiration) <- c(1,0)
car_chinese$aspiration <- as.numeric(levels(car_chinese$aspiration))[car_chinese$aspiration]

# doornumber
summary(factor(car_chinese$doornumber))
levels(car_chinese$doornumber) <- c(1,0)
car_chinese$doornumber <- as.numeric(levels(car_chinese$doornumber))[car_chinese$doornumber]

#carbody
summary(factor(car_chinese$carbody))
dummy_2<- data.frame(model.matrix(~carbody,data=car_chinese))
dummy_2 <- dummy_2[,-1]
car_chinese <- cbind(car_chinese[,-5],dummy_2)

#drivewheel
summary(factor(car_chinese$drivewheel))
dummy_3<- data.frame(model.matrix(~drivewheel,data=car_chinese))
dummy_3 <- dummy_3[,-1]
car_chinese <- cbind(car_chinese[,-5],dummy_3)

#enginelocation
summary(factor(car_chinese$enginelocation))
levels(car_chinese$enginelocation) <- c(1,0)
car_chinese$enginelocation <- as.numeric(levels(car_chinese$enginelocation))[car_chinese$enginelocation]

#enginetype
summary(factor(car_chinese$enginetype))
dummy_4<- data.frame(model.matrix(~enginetype,data=car_chinese))
dummy_4 <- dummy_4[,-1]
car_chinese <- cbind(car_chinese[,-11],dummy_4)

#cylindernumber
summary(factor(car_chinese$cylindernumber))
dummy_5<- data.frame(model.matrix(~cylindernumber,data=car_chinese))
dummy_5 <- dummy_5[,-1]
car_chinese <- cbind(car_chinese[,-11],dummy_5)

#fuelsystem
summary(factor(car_chinese$fuelsystem))
dummy_6<- data.frame(model.matrix(~fuelsystem,data=car_chinese))
dummy_6 <- dummy_6[,-1]
car_chinese <- cbind(car_chinese[,-12],dummy_6)

#symboling
#creating bins as -2 & -1 as "Safe", 0 & 1 as "Medium Risky" and 2 & 3 as "Risky" 

levels(car_chinese$symboling)[1:2] <-"Safe"
levels(car_chinese$symboling)[2:3] <-"Medium Risky"
levels(car_chinese$symboling)[3:4] <-"Risky"

#Creating dummy variable for symboling

summary(factor(car_chinese$symboling))
dummy_7<- data.frame(model.matrix(~symboling,data=car_chinese))
dummy_7 <- dummy_7[,-1]
car_chinese <- cbind(car_chinese[,-1],dummy_7)

#Checking if any variable is still a factor
which(sapply(car_chinese, is.factor))

#####################################End of creation of dummy variables########################

#####################################Creation of derived variables#############################
#create 3 new derived variables
#car_vol (Volume of the car)
#torque ( horsepower/peakrpm multiplied by 5252)
#overall_mileage (city_mileage+highway_mileage/2)

car_chinese$car_vol <- car_chinese$carlength * car_chinese$carwidth * car_chinese$carheight
car_chinese$torque <- (car_chinese$horsepower*5252)/car_chinese$peakrpm
car_chinese$overall_mileage <- (car_chinese$citympg+car_chinese$highwaympg)/2

###############################MODEL CREATION###################################################

# Divide into training and test data set
#set the seed to 100
set.seed(100)

# randomly generate row indices for training dataset (70 % of the dataset)
trainindices= sample(1:nrow(car_chinese), 0.7*nrow(car_chinese))
# generate the training data set
train = car_chinese[trainindices,]

#The rest of the observations are stored into "test (To create testing dataset(30% of the dataset).
test = car_chinese[-trainindices,]

# model_1 consisting of all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

#Using stepAIC function
step <- stepAIC(model_1, direction="both")
step


model_2<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + peakrpm + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + car_vol + 
               torque, data = train)

summary(model_2)

#check for multicollinearity
vif(model_2)

#Removing torque 

model_3<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + peakrpm + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + car_vol , data = train)

summary(model_3)

#check for multicollinearity
vif(model_3)

#Removing fueltype

model_4<- lm(formula = price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + peakrpm + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + car_vol , data = train)

summary(model_4)

#check for multicollinearity
vif(model_4)

#Removing fuelsystem2bbl

model_5<- lm(formula = price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + peakrpm + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystemmpfi + car_vol , data = train)


summary(model_5)

#check for multicollinearity
vif(model_5)

# Removing carbodyhardtop

model_6<- lm(formula = price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + peakrpm + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_6)

#check for multicollinearity
vif(model_6)


#Removing carbodysedan

model_7<- lm(formula = price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + peakrpm + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodyhatchback +  carbodywagon + 
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_7)

#check for multicollinearity
vif(model_7)


#Remove peakrpm

model_8<- lm(formula = price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodyhatchback +  carbodywagon + 
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_8)

#check for multicollinearity
vif(model_8)

#Removing carbodyhatchback

model_9<- lm(formula = price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyisuzu + car_companyjaguar + 
               car_companymazda + car_companymercury + car_companymitsubishi + 
               car_companynissan + car_companypeugeot + car_companyplymouth + 
               car_companyrenault + car_companysaab + car_companysubaru + 
               car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
               carbodywagon + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_9)

#check for multicollinearity
vif(model_9)


#Removing car_companyisuzu.

model_10<- lm(formula = price ~ aspiration + enginelocation + 
                carlength + carwidth + carheight + curbweight + enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
                carbodywagon + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_10)

#check for multicollinearity
vif(model_10)

#Removing carwidth

model_11<- lm(formula = price ~ aspiration + enginelocation + 
                carlength + carheight + curbweight + enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen + car_companyvolvo + 
                carbodywagon + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_11)

#check for multicollinearity
vif(model_11)

#Removing car_companyvolvo

model_12<- lm(formula = price ~ aspiration + enginelocation + 
                carlength + carheight + curbweight + enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_12)

#check for multicollinearity
vif(model_12)


#Removing carlength

model_13<- lm(formula = price ~ aspiration + enginelocation + 
                carheight + curbweight + enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_13)

#check for multicollinearity
vif(model_13)


#Remove curbweight

model_14<- lm(formula = price ~ aspiration + enginelocation + 
                carheight + enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_14)

#check for multicollinearity
vif(model_14)


#Remove carheight

model_15<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_15)

#check for multicollinearity
vif(model_15)

#Remove drivewheelrwd

model_16<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + car_vol , data = train)

summary(model_16)

#check for multicollinearity
vif(model_16)


#Remove fuelsystemmpfi

model_17<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysaab + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_17)

#check for multicollinearity
vif(model_17)


# Remove car_companysaab

model_18<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companyrenault + car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_18)

#check for multicollinearity
vif(model_18)


#Remove car_companyrenault

model_19<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_19)

#check for multicollinearity
vif(model_19)

#Remove car_companymercury

model_20<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_20)

#check for multicollinearity
vif(model_20)

#Remove carbodywagon

model_21<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companysubaru + 
                car_companytoyota + car_companyvolkswagen +  
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_21)

#check for multicollinearity
vif(model_21)


#Remove car_companyvolkswagen

model_22<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymitsubishi + 
                car_companynissan + car_companypeugeot + car_companyplymouth + 
                car_companysubaru + 
                car_companytoyota + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_22)

#check for multicollinearity
vif(model_22)

#Remove car_companynissan

model_23<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymitsubishi + 
                car_companypeugeot + car_companyplymouth + 
                car_companysubaru + 
                car_companytoyota + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_23)

#check for multicollinearity
vif(model_23)


#Remove car_companyplymouth

model_24<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + car_companymitsubishi + 
                car_companypeugeot + car_companysubaru + 
                car_companytoyota + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_24)

#check for multicollinearity
vif(model_24)


#Removing car_companymitsubishi

model_25<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companydodge + car_companyjaguar + 
                car_companymazda + 
                car_companypeugeot + car_companysubaru + 
                car_companytoyota + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_25)

#check for multicollinearity
vif(model_25)

# Removing car_companydodge

model_26<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companymazda + 
                car_companypeugeot + car_companysubaru + 
                car_companytoyota + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_26)

#check for multicollinearity
vif(model_26)

#Removing car_companypeugeot

model_27<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companymazda + 
                car_companysubaru + 
                car_companytoyota + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_27)

#check for multicollinearity
vif(model_27)

#Remove car_companymazda

model_28<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                car_companytoyota + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_28)

#check for multicollinearity
vif(model_28)

#Remove car_companytoyota

model_29<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_29)

#check for multicollinearity
vif(model_29)
#Adjusted R-squared:0.9437

######### Now checking adjusted squared value by removing variables with high VIF(greater than 2) one by one.


#checking adjusted Rsquared after removing cylindernumberfour from after model_29
model_30<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumberfive +  
                cylindernumbersix + car_vol , data = train)

summary(model_30)
vif(model_30)
#Adjusted R-squared:  0.891

#checking adjusted Rsquared after removing cylindernumberfive from after model_29

model_31<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_31)
vif(model_31)

#Adjusted R-squared:  0.9218

#checking adjusted Rsquared after removing cylindernumbersix from after model_29

model_32<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumbersix + car_vol , data = train)

summary(model_32)
vif(model_32)

#Adjusted R-squared:  0.8918


#checking adjusted Rsquared after removing enginesize from after model_29

model_33<- lm(formula = price ~ aspiration + enginelocation + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_33)
vif(model_33)

#Adjusted R-squared:  0.8944

#checking adjusted Rsquared after removing car_vol from after model_29

model_34<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companybuick + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)

summary(model_34)
vif(model_34)

#Adjusted R-squared:  0.9259

#checking adjusted Rsquared after removing car_companybuick from after model_29

model_35<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + car_companychevrolet + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_35)
vif(model_35)

#Adjusted R-squared:  0.9331

# If we remove car_companybuick there is no significant impact in the value of adjusted R square. It is getting dropped from 0.9437 to 0.9331
# So will continue taking model_35

# Removing car_companychevrolet from model_35

model_36<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + 
                car_companyjaguar + 
                car_companysubaru + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_36)
vif(model_36)
#Adjusted R-squared:  0.9264

#Removing variable car_companysubaru 

model_37<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                stroke + car_companybmw + 
                car_companyjaguar + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_37)
vif(model_37)

#Adjusted R-squared:  0.9215

#Removing variable stroke

model_38<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                car_companybmw + 
                car_companyjaguar + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_38)
vif(model_38)

#Adjusted R-squared:  0.9156

#####Below Checking adjusted squared value by removing variables with high VIF(greater than 2) one by one from after model_38

#checking adjusted Rsquared after removing cylindernumberfour from after model_38
model_39<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                car_companybmw + 
                car_companyjaguar + 
                cylindernumberfive + 
                cylindernumbersix + car_vol , data = train)


summary(model_39)
vif(model_39)

#Adjusted R-squared:  0.8248

#checking adjusted Rsquared after removing cylindernumbersix from after model_38
model_40<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                car_companybmw + 
                car_companyjaguar + 
                cylindernumberfive + cylindernumberfour + 
                car_vol , data = train)

summary(model_40)
vif(model_40)

#Adjusted R-squared:  0.8613

#checking adjusted Rsquared after removing enginesize from after model_38
model_41<- lm(formula = price ~ aspiration + enginelocation + 
                car_companybmw + 
                car_companyjaguar + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_41)
vif(model_41)

#Adjusted R-squared:  0.8333

#checking adjusted Rsquared after removing car_vol from after model_38
model_42<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                car_companybmw + 
                car_companyjaguar + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)

summary(model_42)
vif(model_42)

#checking adjusted Rsquared after removing cylindernumberfive from after model_38
model_43<- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + 
                car_companybmw + 
                car_companyjaguar + 
                cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_43)
vif(model_43)

#Adjusted R-squared:  0.8889

# If we remove cylindernumberfive there is not so much impact in the value of adjusted R square.It has decreased from 0.9156 to 0.8889
# So will continue taking model_43

# Removing aspiration from after model_43

model_44<- lm(formula = price ~ enginelocation + 
                enginesize + 
                car_companybmw + 
                car_companyjaguar + 
                cylindernumberfour + 
                cylindernumbersix + car_vol , data = train)

summary(model_44)
vif(model_44)

#Adjusted R-squared:0.8856

# Finally the  model is created where all variables are significant and there is no threat of multicollinearity.
# Final model is model_44 having Adjusted R-squared:0.8856
# There are finally 7 independant variables in model_44 as below:
#1) enginelocation
#2) enginesize
#3) car_companybmw
#4) car_companyjaguar
#5) cylindernumberfour
#6) cylindernumbersix
#7) car_vol

###################################################TEST SAMPLE########################
# Predicting the results in Test dataset
Predict_1 <- predict(model_44,test[,-18])
test$test_price <- Predict_1

# Test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

########## rsquared is 0.838477 #####################################

########## The deviation in Rsquare between actual vs predicted and Adjusted R-squared of the model is less then 5% (0.047)