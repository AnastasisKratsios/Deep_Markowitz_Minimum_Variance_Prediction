# Load Data
#-----------------------------------#
if(!exists("Data.Loaded.Q")){
# Move to Data Directory
setwd(anchor_directory)
setwd(paste(getwd(),"/Data/",sep="/"))
gemini_ETHUSD_2019_1min <- read.csv("gemini_ETHUSD_2019_1min.csv", header=FALSE)
gemini_BTCUSD_2019_1min <- read.csv("gemini_BTCUSD_2019_1min.csv", header=FALSE)

# Move back to anchor/reference directory
setwd(anchor_directory)
# Mark Completion
Data.Loaded.Q<-TRUE
# Signals Completion
beep(6)
}


## Extract Covariances
#---------------------#
# Coercion ETH
close.prices_ETH<-gemini_ETHUSD_2019_1min$V7
close.prices_ETH<-as.numeric(as.vector(unlist(close.prices_ETH)))
close.prices_ETH<-close.prices_ETH[-c(1,2)]
# Coercion BTC
close.prices_BTC<-gemini_BTCUSD_2019_1min$V7
close.prices_BTC<-as.numeric(as.vector(unlist(close.prices_BTC)))
close.prices_BTC<-close.prices_BTC[-c(1,2)]
# Prices_TS
prices_TS<-cbind(close.prices_ETH,close.prices_BTC)
# Vlisualizes Prices
head(prices_TS)


# Initializations
#-----------------#
# If no prespecified data-size is given then N is taken to be the length of the entire dataset!
if(!exists("N_train")){
  N_train<-nrow(prices_TS); N_train<- round((N_train - Window.size)*Train_Test_Ratio,0) #Number of Observations in Training/Test Sets
  N_test<-N_train
}


# Data + history
#----------------#
# PRICE DATA
# Training
prices_TS_considered<-prices_TS[c(1:(N_train+Window.size)),] 
# Testing
prices_TS_considered_test<-prices_TS[c((1+N_train+Window.size):((N_train+N_test)+Window.size)),] 


# MATRIX DATA
# Training
Y<-Realized_Covariance(prices_TS_considered[,]) 
  Y<-Y[-1,] # Sets to Day-ahead predictions
X<-Realized_Covariance(prices_TS_considered[,]) # Current Covariance
  X<-X[-(nrow(X)),] # Removes last dat to align with Y

# Test
  Y_test<-Realized_Covariance(prices_TS_considered_test[,]) 
    Y_test<-Y_test[-1,] # Sets to Day-ahead predictions
  X_test<-Realized_Covariance(prices_TS_considered_test[,]) # Current Covariance
    X_test<-X_test[-(nrow(X_test)),] # Removes last dat to align with Y
  

# Price Data
#------------#
    # Syncted data to matrix data dates....this is used to run tests not for predictions...in that case use prices_TS_considered is it contains historical data
    # These are synced to the time indices for X
# PRICE DATA
# Training
prices_TS_considered_synced<-prices_TS_considered[-c(1:(Window.size+1)),] # Window.size+1 because there is a diff operator used when computing X so remove first day here
# Test
prices_TS_considered_test_synced<-prices_TS_considered_test[-c(1:(Window.size+1)),] # Window.size+1 because there is a diff operator used when computing X so remove first day here

# MATRIX DATA
Y_synced<-Realized_Covariance(prices_TS_considered[,]) 
  Y_synced<-Y_synced[-c(1:(Window.size-1)),] # Remove Window used for Initialization
  Y_synced<-Y_synced[-1,] # Sets to Day-ahead predictions
X_synced<-Realized_Covariance(prices_TS_considered[,]) # Current Covariance
  X_synced<-X_synced[-c(1:(Window.size-1)),] # Remove Window used for Initialization
  X_synced<-X_synced[-(nrow(X_synced)),] # Removes last dat to align with Y

  # Test
Y_synced_test<-Realized_Covariance(prices_TS_considered_test[,]) 
  Y_synced_test<-Y_synced_test[-c(1:(Window.size-1)),] # Remove Window used for Initialization
  Y_synced_test<-Y_synced_test[-1,] # Sets to Day-ahead predictions
X_synced_test<-Realized_Covariance(prices_TS_considered_test[,]) # Current Covariance
  X_synced_test<-X_synced_test[-c(1:(Window.size-1)),] # Remove Window used for Initialization
  X_synced_test<-X_synced_test[-(nrow(X_synced_test)),] # Removes last dat to align with Y
