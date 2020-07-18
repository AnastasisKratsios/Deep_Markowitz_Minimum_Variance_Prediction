# Initialize Background Variables
#--------
d<-ncol(mat(X_synced_test[1,])); d_intrinsic<- d*(d+1)/2



#-----------------------------------------#
#-----------------------------------------#
## BENCHMARK 1
# Carry Historical Weights to Day Ahead
#-----------------------------------------#
#-----------------------------------------#
# Initialize
portfolio_weights_Mark_hist<-matrix(data=NA,nrow=(nrow(X_synced_test)-1),ncol=d)

# Define Portfolio
for(i in 1:(nrow(X_synced_test)-1)){
  # Compute Corrected Covariance Matrix
  Historical.Realized.Cov<-mat(X_synced_test[i,]) + invertibility_factor*diag(d)
  # Predict Markowitz Portfolio
  portfolio_weights_Mark_hist[i,]<-Efficient(Cov_in = Historical.Realized.Cov,rep(1,d),mean_variance_tradeoff)
}


# Compute Gains From Trading
#----------------------------#
gains_Fix_Day_Ahead<-Gains_Trading(portfolio_weights_Mark_hist,prices_TS_considered_test_synced)

#-----------------------------------------#
#-----------------------------------------#
## BENCHMARK 2
# Matrix Regression - Fletcher: Log Exponential
#-----------------------------------------#
#-----------------------------------------#
# NOTE:  X_test[(i+Window.size-1),]==X_synced_test[i,] (to help keep track of indices)

# Initialize
portfolio_weights_Mark_log_exp<-matrix(data=NA,nrow=(nrow(X_synced_test)-1),ncol=d)

# Updates Matrix (Online Design)
for(i in 1:(nrow(X_synced_test)-1)){
  
  # Reste Reference Matrix
  Covariance_Estimate_Online<-diag(d)
  
  
  # Extract t-1 covariance matrix
  Output_feature_map<-feature.map((mat(X[(i+Window.size-2),]) +  invertibility_factor*diag(d)),reference_matrix = Covariance_Estimate_Online)
  # Extract t covariance matrix
  Online_Target<-feature.map((mat(X[(i+Window.size-1),]) +  invertibility_factor*diag(d)),reference_matrix = Covariance_Estimate_Online)
  
  # Regress Covariance t-1 -> t 
  fit_online<-lm(Online_Target~Output_feature_map)
  predicted_Euc_feature<-(Output_feature_map*(fit_online$coefficients[-1] )) + (fit_online$coefficients[1])
  Covariance_Estimate_Online<-readout.map(predicted_Euc_feature,Covariance_Estimate_Online)
  
  # Determine Porfolio Weights
  portfolio_weights_Mark_log_exp[i,]<-Efficient(Cov_in = Covariance_Estimate_Online,rep(1,d),mean_variance_tradeoff)
  
  
  # Update on Completion Percentage
  print(i/(nrow(X_synced_test)-1))
}; beep(1) # END ONLINE ALGORITHM



# Compute Gains From Trading
#----------------------------#
gains_Fix_Online_log_exp<-Gains_Trading(portfolio_weights_Mark_log_exp,prices_TS_considered_test_synced)



#------------------------------------------------#
#------------------------------------------------#
# Benchmark 3
# Deep Hedging of Day-Ahead's Markowitz Portfolio
#------------------------------------------------#
#------------------------------------------------#

# Compute Markowitz using times t-W,...,t-1 & t
# Learn function mapping t-W,...,t-1 -> t

# Prepare Training Data
#------------------------#
train_data<-matrix(NA,nrow=(nrow(X_synced)-1),ncol = (Window.size*ncol(prices_TS_considered_synced)))
test_data<-matrix(NA,nrow=(nrow(X_synced_test)-1),ncol = (Window.size*ncol(prices_TS_considered_test_synced)))

# Transform X to Train Data 
for(i in 1:(nrow(train_data))){
  # Initialize current time indices
  index.loop.deepdata<-c(1:Window.size)+(i-1)
  
  # Vectorize Historical Prices (Training)
  train_data[i,]<-as.numeric(prices_TS_considered[index.loop.deepdata,])
}



# Transform X to Test Data
# NOTE: done separately because sizes may not line up
for(i in 1:(nrow(test_data))){
  # Initialize current time indices
  index.loop.deepdata<-c(1:Window.size)+(i-1)
  # Vectorize Historical Prices (Test)
  test_data[i,]<-as.numeric(prices_TS_considered_test[index.loop.deepdata,])
}

# Training Targets (Day Ahead Markowitz Portfolios on training_set time indices)
trainingtarget<-matrix(data=NA,nrow=(nrow(Y_synced)-1),ncol=d)

# Define Portfolio
for(i in 1:(nrow(trainingtarget))){
  # Compute Corrected Covariance Matrix
  Historical.Realized.Cov<-mat(Y_synced[i,]) + invertibility_factor*diag(d) # note Y_synced is t+1's realized cov
  # Predict Markowitz Portfolio
  trainingtarget[i,]<-Efficient(Cov_in = Historical.Realized.Cov,rep(1,d),mean_variance_tradeoff)
}

# Training Targets (Day Ahead Markowitz Portfolios on test_set time indices)
testtarget<-matrix(data=NA,nrow=(nrow(Y_synced_test)-1),ncol=d)

# Define Portfolio (Training Tagets)
for(i in 1:(nrow(testtarget))){
  # Compute Corrected Covariance Matrix
  Historical.Realized.Cov<-mat(Y_synced_test[i,]) + invertibility_factor*diag(d) # Note Y_synced_test is t+1's realized cov (on test set)
  # Predict Markowitz Portfolio
  trainingtarget[i,]<-Efficient(Cov_in = Historical.Realized.Cov,rep(1,d),mean_variance_tradeoff)
}






# Normalize Training Data
m.train<-colMeans(train_data)
s.train<-apply(train_data,2,sd)
train_data <- scale(train_data,center = m.train,scale = s.train) 

# Use means and standard deviations from training set to normalize test set
test_data <- scale(test_data, center = m.train, scale = s.train)


#-------------------------------#
# Build and Train ffNN
#-------------------------------#

# Build ffNN
#-----------#
layers<-keras_model_sequential()
# Define bulk of the network
for(i.d in 1:Depth){
  # Define i.d^th layer
  if(i.d<Depth){#Non-Readout Layer
    layers %>% layer_dense(units=Height,
                      activation = "relu",
                      trainable=TRUE,
                      input_shape = (train_data %>%ncol()),
                      bias_initializer = "random_normal",
                      name=str(i.d)) 
  
  }else{ # Readout Layer
    layers %>% layer_dense(units = d,
                      activation = "linear",
                      trainable = TRUE,
                      bias_initializer = "random_normal",
                      name=str(i.d))
}  
#
}#END NETWORK BUILDER  





# Compile & Train Model
#----------------------#
# Compile (ffNN)
layers %>% keras::compile(loss="mse",
                         optimizer="adam",
                         metrics="mse")


## Report Model (Summary)
layers %>% summary()

# Compute Batch Size
batch.size<-max(1,(round(min(1,abs(Batch.size.percent))*length(train_data),digits = 0)))

# Train ffNN
#-----------------#
fit.ffNN.start<-Sys.time()
fittedmodel<- layers %>%
  keras::fit(train_data,
             trainingtarget,
             epochs=Epochs,
             batch_size=batch.size)

# Notify
beep(5)

## Predictions ffNN
#-------------------#
y.predict_Deep_Heding_Markowitz_weights<-layers %>% predict(train_data)
y.predict_Deep_Heding_Markowitz_test_weights<-layers %>% predict(test_data)





# Compute Gains From Trading
#----------------------------#
gains_Trading_Deep_Hedging_DH_Markowitz<-Gains_Trading(y.predict_Deep_Heding_Markowitz_test_weights,prices_TS_considered_test_synced) 















#===============#
### SUMMARIZE ###
#===============#
# Terminal Wealth
Terminal_Wealth<-rbind(
sum(gains_Fix_Day_Ahead[nrow(gains_Fix_Day_Ahead),]),
sum(gains_Fix_Online_log_exp[nrow(gains_Fix_Online_log_exp),]),
sum(gains_Trading_Deep_Hedging_DH_Markowitz[nrow(gains_Trading_Deep_Hedging_DH_Markowitz),]));rownames(Terminal_Wealth)<-c("Fix","LE","DH");colnames(Terminal_Wealth)<-"TerminalGains_From_Traiding"


# Empirical Sharpe Ratios
Empirical_Sharpe<-rbind(Sharpe_Realized(gains_Fix_Day_Ahead),
Sharpe_Realized(gains_Fix_Online_log_exp),
Sharpe_Realized(gains_Trading_Deep_Hedging_DH_Markowitz));rownames(Empirical_Sharpe)<-c("Fix","LE","DH")
