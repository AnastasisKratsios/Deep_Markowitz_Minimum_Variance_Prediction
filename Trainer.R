sharpe_ratio_grid<-seq(from=0,to=1,length.out = 10)
### Trainer
#============#
# Note: Trains Model and Benchmarks then evaluates Quality
#---------------------------------------------------------#
# RESET: rm("NO.CHECK_Q")



#---------------------------------------------------------------------#
# Initializations
#---------------------------------------------------------------------#
if(!exists("NO.CHECK_Q")){ 
  # Only run on local machine
  print("Setting: Paths"); 
  #setwd(paste(getwd(),"Polybox/UniBag/NE_BLNs/Numerics/",sep="/"))
  print("loading: Utils"); source("utils_NE_BLNS.R")
  beep(1)
  # Simulated DATA: ONLY FOR TESTING: source("Simul.R")
  # Crypto DATA:
  print("loading: Crypto_Data"); source("Crypto_Currency_Data_Processor.R")
  beep(2) 
  # Ensure that Initializations are only run once
  NO.CHECK_Q<-TRUE
  # Re-Anchor Working directory
  setwd(anchor_directory)
  # Beep to signal completion
  beep(3) 
}

# BEGIN TIMER A
Time.Start.Cov.Learning  = Sys.time() 

### Automated (Descriptive) Parmaeters
#-------------------------------------#
# Data Parameters
#-------------------------------------#
# Dimension Simulated Matrices
d<-(dim(X)[1])
# Number of Data Points
N<-(dim(X)[3])


##### -----------#
# Learning Phase
##### -----------#

# Apply Random Feature Map
#-------------------------#
# Initialize Dimension of Problem
d_intrinsic<-d*(d+1)/2
# Initialize Matrix of Xs
X_vect_test<-X_vect<-matrix(NA,nrow=(dim(X)[3] - 1),ncol=(d_intrinsic+prod(dim(X.ts)[-3]))) # First Dimension for Estimated Cov Matrix and Second For Data On that Time-Window
Y_vect_test<-Y_vect<-matrix(NA,nrow=(dim(X)[3] - 1),ncol=d_intrinsic)

# Initialize Results Matrix
Y_Predicted_with_NE_X_TEST<-Y_Predicted_with_NE_X<-matrix(NA,nrow=(N*d_intrinsic),ncol=1)
Y.pred<-as.numeric(Y_vect)

  # Random Reference Matrix
  rand.ref.mat<-rSPDd(d)

  for(i in 1:(X_vect %>% nrow())){
    # Generate Training-Set Features
    X.i.loop<-feature.map(X[,,i],rand.ref.mat) # Map SPD to R^*
    X.i.loop<-c(X.i.loop,as.numeric(X.ts[,,i])) # Append TS Data from time-window used to generate SPD matrix (Cov)
    X_vect[i,]<-X.i.loop # Update Features Matrix
    
    # Generate Test-Set Features 
    X.i.loop<-feature.map(X_test[,,i],rand.ref.mat) # Map SPD to R^*
    X.i.loop<-c(X.i.loop,as.numeric(X.ts_test[,,i])) # Append TS Data from time-window used to generate SPD matrix (Cov)
    X_vect_test[i,]<-X.i.loop # Update Features Matrix
    
    # Generates Targets (a.k.a: Responses/Ys)
    Y_vect[i,]<-feature.map(Y[,,i],rand.ref.mat)
    Y_vect_test[i,]<-feature.map(Y_test[,,i],rand.ref.mat)
    
    # Update User on progress of scheme
    print(i/(X_vect %>% nrow()))
  }
  
  
  # Data Segmentation 
  train_data<-X_vect
  trainingtarget<-Y_vect
  test_data<-X_vect_test
  test_target<-Y_vect_test
  
  #-------------------------------#
  # Pre-processing
  #-------------------------------#
  # Normalize Training Data
  m.train<-colMeans(train_data)
  s.train<-apply(train_data,2,sd)
  train_data <- scale(train_data,center = m.train,scale = s.train) 
  
  # Use means and standard deviations from training set to normalize test set
  col_means_train <- attr(train_data, "scaled:center") 
  col_stddevs_train <- attr(train_data, "scaled:scale")
  test_data <- scale(test_data, center = m.train, scale = s.train)
  
  
  #---------------------------------#
  # B.1: (Deep) Covariance Learning
  #---------------------------------#
  
  model_NE_Deep_Cov<-keras_model_sequential()
  # Define bulk of the network
  model_NE_Deep_Cov %>% layer_dense(units=Height,activation = "relu",input_shape = (X_vect %>% ncol()))
  
  
  for(i in 1:Depth){
    model_NE_Deep_Cov %>% layer_dense(units=Height,activation = "relu",input_shape = d_intrinsic)%>% 
    layer_dropout(rate = dropout.rate) 
  }
  
  # Readout Layer (ffNN)
  model_NE_Deep_Cov %>% layer_dense(units=d_intrinsic)
  
  # Compile (ffNN)
  model_NE_Deep_Cov %>% keras::compile(loss="mse",
                           optimizer="adam",
                           metrics="mse")
  
  
  ## Report Model (Summary)
  model_NE_Deep_Cov %>% summary()
  
  # Compute Batch Size
  batch.size<-max(1,(round(min(1,abs(Batch.size.percent))*length(train_data),digits = 0)))
  
  # Fit ffNN
  fittedmodel_NE_Deep_Cov<- model_NE_Deep_Cov %>%
    keras::fit(train_data,
               trainingtarget,
               epochs=epochs,
               batch_size=batch.size, 
               callbacks = list(# Overfitting Reduction
                 callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1)
               ))
  
  
  
  ## Predictions ffNN
  y.predict<-model_NE_Deep_Cov %>% predict(train_data)
  y.predict_test<-model_NE_Deep_Cov %>% predict(test_data)
  
  # END TIMER A
  Time.END.Cov.Learning = Sys.time()
  
  # Error Analysis
  #----------------#
  # LOG-EUC Errors (Vectorized Errors)
  LogEUC_var_err<-c(var(as.numeric(y.predict-trainingtarget)),var(as.numeric(y.predict_test-test_target)))
  LogEUC_RMAE<-c(mean(abs(y.predict-trainingtarget))/mean(abs(trainingtarget)),mean(abs(y.predict_test-test_target))/mean(abs(test_target)))
  # Frobenius Errors (Matricial Errors)
  Frob_MSE<-rep(0,2)
  for(i.error in 1:(y.predict %>% nrow())){# BEGIN EVALUATION
  # Frobenius Errors (Matrix MSE): BEGIN
  #-------------------------------------------------------------------------------#
    
  # Evaluate Frobenius Norm on Training Set
  ith.predicted.matrix<-readout.map(y.predict[i.error,],rand.ref.mat)
  ith.target.matrix<-readout.map(trainingtarget[i.error,],rand.ref.mat)
  Frob_MSE[1]<-Frob_MSE[1]+norm(ith.predicted.matrix-ith.target.matrix,"f")
  
  # Evaluate Frobenius Norm on Test Set
  ith.predicted.matrix<-readout.map(y.predict_test[i.error,],rand.ref.mat)
  ith.target.matrix<-readout.map(test_target[i.error,],rand.ref.mat)
  Frob_MSE[2]<-Frob_MSE[2]+norm(ith.predicted.matrix-ith.target.matrix,"f")  
  
  # Update User on Status of Error Evaluation
  print(i.error/(y.predict %>% nrow()))
  # Frobenius Errors (Matrix MSE): END
  #-------------------------------------------------------------------------------#
  } #END LOOP
  # Average Accross all Observed Matrix (MSEs)
  Frob_MSE<-Frob_MSE/(y.predict %>% nrow())
  #---------------------#
  # END ERROR EVALUATION
  
  
### Report Findings
Reports_Cov_Prediction<-rbind(LogEUC_var_err,LogEUC_RMAE,Frob_MSE); colnames(Reports_Cov_Prediction)<-c("Training","Test")
Reports_Cov_Prediction



#---------------------------------#
# B.2: (Deep) Alpha Learning
#---------------------------------#
Time.Start.Returns.Learning = Sys.time()
# Use data from same input space but notw output space is R^d

# Preprocess Training/Testing Targets (1 time-step ahead Movements)
#-#
# Initialize Price Changes (Training Set)
active_index_train<-(1+Window.size):(Window.size+N) # Start one day previous to evaluate first price movement
coin_exchange_movements_rates_train<-diff(prices_TS[active_index_train,])

# Initialize Price Changes (Tes ting Set)
active_index_test<-(1+N+Window.size):(Window.size+2*N)  # Start one day previous to evaluate first price movement
coin_exchange_movements_rates_test<-diff(prices_TS[active_index_test,])

# Data Segmentation 
trainingtarget_NE_Movements<-coin_exchange_movements_rates_train
test_target_NE_Movements<-coin_exchange_movements_rates_test


# Initialize Network
#-----------------------#
model_NE_Movements<-keras_model_sequential()
# Define bulk of the network
model_NE_Movements %>% layer_dense(units=Height,activation = "relu",input_shape = (X_vect %>% ncol()))


for(i in 1:Depth){
  model_NE_Movements %>% layer_dense(units=Height,activation = "relu",input_shape = d_intrinsic)%>% 
    layer_dropout(rate = dropout.rate) 
}

# Readout Layer (ffNN)
model_NE_Movements %>% layer_dense(units=d)

# Compile (ffNN)
model_NE_Movements %>% keras::compile(loss="mse",
                                     optimizer="adam",
                                     metrics="mse")


## Report Model (Summary)
model_NE_Movements %>% summary()

# Compute Batch Size
batch.size<-max(1,(round(min(1,abs(Batch.size.percent))*length(train_data),digits = 0)))

# Fit ffNN
fittedmodel_NE_Movements<- model_NE_Movements %>%
  keras::fit(train_data,
             trainingtarget_NE_Movements,
             epochs=epochs,
             batch_size=batch.size, 
             callbacks = list(# Overfitting Reduction
               callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1)
             ))



## Predictions ffNN
y.predict_NE_Movements<-model_NE_Movements %>% predict(train_data)
y.predict_NE_Movements_test<-model_NE_Movements %>% predict(test_data)

# END TIMER A
Time.END.Returns.Learning = Sys.time()


#-------------------------------------------------------------------------------#
# C) Generate Efficient Portfolios
#-------------------------------------------------------------------------------#
# Initialize Reports
Financial_Reports_NE_Efficient<-matrix(NA,nrow=(sharpe_ratio_grid %>% length()),ncol=3)
colnames(Financial_Reports_NE_Efficient)<-c("Trading Gains","Portfolio Variance","Sharpe Ratio"); rownames(Financial_Reports_NE_Efficient)<-round(sharpe_ratio_grid,1)
Financial_Reports_NE_Efficient_test<-Financial_Reports_NE_Efficient

for(j.Sharpe_param in 1:(Financial_Reports_NE_Efficient %>% nrow())){# LOOP OVER ALL SHARPE_RATIO PARAMETERS
# Initialize Current Sharpe Ration Meta-Parameter
Sharpe_loop = sharpe_ratio_grid[j.Sharpe_param]


# Initialized MV Portfolios
Efficient_NE_Portfolio_Predictions = matrix(NA, nrow = (y.predict %>% nrow()), ncol=d)
Efficient_NE_Portfolio_Predictions_test = matrix(NA, nrow = (y.predict_test %>% nrow()), ncol=d)


# Generate Minimum Variance Portfolio with NE-Predicited Cov Matrices (Training)
# (Training)
for(i.error in 1:(y.predict %>% nrow())){# BEGIN EVALUATION
  # Generate Predicted Matrix (Training Set)
  ith.predicted.matrix<-readout.map(y.predict[i.error,],rand.ref.mat)
  # Write Predicted Market Returns (alpha)
  alpha.movement.ith<-y.predict_NE_Movements[i.error,]
  # Build Minimum-Variance Portfolio
  Efficient_NE_Portfolio_Predictions[i.error,]<-Efficient(ith.predicted.matrix,alpha.movement.ith,Sharpe_loop)
  #-------------------------------------------------------------------------------#
  # Update User on These Status of Compation
  print(i.error/(y.predict %>% nrow()))
} #END LOOP
#(Test)
# Generate Minimum Variance Portfolio with NE-Predicited Cov Matrices (Test)
for(i.error in 1:(y.predict_test %>% nrow())){# BEGIN EVALUATION
  # Generate Predicted Matrix (Training Set)
  ith.predicted.matrix<-readout.map(y.predict_test[i.error,],rand.ref.mat)
  # Write Predicted Market Returns (alpha)
  alpha.movement.ith<-y.predict_NE_Movements_test[i.error,]
  # Build Minimum-Variance Portfolio
  Efficient_NE_Portfolio_Predictions_test[i.error,]<-Efficient(ith.predicted.matrix,alpha.movement.ith,Sharpe_loop)
  #-------------------------------------------------------------------------------#
  # Update User on These Status of Compation
  print(1 + (i.error/(y.predict %>% nrow()))) # The +1 is to distinguish between the training and test set computations (from the outside)
  #-------------------------------------------------------------------------------#
}

# Evaluate & Record Gains from Trading
Financial_Reports_NE_Efficient[j.Sharpe_param,1]<-sum(colSums(Efficient_NE_Portfolio_Predictions*coin_exchange_movements_rates_train))
Financial_Reports_NE_Efficient_test[j.Sharpe_param,1]<-sum(colSums(Efficient_NE_Portfolio_Predictions_test*coin_exchange_movements_rates_test))

# Evaluate Portfolio Variance
Financial_Reports_NE_Efficient[j.Sharpe_param,2]<-mean(sapply(as.data.frame(Efficient_NE_Portfolio_Predictions*coin_exchange_movements_rates_train),var))
Financial_Reports_NE_Efficient_test[j.Sharpe_param,2]<-mean(sapply(as.data.frame(Efficient_NE_Portfolio_Predictions_test*coin_exchange_movements_rates_test),var))

# Sharpe Ratio
Financial_Reports_NE_Efficient[j.Sharpe_param,3]<-Financial_Reports_NE_Efficient[j.Sharpe_param,2]/Financial_Reports_NE_Efficient[j.Sharpe_param,1]
Financial_Reports_NE_Efficient_test[j.Sharpe_param,3]<-Financial_Reports_NE_Efficient_test[j.Sharpe_param,2]/Financial_Reports_NE_Efficient_test[j.Sharpe_param,1]

}# END:LOOP OVER ALL SHARPE_RATIO PARAMETERS



# Determine Optimal (Largest) Sharpe-Ratio (In Training Set <- Probably can be further improved by CV!): i.e.: Min Variance and Max Returns!
opt.Sharpe<-which.max(Financial_Reports_NE_Efficient[,3])

# Define Optimal NE-Efficient Portfolio
Financial_Reports_NE_Efficient_opt = cbind(Financial_Reports_NE_Efficient[opt.Sharpe,],Financial_Reports_NE_Efficient_test[opt.Sharpe,])



### # Benchmark 1: Naive Markowitz
###---------------------------------#
source("Markowitz.R")



### # Benchmark 2: Deep Hedging Approaches
###----------------------------------------#
source("Deep_Hedging.R")




### AGREGATED REPORTS
#====================#
Financial_Reports_Train<-rbind(Financial_Reports_NE_Efficient_opt[,1],
                               Financial_Reports_DH[,1],
                               Financial_Reports_Mark[,1]); rownames(Financial_Reports_Train)<-c("NE_ffNN","DH","Mark")
Financial_Reports_Test<-rbind(Financial_Reports_NE_Efficient_opt[,2],
                              Financial_Reports_DH[,2],
                              Financial_Reports_Mark[,2]); rownames(Financial_Reports_Test)<-c("NE_ffNN","DH","Mark")

colnames(Financial_Reports_Test)<-colnames(Financial_Reports_Train)<-c("Trading Gains","Portfolio Variance","Sharpe Ratio")

# Reports Finding(s)
Financial_Reports_Train
Financial_Reports_Test
Financial_Reports_NE_Efficient
Financial_Reports_NE_Efficient_test