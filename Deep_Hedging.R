###############################-#
#-    Deep Hedging             -#
#-------------------------------#

# DH: Model 1: Non-Anticipative Portfolio
#----------------------------------------#

# Build a normalized non-anticipative Portfolio defined by the non-adapted rules:
# Buy if value goes up @ time t+1
# Sell if value goes down @ time t+1


# Buy if future value goes up and sell if it goes down
anticipative<-(coin_exchange_movements_rates_train>0)-(coin_exchange_movements_rates_train<0)
# Determine which time points are neural 
neutral_trades<-!(rowMeans(anticipative)==0)
# Normalize non-neural times to only transact by 1$
anticipative[neutral_trades,]<-anticipative[neutral_trades,]/rowSums(abs(anticipative[neutral_trades,]))
# Generate Portfolio Value
Anticipative_Portfolio_Value<-anticipative*coin_exchange_movements_rates_train


#-------------------------------#
# Pre-processing
#-------------------------------#

# Write Training Data on windows of "window.size" days
#----#
# Initialization
  train_data<-matrix(NA,nrow=(X_vect %>% nrow()),ncol=(Window.size*d))
  test_data<-matrix(NA,nrow=(X_vect_test %>% nrow()),ncol=(d*Window.size))
  # Note: The training target is the "Anticipative_Portfolio_Value" and the test-set benchmark will be computed using the market movements (So )
# Population
  for(i.pop in 1:(train_data %>%nrow())){
    # Determine Moving Window
    window.i.pop<-c(i.pop:(i.pop + (Window.size -1)))
    # Training Set
    train_data[i.pop,]<-as.numeric(prices_TS[window.i.pop,])
    # Testing Set
    test_data[i.pop,]<-as.numeric(prices_TS[(window.i.pop+N),])
  }

# Normalize Training Data
m.train<-colMeans(train_data)
s.train<-apply(train_data,2,sd)
train_data <- scale(train_data,center = m.train,scale = s.train) 

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = m.train, scale = s.train)


#-------------------------------#
# Build Model ffNN
#-------------------------------#
model<-keras_model_sequential()
# Define bulk of the network
model %>% layer_dense(units=Height,activation = "relu",input_shape = (d*Window.size))


for(i in 1:Depth){
  model %>% layer_dense(units=Height,activation = "relu",input_shape = 1)%>% 
    layer_dropout(rate = dropout.rate) 
}

# Readout Layer (ffNN)
model %>% layer_dense(units=d)

# Compile (ffNN)
model %>% keras::compile(loss="mse",
                         optimizer="adam",
                         metrics="mse")


## Report Model (Summary)
model %>% summary()

# Compute Batch Size
batch.size<-max(1,(round(min(1,abs(Batch.size.percent))*length(train_data),digits = 0)))

# Fit ffNN
fit.ffNN.start<-Sys.time()
fittedmodel<- model %>%
  keras::fit(train_data,
             Anticipative_Portfolio_Value,
             epochs=epochs,
             batch_size=batch.size)



## Predictions ffNN
y.predict_DH<-model %>% predict(train_data)
y.predict_DH_test<-model %>% predict(test_data)


# Test Predicted Portfolio Weights
#---------------------------------#
# Evaluate Gains from Trading (Train)
gains_from_trading_DH_train<-y.predict_DH*coin_exchange_movements_rates_train 
# Normalize Strategy (for fair comparison)
net_pos<-(rowSums(abs(gains_from_trading_DH_train))==0) # On non-neutral position (else 0/0!)
gains_from_trading_DH_train[!net_pos,]<-gains_from_trading_DH_train[!net_pos,]/rowSums(abs(gains_from_trading_DH_train[!net_pos,]))
# Integrate against path
gains_from_trading_DH_train<-sum(colSums(y.predict_DH*coin_exchange_movements_rates_train))

# Evaluate Gains from Trading (Test)
gains_from_trading_DH_test<-y.predict_DH_test*coin_exchange_movements_rates_test
# Normalize Strategy (for fair comparison)
net_pos<-(rowSums(abs(gains_from_trading_DH_test))==0) # On non-neutral position (else 0/0!)
gains_from_trading_DH_test[!net_pos,]<-gains_from_trading_DH_test[!net_pos,]/rowSums(abs(gains_from_trading_DH_test[!net_pos,]))
# Integrate against path
gains_from_trading_DH_test<-sum(colSums(y.predict_DH_test*coin_exchange_movements_rates_test))



# Evaluate Portfolio Variance
Variance_Portfolio_Weights_DH_train<-sapply(as.data.frame(y.predict_DH*coin_exchange_movements_rates_train),var)
Variance_Portfolio_Weights_DH_test<-sapply(as.data.frame(y.predict_DH_test*coin_exchange_movements_rates_test),var)

# Sharpe
Sharpe_DH<-c(gains_from_trading_DH_train,gains_from_trading_DH_test)/
  (cbind(Variance_Portfolio_Weights_DH_test,Variance_Portfolio_Weights_DH_test) %>% colMeans())

# Prepare Financial Report
Financial_Reports_DH<-rbind(c(gains_from_trading_DH_train,
                                gains_from_trading_DH_test),
                              (5*(Variance_Portfolio_Weights_DH_train+
                                    Variance_Portfolio_Weights_DH_test)),Sharpe_DH)

colnames(Financial_Reports_DH)<-c("Training","Test");rownames(Financial_Reports_DH)<-c("Trading Gains","Variance","Sharpe Ratio")





# DH: Model 2: Minimum Variance Portfolio
#----------------------------------------#
# TBD