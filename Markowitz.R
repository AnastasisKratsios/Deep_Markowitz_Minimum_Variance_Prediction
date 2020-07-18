###-----------------------------------------------------------------------#
# (Static) Econometrician's Markowitz Portfolio Problem (Minumum Variance)
###-----------------------------------------------------------------------#
covariance.training.returns<-cov(coin_exchange_movements_rates_train)
Markowitz.weights<-Markowitz(covariance.training.returns)



# Evaluate Gains from Trading (Train)
gains_from_trading_Mark_train<-Markowitz.weights*coin_exchange_movements_rates_train 
# Normalize Strategy (for fair comparison)
net_pos<-(rowSums(abs(gains_from_trading_Mark_train))==0) # On non-neutral position (else 0/0!)
gains_from_trading_Mark_train[!net_pos,]<-gains_from_trading_Mark_train[!net_pos,]/rowSums(abs(gains_from_trading_Mark_train[!net_pos,]))
# Integrate against path
gains_from_trading_Mark_train<-sum(colSums(gains_from_trading_Mark_train*coin_exchange_movements_rates_train))

# Evaluate Gains from Trading (Test)
gains_from_trading_Mark_test<-Markowitz.weights*coin_exchange_movements_rates_test
# Normalize Strategy (for fair comparison)
net_pos<-(rowSums(abs(gains_from_trading_Mark_test))==0) # On non-neutral position (else 0/0!)
gains_from_trading_Mark_test[!net_pos,]<-gains_from_trading_Mark_test[!net_pos,]/rowSums(abs(gains_from_trading_Mark_test[!net_pos,]))
# Integrate against path
gains_from_trading_Mark_test<-sum(colSums(gains_from_trading_Mark_test*coin_exchange_movements_rates_test))



# Evaluate Portfolio Variance
Variance_Portfolio_Weights_Mark_train<-sapply(as.data.frame(Markowitz.weights*coin_exchange_movements_rates_train),var)
Variance_Portfolio_Weights_Mark_test<-sapply(as.data.frame(Markowitz.weights*coin_exchange_movements_rates_test),var)

# Sharpe
Sharpe_Mark<-c(gains_from_trading_Mark_train,gains_from_trading_Mark_test)/
  (cbind(Variance_Portfolio_Weights_Mark_test,Variance_Portfolio_Weights_Mark_test) %>% colMeans())

# Prepare Financial Report
Financial_Reports_Mark<-rbind(c(gains_from_trading_Mark_train,
                                gains_from_trading_Mark_test),
                              (.5*(Variance_Portfolio_Weights_Mark_train+
                                    Variance_Portfolio_Weights_Mark_test)),Sharpe_Mark)

colnames(Financial_Reports_Mark)<-c("Training","Test");rownames(Financial_Reports_Mark)<-c("Trading Gains","Variance","Sharpe Ratio")



