## Visualize Results
#---------------------#
layout(matrix(c(1,2,3,3),nrow=2,byrow=TRUE))
plot(gains_Fix_Day_Ahead[,1],type="l",col="red")
  lines(gains_Fix_Online_log_exp[,1],type="l",col="blue")
  lines(gains_Trading_Deep_Hedging_DH_Markowitz[,1],type="l",col="green")
  lines(y_predict_Deep_Heding_NE_Markowitz_test_weights_matrix_predictions[,1],type="l",col="purple",lwd=1.5)

plot(gains_Fix_Day_Ahead[,2],type="l",col="red")
  lines(gains_Fix_Online_log_exp[,2],type="l",col="blue")
  lines(gains_Trading_Deep_Hedging_DH_Markowitz[,2],type="l",col="green")
  lines(y_predict_Deep_Heding_NE_Markowitz_test_weights_matrix_predictions[,2],type="l",col="purple",lwd=1.5)
  
plot(rowSums(gains_Fix_Day_Ahead),type="l",col="red")
  lines(rowSums(gains_Fix_Online_log_exp),type="l",col="blue")
  lines(rowSums(gains_Trading_Deep_Hedging_DH_Markowitz),type="l",col="green")
  lines(rowSums(y_predict_Deep_Heding_NE_Markowitz_test_weights_matrix_predictions),type="l",col="purple",lwd=1.5)
  
# LEGEND
  legend("topleft", legend=c("Fix","LE","DH","DM"),
         col=c("red", "blue","green","purple"), cex=0.5,pch=c(1,1,1))
    
par(mfrow=c(1,1))


# Report Improvements over Naive Model
#----------------------------------------#
Terminal_Wealth
Gains_Trading(y_predict_Deep_Heding_NE_Markowitz_test_weights_matrix_predictions,prices_TS_considered_test_synced) 

Empirical_Sharpe
Sharpe_Realized(y_predict_Deep_Heding_NE_Markowitz_test_weights_matrix_predictions)
