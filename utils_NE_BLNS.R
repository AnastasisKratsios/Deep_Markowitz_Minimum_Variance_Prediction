# rm() 
# set.seed(123)

# Load packages
### INITIALIZATION PORTION
#-############ -----------------------------------################-#
# Load required packages if available...if not install - BEGIN
#-############ -----------------------------------################-#
### The Script is in the file: Installations.R

if(!exists("NO.CHECK_Q")){
  # Check for missing packages
  list.of.packages <- c("expm","MASS","dplyr","stats","keras","abind","nnls","beepr","glmnet")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  # Download missing packages
  if(length(new.packages)) install.packages(new.packages)
  # Load packages requried for code...
  lapply(list.of.packages, require, character.only = TRUE)
  #
  
  # Ensure that this bit is not re-run until next time code it loaded
  NO.CHECK_Q<-TRUE
  # Signal that packags have been loaded and installed
  beep()
}
#
#-############ -----------------------------------################-#
# Load required packages if available...if not install - END
#-############ -----------------------------------################-#





# Coersion Functions
#-------------------------#
# Vectorization: Symmetric Matrix -> Vector
vect<-function(mat_in){
  as.numeric(mat_in[upper.tri(mat_in,diag = TRUE)])
  }
# Matrixification: Vector -> Symmetric Matrix
mat<- function(vect_in){
  # Solves for dimension of Matrix
  d_vect_in<-length(vect_in)
  dim_mat_out<- (sqrt(8*d_vect_in +1) -1)/2
  # Test if Vector of Correct Dimensions
  if(round(dim_mat_out)==dim_mat_out){
    # Initializes Matrix
    dummy.out = matrix(0, nrow = dim_mat_out, ncol = dim_mat_out)
    # Populates Lower Entries
    dummy.out[lower.tri(dummy.out, diag = TRUE)]   <-vect_in
    # Populates Upper Entries
    dummy.out_t<-dummy.out - diag(diag(dummy.out))
    dummy.out<- dummy.out + t(dummy.out_t)
  }else{
    dummy.out = NA
  }  
  # Return Matrix
  return(dummy.out)
}

# Note the mat and vect operators use a different indexing than the paper but this is inconsequential both for contiuity and defininig the bijection...

# Core Functions for Lift
#-------------------------#
# Feature Map 
feature.map<-function(input_symmetric_matrix,reference_matrix){
# Compute Matrices Required for Transformation
reference_matrix_sqrt<-sqrtm(reference_matrix)
reference_matrix_sqrt_in<-solve(reference_matrix_sqrt)
# Produce Output SPD Matrix
output.matrix<-reference_matrix_sqrt%*%logm(reference_matrix_sqrt_in%*%input_symmetric_matrix%*%reference_matrix_sqrt_in)%*%reference_matrix_sqrt
# Vectorize Output
output.vector<-vect(output.matrix)
return(output.vector)
}


# Readout Map 
readout.map<-function(input_symmetric_vector,reference_matrix){
  # Matrixify Input
  input_symmetric_matrix<-mat(input_symmetric_vector)
  # Compute Matrices Required for Transformation
  reference_matrix_sqrt<-sqrtm(reference_matrix)
  reference_matrix_sqrt_in<-solve(reference_matrix_sqrt)
  # Produce Output SPD Matrix
  output.matrix = reference_matrix_sqrt_in%*%input_symmetric_matrix%*%reference_matrix_sqrt_in
  output.matrix = output.matrix %>% expm::expm()
  output.matrix = reference_matrix_sqrt%*% output.matrix%*%reference_matrix_sqrt
  return(output.matrix)
}

# Random PD-Matrix Generation
#-----------------------------#
rSPDd<-function(dim_mat,means.components=0,sd.components=.1,positive.definite.Q=TRUE,normalize.Q=TRUE){
  # Determine Dimension of Random Vector
  dim_vect<-dim_mat*(dim_mat+1)/2 # Number of entries in upper triangular portion of a dim.mat x dim.mat matrix
  # Generate Random Gaussian Vector
  rand_SPDd_vect = rnorm(n=dim_vect,mean = means.components,sd=sd.components)
  # Map Random Vector to SPD Matrix
  rand_SPDd_out =  mat(rand_SPDd_vect)
  
  # Check if the output should be positive definite (or keep it as is.. ie: positive semi-definite) #Can't use feature map (except at the identity) since it requires a reference SPD matrix...
  if(positive.definite.Q){ # Make positive definite using Gershgorin's Disk Theorem
    # Remove Diagonal
    rand_SPDd_out = rand_SPDd_out - diag(diag(rand_SPDd_out))
  # Compute off-diagonal Elements
    abs_rowsums = rowSums(abs(rand_SPDd_out))
    abs_rowsums = abs_rowsums+10^(-2) # Add small quantity to row-sums
    # Replace Diagonal  (Make Main Diagonal Larger than absolute row sums)
    rand_SPDd_out = rand_SPDd_out + diag(abs_rowsums)
  }
  
  # Check weather or not SPD matrix should be normalized
  if(normalize.Q){
    rand_SPDd_out<-cov2cor(rand_SPDd_out)
  }
  
  # Return Value
  return(rand_SPDd_out)
}




# Finance
##------------------------------------------------#
Markowitz<-function(Cov_in){
  # Note(s): Minimum Variance Portfolio
  # See: H. M. Markowitz.Portfolio selection:  Efficient diversification of investments. John Wiley& Sons, Inc., New York, 1959

  # Define Vectors & Matrices Required For Markowitz Portfolio Selection
dim_cov = Cov_in %>% ncol()
ones = rep(1,dim_cov)
Cov_inv = Cov_in %>% solve()
Rescaling_factor = 1/(as.numeric(t(ones)%*%Cov_inv%*%ones))
# Generate Markowitz Portfolio
Markowitz_out<-as.numeric(Rescaling_factor*(Cov_inv%*%ones))
# Return Output
return(Markowitz_out)
}


Efficient<-function(Cov_in,mean_in,min_var_to_mrk_porfolio_ratio=0,normalize.Q=TRUE){
  # Optimizer of:
  # Minimize: .5 w^TΣw
  # s.t.:     m^Tw >= μ 
  #           e^Tw = 1
  
  # Initializations
  Cov_inv = solve(Cov_in)
  ones = rep(1,ncol(Cov_in))
  
  # Minimum-Variance Portfolio Component
  min_var_port = (Cov_inv%*%ones)/(as.numeric(t(ones)%*%Cov_in %*%ones)); min_var_port = min_var_port %>% as.numeric()

  # Market Portfolio
  mrkt_port = (Cov_inv%*%mean_in)/(as.numeric(t(ones)%*%Cov_in %*%mean_in)); mrkt_port = mrkt_port %>% as.numeric()
  
  # Efficient Portfolio
  Markowitz_out = ((1-min_var_to_mrk_porfolio_ratio)*min_var_port) + (min_var_to_mrk_porfolio_ratio*mrkt_port)
  
  # Normalize (remove levaraging)
  if(normalize.Q==TRUE){
    Markowitz_out<- Markowitz_out/sum(abs(Markowitz_out))
  }
  
  # Return Output
  return(Markowitz_out)
}



### Realized Covariance
#Times_Series_in = prices_TS[c(1:(N+Window.size)),]

# Function for producing Sequence
Realized_Covariance<-function(Times_Series_in,normalize_Q=FALSE){ 
  # Takes Differences of Time-Series  
  Delta_TS<-diff(Times_Series_in)
  
  # Initialize Realized Covariance (RC) Estimator (vectorized to save space)
  d<-ncol(Delta_TS)
  RC<-matrix(NA,nrow=nrow(Delta_TS),ncol=(d*(d+1)/2))
  # Populate RC
  for(i.RC in 1:nrow(RC)){
    if(!normalize_Q){ # Covariance Matrix
      RC[i.RC,]<-vect(Delta_TS[i.RC,]%*%t(Delta_TS[i.RC,]))
    }else{ # Correlation Matrix
      RC[i.RC,]<-vect(cov2cor(Delta_TS[i.RC,]%*%t(Delta_TS[i.RC,])))
    }
  }
  # Cumulative Sum
  RC<-apply(RC, 1, cummean) # Scaling is important to obtain convergence; since it's not the # obs that only goes to infinity but also the frequency!
  # Return Result
  return(t(RC))
}

# Time_Series of succesive Realized Volatility Estimates
Realized_Vol<-function(Times_Series_in,normalize_Q=FALSE){
  RC_ts_in<-Realized_Covariance(Times_Series_in)
  # Aproximate Derivative
  RC_ts_in<-diff(RC_ts_in)
  # Evaluate Squre-Root (since SPD this makes sense!)
  for(i.RC in 1:nrow(RC_ts_in)){
    if(!normalize_Q){ # Covariance Matrix
      RC_ts_in[i.RC,]<-vect(expm::sqrtm(mat(RC_ts_in[i.RC,])))
    }else{ # Correlation Matrix
      RC_ts_in[i.RC,]<-vect(cov2cor(expm::sqrtm(mat(RC_ts_in[i.RC,]))))
    }
  }
  # Return output 
  return(RC_ts_in)
}


### FINANCE
# Compute Gains From Trading
#----------------------------#
Gains_Trading<-function(strategy_in,prices_in,transaction_costs_in=0){#Transaction Costs Unused ATM
  # Compute Path Integral
  #-----------------------------#
  # Compute changes in portfolio value at each time point
  gains_trading_internal<-strategy_in*diff(prices_in)
  # Integral
  gains_trading_internal<-apply(gains_trading_internal,2,cumsum)
  # Return Day trading gains
  return(gains_trading_internal)
}

# (Realized/Empirical) Shape Ratio
#---------------------------------#
Sharpe_Realized<-function(TS_in){
  # Initialize Output
  out_SR = rep(NA,3);names(out_SR)<-c("Ratio","Returns","Vars")
  # Compute Returns
  TS_in_returns<-diff(TS_in)
  # Compute Mean Returns
  out_SR[2]<-mean(rowSums(TS_in_returns))
  # Compute Portolio Variance
  out_SR[3]<-sd(rowSums(TS_in_returns))
  # Compute Sharpe Ratio
  out_SR[1]<-(out_SR[2]/out_SR[3])
  # Return Output
  return(out_SR)
}

# Portfolio Weights' Variance
#------------------------------#



# Benchmarking Tools
#----------------------#


# Improvement over Naive
#------------------------#
relative_improvement<-function(test_model,benchmark_model){
  # Compute Errors
  error_rel_improvement_internal<-(rowSums(test_model)-rowSums(benchmark_model))
  # Compute MRE                                   
  MRE_internal<-mean(error_rel_improvement_internal/rowSums(abs(benchmark_model)))
  # Return Output
  return(as.numeric(MRE_internal))
}
