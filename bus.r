# Set seed for reproducibility
set.seed(123)
nreps<-10000
p_enter <- c(0.5, 0.4, 0.1)
L1_arr<-c()
L2_arr<-c()
for(i in 1:nreps){

##stop1
passenger=0
if(passenger>0){
  for(k in 1:passenger){
    if(runif(1)<0.2){
      passenger=passenger-1
    }
  }

}

# Determine number of passengers entering
new_pass <- sample(0:2, 1, prob = p_enter)

passenger<-passenger+new_pass

L1_arr[i]<-passenger


##stop2
if(passenger>0){
  for(k in 1:passenger){
    if(runif(1)<0.2){
      passenger=passenger-1
    }
  }
  
}

# Determine number of passengers entering
new_pass <- sample(0:2, 1, prob = p_enter)

passenger<-passenger+new_pass

L2_arr[i]<-passenger

}

L1_L2_arr=L1_arr-L2_arr

var(L1_L2_arr)

var(L1_arr)

var(L2_arr)

cov(L1_arr,L2_arr)
