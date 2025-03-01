#############################################################
### Nickname: Phillip
### Last Name: Malicdem
### Student Number:2015-12162
### Section:THZAB
################ Approximating Logarithms #1 #####################

log_approx <- function(x, tolerance=1e-6){
  k <- 0
  
  guess <- (2/(2*k+1))*(((x-1)/(x+1))^(2*k+1))
  
  while(abs(exp(guess) - x)>tolerance){#Using iteration to approximate the logarithm of a number
    k <- k+1
    guess <-  guess + (2/(2*k+1))*(((x-1)/(x+1))^(2*k+1))

  }
  return(guess) 
  
}

log_approx(10)


################ Approximating Logarithms #2 #####################

log_approx_improved <- function(x, tolerance=1e-6){
  
  k <- 0
  
  if(x<0){#Returns NaN when x<0
    return(NaN)
  }else if(x==0){#Returns -Inf when x==0
    return(-Inf)
  }else if(is.numeric(x)==FALSE){#Returns NA when x is non-numeric
    return(NA)
  }else{
    guess <- (2/(2*k+1))*(((x-1)/(x+1))^(2*k+1))
    
    while(abs(exp(guess) - x)>tolerance){ 
      k <- k+1
      guess <-  guess + (2/(2*k+1))*(((x-1)/(x+1))^(2*k+1))
      
    }
    return(guess) 
  }
  
}

log_approx_improved(-10)
log_approx_improved(0)
log_approx_improved("10")
log_approx_improved(10)


################ Approximating Logarithms #3 #####################

log_approx_recursion <- function(x, tolerance=1e-6){
  
  k<-0
  
  
  if(x<0){
    return(NaN)
  }else if(x==0){
    return(-Inf)
  }else if(is.numeric(x)==FALSE){
    return(NA)
  }else{
    guess <- (2/(2*k+1))*(((x-1)/(x+1))^(2*k+1))
    return(check_exp(guess, x, k+1, tolerance))
  }

}

check_exp <- function(guess, x, k, tolerance){#Function that does the recursion
  
  if(abs(exp(guess) - x)<tolerance){
    return(guess)
  }else{
    guess <- guess + (2/(2*k+1))*(((x-1)/(x+1))^(2*k+1))
    return(check_exp(guess, x, k+1, tolerance))
  }
}

log_approx_recursion(-10)
log_approx_recursion(0)
log_approx_recursion("10")
log_approx_recursion(10)



################Approximating Logarithms #4 #####################

log_approx_self <- function(x, tolerance = 1e-6, k=0, guess = 0){
  
  if(x<0){
    return(NaN)
  }else if(x==0){
    return(-Inf)
  }else if(is.numeric(x)==FALSE){
    return(NA)
  }else{
    guess <- guess + (2/(2*k+1))*(((x-1)/(x+1))^(2*k+1))
  
    if(abs(exp(guess) - x)<tolerance){
      return(guess)#base case
    }else{
      return(log_approx_self(x, tolerance, k = k+1, guess))#Self Recursive step
    }
  } 
}

log_approx_self(-10)
log_approx_self(0)
log_approx_self("10")
log_approx_self(10)
############################################################

################### What’s happening? ######################

#1 The object returned is 1234

#2 The cascade function will be called 3 times so the answer is C.

#3 When the cascade function is called, 4 local environments will be created, so the answer is D.

#4 Changing "error_print()" to "return(error_print())" will obtain this behavior, so the answer is D.

#5 An error message will be returned

#6 The class of log_approx2 is "function"

#7 The class being returned by log_approx2 is "numeric"

#8 Value bound to k is still 1

#9

  #a. The class of log_approx2 is now "numeric"

  #b. The value of log_approx2 is 0.4054651

  #c. An error message will be returned

#############################################################
### RECEIVING HELP LOG
#############################################################

#Figured out what class() does from here https://www.educative.io/answers/what-is-the-class-function-in-r
#Learned how Recursion deals with environments form the study materials
##Got the function is.numeric() from https://www.geeksforgeeks.org/check-if-an-object-is-of-type-numeric-in-r-programming-is-numeric-function/
#Got some clarity for question #3 and #4 in approximation from Sir Xavier Bilon






