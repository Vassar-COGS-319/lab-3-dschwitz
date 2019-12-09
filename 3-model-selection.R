################### Revisions: ##################### 

## The parameters (.0125/4.91, 83.4/90) I chose are extremely close to yours.
## Furthermore, my models approximate the experiment data (i.e., fit) similarly to
## yours, indicating that my modeling approaches and parameters are valid.
## I also draw the same conclusions from the histograms as you did - the RW
## model seems more skewed, while the acc model seems more normally/symetrically
## distributed. However, I draw a different conclusion about the relative
## efficacy of each model for fitting the data - while you conclude that
## neither model is preferable, I conclude that the random walk model
## is preferable. Although it's impossible for me to reliably determine
## whether or not my answer is "right," I did struggle far more to fit the
## data with the acc model than with the RW model. 

random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3){
  rt.array <- c()
  accuracy.array <- c()
  for(i in 1:samples){
    steps <- 0
    current.sum <- 0
    correctness <- FALSE
    while((abs(current.sum) <= criterion)){
      current.sum <- current.sum + rnorm(1, drift, sdrw)
      steps <- steps + 1
    }
    if(current.sum >= criterion) {
      correctness <- TRUE
    }
    accuracy.array <- c(accuracy.array, correctness)
    rt.array <- c(rt.array, steps)
    
  }
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}

#Accumulator

accumulator.model <- function(samples, rate.1=40, rate.2=40, criterion=3){
  
  rt.array <- c()
  accuracy.array <- c()
  for(i in 1:samples){
    sum.1 <- 0
    sum.2 <- 0
    steps <- 0
    correctness <- FALSE
    while((sum.1 <= criterion) && (sum.2 <= criterion)){
      sum.1 <- sum.1 + rexp(1, rate.1)
      sum.2 <- sum.2 + rexp(1, rate.2)
      steps <- steps + 1
    }
    if((sum.1 >= criterion) && (sum.1 >= sum.2)) {
      correctness <- TRUE
    }
    accuracy.array <- c(accuracy.array, correctness)
    rt.array <- c(rt.array, steps)
    
  }
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}

# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 

library(dplyr)

set.seed(12604)

walk.test <- random.walk.model(10000, drift=.0125, criterion= 4.91)

sum(walk.test$correct) / length(walk.test$correct) 
correct.data <- walk.test %>% filter(correct==TRUE)
incorrect.data <- walk.test %>% filter(correct==FALSE)
mean(correct.data$rt)
mean(incorrect.data$rt)

accumulator.test <- accumulator.model(10000, rate.1 = 83.4, rate.2 = 90)

sum(accumulator.test$correct) / length(accumulator.test$correct) 
correct.data <- accumulator.test %>% filter(correct==TRUE)
incorrect.data <- accumulator.test %>% filter(correct==FALSE)
mean(correct.data$rt)
mean(incorrect.data$rt)

hist(walk.test$rt)
hist(accumulator.test$rt)


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:

#While both models can approximate the accuracy of the data well, 
#fitting the mean RT correct/incorrect was far more finicky. For
#the random walk model, this required adjusting the value of the criterion
#while simultaneously keeping it an appropriate distance from the drift
#to maintain the accuracy. For the accumulator model, this required
#adjusting the absolute size of both of the rates, while keeping the relative
#distance between the two roughly similar to maintain the accuracy.

#Overall, the random walk model seemed to be a more efficacious means of
#fitting the data. After some adjustments, it was possible to get both the
#accuracies and correct/incorrect RT very close to the data. However, for
#the accumulator model, it seems nigh impossible to precisely match the
#correct/incorrect RT of the data - while it was possible to get both RT's
#within a ballpark range (245-255), the model seemed unable to reliably
#produce higher correct RT's relative to the incorrect RT's, as the data demand. 


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

#We could use information about the "shape" of the distribution - e.g., normal
#versus skewed. The histograms of the accumulator test produce a fairly normal
#distribution of RT's, while the histograms of the random walk produce 
#a strongly right-skewed distribution. Based on the distribution of data produced
#by the empirical test (i.e., the data to which we are fitting models), we
#can identify which model produces a similar shape. 
