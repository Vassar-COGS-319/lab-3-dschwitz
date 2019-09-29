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

