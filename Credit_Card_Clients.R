###########################################
### German Credit Card ###
### Author: Roger Lopez Benet ###
### Email address: rogerlbcn@gmail.com ###
### Date: 3/17/2023 ###
### Design with SF MBAN First year ###
###########################################


# Importing all required libraries
library(readxl)

# Importing Excel data source
my_germ <- read_excel("C:/Users/bromi/OneDrive/Python_Projects/In_class_DataSets/german credit card.xls")
View(my_germ)

### Creating a few objects and information gain process ###

# Scaler of the number of customer (rows)
my_cust <- nrow(my_germ) 
# Scaler of the number of variables (columns)
my_var <- ncol(my_germ)

# Creating a vector. THE ORDER MATTERS! (FIRST ROWS AND SECOND COLUMNS). Here dim stands for dimension
my_dim <- c(my_cust, my_var)

# Creating a matrix from a vector

# Creating variables for the matrix below:
var1 <- my_germ$purpose 
var2 <- my_germ$good_bad

# Combining the 2 vectors in a large vector (matrix)
my_matrix <- matrix(c(var1, var2), nrow = my_cust, ncol = 2)

# Transforming the matrix defined above into a DataFrame:
my_data_frame <- as.data.frame(my_matrix)

# creating list combining all objects defined before:
my_list <- list(my_data_frame, my_matrix, var1, var2, my_cust)

#################
### SESSION 3 ###
#################

# Information loss process:
# Getting the 10 values between 990 and 1000
var1[990:1000] # We don't need a comma because vectors only have one index
var2[990:1000] # We lost information here, because we only see the data from 990 to 1000, we lost the rest of teh values

# Selecting the first 5 rows and first 3 columns:
my_germ[1:5, 1:3]

# Running a frequency table
table(var1) # This tells us how many of each value we have (i.e. we have 234 zeros, 103 ones...)
table(var2) # We get BAD: 300 and GOOD: 700. The problem is that they are unbalanced. A possible answer is some clients might be labeled wrongly. 

### Trying to fix our data types for purpose and good_bad
is.character(var1)
is.numeric(var1)

# Converting var1 (character) to numeric
as.numeric(var1) # We get NA because R doesn't know what to do with X
as.numeric(var2) # We only get NA, because the data type is character for all values

#### Subsetting with which() function: ###

# The output will give us indexes that meet the conditions specified:
which(my_germ$good_bad == "good" & my_germ$history > 2) # == looking for equality 

# Value count the which function defined above:
length(which(my_germ$good_bad == "good" & my_germ$history > 2))

######################################
### Investigating information loss ###
######################################

# Getting indexes of the clients that have "X"
which(my_germ$purpose == "X")

# Only retrieving the 12 clients found above and their info for all variables 
my_germ[ which(my_germ$purpose == "X") , ]

# Separating these 12 clients into good and bad:
which(my_germ$good_bad == "good")

# Now we create a smaller dataset with only good clients. And saving this subset as my_good
my_good <- my_germ[which(my_germ$good_bad == "good") , ]

# Subsetting it for the bad customers. Saving it as my_bad
my_bad <- my_germ[which(my_germ$good_bad == "bad") , ]

#########################################################
### Summary function (similar to describe() in python)###
#########################################################

summary(my_good)
summary(my_bad)
# Checking the stats for the X customers. They are more similar to the bad customers
summary(my_germ[ which(my_germ$purpose == "X") , ])

#####################################################
### Replacing X clients using the gsub() function ###
#####################################################

# Fixing the problems (replacing values with the gsub() function)
# we're giving these clients a 10, because they are closer to bad clients. Therefore, we give them an outlier number!!!
# Also another reason why we chose 10, is because it has two digits. Maybe clients had two digit values, so the number got converted into X (10 in roman)
gsub("X", "10", my_germ$purpose) # The 10 needs to be in the same data type as our original one

### NOW WE TRANSFORM THE VALUES INTO NUMERIC ### 
# we saved it and created a new variable in my_germ (column) called purpose_fixed:
my_germ$purpose_fixed <- as.numeric(gsub("X", "10", my_germ$purpose))


#######################
my_germ$purpose_fixed

# Substituting good for 1
# Only substituting good not also bad, because it's easier to read to other people
my_germ$binary <- gsub("good", "1", my_germ$good_bad)

# Substituting bads for 0
my_germ$binary <- gsub("bad", "0", my_germ$binary)

#####################################################   
### TRANSFORM DATA TYPE FROM CHARACTER TO numeric ###
#####################################################   

my_germ$binary <- as.numeric(my_germ$binary) # overwriting the new variable binary (my_germ$binary)

my_germ$binary 

summary(my_germ$binary)

####################################################

#################
### Session 4 ###
#################

# Creating loops to check desc stats - horizontal loop 
# Calculate min, mean, max for each variable: 

my_germ <- as.data.frame(my_germ) # Re-defining my_germ as a data frame

# For loop to check whether our data is homogeneous or heterogeneous 
for (i in 1:ncol(my_germ)){
  
  print(min(my_germ[,i], na.rm = TRUE))
  print(mean(my_germ[,i], na.rm = TRUE))
  print(max(my_germ[,i], na.rm = TRUE))
  
} # Closing the i horizontal for loop

my_germ

# Saving (0.5) + salary (0.3) + history (0.1) + age (0.1)

my_germ$amount

##########################################################################
### Implementing the scorecard model for customers (vertical for loop) ###
##########################################################################

my_germ$risk_score <- c() # Creating a new empty veriable (vector) to store the results 

### for loop:

for(i in 1:nrow(my_germ)){
  
  my_germ$risk_score[i] <-    0.5 * my_germ$duration[i] + # We don't need a comma in [] because we have already selected one veriable, which on its own it's a veriable with only one dimension
    0.1 * my_germ$age[i] +
    0.1 * my_germ$amount[i] +
    0.3 * my_germ$installp[i]
  
} # nrow(my_germ) because it's a vertical loop


####################
##### Homework #####
####################


my_germ$risk_score_team <- c() # Creating a new empty veriable (vector) to store the results 

### for loop:

for(i in 1:nrow(my_germ)){
  
  my_germ$risk_score[i] <-    0.075 * my_germ$savings[i] + # We don't need a comma in [] because we have already selected one veriable, which on its own it's a veriable with only one dimension
                              0.075 * my_germ$amount[i] +
                              0.07 * my_germ$history[i] +
                              0.1 * my_germ$age[i]
  
} # nrow(my_germ) because it's a vertical loop

# savings 0.5, amount 0.3, history 0.1, age 0.1


###########################################################################
### Introducing business logic to help DB (Douche Bank) be conservative ###
###########################################################################

my_germ$label <- c()

# Verticle for loop:

for(i in 1:nrow(my_germ)){
  
  # if(){} else {}
  if(my_germ$binary[i] == 1 & my_germ$risk_score[i] < 250){ 
    my_germ$label[i] <- "outstanding"
    
  } else {my_germ$label[i] <- "non-outstanding"}
  
} # Closing for loop


# Being more conservative because only 630 clients would get a credit card
table(my_germ$label)


my_germ[which(my_germ$label == "non-outstanding" & my_germ$binary),]

################################################

#################### 
### SESSION # 5 ####
#################### 
# NOTES:
## There are 2 types of for loops: 
### HORIZONTAL (variables) and 
### VERTICAL (observations (members of our population))


### Creating UDF (User-Defined_Functions) for unique inputs for team scores: ###

# Note: the variables in the UDF only exist locally they are not global variables. 
# i.e. w1 stands for weight 1 

risk_score <- function(var1, w1, var2, w2, var3, w3, var4, w4){
  my_score <- var1*w1 + var2*w2 + var3*w3 + var4*w4 
  return(my_score)
  
} # Closing the risk_score UDF

################################################

# Calling the UDF above for team 13:
my_germ$team13_score <- risk_score(var1 = my_germ$duration, w1 = 0.2, 
                                   var2 = my_germ$age, w2 = 0.2, 
                                   var3 = my_germ$amount, w3 = 0.3, 
                                   var4 = my_germ$job, w4 = 0.3)

################################################

# Calling the UDF for team 12:
my_germ$team12_score <- risk_score(var1 = my_germ$checking, w1 = 0.85, 
                                   var2 = my_germ$duration, w2 = 0.04, 
                                   var3 = my_germ$savings, w3 = 0.22, 
                                   var4 = my_germ$employed, w4 = 0.2)

################################################

# Calling UDF applying our team's percentages:
my_germ$team4_score <- risk_score( var1 = my_germ$duration, w1 = 0.075, 
                                   var2 = my_germ$age, w2 = 0.075, 
                                   var3 = my_germ$amount, w3 = 0.7, 
                                   var4 = my_germ$job, w4 = 0.1)

################################################

# Types of location: mean, median, mode

### Designing a for loop to look at descriptive stats for each variable ###

# Horizontal for loop:

for (i in 1:ncol(my_germ)) {
  
  # Calculating the minimum:
  # The try() function lets us try a function that might fail
  my_min  <- try(min(my_germ[,i], na.rm  = TRUE)) 
  my_max  <- try(max(my_germ[,i], na.rm  = TRUE)) 
  my_mean <- try(mean(my_germ[,i], na.rm = TRUE)) 
  my_std  <- try(sd(my_germ[,i], na.rm   = TRUE)) 
  
  # Creating a vector with the right order to get information gain (biz insights)
  print(c(my_min, my_mean, my_std, my_max))
  
} # Co losing for loop

###################
### Casino game ###
###################

### Game 1 ###
casino_vec <- c(4, 6, 2, 3, 4, 3, 4, 6, 5, 3, 1, 3, 5, 3, 4, 6, 3, 2, 5, 2, 2, 1, 2, 6, 3, 6, 4, 3, 5, 3, 6, 4, 4, 6, 2, 2, 1, 6, 2, 5, 4, 4, 6, 4, 6, 2, 6, 5, 6, 6, 1, 6, 2, 3, 1, 4, 5, 4, 6, 1, 2, 2, 4, 2, 2, 2, 1)

mean(casino_vec)

# Simplifying our dice casino game:

dice <- sample(c(1, 2, 3, 4, 5, 6), size = 15000, replace = TRUE) # replace = TRUE, because once you roll the dice you replace it to roll it again

mean(dice)

# Plotting the shape (distribution of dice)
hist(dice)


### Game #2 ##

coin1 <- c(1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0)

length(coin1) # This gives us the size of the vector. Gives us 50

mean(coin1)

##############

coin <- sample(c(0, 1), size = 50, replace = TRUE)

mean(coin)


# increase sample size to 50000

coin50 <- sample(c(0, 1), size = 50000, replace = TRUE)

mean(coin50)
hist(coin50)

#####################################
### Session # 6 Exponential Japan ###
#####################################

# Sample size: 10,000 trains and rate (lambda) = 15
train <- rexp(10000, rate = 15)

# mean = 1/lambda OR mean = 1/15 = 0.06723578 it gives us the same number 
mean(train) # The distribution is not compact. We see that the longest you can wait to get onto your train is 30 minutes. 
hist(train)

# San Francisco train (BART0)
trainSF <- rexp(10000, rate = 0.5) # mean = 2.010869

mean(trainSF)
hist(trainSF) # The distribution is wider, which means that train in SF it takes hours to go down. Therefore, in SF you can wait up to 15 hours to get on your train. 


#####################################################
### Applying it to the German (Douche Bank) data set:

# We'll use a horizontal for loop, because we're looping through our variables: 

# Looping through horizontal to see the shape of all the vars:
for(i in 1:ncol(my_germ)){
  try(hist(my_germ[,i])) # i on the right because we loop through our columns (variables) # We use the try() function to avoid the loop from stopping in the first iteration
  
} # Closing the for loop

#######################################
### Removing units using normalization:

normalize <- function(x){ # x is the variable vector
  min_max <- (x - min(x)) / (max(x) - min(x)) # calculating the min-max
  return(min_max)
} # Closing normalize function 


# Normalizing variables and creating new variables
# Since we only have one input R understands this is x

my_germ$checking_norm <- normalize(my_germ$checking) 
my_germ$duration_norm <- normalize(my_germ$duration) 
my_germ$history_norm <- normalize(my_germ$history) 
my_germ$amount_norm <- normalize(my_germ$amount) 
my_germ$employed_norm <- normalize(my_germ$employed) 
my_germ$savings_norm <- normalize(my_germ$savings) 
my_germ$installp_norm <- normalize(my_germ$installp) 
my_germ$coapp_norm <- normalize(my_germ$coapp) 
my_germ$age_norm <- normalize(my_germ$age) 

# There are no business units now that the data in these variables has been normalized

##################################################################
# Check the correlation of the variables:  # duration, age, amount

cor.test(my_germ$age, my_germ$amount) # Pearson correlation coefficient is = 0.03271642. This is a weak correlation. Good!

###########################
### Predictive modeling ###
###########################

# Before Predictive modeling: Splitting data set into Training and testing 
# Vertical sample:

training_idx <- sample(1 : 1000, size = 0.8 * nrow(my_germ)) # We don't replace to not get duplicates 

# Train set:
my_germ_train <- my_germ[training_idx, ]
# Test set:
my_germ_test <- my_germ[-training_idx, ]

#######################################################

##################
### Session 7 ###
##################

#####################################
######## Logistic Regression ########

### Our glm model predicts odds: odds = probability of biz success (1)/ probability of biz failure (0)
### To get biz insights from our model we must do exp()

# Calculating the odds: 
exp(-0.08)-1 # 7.6% business failure

exp(0.9)-1


#################################
### Building a logistic model ###

my_logit <- glm(binary ~ age + 
             duration +
             coapp +
             savings +
             amount, 
               data = my_germ_train,
               family = "binomial")

# Summary function to view the logic
summary(my_logit)# p-values: The best variables are the ones with the stars


### based on the summary info: Biz units for age:###
# 1.457e-02 is the coefficient of the variable age
exp(1.457e-02)-1 # Therefore, for every additional year of age, the ODDS of biz success increase by 1.46%

# -3.791e-02 is the coefficient of the duration variable
exp(-3.791e-02)-1 # For every additional unit of duration, the ODDS of success decrease by 3.72% (the output is -0.03720041, since it's negative, it decreases)


my_logit_norm <- glm(binary ~ age_norm + 
                              duration_norm +
                              coapp_norm +
                              checking_norm +
                              amount_norm, 
                                data = my_germ_train,
                                family = "binomial")

summary(my_logit_norm)

############################
### Biz insights for AGE ###
### We cannot get the business units for each variable (separately) # Therefore we cannot do: exp(0.8711)-1

# Instead, we look at the strongest and weakest coefficients: Estimate Std. I.E. Here the weakest var is duration_norm
# While the strongest one is checking_norm with  1.9260. 
### Therefore, the two variables that the business should focus more on are the ones mentioned above: checking_norm and duration_norm

####################################
### Testing Performance of model ###

install.packages(c("cli", "dplyr", "gtable", "hms", "pillar", "prodlim","tibble"))
install.packages("ggplot2")
install.packages("caret")
# install.packages("caret", dependencies = TRUE)
library(ggplot2)
library(caret)


# Predicting the probability of biz success:
my_prediction <- predict(my_logit, my_germ_test, type = "response")

# Based on the above my_prediction: Transforming probability to odds for client 4: 0.5084765/0.14
biz_failure <- 1-0.5084765
odds <- 0.5084765/biz_failure
odds

########################
### Confusion Matrix ###
confusionMatrix(data = as.factor(as.numeric(my_prediction > 0.8)), # If it's above 0.5 the it's Business Success
                reference = as.factor(as.numeric(my_germ_test$binary)))

#################################
### Building a decision tree: ###
#################################
# install.packages("rlang")
library(rlang)
library(rpart)
library(rpart.plot)


my_tree <- rpart(binary ~ age + 
                          duration +
                          checking +
                          coapp +
                          savings +
                          amount, 
                            data = my_germ_train,
                            method = "class", 
                            cp = 0.02) # Added and changed the cp parameter to shrink the tree, and make it more readable  

# Plotting my_tree model:
rpart.plot(my_tree, type = 1, extra = 1)
# Predict biz success 
my_germ_tree_predict <- predict(my_tree, my_germ_test, type = "prob")

confusionMatrix(data = as.factor(as.numeric(my_germ_tree_predict[ , 2] > 0.5)), 
                reference = as.factor(as.numeric(my_germ_test$binary)))




