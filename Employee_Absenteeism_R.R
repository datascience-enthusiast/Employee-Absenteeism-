## Clear all the global variables and set working directory
rm(list=ls(all=T))
setwd("C:/Users/Mohammeds.Fakir/Employee Absenteesm Final")

# For Reading Error Metrics in normal form, rather than in exponential
options(scipen = 999)

# Set Seed for getting constant results
set.seed(12345)

## Load Libraries
x = c("dummies","caret","rpart.plot","plotly","plyr","dplyr","gbm", "Matrix","ggplot2","rpart","xgboost","DMwR","randomForest","usdm","corrgram","DataCombine","xlsx")
lapply(x,require, character.only = TRUE)
rm(x)

# Read Data
employee_df = read.xlsx(file = "Absenteeism_at_work.xls", sheetIndex = 1)

######################################## EXPLORATORY DATA ANALYSIS ########################################
# Check shape
dim(employee_df)

# Display Top 5 rows
head(employee_df)

# Describe dataframe
str(employee_df)

################# Converting appropriate required Datatypes ####################
employee_df$Reason.for.absence[employee_df$Reason.for.absence %in% 0] = 20
employee_df$Month.of.absence[employee_df$Month.of.absence %in% 0] = NA

#Converting colmnns into categorical factors as they contain unqiue values
category_column_names= c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Disciplinary.failure","Education",
                          "Son", "Social.drinker","Social.smoker","Pet")

numerical_column_names = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
                           'Work.load.Average.day.', 'Transportation.expense',
                           'Hit.target', 'Weight', 'Height', 
                           'Body.mass.index', 'Absenteeism.time.in.hours')
# using lapply function to convert num to factors
employee_df[,category_column_names] <- lapply(employee_df[,category_column_names] , factor)

# Checking Again Data type of variables
str(employee_df)

# Copy the EDA dataset
df = employee_df


##################################Missing Values Analysis###############################################

# Creating dataframe with missing values present in each variable
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"

# Calculating percentage missing value
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100

# Sorting missing_val in Descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL

# Reordering columns
missing_val = missing_val[,c(2,1)]

# Write to CSV file
write.csv(missing_val, "R_Missing_perc.csv", row.names = F)


# Plot Percentage of missing values in all columns
ggplot(data = missing_val[1:18,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()

# Create missing value and test best imputation technique using mean, median and knn
# Value = 30
# Mean = 26.67
# Median = 25
# KNN = 30
  #df[["Body.mass.index"]][5]
  #df[["Body.mass.index"]][5] = NA
  #df[["Body.mass.index"]][3] = mean(df$Body.mass.index, na.rm = T)
  #df[["Body.mass.index"]][3] = median(df$Body.mass.index, na.rm = T)
df = knnImputation(data = df, k = 5)

# Cross Check if any missing values
sum(is.na(df))

######################################## DATA DISTRIBUTION USING GRAPHS ########################################

############### Univariate Analysis ##########################
# Distribution of Target variable : Absent in Hours
fit <- density(df$Absenteeism.time.in.hours)
plot_ly(x = df$Absenteeism.time.in.hours, type = "histogram", name = "Count Distribution") %>%
  add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))


# Distribution of All Numerical Columns
for( i in numerical_column_names){
  fit <- density(df[,i])
  p <- plot_ly(x = df[,i], type = "histogram", name = "Distribution") %>%
    add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
    layout(yaxis2 = list(overlaying = "y", side = "right") , title = i)
  print(p)
  Sys.sleep(3)
}

# Countplot for all Categorical Columns
bar1 = ggplot(data = df, aes(x = ID)) + geom_bar() + ggtitle("Count of ID") + theme_bw()
bar2 = ggplot(data = df, aes(x = Reason.for.absence)) + geom_bar() + 
  ggtitle("Count of Reason for absence") + theme_bw()
bar3 = ggplot(data = df, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Count of Month") + theme_bw()
bar4 = ggplot(data = df, aes(x = Disciplinary.failure)) + geom_bar() + 
  ggtitle("Count of Disciplinary failure") + theme_bw()
bar5 = ggplot(data = df, aes(x = Education)) + geom_bar() + ggtitle("Count of Education") + theme_bw()
bar6 = ggplot(data = df, aes(x = Son)) + geom_bar() + ggtitle("Count of Son") + theme_bw()
bar7 = ggplot(data = df, aes(x = Social.smoker)) + geom_bar() + 
  ggtitle("Count of Social smoker") + theme_bw()

gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)
gridExtra::grid.arrange(bar5,bar6,bar7,ncol=2)

############# Bivariate Analysis ######################

#Absentism on Monthly Basis
df_temp <- aggregate(Absenteeism.time.in.hours ~ Month.of.absence, df, sum)
plot_ly(x = ~Month.of.absence, y = ~Absenteeism.time.in.hours , data = df_temp, type = "bar", text = ~Absenteeism.time.in.hours , marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)', width = 1.5)))

#Absentism on Basis of Age
df_temp <- aggregate(Absenteeism.time.in.hours ~ Age, df, sum)
plot_ly(x = ~Age, y = ~Absenteeism.time.in.hours , data = df_temp, type = "bar", text = ~Absenteeism.time.in.hours , marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)', width = 1.5)))

#Absentism on Basis of Work Load Average/Day
df_temp <- aggregate(Absenteeism.time.in.hours ~ Work.load.Average.day., df, sum)
plot_ly(df_temp, x = ~Work.load.Average.day., y = ~Absenteeism.time.in.hours ,type = "bar")


#Bike Rental Count on basis of Reason of Absence
df_temp <- aggregate(Absenteeism.time.in.hours ~ Reason.for.absence, df, sum)
plot_ly(df_temp, x = ~Reason.for.absence, y = ~Absenteeism.time.in.hours ,type = "box") %>%
  add_trace( x = ~Reason.for.absence, y = ~Absenteeism.time.in.hours, type="scatter", mode = "lines+markers")


################################# Outlier Analysis - Box plot ########################
for (i in 1:length(numerical_column_names))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (numerical_column_names[i]), x = "Absenteeism.time.in.hours"), data = subset(df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numerical_column_names[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of absenteeism for",numerical_column_names[i])))
}

### Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)


############ Remove outliers using boxplot method

##loop to remove from all variables
for(i in numerical_column_names)
{
  print(paste0("Removing : ",i))
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df = df[which(!df[,i] %in% val),]
}

#Replace all outliers with NA and impute
for(i in numerical_column_names)
{
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df[,i][df[,i] %in% val] = NA
}

# Imputing missing values
df = knnImputation(df,k=3)

## Check for Null Values
sum(is.na(df))

######################################## FEATURE SELECTION ########################################
#Check for multicollinearity using VIF

vifcor(df[,numerical_column_names])

#Check for multicollinearity using corelation graph
corrgram(df[,numerical_column_names], order = F, upper.panel=panel.pie, 
         text.panel=panel.txt, main = "Correlation Plot")

## ANOVA test for Categprical variable
summary(aov(formula = Absenteeism.time.in.hours~ID,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Education,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Son,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Pet,data = df))

#Variable Reduction
df = subset.data.frame(df, select = -c(Weight))

#Make a copy of Clean Data
clean_data = df
write.xlsx(clean_data, "clean_data.xlsx", row.names = F)

######################################## FEATURE SCALING ########################################
#Normality check
hist(df$Absenteeism.time.in.hours)

#Remove dependent variable
numeric_index = sapply(df,is.numeric)
numeric_data = df[,numeric_index]
numeric_columns = names(numeric_data)
numeric_columns = numeric_columns[-9]

#Normalization of continuous variables
for(i in numeric_columns){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i]) - min(df[,i]))
}

#Create dummy variables of factor variables
df = dummy.data.frame(df, category_column_names)

rmExcept(keepers = c("df","employee_df"))

######################## Model Development ######################
####################### DECISION TREE############################

#Splitting data into train and test data
train_index = sample(1:nrow(df), 0.8*nrow(df))        
train = df[train_index,]
test = df[-train_index,]

#Build decsion tree using rpart
c50_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")

#Plot the tree
rpart.plot(c50_model)

#Perdict for test cases
c50_predictions = predict(c50_model, test[,-102])

#Create data frame for actual and predicted values
c50_pred = data.frame("actual"=test[,102], "predictions"=c50_predictions)
head(c50_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = c50_predictions, obs = test[,102]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(c50_predictions,col="blue")


#RMSE: 2.310
#MAE: 1.64
#R squared: 0.446


######################################## RANDOM FOREST ########################################
##Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train, ntree = 1000)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-102])

#Create dataframe for actual and predicted values
rf_pred = data.frame("actual"=test[,102], "predictions"=rf_predictions)
head(rf_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test[,102]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

#RMSE: 2.210
#MAE: 1.57
#R squared: 0.489


########################################LINEAR REGRESSION########################################

##Train the model using training data
linear_regressor = lm(formula = Absenteeism.time.in.hours~., data = train)

#Get the summary of the model
summary(linear_regressor)

#Predict the test cases
lr_predictions = predict(linear_regressor, test[,-102])

#Create dataframe for actual and predicted values
df_pred = data.frame("actual"=test[,102], "predictions"=rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs = test[,102]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#RMSE: 5.74
#MAE: 2.08
#R squared: 0.12

############################# XGBoost ##########################
train_matrix <- sparse.model.matrix(Absenteeism.time.in.hours ~ .-1, data = train)
test_matrix <- sparse.model.matrix(Absenteeism.time.in.hours ~ .-1, data = test)

xgb <- xgboost(data = as.matrix(train_matrix),
               label = as.matrix(train$Absenteeism.time.in.hours),
               booster = "gbtree", 
               objective = "reg:linear", 
               max.depth = 8, 
               eta = 0.5, 
               nthread = 2, 
               nround = 100, 
               min_child_weight = 1, 
               subsample = 0.75, 
               colsample_bytree = 1, 
               num_parallel_tree = 3)

xgb_predictions <- predict(xgb, test_matrix)

#Create dataframe for actual and predicted values
df_pred = data.frame("actual"=test[,102], "predictions"=xgb_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = xgb_predictions, obs = test[,102]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(xgb_predictions,col="blue")

#RMSE: 2.32
#MAE: 1.68
#R squared: 0.43

########################################DIMENSION REDUCTION USING PCA########################################
#Principal component analysis
prin_comp = prcomp(train)

#Compute standard deviation of each principal component
pr_stdev = prin_comp$sdev

#Compute variance
pr_var = pr_stdev^2

#Proportion of variance explained
prop_var = pr_var/sum(pr_var)

#Cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Add a training set with principal components
train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)

# From the above plot selecting 45 components since it explains almost 95+ % data variance
train.data =train.data[,1:45]

#Transform test data into PCA
test.data = predict(prin_comp, newdata = test)
test.data = as.data.frame(test.data)

#Select the first 45 components
test.data=test.data[,1:45]

########################################DECISION TREE########################################

#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~., data = train.data, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model,test.data)

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,102], "dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

#RMSE: 0.393
#MAE: 0.227
#R squared: 0.983


########################################RANDOM FOREST########################################

#Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train.data, ntrees = 1000)

#Extract the rules generated as a result of random Forest model
library("inTrees")
rules_list = RF2List(rf_model)

#Extract rules from rules_list
rules = extractRules(rules_list, train.data[,-45])
rules[1:2,]

#Convert the rules in readable format
read_rules = presentRules(rules,colnames(train))
read_rules[1:2,]

#Determining the rule metric
rule_metric = getRuleMetric(rules, train[,-45], train$Absenteeism.time.in.hours)
rule_metric[1:2,]


#Predict the test cases
rf_predictions = predict(rf_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

#RMSE: 0.620
#MAE: 0.967
#R squared: 0.309

##################################### XGBOOST ##############################################

#Develop Model on training data
fit_XGB = gbm(Absenteeism.time.in.hours~., data = train.data, n.trees = 500, interaction.depth = 2)

#Lets predict for training data
pred_XGB_train = predict(fit_XGB, train.data, n.trees = 500)

#Lets predict for testing data
pred_XGB_test = predict(fit_XGB,test.data, n.trees = 500)

# For testing data 
print(postResample(pred = pred_XGB_test, obs = test$Absenteeism.time.in.hours))

#RMSE: 0.33
#MAE: 0.14
#R squared: 0.988

######################################## LINEAR REGRESSION ########################################

#Train the model using training data
lr_model = lm(Absenteeism.time.in.hours ~ ., data = train.data)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs =test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#RMSE: 0.001
#MAE: 0.0009
#R squared: 0.999