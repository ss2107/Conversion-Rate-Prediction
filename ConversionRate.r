#Sarvesh

install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("randomForest")

#libraries needed
require(dplyr)
require(rpart)
require(ggplot2)
require(randomForest)
require(magrittr)

setwd("C:/Users/Sarvesh/Desktop/case dtudy/Conversion rate")

# Load the dataset in R
data = read.csv('conversion_data.csv')

head(data)
str(data)

# Summary of the data
summary(data)
y <- table(data$converted)
round(prop.table(y) * 100, digits = 1) # Conversion rate is around 3%

# check age values
sort(unique(data$age), decreasing=TRUE)

# Remove unrealistic age values like 111, 127 etc
subset(data, age>79)
data = subset(data, age<80)

# Plot of Conversion rates Vs Country
data_country = data %>%
  group_by(country) %>%
  summarise(conversion_rate = mean(converted))

ggplot(data=data_country, aes(x=country, y=conversion_rate))+
  geom_bar(stat = "identity", aes(fill = country))

# Plot of Conversion rates Vs Total_pages_visited
data_pages = data %>%
  group_by(total_pages_visited) %>%
  summarise(conversion_rate = mean(converted))
qplot(total_pages_visited, conversion_rate, data=data_pages, geom="line")

subset(data,total_pages_visited>20)

# Split the dataset into training and test
data$converted = as.factor(data$converted) # let's make the class a factor
data$new_user = as.factor(data$new_user) #also this a factor
levels(data$country)[levels(data$country)=="Germany"]="DE" # Shorter name, easier to plot

train_sample = sample(nrow(data), size = nrow(data)*0.66)
train_data = data[train_sample,]
test_data = data[-train_sample,]

########################### Random Forests ####################################
rf = randomForest(y=train_data$converted, x = train_data[, -ncol(train_data)],
                  ytest = test_data$converted, xtest = test_data[, -ncol(test_data)],
                  ntree = 100, mtry = 3, keep.forest = TRUE)

rf

varImpPlot(rf,type=2)
# We can see that total_pages_visited is the most important variable. But its the least
# actionable variable. So we run the RF again without that variable.
# Add a classwt option to account for class imbalance.

rf = randomForest(y=train_data$converted, x = train_data[, -c(5, ncol(train_data))],
                  ytest = test_data$converted, xtest = test_data[, -c(5, ncol(train_data))],
                  ntree = 100, mtry = 3, keep.forest = TRUE, classwt = c(0.7,0.3))

rf

varImpPlot(rf,type=2)
# Source is not important. new_user is the most important variable.

# Run partial dependence plots
op <- par(mfrow=c(2, 2))
partialPlot(rf, train_data, country, 1)
partialPlot(rf, train_data, age, 1)
partialPlot(rf, train_data, new_user, 1)
partialPlot(rf, train_data, source, 1)

# Build a Decision Tree to check the most important splits
tree = rpart(data$converted ~ ., data[, -c(5,ncol(data))],
             control = rpart.control(maxdepth = 3),
             parms = list(prior = c(0.7, 0.3))
)
tree

# We find that new_users, country and age are the most important splits.

# Recommendations

# 1) The site is working very well for young users. Definitely let's tell marketing to advertise and use
# marketing channel which are more likely to reach young people.

# 2) The site is working very well for Germany in terms of conversion. But the summary showed that
# there are few Germans coming to the site: way less than UK, despite a larger population. Again,
# marketing should get more Germans. Big opportunity.
# 
# 3) Users with old accounts do much better. Targeted emails with offers to bring them back to the site
# could be a good idea to try.
# 
# 4) Something is wrong with the Chinese version of the site. It is either poorly translated, doesn't fit the
# local culture, some payment issue or maybe it is just in English! Given how many users are based in
# China, fixing this should be a top priority. Huge opportunity.
# 
# 5) Maybe go through the UI and figure out why older users perform so poorly? From 30 y/o conversion
# clearly starts dropping.
# 
# 6) If I know someone has visited many pages, but hasn't converted, she almost surely has high
# purchase intent. I could email her targeted offers or sending her reminders. Overall, these are
# probably the easiest users to make convert.
