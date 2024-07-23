# Sales data collected from new product launch, sales team wants to know optimal sales
# method to use that will yield highest revenue numbers for the organization
#Goal: leverage sales data to determine the optimal sales strategy to use to ensure
# new product line is as profitable as possible
# Need to know:
#  Number of customers for each approach?
#  Spread of revenue overall and for each method?
#  Difference in revenue over time for each method?
#  Recommendation for optimal method to use?
#  Any additional information about the data


library(tidyverse)
library(mice)
getwd()
setwd("C:\\Users\\Matt Fiorini\\Documents\\Coding_Projects\\DataCamp Revenue Forcasting")
df = read.table(file = "product_sales.csv", header = T, sep = ",")
str(df)
factor_col = c("sales_method","state","customer_id")
numeric_col = c("week","nb_sold","revenue","years_as_customer","nb_site_visits")

#convert data to proper type for manipulation later
for (i in 1:length(factor_col)){
  df[,factor_col[i]] <- as.factor(df[,factor_col[i]])
}
for (i in 1:length(numeric_col)){
  df[,numeric_col[i]] <- as.numeric(df[,numeric_col[i]])
}
str(df)

#determine the number of missing values in data
sapply(df, function(x) sum(is.na(x)))

#updating factor_col variable to run a data hygeine check to understand the levels of the data
factor_col = c("sales_method","state")

for (column in factor_col) {
  if (is.factor(df[[column]])) {
    cat("Levels for", column, ":\n")
    print(levels(df[[column]]))
  } else {
    cat(column, "is not a factor.\n")
  }
}

#determining duplicates for customer ID to determine no repeat customers listed
duplicate_customers = duplicated(df$customer_id)
sum(duplicate_customers)

for (column in numeric_col){
  if (is.numeric(df[[column]])) {
    cat("Summary for", column, ":\n")
    print(summary(df[[column]]))
  }
}

#updating the errors made in the sales_method variable to ensure only 3 levels appear
df = mutate(df, sales_method = recode(sales_method,
                                      "Call" = "Call",
                                      "em + call" = "Email + Call",
                                      "email" = "Email",
                                      "Email" = "Email",
                                      "Email + Call" = "Email + Call"))
levels(df$sales_method)
#assessing distribution of data to understand how the numeric variables are
ggplot(df, aes(week))+geom_histogram() + labs(title = "Sales By Week",
                                              x = "Weeks",
                                              y = "Count")

ggplot(df, aes(nb_sold))+geom_histogram() + labs(title = "Units Sold",
                                                 x = "Units Sold",
                                                 y = "Count")

ggplot(df, aes(nb_site_visits))+geom_histogram() + labs(title = "Site Visits",
                                                        x = "Website Visits",
                                                        y = "Count")


#identifying the mode of the years_as_customer variable
df %>% group_by(years_as_customer) %>% summarize (count_of_customers = n()) %>% arrange(desc(count_of_customers))

#confirming that observations that have years_as_customer listed as 0 do not have revenue listed as NA
df %>% filter(years_as_customer == 0) %>% summarize(proportion_of_na = (sum(is.na(revenue))/n()*100))

#distribution of sales data, leaning toward right skew
ggplot(df, aes(revenue))+geom_histogram() + labs(title = "Revenue",
                                                 x = "Sale Total",
                                                 y = "Count")

#visualizing the relationship between number of units sold and revenue, comapred to
#relationship with years_as_customer. Seeing stronger relationship with nb_sold
ggplot(df, aes(revenue, nb_sold))+geom_point()
ggplot(df, aes(revenue, years_as_customer))+geom_point()

#grouping na values for revenue by the number of units sold. Also confirms that
#missing values do not represent scenrios where a NA = 0
df %>% filter(is.na(revenue)==TRUE) %>% group_by(nb_sold) %>%
  tally()

#identify the missing data broken down by sales channel
df %>% filter(is.na(revenue)==TRUE) %>% group_by(sales_method) %>% summarize(null_by_chanel = n())


###########################################################################################################
# Can update the data multiple ways, either through manual update based on taking the median revenue based
#on the number of units sold, or leveraging the mice package to impute missing values
#choosing to manually update the data to reflect the median revenue based on number of units sold
#given skew with data, chosing to replace NA values with the median revenue to avoid influence from outlines
null_rev = df %>% filter(is.na(revenue)==FALSE) %>% group_by(nb_sold) %>% summarise(median_rev = median(revenue))
df = df %>% left_join(null_rev, by = "nb_sold")
df$revenue = ifelse(is.na(df$revenue),df$median_rev,df$revenue)
df = df %>% select(-median_rev)
summary(df$revenue)
# output on revenue summary once changed manually
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#32.54   53.01   90.30   95.69  107.55  238.32

#inputation using mice package
imputedValues <- mice(data=df
                      , seed=2016     
                      , method="cart" 
                      , m= 5          
                      , maxit = 1)

df <- mice::complete(imputedValues,1)
summary(df$revenue)   
#output on revenue once updated using mice
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#32.54   52.75   90.25   95.71  108.28  238.32 

#identifying outliers within our years as customer, business has only been around
# for 40 years, making anything above 40 an error
df %>% filter(years_as_customer > 40) %>% group_by(years_as_customer) %>% 
  summarize(count_of_customers = n(), revenue_amount = mean(revenue)) %>% 
  arrange(desc(years_as_customer))
#replacing values where number of years as customer is greater than 40 with the mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_value <- calculate_mode(df$years_as_customer)
df$years_as_customer[df$years_as_customer > 40] <- mode_value
df %>% group_by(years_as_customer) %>% summarise(count_of_years = n()) %>% arrange(desc(count_of_years))
summary(df$years_as_customer)

#visualizing the updated revenue data
ggplot(df, aes(revenue))+geom_histogram() + labs(title = "Revenue",
                                                 x = "Sale Total",
                                                 y = "Count")

#volume of sales per week
week_count = df %>% group_by(week) %>% summarize(count_by_week = n()) 
ggplot(week_count, aes(x = week, y = count_by_week)) +
  geom_area(fill = "blue", alpha = 0.5) +
  labs(title = "Volume per Week",
       x = "Week",
       y = "Count") +
  theme_minimal()

#volume of sales broken down by sales method
totals = df %>% group_by(sales_method) %>% summarise(count = n())
ggplot(df,aes(sales_method)) + geom_bar() + geom_text(data = totals, aes(x = sales_method, y = count, label = count), vjust = -0.5) + 
  labs(title = "Number of Customers in Each Sales Method",
       x = "Sales Method",
       y = "Total")

#vizualizing the revenue distribution by sales method
ggplot(df, aes(sales_method, revenue)) + geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = 1.5, color = "blue") +
  geom_text(data = df %>%
              group_by(sales_method) %>%
              summarise(min_revenue = min(revenue),
                        max_revenue = max(revenue)),
            aes(x = sales_method, y = min_revenue, label = round(min_revenue, 2)), vjust = 1.5, color = "green") +
  geom_text(data = df %>%
              group_by(sales_method) %>%
              summarise(min_revenue = min(revenue, na.rm = TRUE),
                        max_revenue = max(revenue, na.rm = TRUE)),
            aes(x = sales_method, y = max_revenue, label = round(max_revenue, 2)), vjust = -0.5, color = "green") +
  labs(title = "Revenue Distribution by Sales Method with Min, Max, and Median",
       x = "Sales Method",
       y = "Revenue") +
  theme_minimal()
  
  df %>% group_by(week, sales_method) %>% summarise(median_rev = median(revenue)) %>%
    ggplot(aes(week, median_rev)) + geom_line(aes(color = sales_method))+labs(title = "Median Revenue by Week",
                                                                              x = "Week",
                                                                              y = "Median Revenue")
## run ML, split data set into test and train set based on sales method and run estimation
#splitting data set into 2, email+ call and non-email+call

#splitting the data frame into test and train based on what sales method they had
train = df %>% filter(sales_method == "Email + Call")  
test = df %>% filter(sales_method != "Email + Call")
str(test)
#moving revenue to the first in the data frame to avoid data leakage into predicting revenue for
# non-email+call revenue, removing method since only email + call are within this data frame
train = train[,c(5,1,4,6:8)]
names(train)[1] = "y"
#removing revenue and sales method entirely, given that we know the methods are either email or call
test = test[,c(3,1,4,6:8)]

library(caret)
set.seed(12345)
#one hot encoding the factor level data
dummiesTr = dummyVars(y ~ ., data=train)
ex = data.frame(predict(dummiesTr, newdata = train))
names(ex) = gsub("\\.","",names(ex))
train = cbind(train$y, ex)

dummiesTe = dummyVars(customer_id ~ ., data=test)
ex = data.frame(predict(dummiesTe, newdata = test))
names(ex) = gsub("\\.","",names(ex))
test = cbind(test$customer_id, ex)

rm(ex, dummiesTr, dummiesTe)

#removing large correlation variables from the data set to maintain accurate model
descCor = cor(train[,2:ncol(train)])
highlyCorDescr = findCorrelation(descCor, cutoff = 0.85)
filteredDesc = train[,2:ncol(train)][,-highlyCorDescr]
names(train)[1] = "y"
train = cbind(train$y, filteredDesc)
names(train)[1] = "y"
str(train)
rm(descCor, highlyCorDescr, filteredDesc)

y= train$y
train = cbind(rep(1, nrow(train)), train[2:ncol(train)])
names(train)[1] = "ones"
comboInfo = findLinearCombos(train)
train = train[, - comboInfo$remove]
train = train[,c(2:ncol(train))]
train = cbind(y , train)
str(train)
rm(comboInfo)

nzv = nearZeroVar(train, saveMetrics = T)
head(nzv)
train = train[, c(T,!nzv$nzv[2:ncol(train)])]
rm(nzv)

#introducing h20 model
library(h2o)
h2o.init(nthreads = 12, max_mem_size = "64g")

data = as.h2o(train)

y = "y"
x = setdiff(names(data),y)
parts = h2o.splitFrame(data, 0.8, seed = 99)
tr = parts[[1]]
te = parts[[2]]

auto <- h2o.automl(x, y, tr, max_runtime_secs=300)
auto
str(auto)
names(test)[1] = "Id"
data2 <- as.h2o(test)
names(data2)

p <- h2o.predict(auto, data2)
p <- as.data.frame(p)
head(p)
names(p) <- "predict"

#compiling results into revenue_results data frame
revenue_results <- data.frame(Id=test$Id, SalePrice=p$predict)

#calculating total of train revenue (which is only email + call) and newly predicted revenue
# for sales if they occured via email + call as method
predicted_rev = sum(revenue_results$SalePrice)+sum(train$y)
# calculating actual total revenue
actual_rev = sum(df$revenue)
predicted_rev - actual_rev
#based on the model extrapolated outward, focusing all of the energy on email + call
# channels would yield a $965,643.20 gain in revenue compared to utilizing all 3
# sales methods moving forward