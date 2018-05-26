library(ggplot2) # for barplot
library(stringr) # for str_wrap function
library(plotly)
library(glmnet) # for lasso
rm(list = ls())

# load dataset ICPSR 34562
load("P:/2018/ICPSR_34562/DS0001/34562-0001-Data.rda")


# Remove rows where all values in MOS_INCARC columns are 0.
#######################
# glob2rx() converts a pattern to regular expression
reg_exp_MOS_INCARC <- glob2rx("MOS_INCARC_CY*")

# grepl() returns a logical vector (match or not match for each element of x)
columns_MOS_INCARC <- grepl(reg_exp_MOS_INCARC, names(da34562.0001))

# dataset with only MOS_INCARC_* vars
data_MOS_INCARC <- da34562.0001[ columns_MOS_INCARC ]

# Go through each row and determine if all values are zero or NA
row_sub = apply(data_MOS_INCARC, 1, function(row) all(row == 0 ))
row_sub = apply(data_MOS_INCARC, 1, function(row) all(row == 0 | is.na(row)))
head(!row_sub)

# Subset data. Keep only rows in original dataset where R was incarcerated at least once
data <- da34562.0001[!row_sub, ]

# Only MOS_INCARC columns. Only rows where R was incarcerated at least once
data_MOS_INCARC_incarcerated <- data_MOS_INCARC[!row_sub, ]


# Create binary variable for recidivism
#######################
# Count numbers of non-zeros in each row
row_nonzero <- rowSums(data_MOS_INCARC_incarcerated != 0, na.rm = T)

# If row has > 1 non-zero, then R was incarcerated > 1 time
row_recid <- row_nonzero > 1

# create recid variable, binary
data$recid <- as.numeric(row_recid)

head(data$recid)

# create recid variable, binary
data_MOS_INCARC_incarcerated$recid <- as.numeric(row_recid)

# dataset with only R who were incarcerated at least 1 month per year for > 1 year
data_recid <- data[data$recid == 1, ]

# Keep only rows where values in MOS_INCARC columns are non-missing.
#######################
# complete.cases returns T/F vector indicating which cases have no NAs in the columns MOS_INCARC_*
# data2 <- data[complete.cases(data[columns_MOS_INCARC]), ]

# Summary statistics
#######################

# Paragraph 1 section 5
# Count number of R who were incarcerated at least 1 month per year for more than 1 year
sum(data$recid) # 318
sum(data_MOS_INCARC_incarcerated$recid) # should be same as above
nrow(data_recid) # same as above


# Percentage of R who were incarcerated at least 1 month
nrow(data) / nrow(da34562.0001)

# Percentage of R who were incarcerated at least 1 month a year in > 1 year
sum(data$recid) / nrow(da34562.0001)


# Table 1
# Count of R who were incarcerated at least 1 month, by gender and race
table(data$SEX, data$RACE_ETHNICITY) 
# cell counts add to total 584, so race_ethnicity groups are mutually exclusive.

# Percentage of R who were incarcerated at least 1 month, by gender and race. Cell percentages.
prop.table(table(data$SEX, data$RACE_ETHNICITY))
# prop.table(table(data$SEX, data$RACE_ETHNICITY), 1) for row %. 2 for column %.


# Table 2 (not shown)
# Count of R who were incarcerated at least 1 month per year for > 1 year, by gender and race
table(data_recid$SEX, data_recid$RACE_ETHNICITY)

# % of R who were incarcerated at least 1 month per year for > 1 year, by gender and race. Cell percentages.
prop.table(table(data_recid$SEX, data_recid$RACE_ETHNICITY))


# Table 3 (table 2 in draft paper)
# Count of R in full sample, by gender and race
table(da34562.0001$SEX, da34562.0001$RACE_ETHNICITY)
# cell counts add to total 8,984, so race_ethnicity groups are mutually exclusive.

# % of R in full sample, by gender and race
prop.table(table(da34562.0001$SEX, da34562.0001$RACE_ETHNICITY))

# Bar graphs for section 5
#######################

# Graph 1: for gender
# Count of 2 gender groups, R incarcerated at least 1 month
table(data$SEX)

# % of total, for 2 gender groups, R incarcerated at least 1 month
prop.table(table(data$SEX))


# Count of 2 gender groups, full sample
table(da34562.0001$SEX)

# % of total, for 2 gender groups, full sample
prop.table(table(da34562.0001$SEX))


# create dataframe for plotting
percent <- c(prop.table(table(data$SEX)), prop.table(table(da34562.0001$SEX)))
percent <- round(percent * 100, digits = 2)

gender <- c("Male", "Female", "Male", "Female")

sample_type <- c("Incarcerated At Least Once", "Incarcerated At Least Once", "Full Sample", "Full Sample")

graph1_data <- data.frame(sample_type, gender, percent)
graph1_data


png(file = "P:/2018/R work/graphs/Sample-by-gender.png", width = 1200, height = 1000, units = "px", res = 150)
ggplot(data = graph1_data, aes(x = sample_type, y = percent, fill = gender)) +
  # position_dodge creates grouped bar plot instead of stacked bar plot
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) + 
  # specify colors for the "fill" variable earlier
  scale_fill_manual(values = c("Female" = "#ca0020", "Male" = "#0571b0")) +
  # display y values on bars
  geom_text(aes(label = paste(graph1_data$percent, "%")), vjust = -0.3, size = 3, position = position_dodge(0.7)) + 
  # wrap x var names
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  # specify legend title, given by "fill" variable earlier
  labs(title = "Percentages of Males and Females, Incarcerated and Full Sample",
       subtitle = "Data from NLSY 1997 Standalone, 1997-2009",
       fill = "") +
  # make sure all plots for all years have the same y axis scale
  ylim(0, 100) +
  theme_minimal() + 
  # remove x axis title, grid lines and y axis. Change x axis text and legend title to bold.
  theme(axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 13, face = "bold"),
    panel.grid = element_blank(),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    plot.title = element_text(size = 16, face = "bold")
  )

dev.off()


# Set my plotly credentials (to publish plotly plots)
Sys.setenv("plotly_username"="gnrudderham")
Sys.setenv("plotly_api_key"="udCkEqIZHLuoIRuN6CNL")

# Create dataframe for plotting with plotly - graph 1
percent <- c(prop.table(table(data$SEX)), prop.table(table(da34562.0001$SEX)))
percent <- round(percent * 100, digits = 2)

gender <- c("Male", "Female", "Male", "Female")

sample_type <- c("Incarcerated At Least Once", "Full Sample")
Male <- c(prop.table(table(data$SEX))[1], prop.table(table(da34562.0001$SEX))[1])
Female <- c(prop.table(table(data$SEX))[2], prop.table(table(da34562.0001$SEX))[2])

graph1_data <- data.frame(sample_type, gender, percent)
graph1_data 

# Graph 1: plotly
p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
  add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

p <- plot_ly(data = graph1_data, x = ~sample_type, y = ~percent, fill = ~gender, type = "bar", name = "gender",
             marker = list(color = ls("#3182bd", "#de2d26"))) %>%
  layout(title = "Maternal Mortality Ratio Per 100,000 Live Births (2015)",
         xaxis = list(title = "", showticklabels = F),
         yaxis = list(title = ""),
         bargap = 0)
#yaxis = list(title = "Electrification Rate (%)"), 
#barmode = "group")
p




# Graph 2: for race/ethnicity
# Count of 4 race groups, R incarcerated at least 1 month
table(data$RACE_ETHNICITY)

# % of total, for 4 race groups, R incarcerated at least 1 month
prop.table(table(data$RACE_ETHNICITY))


# Count of 4 race groups, full sample
table(da34562.0001$RACE_ETHNICITY)

# % of total, for 4 race groups, full sample
prop.table(table(da34562.0001$RACE_ETHNICITY))


# create dataframe for plotting
percent_graph2 <- c(prop.table(table(data$RACE_ETHNICITY)), prop.table(table(da34562.0001$RACE_ETHNICITY)))
percent_graph2 <- round(percent_graph2 * 100, digits = 2)

race <- c("Black", "Hispanic", "Mixed race (non-Hispanic)", "Non-black / non-Hispanic")

sample_type_graph2 <- c(rep("Incarcerated At Least Once", 4), rep("Full Sample", 4))

graph2_data <- data.frame(sample_type_graph2, race, percent_graph2)
graph2_data

png(file = "P:/2018/R work/graphs/Sample-by-race.png", width = 1500, height = 1000, units = "px", res = 150)
ggplot(data = graph2_data, aes(x = sample_type_graph2, y = percent_graph2, fill = race)) +
  # position_dodge creates grouped bar plot instead of stacked bar plot
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) + 
  # specify colors for the "fill" variable earlier
  scale_fill_manual(values = c("Black" = "#ca0020", "Hispanic" = "#92c5de",
                               "Mixed race (non-Hispanic)" = "#f4a582", 
                               "Non-black / non-Hispanic" = "#0571b0")) +
  # display y values on bars
  geom_text(aes(label = paste(graph2_data$percent_graph2, "%")), vjust = -0.3, size = 3, position = position_dodge(0.7)) + 
  # wrap x var names
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  # specify legend title, given by "fill" variable earlier
  labs(title = "Percentages of Race/Ethnicity Groups, Incarcerated and Full sample",
       subtitle = "Data from NLSY 1997 Standalone, 1997-2009",
       fill = "") +
  # make sure all plots for all years have the same y axis scale
  ylim(0, 100) +
  theme_minimal() + 
  # remove x axis title, grid lines and y axis. Change x axis text and legend title to bold.
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 13, face = "bold"),
        panel.grid = element_blank(),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13),
        legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold")
  )

dev.off()


# Lasso
#####################################
row_data_na = apply(data, 1, function(row) any(is.na(row)))
unique(row_data_na) # all True. So each row in data has at least 1 NA.
# This will create a problem with creating the design matrix.

# options(na.action="na.pass ")

# Drop columns that have at least 1 NA. [Common problem with survey data.]
data2 <- data[, complete.cases(t(data))] 

x <- model.matrix(recid ~ ., data = data2)[, -1]
y <- as.factor(data2$recid)
grid =10^seq (10,-2, length =100)

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]


lasso.mod <- glmnet(x[train, ], y[train], family = "binomial", alpha = 1, lambda = grid)
plot(lasso.mod)


# Cross-validation to get best lambda
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], family = "binomial", alpha = 1)
plot(cv.out)

bestlam = cv.out$lambda.min #0.016

lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ], family = "binomial")
mean((lasso.pred - as.numeric(y.test)) ^ 2) # 3.73


# Fit the entire dataset with all obs
out = glmnet(x, y, family = "binomial", alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam, family = "binomial")[1:53 ,]
chosen_predictors <- lasso.coef[ lasso.coef != 0 ]
chosen_predictors # has 18 predictors


# Save workspace
#######################
setwd("P:/2018/R work")


save.image(file = "data34562.RData")
load(file = "data34562.RData")


#####################################
#####################################
# Example: Experimentation with ridge and lasso
#####################################
swiss <- datasets::swiss
x <- model.matrix(Fertility~., swiss)[,-1]
y <- swiss$Fertility
lambda <- 10^seq(10, -2, length = 100) # lambda is decreasing to 0

#create test and training sets


set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

#OLS
swisslm <- lm(Fertility~., data = swiss)
coef(swisslm)

#ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
# predict(ridge.mod, s = 0, exact = T, type = 'coefficients')[1:6,]
# exact = T throws error
predict(ridge.mod, s = 0, type = 'coefficients')[1:6,]

# use ridge to improve on OLS estimate.
swisslm <- lm(Fertility~., data = swiss, subset = train)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)

#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)

bestlam <- cv.out$lambda.min # 1.386

#make predictions
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
s.pred <- predict(swisslm, newdata = swiss[test,])
#check MSE
mean((s.pred-ytest)^2)

mean((ridge.pred-ytest)^2)

#a look at the coefficients
out = glmnet(x[train,],y[train],alpha = 0)
predict(ridge.mod, type = "coefficients", s = bestlam)[1:6,]

# lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])

mean((lasso.pred-ytest)^2)

lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:6,]
lasso.coef # lasso places high importance on Education, Examination, and Infant.Mortality. 
# Looks like they select the coeff higher than 0.05 in absolute value.


# Toy Example: count # of zeros in a row
########################
a = c(1,2,3,4,5,6,0,2,5)
b = c(0,0,0,2,6,7,0,0,0)
c = c(0,5,2,7,3,1,0,3,0)
d = c(1,2,6,3,8,4,0,4,0)
e = c(0,4,6,3,8,4,0,6,0)
f = c(0,2,5,5,8,4,2,7,4)
g = c(0,8,5,4,7,4,0,0,0)
h = c(1,3,6,7,4,2,0,4,2)
i = c(1,5,3,6,3,7,0,5,3)
j = c(1,5,2,6,4,6,8,4,2)

DF<- data.frame(a=a,b=b,c=c,d=d,e=e,f=f,g=g,h=h,i=i,j=j)
rowSums(DF == 0) # return # zeros in each row
DF[rowSums(DF == 0) <= 4, ] # subset data. Take only rows where # zeros <= 4.

# Toy Example 2: count # of non-zeros in a row
########################

example = matrix(sample(c(0,0,0,100),size=70,replace=T),ncol=7)
example
rowSums(example != 0)
