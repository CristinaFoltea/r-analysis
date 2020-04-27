## MPG Regression

# read the csv file
mecha_car <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F)

# use the corellation matrix to identify variable with strong correlation to the mpg
mecha_car_matrix <- as.matrix(mecha_car)
cor(mecha_car_matrix)

# build a model using the vehicle.length and ground.clearance as independed variables
model <- lm(mpg ~ vehicle.length + ground.clearance, data=mecha_car)

# retrive summary data
summary(model)

# r-squared = 0.674 meaning 33% of the of the prediction will be correct using this model
# p-value = 3.637e-12 which is a lot smaller that 0.05% therefore we there is sufficient evidence to reject our
# null hypothesis, meaning that the slope of your model is not zero


## Suspension Coil Summary
# read the csv data 
suspension_coil_data <- read.csv('Suspension_Coil.csv',stringsAsFactors = F)

# build the statistical summary table
summary_table <- suspension_coil_data %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), 'Standart Deviation'=sd(PSI))
summary_table

## Suspension Coil T-Test

population_mean <- 1500

# using the one-sample t-test to determine statistical difference between the means of 
# Suspension Coil dataset and population mean of 15000

t.test(suspension_coil_data$PSI, mu=population_mean)

# Assuming our significance level was the common 0.05 percent, 
# our p-value=0.06028 is above our significance level.
# Therefore, we do not have sufficient evidence to reject the null hypothesis, 
# and we would state that the two means are statistically similar.

