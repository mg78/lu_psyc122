# PSYC122: Week 13 - Lab activity 2

# Background ------------------------------------------------------------
# Our hypothesis is that greater anxiety would be reflected in lower engagement. 
# QUESTION: If our hypothesis is correct, what type of correlation (if any) should we observe between students' mean anxiety levels and the variable n_week?
# ANSWER: A negative correlation

# Step 1 ------------------------------------------------------------------

# Run this code to empty the R environment
rm(list=ls())                            

# Run this code to load relevant libraries
library("broom")
library("car")
library("pwr")  
library("tidyverse")

# TASK: Read in both files (using the read_csv() function), have a look at the layout of the data and
# familiarise yourself with it. You can use the head() function for that.

# Write the code to read in data and have a look at the tables
stars <- read_csv("stars2.csv")                                   # Read in the data files
engage <- read_csv("psess.csv")

head(stars)                                                         # Look at the data frames
head(engage)

# QUESTION: In the stars table, what do the numbers in the first row across the three columns refer to?
# ANSWER: ID = 3, Question  = Q01 and Score  = 1 shows us that participant 3 reported a score of 1 on question 1.

# Step 2 ------------------------------------------------------------------
# Now that we’ve read in both data files, the next step is to calculate the mean anxiety scores for each participant
#
# TASK: At the moment we have scores on all questions separately for each participant in the stars2 table. Instead we need 
# one mean anxiety score for each participant. Remember that participant is identified by the ID variable. Store the resulting
# table in a variable named stars_mean.
#
# HINT: Use group_by() and summarise(). Also, remember to use na.rm = TRUE when calculating the mean scores to deal with
# participants who have missing data (NAs).

# Write the code to calculate mean anxiety scores
stars_means <- stars %>%
  group_by(ID) %>%
  summarise(mean_anxiety = mean(Score, na.rm = TRUE))

# QUESTION: What is the mean anxiety score for participant 3?
# ANSWER: 1.058824

# Step 3 ------------------------------------------------------------------
# Ok, before we get ahead of ourselves, in order to perform the regression analysis we need to combine the data
# from stars2 (the mean anxiety scores) with the data from engage (n_weeks).
#
# TASK: Join the two tables, call the resulting table 'joined'.
#
# HINT: Use the inner_join() function (making use of the variable that is common across both tables) to join. 

# Write the code to join datasets together
joined <- inner_join(stars_means, engage, "ID")

# Step 4 ------------------------------------------------------------------
# We now need descriptive statistics for both variables.
#
# TASK: Calculate the mean and standard deviations for the anxiety scores and the engagement data.

# Write the code to calculate means and standard deviations for anxiety and engagement
descriptives <- joined %>%
  summarise(mean_anx = mean(mean_anxiety, na.rm = TRUE),
            sd_anx = sd(mean_anxiety, na.rm = TRUE),
            mean_weeks = mean(n_weeks, na.rm = TRUE),
            sd_weeks = sd(n_weeks, na.rm = TRUE))

# QUESTION: What are the means and standard deviation for anxiety and engagement with the statistics module?
# ANSWER: Anxiety M = 2.08, SD = 0.56; Engagement M = 4.54, SD = 2.42

# Step 5 ------------------------------------------------------------------
# As always, it is a good idea to visualise your data.
#
# TASK: Now that we have all the variables in one place, make a scatterplot of anxiety as a function of engagement.
#
# HINT: For this you'll need the ggplot() function together with geom_point() and geom_smooth(). Make
# sure to give your axes some sensible labels with the labs() function.

# Write the code to create the scatterplot
ggplot(joined, aes(x = mean_anxiety, y = n_weeks)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Anxiety", y = "Engagement")
  theme_bw()

# QUESTION: What does the scatterplot suggest about the relationship between anxiety and engagement?
# ANSWER: That there might indeed be a relatively strong negative correlation between the two; students with more anxiety,
# engage less.

# Step 6 ------------------------------------------------------------------
# With all the variables in place, we're ready now to start building the regression model.
#
# TASK: Use the lm() function to run the regression model when you model engagement (the outcome variable) as a function
# of anxiety (the predictor variable) and use the summary() function to look at the output
#
# HINT: lm(outcome ~ predictor, data = my_data)

# Run the code to build the regression model
mod <- lm(n_weeks ~ mean_anxiety, data = joined)
mod_summary <- summary(mod)
mod_summary

# QUESTION: a) What is the estimate of the y-intercept for the model, rounded to three decimal places?
# ANSWER: 9.057
# EXPLANATION: In the summary table, this is the estimate of the intercept

# QUESTION: b)	To three decimal places, if the General Linear Model for this model is Y=beta0 + beta1X + e, then beta1 is …
# ANSWER: -2.173
# EXPLANATION: In the summary table, this is the estimate of mean_anxiety, i.e., the slope.

# QUESTION: c)	To three decimal places, for each unit increase in anxiety, n_weeks decreases by …
# ANSWER: 2.173
# EXPLANATION: In the summary table, this is also the estimate of mean_anxiety, the slope is how much it decreases so you just remove the - sign.

# QUESTION: d)	To two decimal places, what is the overall F-ratio of the model?
# ANSWER: 11.99
# EXPLANATION: In the summary table, the F-ratio is noted as the F-statistic.

# QUESTION: e)	Is the overall model significant?
# ANSWER: Yes
# EXPLANATION: The overall model p-value is .001428 which is less than .05, therefore significant.

# QUESTION: f)	What proportion of the variance does the model explain?
# ANSWER: 25.52%
# EXPLANATION: The variance explained is determined by R-squared, you simply multiple it by 100 to get the percent.

# Step 7 ------------------------------------------------------------------
# Now that we've fittes a model, let's check whether the meets the assumptions of
# linearity, normality and homoscedasticity. 
#
# HINT: crPlots() to check linearity
#       qqPlot() to check normality of the residuals
#       residualPlot() to check homoscedasticity of the residuals

# Run the code below to check the assumptions
crPlots(mod)                  # Plot linear line and line that best fits the data to check the relationship between outcome and predictor is linear

qqPlot(mod$residuals)         # Create qq-plot to check residuals are normally distributed

residualPlot(mod)             # Create residual plot to check residual show homoscedasticity

# QUESTION: Does the relationship appear to be linear?
# ANSWER: Yes, the pink link roughly falls across the dashed blue line and looks mostly linear.

# QUESTION: Do the residuals show normality?
# ANSWER: Yes, in the qq-plot the open circles mostly assemble around the solid blue line, and fall mostly within the range of
# the dashed blue lines.

# QUESTION: Do the residuals show homoscedasticity?
# ANSWER: Yes, the residual plot shows that the spread of the residuals is roughly similar for different fitted values.

# Step 8 -----------------------------------------------------------------
# TASK: Write up the results following APA guidelines.
#
# HINT: The Purdue writing lab website (https://owl.purdue.edu/owl/research_and_citation/apa6_style/apa_formatting_and_style_guide/statistics_in_apa.html)
# is helpful for guidance on punctuating statistics.

# Write up:
# A simple linear regression was performed with engagement (M = 4.54, SD = 0.56) as the outcome variable and
# statistics anxiety (M = 2.08, SD = 0.56) as the predictor variable. The results of the regression indicated that the
# model significantly predicted course engagement (F(1, 35) = 11.99, p < .001, R2 = 0.25), accounting
# for 25% of the variance. Anxiety was a significant negative predictor (beta = -2.17, p < 0.001): as anxiety increased,
# course engagement decreased.
