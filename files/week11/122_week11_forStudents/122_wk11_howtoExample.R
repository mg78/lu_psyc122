library(broom)
library(tidyverse)

# read in the data
mh <- read_csv("MillerHadenData.csv")
mh

# plot the relationship between reading ability and IQ using a scatterplot and a line of best fit
ggplot(mh, aes(x = Abil, y = IQ)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Reading ability")

# conduct a correlation analysis, using Pearson's r
results <- cor.test(mh$Abil, 
                    mh$IQ, 
                    method = "pearson", 
                    alternative = "two.sided") %>% 
  tidy()

results

# pull out Pearson's r, the degrees of freedom and the p-value for reporting the results
r <- results %>%
  pull(estimate) %>%
  round(2)

df <- results %>%
  pull(parameter)

pvalue <- results %>%
  pull(p.value) %>%
  round(3)
