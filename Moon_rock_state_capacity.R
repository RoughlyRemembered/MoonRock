## Moon rock locatability - state capacity graph ##

setwd("~/OneDrive - Imperial College London/Roughly Remembered")
getwd()

library(tidyverse)
library(ggthemes)
library(janitor)

moon_rock_data <- read.csv("Moon Rock Data - Table.csv", stringsAsFactors = TRUE, na.strings = c("#N/A")) %>%
  clean_names()%>%
  filter(!is.na(x2021_estimate)) %>%
  select(current_state, moon_rock_locatable, x2021_estimate, x)%>%
  mutate(moon_rock_lost = if_else(moon_rock_locatable == 0, 1, 0))

str(moon_rock_data)
summary(moon_rock_data)
hist(moon_rock_data$x2021_estimate)
sum(is.na(moon_rock_data)) # no null values

moon_rock_boxplot <- moon_rock_data %>%
  ggplot(aes(x = x, y = x2021_estimate))+
  geom_boxplot(colour = "black", fill = c("#4D9221", "#C51C7D"))+
  geom_point(aes(alpha = 1),size = 3, shape = 16, position = position_jitterdodge()) +
  guides(alpha = "none")+
  theme_classic()+
  labs(x = "Moon rock location", y = "2021 Government Effectiveness Score", title = 
         "Government Effectiveness of countries that do and don't know where their moon rock is",
         subtitle = "Government Effectiveness Score from http://info.worldbank.org/governance/wgi/")+
  scale_x_discrete(labels = c("Known", "Unknown"))+
  theme(plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

moon_rock_boxplot

lm1 <- glm(moon_rock_locatable ~ x2021_estimate, family = binomial(link = "logit"), data = moon_rock_data)
summary(lm1)

# A one unit increase in the predictor variable Government Effectiveness Score is associated
# with an average change of 1.1839 in the log odds of the moon rock being locatable. To 
# convert log odds into odds you just take the exponential.

exp(cbind(OR = coef(lm1), confint(lm1)))

# For a one-unit increase in x2021_estimate, the odds of the moon rock being locatable are expected 
# to increase by a factor of 3.267. This means that as x2021_estimate increases, the 
# likelihood of moon_rock_locatable being 1 (rather than 0) increases by approximately 
# 3.267 times.

# The Pr(>|z|) is significant (5.99e-07) so Government Effectiveness Score is a
# significantly significant predictor variable.

# To determine if the model is useful we calculate the Chi-squared statistic for the 
# null vs residual deviance:

chi_sq <- 187.9 - 147.63
chi_sq

# there is 1 predictor variable so we find the p-value for a chi-squared value of 40.27 with
# 1 degree of freedom. This is p = 0 so the model is highly useful.

# Less easily interpetable is modelling the odds of the moon rock being lost with each
# unit increase in GES because the odds get smaller, but this is just represented by
# an odds ratio of less than 1.

lm2 <- glm(moon_rock_lost ~ x2021_estimate, family = binomial(link = "logit"), data = moon_rock_data)
summary(lm2)

exp(cbind(OR = coef(lm2), confint(lm2)))

# This model can be used to predict the likelihood that other countries would have lost
# their moon rock had they been given one.

ges_score <- read.csv("Government_Effectiveness.csv", stringsAsFactors = TRUE, na.strings = c("#N/A")) %>%
  clean_names()%>%
  rename(current_state = country_territory)

# Only keep those countries that were not given moon rock

grey_countries <- ges_score %>%
  left_join(moon_rock_data, by = "current_state")%>%
  filter(is.na(moon_rock_locatable))%>%
  select(current_state, x2021_estimate.x)%>%
  rename(x2021_estimate = x2021_estimate.x)

ges_df <- data.frame(grey_countries$x2021_estimate)%>%
  rename(x2021_estimate = grey_countries.x2021_estimate)
probs_lost <- predict(lm2,ges_df, type = "response")

grey_countries <- grey_countries %>%
  add_column(probs_lost)

write_csv(grey_countries, "odds_lost_grey_countries.csv")


# How much more likely are countries in the bottom quartile
# to have lost the rock than countries in the top quartile?

# Divide the GES scores into quartiles
quartiles <- quantile(moon_rock_data$x2021_estimate, probs = c(0, 0.25, 0.5, 0.75, 1))

# Filter the data for the bottom and top quartiles
bottom_quartile <- moon_rock_data %>% filter(x2021_estimate <= quartiles[2])
top_quartile <- moon_rock_data %>% filter(x2021_estimate >= quartiles[4])

# What proportion of countries in the bottom quartile lost the rock
prop_bottom <- mean(bottom_quartile$moon_rock_locatable == 0)
# What proportion of countries in the top quartile lost the rock
prop_top <- mean(top_quartile$moon_rock_locatable == 0)

# The likelihood_ratio value will indicate the relative likelihood of moon_rock_locatable 
# being 0 in the bottom quartile compared to the top quartile of x2021_estimate.
likelihood_ratio <- prop_bottom / prop_top

likelihood_ratio

