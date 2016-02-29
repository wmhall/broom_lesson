library(gapminder)
library(dplyr)
library(broom)

gapminder


# fit a model -------------------------------------------------------------

fm <- lm(lifeExp ~ gdpPercap, data = gapminder)


# explore the model output ------------------------------------------------

str(fm)

#### explore with r's helper functions
summary(fm)
#coef doesn't provide many useful things: t stat, standarrd error, pvalues, 
#+conf intervals
coef(fm)
terms(fm)

#probably don't need to incluude these

#resid gives you the residuals from your fit model: i.e. how far the 
#+ predicted value is from the actual value
resid(fm)
#predict gives you the predicted y value.
predict(fm)

#### this doesn't work 
write.csv(summary(fm), "test.csv")


####exploring using broom
## broom provides more useful helper functions for a variety of statistical objects
?broom

#tidy provides a statistical summary of your model. In regression: 
#this includes:
#  things like coeficients, p values, standard errors
# the important and useful thing is that broom returns dataframes

tidy(fm)

#glance provides a one row summary of model level statistics. This might
#include thigs like mesaures of model fit. 
glance(fm)


#augment provides observation level information about your model
#e.g. in a simple regression this would be row level information
#augment columns:
# fitted: predicted y value
# se fit: Standard errors of fitted values
# residual: difference between predicted and actual y value
# hat: Diagonal of the hat matrix
#sigma: ?Estimate of residual standard deviation when corresponding observation is dropped
#from mode
#cooksd: outlier meausure.i.e. how much does the regression change 
#change because of a particiular value (or if the value is deleted)
# std residuals: standardized residual- z-scores of your residuals

head(augment(fm), 20)

??augment

??broom

#key point: broom returns dataframes that can easily be used with other
#+
augment(fm) %>% class

#you can start to use dplyr fucntions to manipulate your data. 
augment(fm) %>% filter(lifeExp < 30)

write.csv(fm_tidy, "fm1.csv", row.names = F)

#challenge problem: what is the largest standardized residual in the data
#bonus figure out the row in the data set that contibutes to those outliers
#hint: check out the function augment_columns()

head(augment(fm))

augment(fm) %>% 
  filter(.std.resid < -2.5 | .std.resid > 2.5) %>% arrange(.std.resid)

augment_columns(fm, data = gapminder) %>% 
  filter(.std.resid < -2.5 | .std.resid > 2.5) %>% arrange(.std.resid)


# fitting multiple models -------------------------------------------------

unique(gapminder$country) %>% length()

gapminder %>% 
  group_by(country) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data = .)))


###only show me the slopes

gapminder %>% 
  group_by(country) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data = .))) %>% 
  filter(term != "(Intercept)") %>% 
  ungroup %>% 
  arrange(-estimate)

##only show me the slopes that have p values less than .05

gapminder %>% 
  group_by(country) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data = .))) %>% 
  filter(term != "(Intercept)" & p.value <.05)

#### add CIs

gapminder %>% 
  group_by(country) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data = .), conf.int = T))

#For each continent, find out how many countries have no relationship between
#+ lifeExp and gdp

gapminder %>%  
  group_by(continent, country) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data = .))) %>% 
  filter(term !="(Intercept)" & p.value >.05) %>% 
  group_by(continent) %>% 
  summarise(n_count = n())


# create a plot -----------------------------------------------------------

library(ggplot2)

gapminder %>% 
  group_by(year) %>%
  do(tidy(lm(lifeExp ~ gdpPercap, data = .), conf.int =T)) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(y = estimate, x = factor(year))) + geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) 



gapminder %>%
  filter(continent != "Oceania") %>% 
  group_by(year, continent) %>%
  do(tidy(lm(lifeExp ~ gdpPercap, data = .), conf.int =T)) %>% 
  filter(term != "(Intercept)") %>% 
  group_by(continent) %>% 
  do(
    {
      p <- ggplot(., aes(y = estimate, x = factor(year))) + geom_point() +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high))
      ggsave(p, filename=paste0(unique(.$continent),".png"))  
    }
  )





#outlier plot

gapminder %>% 
  do(augment(lm(lifeExp ~ gdpPercap, data =.))) %>% 
  ggplot(., aes(y = .std.resid, x =.fitted)) +
  geom_point() +
  geom_smooth()


# bootstrapping -----------------------------------------------------------

#show this first

gapminder %>% 
  bootstrap(., 10) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data =.))) 

#then this

gapminder %>% 
  bootstrap(., 10) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data =.))) %>% 
  group_by(term) %>% 
  summarise(conf.low = quantile(estimate, .025), 
            conf.high = quantile(estimate, .975))


gapminder %>% 
  bootstrap(., 10) %>% 
  group_by(country) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data =.))) %>% 
  group_by(country, term) %>% 
  summarise(conf.low = quantile(estimate, .025), 
            conf.high = quantile(estimate, .975))


#note that this is a slow way to bootstrap. faster tools exist e.g. boot()
# however could be a good way to teach bootstrapping as not a blackbox like
#+ other tools.

# simulation --------------------------------------------------------------
###?????





# create a regression plot ------------------------------------------------

#### too much work

library(tidyr)

gapminder %>% 
  group_by(continent) %>%
  do(tidy(lm(lifeExp ~ gdpPercap, data = .), conf.int =T)) %>% 
  ungroup() %>% 
  mutate(term = stringr::str_replace_all(term, "\\)|\\(", "")) %>% 
  select(continent, term, estimate) %>% 
  spread(term, estimate) %>% 
  ggplot(aes())

