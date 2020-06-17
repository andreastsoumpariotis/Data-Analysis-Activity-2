
##################### Salaries and Anxiety ###############################


data1 = read.csv("https://vicpena.github.io/sta9750/salary.csv")
data1
View(data1)
head(data1)

library(ggplot2)
library(dplyr)
library(tidyr)
library(openintro)
library(GGally)

# 1. Create a figure that displays the relationship between salaries, 
# anxiety, and education levels. Your figure can contain more than 
# one plot / facet / panel. Make sure that the labels and the title 
# are interpretable. Interpret in detail the relationships 
# that you see.

ggpairs(data1)

# Salary (y) vs. Anxiety (x)
ggplot(data1) + 
  aes(x = Anxiety, y = Salary, color = Education) +
  geom_point(alpha = .6) + geom_smooth(method = "lm") +
  ggtitle("Salary vs. Anxiety by Education Level")

# Anxiety (y) vs. Salary (x)
ggplot(data1) + 
  aes(x = Salary, y = Anxiety, color = Education) +
  geom_point(alpha = .6) + geom_smooth(method = "lm") +
  ggtitle("Anxiety vs. Salary by Education Level")

# The higher the education level is, the higher the salary
# is on average as well. However, a higher education level 
# also represents generally higher anxiety on average too. 
# For each given education level, lower anxiety, on average,
# indicates a higher average salary.

#2. An article claims that higher salaries come at the cost of 
# higher anxiety levels. Does the figure you created in part 1 
# support this claim?

# No, the figure I created in part 1 does not support this claim.
# My figure shows that for each given education level,higher salaries show 
# lower anxiety levels on average.

##################### Italian Restaurants in NYC ###############################

restaurant = read.csv("https://vicpena.github.io/sta9750/spring19/nyc.csv")
View(restaurant)

#1.	Create a figure that contains plots for all the pairs of variables 
# in the dataset, except Case and Restaurants (i.e., a figure that contains plots 
# for Restaurant vs Price, Food vs Price, Decor vs Service, etc.). 
# Describe what you see in the plots. What are the strongest and 
# weakest relationships you see?

r = restaurant %>% select(-Case, -Restaurant)
r

ggpairs(r)

# By studying the scatterplots, we can see that correlations between
# Price vs. Food, Price vs. Decor, Price vs. Service, etc. are
# all positive. The strongest relationship is Food vs. Service 
# (corr. = ".798") and the weakest is Food vs. Decor (corr. = ".501").

# 2. Provide a heatmap for the correlation between the numerical 
# variables in the dataset. What can you see?

ggcorr(r[, -5], label = TRUE)

# I see the strongest correlation between Food vs. Service and the weakest
# between Food vs. Decor. I also see that each correlation is positive.

# 3. Find 2 examples of cheap restaurants that have relatively good 
# food and 2 examples of expensive restaurants that have relatively 
# bad food. 

# Cheap Restaurants with relatively good food:
CheapPrice = restaurant %>% select(Restaurant, Price) %>%
  arrange(Price)
CheapPrice
GoodFood = restaurant %>% select(Restaurant, Food) %>%
  arrange(desc(Food))
GoodFood
CheapGood = CheapPrice %>% left_join(GoodFood)
CheapGood
# Veronica: (avg. price = 22) & (food = 21)
# Puccini: (avg. price = 26) & (food = 20)

# Expensive Restaurants with relatively bad food:
ExpensivePrice = restaurant %>% select(Restaurant, Price) %>%
  arrange(desc(Price))
ExpensivePrice
BadFood = restaurant %>% select(Restaurant, Food) %>%
  arrange(Food)
BadFood
ExpensiveBad = ExpensivePrice %>% left_join(BadFood)
ExpensiveBad
# Rainbow Grill: (avg. price = 65) & (food = 19)
# Torre di Pisa: (avg. price = 47) & (food = 19)

# 4 Suppose you’re going on a date and want to use the information in 
# this dataset to pick where to go. Assume your budget is at most $40.
# Assuming that you can get a table anywhere you want, where would 
# you go and why?

Forty = restaurant %>% select(Restaurant, Price, Food) %>%
  filter(Price == "40") %>%
  arrange(desc(Food))
Forty
# As I care mostly about the food rating and am assuming that each person 
# pays for their own meal, I would go to "Via Oreto" as it is the 
# restaurant with the best food (22) at the average price of $40.

# 5. Create a figure that displays the relationship between 
# price, food, decor, service, and the East / West indicator. 
# Your figure can contain more than one plot / facet / panel. 
# Make sure that the labels and the title are interpretable. 
# Interpret in detail the relationships that you see.

ggpairs(r)

# These plots show a variety of different things. The scatter plots
# all show quite strongly positive correlations, the strongest being
# Food vs. Service and the weakest being Food vs. Decor. The density
# plots running diagonally down the middle represent the densities
# for each numerical variable while the last diagonal plot, which 
# is a barplot, represents the number of restaurants located in the 
# East and West respectively. The histograms on the bottom show East
# and West restaurant numbers relating to their corresponding 
# numerical variable (Price, Food, Decor, etc.). Lastly, the 
# boxplots on the right represent East and West restaurants 
# corresponding to each numeric variable. They show the distribution
# of data based on restaurants with min., max., median, and quartile
# values which can be used to compare between East/West restaurants.
# For example, the boxplot showing Price vs. East restaurants shows
# that the price for East restaurants have higher median prices compared
# to West restaurants. The same can be said for East food, decor, and
# service. 

##################### Interfaith Dating Data ###############################

Interfaith = read.table("http://users.stat.ufl.edu/~winner/data/interfaith.dat", header = FALSE)
colnames(Interfaith) = c("sec", "religion", "gender", "dating", "count")
Interfaith$sec = factor(Interfaith$sec)
Interfaith$religion = factor(Interfaith$religion)
Interfaith$gender = factor(Interfaith$gender)
Interfaith$dating = factor(Interfaith$dating)
levels(Interfaith$sec) = c("Low", "Middle", "High")
levels(Interfaith$religion) = c("Protestant", "Catholic")
levels(Interfaith$gender) = c("Male", "Female")
levels(Interfaith$dating) = c("Yes", "No")
Interfaith

# Create a figure that shows the relationship between  
# socioeconomic class, religion, gender, and the indicator of 
# interfaith dating. Your figure can contain more than one 
# plot/facet / panel. Interpret in detail the relationships that 
# you see in the plots. Make sure that the labels and the title are 
# interpretable.

library(viridis)
library(ggplot2)

Interfaith

ggplot(Interfaith, aes(fill=religion, y=count, x=sec)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Relationships between SEC, Religion, Gender, and Dating") +
  facet_wrap(c("gender", "dating")) +
  theme() +
  xlab("")

# Each panel in my plot indicates males that are dating/not dating as well
# as females that are dating/not dating. The y-axis indicates count while
# the x-axis indicates SEC (low, medium, and high). Each bar for each SEC 
# is categorized by religion (dark blue indicates Protestant and yellow 
# indicates Catholic). These graphs show multiple relationships between
# the categorical variables. The most interesting (in my opinion) is the
# difference in count between female Protestants and Catholics that aren't
# dating. Essentially, there are significantly more female Protestants
# that aren't dating compared to female Catholics that aren't dating 
# for each SEC. Moreover, another interesting observation is that there 
# are more male Protestants who aren't dating as opposed to male Catholics
# who aren't dating for each SEC as well. The only times we see more Catholics
# than Protestants is under the low SEC's for males and females that are dating,
# which is another interesting occurence in my opinion. 
