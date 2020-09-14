rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")


# Load data 
wine_price = read_csv("data/raw.csv") 
head(wine_price)
names(wine_price)


# Wrangle variables ------------

"Number of levels ?"
# levels = apply(wine_price, 2, function(x) { factor(x) %>% levels() %>% length() })
# save(levels, file = "data/levels.RData")
load("data/levels.RData")
# Remove designation and winery as there are too many levels and not much information
# Remove province, keep country and region

"NAs ?"
apply(wine_price, 2, function(x) { sum(is.na(x)) })
# Remove NA prices since it's the outcome
# We assume that the taster may have a strong impact on the price so we remove NAs
# Remove NA variety and countries (<100 obs)
# We can find regions in the title

wine_price = wine_price %>%
   select(country, description, points, price, 
          region_1, taster_name, variety, title) %>%
   filter(!is.na(price), !is.na(variety), !is.na(country), !is.na(taster_name)) %>%
   rename(region = region_1)

"Find regions"
temp = wine_price %>% 
   select(region, title) %>%
   filter(is.na(region))
# Find the pattern
str_view(temp$title[1:20], "\\(([:print:]+)\\)")
# How good does it work ?
temp$title[!str_detect(temp$title, "\\(([:print:]+)\\)")]
# Clean data
wine_price = wine_price %>%
   filter(!is.na(region) | str_detect(title, "\\(([:print:]+)\\)")) %>%
   mutate(region = ifelse(is.na(region), str_extract(title, "\\(([:print:]+)\\)"), region)) %>%
   mutate(region = str_remove_all(region, "\\(|\\)")) %>%
   filter(str_count(region) < 25)
#
rm(temp)
wine_price = select(wine_price, -title)

save(wine_price, file = "data/wine_price.RData")



# Explore and clean data ------------------------------------------------------------

load(file = "data/wine_price.RData")
head(wine_price)

# Ratings distribution
hist(wine_price$points, main = "Grades distribution")
# Price distribution
ggplot(wine_price, aes(price)) +
   geom_histogram(color = "black", fill = "red", alpha = 0.8) +
   scale_x_log10() +
   ggtitle("Prices distribution") +
   theme(plot.title = element_text(hjust = 0.5))

# By country
wine_price %>%
   group_by(country) %>%
   summarise(n = n()) %>%
   mutate(country = reorder(country, n)) %>%
   ggplot(aes(y = country, x = n)) +
   geom_bar(stat = "identity", fill = "#f68060", alpha = 0.8, width = 0.6) +
   scale_x_log10() +
   xlab("# of observations - log scale") +
   ylab("") +
   ggthemes::theme_clean()
# As we can see some countries have few observations, we'd like to know if the country from
# which wines come from has an effect on its price, that's why we will remove countries
# with less than 50 wines
temp = wine_price %>%
   group_by(country) %>%
   summarise(n = n())
countries = temp$country[which(temp$n >= 50)]
wine_price = wine_price %>%
   filter(country %in% countries)
rm(temp)

# By taster name
wine_price %>%
   group_by(taster_name) %>%
   summarise(n = n()) %>%
   mutate(taster_name = reorder(taster_name, n)) %>%
   ggplot(aes(y = taster_name, x = n)) +
   geom_bar(stat = "identity", fill = "#f68060", alpha = 0.8, width = 0.6) +
   scale_x_log10() +
   xlab("# of observations - log scale") +
   ylab("") +
   ggthemes::theme_clean()
# Same process
temp = wine_price %>%
   group_by(taster_name) %>%
   summarise(n = n())
tasters = temp$taster_name[which(temp$n >= 50)]
wine_price = wine_price %>%
   filter(taster_name %in% tasters)
rm(temp)

# By variety
wine_price %>%
   group_by(variety) %>%
   summarise(n = n()) %>%
   mutate(variety = reorder(variety, n)) %>%
   ggplot(aes(y = variety, x = n)) +
   geom_bar(stat = "identity", fill = "#f68060", alpha = 0.8, width = 0.6) +
   scale_x_log10() +
   xlab("# of observations - log scale") +
   ylab("") +
   ggthemes::theme_clean()
# Same process
temp = wine_price %>%
   group_by(variety) %>%
   summarise(n = n())
varieties = temp$variety[which(temp$n >= 50)]
wine_price = wine_price %>%
   filter(variety %in% varieties)
rm(temp)

# By region
wine_price %>%
   group_by(region) %>%
   summarise(n = n()) %>%
   mutate(region = reorder(region, n)) %>%
   ggplot(aes(y = region, x = n)) +
   geom_bar(stat = "identity", fill = "#f68060", alpha = 0.8, width = 0.6) +
   scale_x_log10() +
   xlab("# of observations - log scale") +
   ylab("") +
   ggthemes::theme_clean()
# Same process 20
temp = wine_price %>%
   group_by(region) %>%
   summarise(n = n())
regions = temp$region[which(temp$n >= 20)]
wine_price = wine_price %>%
   filter(region %in% regions)
rm(temp)

save(wine_price, file = "data/wine_price_2.RData")




# Relationships -----------------------------------------------------------

load(file = "data/wine_price_2.RData")
head(wine_price)

"Score effect"
plot1 = wine_price %>%
   ggplot(aes(points, price)) +
   geom_point(alpha = 0.6) +
   geom_smooth() +
   ggthemes::theme_economist_white() +
   xlab("Score") + ylab("Price") +
   scale_y_log10() +
   ggtitle("Relationship Price ~ Score")
save(plot1, file = "data/plot1.RData")
load("data/plot1.RData"); plot1
# Seems that there is a log-lin relationship though the score doesn't explain outsiders i.e. luxury wines

"Country effect"
wine_price %>%
   group_by(country) %>%
   mutate(median = median(price)) %>%
   ggplot(aes(reorder(country, desc(median)), price, fill = reorder(country, desc(median)))) +
   geom_boxplot(show.legend = F, outlier.size = 1, outlier.alpha = 0.8) +
   scale_y_log10() +
   theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
   xlab("") + ylab("Price") +
   ggtitle("Price distributions across countries")
# The heterogeneity is striking ! Some countries have tiny price range while others
# have many outliers. Actually these countries are well known for their wines (France, Italy, ...)

"Region effect"
# We will study the regions effect in countries with the most regions recorded for wines
wine_price %>% 
   group_by(country) %>%
   summarise(regions = n_distinct(region)) %>%
   arrange(desc(regions))
# US, France 
temp = wine_price %>% 
   filter(country == "US") %>%
   group_by(region) %>%
   mutate(median = median(price),
          max = max(price),
          min = min(price))
summary(temp)
temp %>% ggplot(aes(median)) +
   geom_density(fill = "red", alpha = 0.2, bw = 0.05) +
   geom_density(aes(max), fill = "blue", alpha = 0.2, bw = 0.05) +
   scale_x_log10() +
   geom_density(aes(min), fill = "green", alpha = 0.2, bw = 0.05) +
   coord_flip() +
   xlab("Min - Median - Max Price") +
   ggtitle("Price distribution accross US regions")
# These 3 density plots show the distributions of the min, median and max price across regions
#> Median price reanges between 14 and 90 which is very large -> some regions produce expansive wines
#> Min price ranges from 4 to 40 !!
#> Max price rockets to $2,013 while some regions have max price lower than $100, minmax = 22
temp = wine_price %>% 
   filter(country == "France") %>%
   group_by(region) %>%
   mutate(median = median(price),
          max = max(price),
          min = min(price))
summary(temp)
temp %>% ggplot(aes(median)) +
   geom_density(fill = "red", alpha = 0.2, bw = 0.05) +
   geom_density(aes(max), fill = "blue", alpha = 0.2, bw = 0.05) +
   scale_x_log10() +
   geom_density(aes(min), fill = "green", alpha = 0.2, bw = 0.05) +
   coord_flip() +
   xlab("Min - Median - Max Price") +
   ggtitle("Price distribution accross France regions")
# In france the median goes up to 231
temp %>% filter(median > 150) %>% .$region %>% unique()
# 3 regions have a median price above 150
temp %>% filter(min > 50) %>% .$region %>% unique()
# 5 regions have a min price above 50
temp %>% filter(max < 25) %>% .$region %>% unique()
# 4 regions have a max price below 25
#> We see that some countries have more heterogeneous prices across regions
rm(temp)

"Variety effect"
factor(wine_price$variety) %>% levels() %>% length()
# For visualization comfort purposes we randomly select 40 of the 104 varieties
seed(10)
selection = sample(unique(wine_price$variety), 40)
wine_price %>%
   filter(variety %in% selection) %>%
   group_by(variety) %>%
   mutate(median = median(price)) %>%
   ggplot(aes(reorder(variety, desc(median)), price, fill = reorder(variety, desc(median)))) +
   geom_boxplot(show.legend = F, outlier.size = 1, outlier.alpha = 0.8) +
   scale_y_log10() +
   theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
   xlab("") + ylab("Price") +
   ggtitle("Price distributions across varieties")
# We see less variability across varieties and we expect the effect to be minor

"Taster effect"
factor(wine_price$taster_name) %>% levels()
wine_price %>%
   group_by(taster_name) %>%
   mutate(max = max(price)) %>%
   ggplot(aes(reorder(taster_name, desc(max)), price, fill = reorder(taster_name, desc(max)))) +
   geom_boxplot(show.legend = F, outlier.size = 1, outlier.alpha = 0.8) +
   scale_y_log10() +
   theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
   xlab("") + ylab("Price") +
   ggtitle("Price distributions depending on taster name")
# We do not observe strong variability in medians and interquartiles but it seems
# that some taster allow for higher prices, maybe due to their reputation




# Checkpoint --------------------------------------------------------------

"We will divide our ML process in two steps
 1. Work with these variables : country/region, score, taster and variety
 2. Try to improve the resulting model by adding the description effect

For this second step we will need to cut sentences in words, delete stop words, ...
Each word will be a binary variable with value 1 if the word is in the description
We will proceed a feature selection through linear regression to remove low effect words
Finally we will add these variables to the final model produced in the first step"




# Add description ---------------------------------------------------------

load(file = "data/wine_price_2.RData")

descriptions = wine_price$description
head(descriptions)

descriptions = descriptions %>%
   str_remove_all("[\\.|\\,|\\;|\\:|\\(|\\)|\\?|\\!|\\-|\\'|\"]")
wine_price = wine_price %>%
   mutate(description = description %>%
             str_remove_all("[\\.|\\,|\\;|\\:|\\(|\\)|\\?|\\!|\\-|\\'|\"]"))

words = descriptions %>% str_split(" ") %>% unlist()
length(words)
# We have 3,449,502 words i.e. ~ 41 per description

# Remove stop words
data("stop_words", package = "tidytext")
words = words[!(words %in% stop_words$word)]
# 2,073,731 remaining words i.e. ~ 25 per description

# Keep distinct words and remove some issues
words = unique(words)
words = words[!(words %in% c("", "+", "“**”"))]
# 41,022 remaining 

# Remove problems
# freq = sapply(words, function(word){
#    str_detect(wine_price$description, word) %>%
#       sum()
# }) 
save(freq, file = "data/freq.RData")
load("data/freq.RData")
sum(freq == 0) 
# There are 48 issues
words = words[freq != 0]
save(words, file = "data/words.RData")



















