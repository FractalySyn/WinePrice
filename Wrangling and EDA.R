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
apply(wine_price, 2, function(x) { factor(x) %>% levels() %>% length() })
# Remove designation and winery as there are too many levels and not much information

"NAs ?"
apply(wine_price, 2, function(x) { sum(is.na(x)) })
# Remove NA prices since it's the outcome
# We will assign "unspecified" to NA tasters, remove twitter id
# Remove NA variety, countries and province (<100 obs)
# We can find regions in the title

wine_price = wine_price %>%
   select(country, description, points, price, province, 
          region_1, taster_name, variety, title) %>%
   filter(!is.na(price), !is.na(variety), !is.na(country), !is.na(province)) %>%
   mutate(taster_name = ifelse(is.na(taster_name), "na", taster_name)) %>%
   rename(region = region_1)

"Find regions"
temp = wine_price %>% 
   select(region, title) %>%
   filter(is.na(region))
# Find the pattern
str_view(temp$title[1:20], "\\(([:print:]+)\\)")
# How good does it work ?
temp$title[!str_detect(temp$title, "\\(([:print:]+)\\)")]
temp = temp[str_detect(temp$title, "\\(([:print:]+)\\)"), ]
# Assign regions
temp$region = str_extract(temp$title, "\\(([:print:]+)\\)") %>%
   str_remove_all("\\(|\\)")
# On the data now
wine_price = wine_price[which(str_detect(temp$title, "\\(([:print:]+)\\)")),]
wine_price$region = temp$region
#
rm(temp)
wine_price = select(wine_price, -title)



# Explore data ------------------------------------------------------------

head(wine_price)








