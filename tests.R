rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate")
lapply(libs, library, character.only = TRUE)
options(digits = 20)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")



load(file = "data/wine_price_2.RData")
wine_price = wine_price %>%
   mutate(country = factor(country),
          #points = factor(points),
          region = factor(region),
          variety = factor(variety),
          taster_name = factor(taster_name))
head(wine_price)

RMSE = function(actual, predicted) sqrt(mean((actual - predicted)^2))


load("data/words_reg.RData")
load("data/words.RData")
seed(66)
ind = createDataPartition(wine_price$price, p = 0.15, list = F)
wp_train = wine_price[-ind,]; validation = wine_price[ind,]
wp_train = wp_train %>%
   mutate(description = description %>%
             str_remove_all("[\\.|\\,|\\;|\\:|\\(|\\)|\\?|\\!|\\-|\\'|\"]"))
validation = validation %>%
   mutate(description = description %>%
             str_remove_all("[\\.|\\,|\\;|\\:|\\(|\\)|\\?|\\!|\\-|\\'|\"]"))

words_reg = mat %>% as.data.frame() %>%
   mutate(word = words) %>%
   filter(pvalue < 0.05) %>% 
   mutate(type = ifelse(beta >= 0, "+", "-")) %>%
   mutate(beta = abs(beta))

load("data/occurences1.RData")

words_reg = words_reg %>%
   mutate(occurences) %>%
   filter(occurences > 1000,
          !(word %in% colnames(wine_price))) %>%
   arrange(desc(beta)) %>% .[1:10,]
head(words_reg)

for(i in 1:nrow(words_reg)){
   wp_train = wp_train %>%
      mutate(ifelse(str_detect(description, words_reg$word[i]), 1, 0) %>%
                factor(levels = c(0,1)))
   validation = validation %>%
      mutate(ifelse(str_detect(description, words_reg$word[i]), 1, 0) %>%
                factor(levels = c(0,1)))
   colnames(wp_train)[i+7] = words_reg$word[i]
   colnames(validation)[i+7] = words_reg$word[i]
}

tree = train(log(price) ~ points + country + taster_name + impressive + velvet +
                powerful + beautiful,
             data = wp_train, method = "xyf",
             trControl = trainControl(method = "cv", number = 2))
RMSE(predict(tree, validation) %>% exp(), validation$price) 
 

