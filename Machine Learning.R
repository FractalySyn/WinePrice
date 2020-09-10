rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate")
lapply(libs, library, character.only = TRUE)
options(digits = 4)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")



load(file = "data/wine_price_2.RData")
wine_price = wine_price %>%
   mutate(country = factor(country),
          #points = factor(points),
          region = factor(region),
          variety = factor(variety),
          taster_name = factor(taster_name))

RMSE = function(actual, predicted) sqrt(mean((actual - predicted)^2))



# Checkpoint --------------------------------------------------------------

"We will divide our ML process in two steps
 1. Work with these variables : country/region, score, taster and variety
 2. Try to improve the resulting model by adding the description effect

For this second step we will need to cut sentences in words, delete stop words, ...
Each word will be a binary variable with value 1 if the word is in the description
We will proceed a feature selection through linear regression to remove low effect words
Finally we will add these variables to the final model produced in the first step

Because of the categorical variables (excepted score which is discrete), we will focus
on decision trees"




# First Step : simple variables -------------------------------------------

seed(66)
ind = createDataPartition(wine_price$price, p = 0.15, list = F)
wp_train = wine_price[-ind,]; validation = wine_price[ind,]
validation = validation %>% filter(region %in% wp_train$region)

"Subset for tests"
seed(10)
wp_train = wp_train[createDataPartition(wp_train$price, p = 0.1, list = F),] %>%
   .[, -2]


"We first take a look at LM and Trees"

"LM : first look at effects"
lm1 = train(log(price) ~ points, method = "lm", data = wp_train)
summary(lm1); lm1$results
# explains only 40% of the variabiliy
# one point increase may improve the price by an avg of $1.15
lm2 = train(log(price) ~ points + country, method = "lm", data = wp_train)
summary(lm2); lm2$results
# the country have a strong impact as this regression reveals the heterogeneity across countries
lm3 = train(log(price) ~ points + variety, method = "lm", data = wp_train)
summary(lm3); lm3$results
# many varieties seem to have low to no impact
lm4 = train(log(price) ~ points + taster_name, method = "lm", data = wp_train)
summary(lm4); lm4$results
# tasters have overall negative effects on price, they vary a lot

"Decision trees"
tree = train(log(price) ~ points, data = wp_train, method = "rpart",
             trControl = trainControl(method = "cv", number = 5),
             tuneGrid = data.frame(cp = 0.001))
tree$results
# It converges to the results of lm1
# score + country
tree = train(log(price) ~ points + country, data = wp_train, method = "rpart",
             trControl = trainControl(method = "cv", number = 5),
             tuneGrid = data.frame(cp = 0.001))
tree$results
# score + country + region + variety + taster
tree = train(log(price) ~ points + country + region + variety + taster_name, 
             data = wp_train, method = "rpart",
             trControl = trainControl(method = "cv", number = 5),
             tuneGrid = data.frame(cp = 0.001))
tree$results
# 0.4518


"ML training : we selected 5 ML models on tests that we don't show here, criteria
were RMSe and computation time. We'll build an ensemble
We do not show parameter tuning as well for the same reason
!!!!! comment all this section and tune parameters to show it in the report, don't run it"
# # LM 0.4193
# lm_train = train(log(price) ~ points + country + region + variety + taster_name, 
#                  data = wp_train, method = "lm",
#                  trControl = trainControl(method = "cv", number = 5))
# lm_train$results
# # Rpart 0.4545
# rpart_train = train(log(price) ~ points + country + region + variety + taster_name, 
#                     data = wp_train, method = "rpart",
#                     trControl = trainControl(method = "cv", number = 5),
#                     tuneGrid = data.frame(cp = 0.001))
# rpart_train$results
# # ctree 0.4585
# ctree_train = train(log(price) ~ points + country + region + variety + taster_name, 
#                     data = wp_train, method = "ctree",
#                     trControl = trainControl(method = "cv", number = 5),
#                     tuneGrid = data.frame(mincriterion = 0.01))
# ctree_train$results
# # gcvEarth 0.4323
# earth_train = train(log(price) ~ points + country + region + variety + taster_name, 
#                     data = wp_train, method = "gcvEarth",
#                     trControl = trainControl(method = "cv", number = 5),
#                     tuneGrid = data.frame(mincriterion = 0.01))
# earth_train$results
# # gbm 0.4
# gbm_train = train(log(price) ~ points + country + region + variety + taster_name, 
#                   data = wp_train, method = "gbm",
#                   trControl = trainControl(method = "cv", number = 5),
#                   tuneGrid = data.frame(shrinkage = 0.25,
#                                         interaction.depth = 8,
#                                         n.minobsinnode = 5,
#                                         n.trees = 150))
# gbm_train$results


"Train models"
# seed(66)
# wp_train = wine_price[-ind,]; validation = wine_price[ind,]
# validation = validation %>% filter(region %in% wp_train$region)
# 
# lm_fit = lm(log(price) ~ points + country + region + variety + taster_name,
#             data = wp_train)
# metrics = data.frame(Model = "lm",
#                      RMSE = RMSE(predict(lm_fit, validation) %>% exp(), validation$price))
# tree_fit = rpart(log(price) ~ points + country + region + variety + taster_name,
#                  data = wp_train, cp = 0.001)
# metrics = rbind(metrics, c("tree",
#                            RMSE(predict(tree_fit, validation) %>% exp(), validation$price)))
# ctree_fit = party::ctree(log(price) ~ points + country + region + variety + taster_name,
#                          data = wp_train, controls = party::ctree_control(mincriterion = 0.01))
# metrics = rbind(metrics, c("ctree",
#                            RMSE(predict(ctree_fit, validation) %>% exp(), validation$price)))
# earth_fit = earth::earth(log(price) ~ points + country + region + variety + taster_name,
#                          data = wp_train)
# metrics = rbind(metrics, c("earth",
#                            RMSE(predict(earth_fit, validation) %>% exp(), validation$price)))
# gbm_fit = gbm::gbm(log(price) ~ points + country + region + variety + taster_name,
#                    data = wp_train, n.trees = 150,
#                    shrinkage = 0.25, interaction.depth = 8, n.minobsinnode = 5)
# metrics = rbind(metrics, c("gbm",
#                            RMSE(predict(gbm_fit, validation) %>% exp(), validation$price)))
# metrics[,2] = metrics[,2] %>% as.numeric()
# save(metrics, file = "data/metrics.RData")
load("data/metrics.RData")
metrics %>% knitr::kable(align = "c")


"Ensembles (arithmetic and weighted)"
load("data/lm.RData"); load("data/tree.RData"); load("data/ctree.RData")
load("data/earth.RData"); load("data/gbm.RData")
predictions = data.frame(lm, tree, ctree, earth, gbm)
# ensemble 1
ens_arit = predictions %>% as.matrix() %>%
   rowMeans()
RMSE(ens_arit, validation$price)
# ensemble 2 
weights = 1/metrics$RMSE^10
ens_weighted = apply(predictions %>% as.matrix, 1, function(x){
   (x %*% weights %>% t()) / sum(weights)
})
RMSE(ens_weighted, validation$price)
metrics = rbind(metrics,
                c("Naive Ens", RMSE(ens_arit, validation$price)),
                c("Weighted Ens", RMSE(ens_weighted, validation$price)))
metrics[,2] = metrics[,2] %>% as.numeric()
save(metrics, file = "data/metrics2.RData")
load("data/metrics2.RData")
metrics %>% knitr::kable(align = "c")
# gbm remains the best


"Analyze errors"
errors = sqrt((ens_weighted - validation$price)^2)
data.frame(errors) %>%
   ggplot(aes(1:length(errors), errors)) + 
   geom_point(alpha = 0.5, size = 0.1) +
   xlab("") + scale_y_continuous(trans = "log2") +
   geom_hline(yintercept = mean(errors), color = "red", lwd = 1.5) +
   #geom_hline(yintercept = rmse_tree*2, color = "blue", lwd = 1.5) +
   ggthemes::theme_clean()
# However the range of errors is huge
errors %>% data.frame() %>%
   ggplot(aes(y = errors)) +
   geom_boxplot() +
   scale_y_continuous(trans = "log2")
# Errors have a mean of 11.5$, which is great but make the RMSE relatively high
mean(errors > 11.5)
# Only 26% of errors above the mean -> the RMSE is high because of outliers
mean(errors > 41); mean(errors > 100)
# Very few large errors so these outliers are huge
errors[errors > 100] %>% plot(main = "Outliers' errors")


"What we can conclude at this point is that our model isn't able to predict unusual high prices"




# Analyze words in description --------------------------------------------

seed(66)
ind = createDataPartition(wine_price$price, p = 0.15, list = F)
wp_train = wine_price[-ind,]; validation = wine_price[ind,]
wp_train = wp_train %>%
   mutate(description = description %>%
             str_remove_all("[\\.|\\,|\\;|\\:|\\(|\\)|\\?|\\!|\\-|\\'|\"]"))

"What are the important words ?"
# Remove words that are not in the training set
# index = logical(length(words))
# for(i in 1:length(words)){
#    print(i)
#    sum = wp_train %>%
#       mutate(word = ifelse(str_detect(description, words[i]), 1, 0)) %>%
#       .$word %>% sum()
#    index[i] = (sum != 0)
# }
# words = words[index]
# save(words, file = "data/words.RData")
load("data/words.RData")

# Words effects
# mat = matrix(0, length(words), 2)
# for(i in 1:length(words)) {
#    lm = (wp_train %>%
#             mutate(word = ifelse(str_detect(description, words[i]), 1, 0)) %>%
#             lm(price ~ word, data = .) %>%
#             summary())[[4]][2,]
#    mat[i,] = c(lm[1], lm[4])
# }
# colnames(mat) = c("beta", "pvalue")
# mat[,1] = round(mat[,1], 3); mat[,2] = round(mat[,2], 3)
# head(mat)
# save(mat, file = "data/words_reg.RData")
load("data/words_reg.RData")

# Impactful words
mat %>% as.data.frame() %>%
   mutate(word = words) %>%
   filter(pvalue < 0.05) %>%
   mutate(type = ifelse(beta >= 0, "+", "-")) %>%
   mutate(beta = abs(beta)) %>%
   arrange(desc(beta)) %>%
   .$word %>% .[1:20]
"However we think these words won't be repeated enough in the test set"





# Step 2 : ML with description --------------------------------------------

load("data/words_reg.RData")
load("data/words.RData")
seed(66)
ind = createDataPartition(wine_price$price, p = 0.15, list = F)
wp_train = wine_price[-ind,]; validation = wine_price[ind,]
validation = validation %>% filter(region %in% wp_train$region)

wp_train = wp_train %>%
   mutate(description = description %>%
             str_remove_all("[\\.|\\,|\\;|\\:|\\(|\\)|\\?|\\!|\\-|\\'|\"]"))
validation = validation %>%
   mutate(description = description %>%
             str_remove_all("[\\.|\\,|\\;|\\:|\\(|\\)|\\?|\\!|\\-|\\'|\"]"))


"Keep first 3000 words sorted by impact and filtered for >10 occurences in descriptions"
words_reg = mat %>% as.data.frame() %>%
   mutate(word = words) %>%
   filter(pvalue < 0.05) %>% 
   mutate(type = ifelse(beta >= 0, "+", "-")) %>%
   mutate(beta = abs(beta))
# occurences = numeric(nrow(words_reg))
# for(i in 1:nrow(words_reg)){
#    occurences[i] = wp_train %>%
#       mutate(word = ifelse(str_detect(description, words_reg$word[i]), 1, 0)) %>%
#       .$word %>% sum()
# }
# save(occurences, file = "data/occurences1.RData")
load("data/occurences1.RData")

words_reg = words_reg %>%
   mutate(occurences) %>%
   filter(occurences > 10,
          !(word %in% colnames(wine_price))) %>%
   arrange(desc(beta)) %>% 
   .$word %>% .[1:3000]

"Make 100 groups of 30 words - impact clusters"
words_groups = split(words_reg, sapply(1:100, function(x){rep(x, 30)}))
patterns = c()
for(i in 1:length(words_groups)) {
   patterns[i] = paste0(words_groups[[i]], collapse = "|")
}

for(i in 1:length(patterns)){
   wp_train = wp_train %>%
      mutate(ifelse(str_detect(description, patterns[i]), 1, 0) %>%
                factor(levels = c(0,1)))
   validation = validation %>%
      mutate(ifelse(str_detect(description, patterns[i]), 1, 0) %>%
                factor(levels = c(0,1)))
   colnames(wp_train)[i+7] = paste0("pattern", as.character(i))
   colnames(validation)[i+7] = paste0("pattern", as.character(i))
}
wp_train = wp_train[, -2]; validation = validation[, -2] # remove description column
save(wp_train, file = "data/wp_train.RData"); save(validation, file = "data/validation.RData")

"ML training"
load("data/validation.RData"); load("data/wp_train.RData")
# lm_fit = lm(log(price) ~ ., data = wp_train)
# metrics2 = data.frame(Model = "lm",
#                      RMSE = RMSE(predict(lm_fit, validation) %>% exp(), validation$price))
# tree_fit = rpart(log(price) ~ ., data = wp_train, cp = 0.001)
# metrics2 = rbind(metrics2, c("tree",
#                            RMSE(predict(tree_fit, validation) %>% exp(), validation$price)))
# ctree_fit = party::ctree(log(price) ~ ., data = wp_train,
#                          controls = party::ctree_control(mincriterion = 0.01))
# metrics2 = rbind(metrics2, c("ctree",
#                            RMSE(predict(ctree_fit, validation) %>% exp(), validation$price)))
# earth_fit = earth::earth(log(price) ~ ., data = wp_train)
# metrics2 = rbind(metrics2, c("earth",
#                            RMSE(predict(earth_fit, validation) %>% exp(), validation$price)))
# gbm_fit = gbm::gbm(log(price) ~ ., data = wp_train, n.trees = 150,
#                    shrinkage = 0.25, interaction.depth = 8, n.minobsinnode = 5)
# metrics2 = rbind(metrics2, c("gbm",
#                            RMSE(predict(gbm_fit, validation) %>% exp(), validation$price)))
# metrics2[,2] = metrics2[,2] %>% as.numeric()
# save(metrics2, file = "data/metrics3.RData")
load("data/metrics3.RData")
metrics2 %>% knitr::kable(align = "c")


"Ensembles"
load("data/lm2.RData"); load("data/tree2.RData"); load("data/ctree2.RData")
load("data/earth2.RData"); load("data/gbm2.RData")
predictions = data.frame(exp(lm), exp(tree), exp(ctree), exp(earth), exp(gbm))
# ensemble 1
ens_arit2 = predictions %>% as.matrix() %>%
   rowMeans()
# ensemble 2 
weights = 1/metrics2$RMSE^10
ens_weighted2 = apply(predictions %>% as.matrix, 1, function(x){
   (x %*% weights %>% t()) / sum(weights)
})
metrics2 = rbind(metrics2,
                c("Naive Ens", RMSE(ens_arit2, validation$price)),
                c("Weighted Ens", RMSE(ens_weighted2, validation$price)))
metrics2[,2] = metrics2[,2] %>% as.numeric() %>% round(4)
save(metrics2, file = "data/metrics4.RData")
load("data/metrics4.RData")
metrics2 %>% knitr::kable(align = "c")
# gbm remains the best

data.frame(Model = metrics2$Model, No.words = metrics$RMSE,
           With.words = metrics2$RMSE) %>% knitr::kable(align = "c")













