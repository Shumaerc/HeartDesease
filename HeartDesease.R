library(tidyverse)

hd <- read.csv("/Users/ShuLFO/Downloads/What Your Heart Rate Is Telling You/datasets/Cleveland_hd.csv")
View(hd)

hd <- hd %>% mutate(hd = ifelse(class > 0, 1,0), sex = factor(sex, levels = 0:1, labels = c("Female", "Male")))

hd_sex <- chisq.test(hd$hd, hd$sex)
hd_age <- t.test(hd$age ~ hd$hd)
hd_heartrate <- t.test(hd$thalach ~ hd$hd)

print(hd_sex)
print(hd_age)
print(hd_heartrate)

hd <- hd %>% mutate(hd_labelled = ifelse(hd == 1, "Disease", "Not Desease"))

hd %>% ggplot(aes(hd_labelled, age)) +
        geom_boxplot()

hd %>% ggplot(aes(hd_labelled, fill = sex)) +
        geom_bar(position = "fill") +
        labs(y = "Sex %")

hd %>% ggplot(aes(hd_labelled, thalach)) +
        geom_boxplot()


mod1 <- glm(hd ~ age + sex + thalach, data = hd, family = "binomial")
summary(mod1)

library(broom)
tidy_mod <- tidy(mod1)
mod1

tidy_mod$odds_ratio <- exp(tidy_mod$estimate)
tidy_mod$lower_CI <- exp(tidy_mod$estimate - 1.96 * tidy_mod$std.error)
tidy_mod$upper_CI <- exp(tidy_mod$estimate + 1.96 * tidy_mod$std.error)

tidy_mod


pred_prob <- predict(mod1, hd, type = "response")
hd$pred_hd <- ifelse(pred_prob >= 0.5, 1,0)

testdata <- data.frame(age = 45, sex = "Female", thalach = 150)
new_pred <- predict(mod1, testdata, type = "response")
new_pred

library(Metrics)

auc <- auc(hd$hd, hd$pred_hd)
accuracy <- accuracy(hd$hd, hd$pred_hd)
classification_error <- ce(hd$hd, hd$pred_hd)

table(hd$hd, hd$pred_hd, dnn = c("True Status", "Predicted Status"))

