#Biblioteci
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich","tseries", 
                  "olsrr", "moments","whitestrap","ggplot2","DataCombine","car", "vctrs",
                  "splines","mgcv","glmnet","psych")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
library(readxl)
library(tidyverse)
library(magrittr)
library(car)
library(FinTS)
library(moments)
library(whitestrap)
library(olsrr)
library(lmtest)
library(sandwich)
library(tseries)
library(mltools)
library(MLmetrics)
library(caret)
library(DataCombine)
library(vctrs)
library(Boruta)

#Importarea datelor
setwd("G:/Other computers/ASUS/An 3 sem 1/Econometrie E/Seminar/Proiect/Script")
data <- read.csv("Dataset.csv")
#Vizualizarea datelor
View(data)
#Numele variabilelor
names(data)

#Descriptive statistics
data %>% stargazer(type = "text")

#Crearea variabilei dummy pt scor
#1 pt scor de 4 sau 5 si 0 pt scor de 1, 2 sau 3
data$Rating <- ifelse(data$Scor > 3, 1, 0)
View(data)

#Simplificarea pretului cu 1000
data <- data %>% mutate(Pret = Pret / 1000)
View(data)

#Grafice pt variabilele independente
labels = c(1, 2, 3, 4, 5)
barplot(table(data$Scor), main="Scor", names.arg = labels, xlab = "Scor", ylab = "Case")

labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
barplot(table(data$NrCamere), main="NrCamere", names.arg = labels, xlab = "NrCamere", ylab = "Case")

labels = c(1, 2, 3, 4, 5, 6)
barplot(table(data$Sector), main="Sector", names.arg = labels, xlab = "Sectoare", ylab = "Case")

labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 24)
barplot(table(data$TotalEtaje), main="Total etaje", names.arg = labels, xlab = "Numar Etaje", ylab = "Case")

labels = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 20)
barplot(table(data$TotalEtaje), main="Etaj", names.arg = labels, xlab = "Etaje", ylab = "Etajele caselor puse la vanzare")

#Regresia simpla pt fiecare variabila

#Model de regresie simpla pt etaj
modelFloor <- lm(Pret ~ Etaj, data)
summary(modelFloor)

ggplot(data = data, mapping = aes(x = Etaj, y = Pret)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Model de regresie simpla pt total etaje
modelTotalFloor <- lm(Pret ~ TotalEtaje, data)
summary(modelTotalFloor)

ggplot(data = data, mapping = aes(x = TotalEtaje, y = Pret)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Model de regresie simpla pt sector
modelDistrict <- lm(Pret ~ Sector, data)
summary(modelDistrict)

ggplot(data = data, mapping = aes(x = Sector, y = Pret)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Model de regresie simpla pt nr camere
modelNoRooms <- lm(Pret ~ NrCamere, data)
summary(modelNoRooms)

ggplot(data = data, mapping = aes(x = NrCamere, y = Pret)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Model de regresie simpla pt rating
modelRating <- lm(Pret ~ Rating, data)
summary(modelRating)

ggplot(data = data, mapping = aes(x = Rating, y = Pret)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Cel mai semnificativ
#Model de regresie simpla pt suprafata
modelSurface <- lm(Pret ~ Suprafata, data)
summary(modelSurface)

#Dreapta de regresie
plot(x = data$Suprafata, y = data$Pret)
abline(a = modelSurface$coefficients['(Intercept)'], 
       b = modelSurface$coefficients['Suprafata'],
       col = 'red')

ggplot(data = data, mapping = aes(x = Suprafata, y = Pret)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Filtrarea datelor
data <- data %>%
  filter(Suprafata < 115 & Suprafata > 40 & Pret < 150 & Pret > 30)

#Ipoteze pe reziduuri
#Homoscedasticitate
model <- lm(Pret ~ Suprafata, data)
data %<>% mutate(uhat = resid(model),
                   yhat = fitted(model)) 
ggplot(data, aes(x=uhat, y=yhat)) + 
  geom_boxplot() +
  theme_bw()+
  xlab('Reziduuri') + 
  ylab('Pret') +
  ggtitle('Boxplot reziduuri') + 
  theme(plot.title = element_text(hjust = 0.5))
plot(model) #1 
plot(model) #2

bptest(model)
white_test(model)

data <- data %>% mutate(lPret = log(Pret), lSuprafata = log(Suprafata))

#log-log
model_loglog <- lm(lPret ~ lSuprafata, data)
data %<>% mutate(uhat = resid(model_loglog),
                 yhat = fitted(model_loglog)) #residuals
summary(model_loglog)
bptest(model_loglog) #0.9005
white_test(model_loglog) #2949

#Non-autocorelare
acf(model_loglog$residuals)
dwtest(model_loglog) #0.1788
bgtest(model_loglog) #0.3779
bgtest(model_loglog, order = 2) #0.08544
bgtest(model_loglog, order = 3) #0.1748

#Normalitate
ggplot(data = data) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

shapiro.test(data$uhat) 
jarque.bera.test(data$uhat)
ols_test_normality(model_loglog) 

ols_plot_cooksd_bar(model_loglog)
data_cook <- data[-c(1, 1947, 1886, 2566, 1777, 2250, 1313, 1956, 1944, 1593, 1395, 1591, 2392, 2105,
                     1565, 1100, 1674, 987, 414, 1423, 1766, 1934, 985, 2235, 1559, 413, 759,
                     378, 1421, 1940, 1925, 1934, 2015, 2223, 1554, 1759,
                     1924, 2222, 981, 1662, 1755, 1552, 2216, 1753, 1926,
                     1752, 980, 1915, 2211, 756, 1749, 987, 1090, 411, 977,
                     754, 1300, 1908, 2203, 410, 1746, 1572, 1547,
                     1900, 974, 1085, 1566, 1739, 1539, 1083, 1735, 1895, 2189,
                     973, 752, 1732, 2184, 1537, 1561, 1727, 1886, 2178,
                     971, 409, 1557, 1533, 1881, 1529, 1553, 1401, 1970, 
                     1877, 1526, 1550, 1548, 2312, 2413, 1873, 1716, 1523, 2163,
                     1870, 969), ]
model_cook <- lm(lPret ~ lSuprafata, data_cook) 
summary(model_cook)
data_cook %<>% mutate(uhat_cook = resid(model_cook)) 
jarque.bera.test(data_cook$uhat)
shapiro.test(data_cook$uhat) 

ols_plot_cooksd_bar(model_cook) 

#Prognoza regresiei simple
set.seed(123)
training.samples <- data_cook$lPret %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data_cook[training.samples, ]
test.data <- data_cook[-training.samples, ]

model_forecast <- lm(lPret ~ lSuprafata, data = data_cook) 
summary(model_forecast)

model_train <- lm(lPret ~ lSuprafata, data = train.data) 
summary(model_train)

y_pred <- predict(model_train, newdata = test.data)
y_pred

RMSE(y_pred, test.data$lPret)

MAE(y_pred, test.data$lPret)

mse(y_pred, test.data$lPret)

MAPE(y_pred, test.data$lPret)

out_of_sample <- data.frame(Suprafata = c(50,60,90))

out_of_sample_log <- out_of_sample %>%
  mutate(lSuprafata = log(Suprafata))

out_of_sample_log <- out_of_sample_log %>%
  select(-Suprafata)

y_pred_outsample <- predict(model_forecast, newdata = out_of_sample_log)
y_pred_outsample
exp(y_pred_outsample)

#Regresia multipla
#Cel mai semnificativ model
model_rm <- lm(Pret ~ Suprafata + TotalEtaje + Etaj, data_cook) 
#Ipoteza 1 - este modelul liniar in parametri?
summary(model_rm)
data_cook %<>% mutate(uhat = resid(model_rm)) 

#Ipoteza 2 - nr de observatii > nr de variabile independente
nobs(model_rm) > (model_rm$rank - 1) #TRUE

#Ipoteza 3 - variabilitatea in x este pozitiva
var(data_cook$Suprafata) #219.9197
var(data_cook$Etaj) #7.497803
var(data_cook$TotalEtaje) #9.85714

#Ipoteza 4 - media reziduurilor este 0
mean(model_rm$residuals) #6.43988e-16

#Ipoteza 5 - multicoliniaritate
vif(model_rm) #1.005996   1.422534   1.427903 

#Ipoteza 6 - reziduurile nu sunt corelate cu variabilele independente
cor.test(data_cook$Suprafata, model_rm$residuals) 
cor.test(data_cook$Etaj, model_rm$residuals)
cor.test(data_cook$TotalEtaje, model_rm$residuals) 

data_cook <- data_cook %>% 
    filter(TotalEtaje < 10)
model_rm <- lm(Pret ~ Suprafata + Etaj + TotalEtaje, data_cook) 
data_cook %<>% mutate(uhat = resid(model_rm)) 

#Ipoteza 7 - Homoscedasticitate
bptest(model_rm) 
white_test(model_rm) 
coeftest(model_rm, vcov. = vcovHC(model_rm, type = "HC1"))

#Correcting the model => log
data_cook <- data_cook %>% mutate(lPret = log(Pret), lSuprafata = log(Suprafata),
                                  lTotalEtaje = log(TotalEtaje))

model_rm_log <- lm(lPret ~ Suprafata + Etaj + lTotalEtaje, data_cook)
data_cook %<>% mutate(uhat = resid(model_rm_log),
                      yhat = fitted(model_rm_log)) 
bptest(model_rm_log) #0.4013
white_test(model_rm_log) #0.493376
coeftest(model_rm_log, vcov. = vcovHC(model_rm_log, type = "HC1"))

#Ipoteza 8 - autocorelarea erorilor
acf(model_rm_log$residuals)
dwtest(model_rm_log) #0.2429
bgtest(model_rm_log) #0.5127
bgtest(model_rm_log, order = 2) #0.3247
bgtest(model_rm_log, order = 3) #0.5021

#Ipoteza 9 - normalitate
shapiro.test(data_cook$uhat) 
jarque.bera.test(data_cook$uhat)
ols_test_normality(model_rm_log) 

#Corectie
ols_plot_cooksd_bar(model_rm_log)
data_cook2 <- data_cook[-c(4, 28, 108, 157, 251, 205, 319, 344, 381, 454, 487, 537, 552, 584, 591, 609, 644, 657, 668, 758, 836, 905, 
                           962, 965, 993, 1027, 1047, 1080, 1085, 1089, 1146, 1161, 1183, 1256, 1291, 1309, 1328, 1399, 1437, 1440, 1444, 
                           1451, 1488, 1509, 1573, 1612, 1633, 1636, 1639, 1641, 1649, 1653, 1672, 1763, 1780, 1835, 1855, 1922), ]
model_cook_corectat <- lm(lPret ~ Suprafata + lTotalEtaje + Etaj, data_cook2) 
data_cook2 %<>% mutate(uhat_cook2 = resid(model_cook_corectat)) 

jarque.bera.test(data_cook2$uhat_cook2)
shapiro.test(data_cook2$uhat_cook2) 
ols_plot_cooksd_bar(model_cook_corectat)
summary(model_cook_corectat)

#Model de regresie multipla cu dummy si termen de interactiune
data_cook2 %<>% mutate(RatingXEtaj = Rating*Etaj,
                       RatingXSupr = Rating*Suprafata)

model_dummy_inter <- lm(lPret ~ Suprafata + lTotalEtaje + Etaj + Rating + RatingXSupr, data_cook2)
summary(model_dummy_inter)

#Ridge
y <- data_cook2$lPret
x <- data.matrix(data_cook2[, c('Suprafata', 'lTotalEtaje', 'Etaj')])

model_ridge <- glmnet(x, y, alpha = 0)
summary(model_ridge)

cv_model_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model_ridge$lambda.min
best_lambda #0.01968706

plot(cv_model_ridge) 

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)  

y_predicted <- predict(model_ridge, s = best_lambda, newx = x)

new <- matrix(c(80, 2, 4), nrow = 1, ncol = 3) 
predict(best_model, s = best_lambda, newx = new) #4.555463

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq #0.3527704

#Lasso
model_lasso <- glmnet(x, y, alpha = 1)
summary(model_lasso)

cv_model_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model_lasso$lambda.min
best_lambda #0.0008927843

plot(cv_model_lasso) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

y_predicted <- predict(best_model, s = best_lambda, newx = x)

new <- matrix(c(80, 2, 4), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new) #4.564707

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq #0.3537956

#Elastic Net
model_en <- cv.glmnet(x, y, alpha = 0.5)
cv_model_en <- cv.glmnet(x, y, alpha = 0.5)

best_lambda <- cv_model_en$lambda.min
best_lambda #0.001350717

plot(cv_model_en) 

best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model)

y_predicted <- predict(best_model, s = best_lambda, newx = x)

new <- matrix(c(80, 2, 4), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new) #4.564732

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq #0.353796

#Boruta
set.seed(111)
data_boruta <- data_cook2[,c("lPret", "Etaj", "Sector", "NrCamere", "Rating",
                             "Suprafata", "lTotalEtaje", "RatingXEtaj", "RatingXSupr")]
View(data_boruta)
boruta.bank_train <- Boruta(lPret~., data = data_boruta, doTrace = 2)
print(boruta.bank_train)

getSelectedAttributes(boruta.bank_train, withTentative = T)

model_boruta <- lm(lPret ~ Etaj + Sector + NrCamere + Rating + Suprafata + lTotalEtaje + RatingXSupr + RatingXEtaj, data_cook2)
summary(model_boruta) 

#Prognoza finala pe modelul optim de regresie multipla
set.seed(123)
training.samples <- data_cook2$lPret %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data_cook[training.samples, ]
test.data <- data_cook[-training.samples, ]

model_forecast2 <- lm(lPret ~ Suprafata + Etaj + lTotalEtaje, data = data_cook2) 
summary(model_forecast2)

model_train2 <- lm(lPret ~ Suprafata + Etaj + lTotalEtaje, data = train.data) 
summary(model_train2)

y_pred <- predict(model_train2, newdata = test.data)
y_pred

RMSE(y_pred, test.data$lPret)

MAE(y_pred, test.data$lPret)

mse(y_pred, test.data$lPret)

MAPE(y_pred, test.data$lPret)

out_of_sample <- data.frame(Suprafata = c(50,60,90), Etaj = c(0,4,8), TotalEtaje = c(4,6,9))
out_of_sample_log <- out_of_sample %>%
  mutate(lTotalEtaje = log(TotalEtaje))

out_of_sample_log <- out_of_sample_log %>%
  select(-TotalEtaje)

y_pred_outsample <- predict(model_forecast2, newdata = out_of_sample_log)
y_pred_outsample
exp(y_pred_outsample)

y_pred_outsample <- predict(model_forecast2, newdata = out_of_sample_log, interval = 'confidence')
y_pred_outsample
exp(y_pred_outsample)