install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
# Nesse script é feito a relação Európio/Oxigênio
y = rep(c(0,1),c(870,458))
z = rep(c(0,1),c(421,71))

dados<-read_excel("C:/TCC/Dados.xlsx", 
                  sheet=2, col_names=TRUE)
EUO0 = dados$EU_O[1:870]
EUO1 = dados$EU_O[871:length(dados$EU_O)]
EUO0_under = sample(EUO0, length(EUO1))
EUO1_over = sample(EUO1, length(EUO0), replace=TRUE)

#Undersampling:

under_set = tibble(y=0, EUO = EUO0_under)
under_set_temp = tibble(y=1, EUO = EUO1)
under_set = bind_rows(under_set, under_set_temp)

n_train_under = round(nrow(under_set)*0.8)
n_test_under = nrow(under_set) - n_train_under
idx_train = sample(1:nrow(under_set),n_train_under)
under_set_train = under_set[idx_train,]
under_set_test = under_set[-idx_train,]

ajuste1 = glm(y ~ EUO, family=binomial, data = under_set_train)
summary(ajuste1)

fitted_train = fitted(ajuste1, type="response")

labels_train = under_set_train$y

pred_train = prediction(fitted_train, labels_train)

acc.perf_train = performance(pred_train, measure = "acc")

plot(acc.perf_train)

predict_test = predict(ajuste1, under_set_test,  type="response")

pred_test_cut = predict_test > 0.4

labels_test = under_set_test$y

tabela = xtabs(~ pred_test_cut + labels_test)

#Sensitivity:
tabela[1,1]/sum(tabela[,1])

#Specificity:
tabela[2,2]/sum(tabela[,2])

#Accuracy (precisão):
sum(diag(tabela))/sum(tabela)

#Oversampling:

over_set = tibble(y=0, EUO = EUO0)
over_set_temp = tibble(y=1, EUO = EUO1_over)
over_set = bind_rows(over_set, over_set_temp)

n_train_over = round(nrow(over_set)*0.8)
n_test_over = nrow(over_set) - n_train_over
idx_train = sample(1:nrow(over_set),n_train_over)
over_set_train = over_set[idx_train,]
over_set_test = over_set[-idx_train,]

ajuste1_over = glm(y ~ EUO, family=binomial, data = over_set_train)
summary(ajuste1_over)

fitted_train = fitted(ajuste1_over, type="response")

labels_train = over_set_train$y

pred_train = prediction(fitted_train, labels_train)

acc.perf_train = performance(pred_train, measure = "acc")

plot(acc.perf_train)

predict_test = predict(ajuste1_over, over_set_test,  type="response")

pred_test_cut = predict_test > 0.4

labels_test = over_set_test$y

tabela = xtabs(~ pred_test_cut + labels_test)

#Sensitivity:
tabela[1,1]/sum(tabela[,1])

#Specificity:
tabela[2,2]/sum(tabela[,2])

#Accuracy (precisão):
sum(diag(tabela))/sum(tabela)




