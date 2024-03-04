---
title: "Linear Discriminant Analysis"
author: "Fikri Septrian Anggara"
date: "6/16/2021"
output:
  word_document: default
  html_document: default
---

```{r}
library(readr)
library(mvnormtest)
library(rstatix)
library(MASS)
library(caret)
library(dplyr)
datapert11 <- read_delim("datapert11.txt", 
    ";", escape_double = FALSE, trim_ws = TRUE)
head(datapert11)
```

```{r}
# melihat struktur data dan mengubah atribut kelas menjadi faktor lalu mengambil banyak domain kelas dan banyak sampel
str(datapert11)
datapert11$kelas<-as.factor(datapert11$kelas)
summary(datapert11)

k<-length(levels(datapert11$kelas))
n<-dim(datapert11)[1]
```

```{r}
# uji asumsi

#uji multivariate nornmal
datapert11 %>%  group_by(kelas) %>% summarise(mshapiro_test(data.frame(y1, y2, y3, y4)))

# uji kesamaan matriks kovarians
box_m(datapert11[,1:4], datapert11$kelas)
```
interpretasi : setelah dilakukan uji multivariat normalitas dan kesamaan varians, dengan signifikansi 0,05 persen, terbukti secara statistik bahwa data berdistribusi multivariat normal dan memiliki covarians yang homogen.

```{r}
# split data
set.seed(123)
sampel <- sample(2, n, replace = T, prob = c(0.8,0.2))
trainingdat <- datapert11[sampel==1, ]
testingdat <- datapert11[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdat), "| Jumlah Test Data: ", nrow(testingdat)))
```
```{r}
#  membuat model analisis diskriminan
model<-lda(kelas~.,data=trainingdat)
model
```
```{r}
#koefisien fungsi diskriminan
model$scaling[,1]

#pairwaise t test
grouped<-datapert11 %>% gather("variabel", "value", y1, y2, y3, y4) %>% group_by(variabel)
grouped %>% pairwise_t_test(value~kelas, pool.sd = FALSE, var.equal = TRUE, p.adjust.method = "bonferroni")
```
interpretasi : 
berdasarkan model yang dibentuk didapat satu fungsi diskriminan, yang koefisiennya yaitu -0.07(y1), 0.03(y2), 0.04(y3), 0.03(y4).
dari hasil pairwise t test, didapatkan bahwa semua variabel memiliki p-value yang signifikan, sehingga dapat diambil kesimpulan bahwa semua variabel mampu membedakan populasi kelas 1 dan 2. 
