---
title: "Univariate Manova"
author: "Fikri Septrian Anggara"
date: "6/13/2021"
output:
  html_document: default
  word_document: default
---

### Berdasarkan data jenis tanah yang digunakan untuk menanam jagung, tenukan apakah terdapat perbedaan yang signifikan antara jenis tanah yang digunakan berdasarkan hasil panen, jumlah air yang diperlukan, dan herbisida yang diperlukan. jika terdapat perbedaan efek jenis tanah, tentukan variabel mana yang menyebabkan perbedaan tersebut.
```{r}
library("biotools")
library("mvnormtest")
library("dplyr")
library("tidyr")
library("rstatix")
library(readr)
datapert5 <- read_delim("datapert5.txt", 
    ";", escape_double = FALSE, trim_ws = TRUE)
head(datapert5)
```

```{r}
str(datapert5)
datapert5$jtanah <- as.factor(datapert5$jtanah)

```

```{r}
#uji univariate normal
datapert5 %>% group_by(jtanah) %>% shapiro_test(yield, water, herbicide)

#uji multivariate nornmal
datapert5 %>%  group_by(jtanah) %>% summarise(mshapiro_test(data.frame(yield, water, herbicide)))
```
dari uji normal setiap variabel, terlihat bahwa setiap variabel berdistribusi normal, sedangkan pada uji multivariate normal, jenist tanah clay tidak berdistribusi multivariat normal. namun analisis tetap dilanjutkan.

```{r}
# uji kesamaan matriks kovarians
box_m(datapert5[,2:4], datapert5$jtanah)
```
karena p-value lebih besar dari alfa 0,05, maka matriks kovarians antar variat adalah sama.

```{r}
#uji manova
mvtes<-manova(cbind(yield, water, herbicide)~jtanah, data=datapert5)
summary(mvtes)
```
karenaa p value lebih kecil dari alfa 0.05, maka terdapat perbedaan vektor rata-rata antar jenis tanah. selanjutnya dilakukan uji posthoc untuk jenis 

```{r}
# uji posthoc dengan pairwaise t test
newAlfa<-0.05/(2*3)
newAlfa
grouped<-datapert5 %>% gather("variabel", "value", yield, water, herbicide) %>% group_by(variabel)
grouped %>% pairwise_t_test(value~jtanah, pool.sd = FALSE, var.equal = TRUE, p.adjust.method = "bonferroni")

```
dari hasil fungsi pariwaise_t_test(), tidak ada variabel yang menyebabkan kelompok jenis tanah berbeda.