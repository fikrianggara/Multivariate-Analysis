---
title: "Principal Component Anaysis"
author: "Fikri Septrian Anggara 221810306 3SI1"
date: "5/1/2021"
output:
  html_document: default
  word_document: default
---
soal :
  berdasarkan data, lakukan analisis komponen utama, gunakan matriks kovarians dan korelasi, manakah yang paling cocok digunakan, tunjukan proporsi varians yang dijelaskan, berdasarkan eigen value atau screeplot, tentukan komponen utama mana yang dipertahankan.
```{r message=FALSE, warning=FALSE}
# load library dan data
library(tidyverse)
library(readr)
data8 <- read_delim("data8", 
    ";", escape_double = FALSE, trim_ws = TRUE)
head(data8)
data8<-as.data.frame(data8)
data8<-data8[,-1]
head(data8)

```

```{r}
# membuat fungsi PCA berdasarkan matriks kovarians atau korelasi, dan unscaled atau scaled data
principal_component<-function(data=data.frame(), cov=T, scale=T){
  
  # load dua package untuk akses variabel dan membuat screeplot
  if(!require(ggplot2)) install.packages("ggplot2")
  if(!require(dplyr)) install.packages("dplyr")
  
  data<-data
  matrix<-data.frame()
  variabel<-colnames(data8)
  
  # pengecekan apakah ingin menggunakan matriks kovarians atau matriks korelasi, jika menggunakan matriks covariance apakah ingin discale atau tidak
  if(cov){
    if(scale){
      matrix<-cov(scale(data))
      judul<-"Scree-Plot : PCA dari scaled data dan matrix kovarians"
    } else{
      matrix<-cov(data)
      judul<-"Scree-Plot : PCA dari unscaled data dan matrix kovarians"
    }
  } else{
    matrix<-cor(data)
    judul<-"Scree-Plot : PCA dari matrix korelasi"
  }
  # mencari eigen value dan eigen vector
  eigenvector<-eigen(matrix)$vector
  eigenvalue<-eigen(matrix)$value
  
  # mencari standar deviasi, proporsi varians, kumulatif proporsi varians dan loadings setiap komponen utama
  stdev<-sqrt(eigenvalue)
  propvar<-eigenvalue/sum(eigenvalue)
  cumvarcov<-c(1:ncol(matrix))
  temp<-0
  for( i in 1:ncol(matrix)){
  temp<-temp+propvar[i]
  cumvarcov[i]<-temp
  }
  pc<-paste("PC",1:length(eigenvalue))
  loadings<-data.frame(variabel,round(eigenvector,4))
  colnames(loadings)<-append("variabel",pc)
  
  # scree plot
  propvardf<-data.frame(pc,propvar)
  screeplot<-propvardf %>%
  ggplot(aes(x=pc,y=propvar, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title=judul)
  
  objek<-list("std_dev"=stdev, "var_prop"=propvar, "cum_prop_var"=cumvarcov, "loadings"=loadings, "scree_plot"=screeplot)
  return (objek)
}

```

```{r}
# menggunakan matriks kovarians data scaled
  
principal_component(data=data8,cov=T,scale=T)
```

```{r}
# menggunakan matriks kovarians dan unscaled data
principal_component(data=data8, cov=T, scale = F)
```
```{r}
# menggunakan matriks korelasi
principal_component(data=data8, cov=F)
```
berdasarkan data, diketahui bahwa terdapat beberapa variabel yang memiliki skala yang berbeda beda. perbedaan skala ini akan mengakibatkan nilai nilai yang jompang pada matriks kovarians, kemudian berefek pada eigen value dan matriks eigen vektor. proporsi varians yang dijelaskan di setiap komponen selain komponen 1 sangat menjadi kecil karena besarnya varians yang ada pada variabel y3 dan mendominasi varians total, dapat terlihat dari loadings pada komponen utama 1 didominasi oleh y3. hal ini membuat proporsi varians yang dijelaskan memang besar namun sangat sedikit informasi yang kita ketahui mengenai variabel lain. 

pada analisis menggunakan matriks kovarians dengan unscaled data, terdapat perbedaan komponen utama yang dipertahankan apabila dilihat dari eigen value dan scree plot. dengan menggunakan eigen value komponen yang dipertahankan ialah komponen utama 1,2,3 (diambil apabila st deviasi > 1), sementara apabila menggunakan scree plot, komponen utama yang dipertahankan hanya satu buah komponen yaitu komponen utama 1. namun apabila kita melakukan scaling data atau menggunakan matriks korelasi, akan didapat komponen utama yang sama, yaitu komponen utama 1,2,3.

sehingga pada data yang kita gunakan, akan lebih cocok apabila menggunakan matriks korelasi, hal ini karena menurut saya akan lebih baik jika kita tidak melihat hanya dari seberapa besar varians yang dijelaskan, namun kita juga harus melihat dari mana varians yang dijelaskan tersebut diperoleh, menurut saya yang paling baik ialah varians yang dijelaskan berasal dari variabel variabel yang digunakan dalam analisis komponen utama karena itu lebih menjelaskan struktur data yang digunakan dari pada hanya satu variabel saja.
namun apabila ingin bersikeras menggunakan matriks kovarians, scaling data diperlukan untuk menghandle informasi yang didominasi pada variabel tertentu tadi.

data yang discaling akan menghasilkan komponen komponen utama yang sama dengan komponen utama menggunakan matriks korelasi.
