---
title: "Factor Analysis"
author: "Fikri Septrian Anggara"
date: "5/2/2021"
output:
  html_document: default
  word_document: default
---
soal :
  dari data kuesioner, lakukanlah analisis faktor apabila setiap pertanyaan dianggap sebagai variabel manifes!
```{r}
library(tidyverse)
library(haven)
library(REdaS)
library(psych)
data9 <- read_sav("SAQ8.sav")
head(data9)
data9<-as.data.frame(data9)
head(data9)
```

```{r}
scaleddata<-as.data.frame(scale(data9))
bart_spher(scaleddata) # karena p-value lebih kecil dari alfa 0.05, maka tolak H0, terdapat korelasi yang signifikan antara variabel

# mencari banyak faktor yang sesuai dengan menggunakan eigenvalue matriks korelasi
eigenvalue<-eigen(cor(scaleddata))$value
print("eigen value :")
eigenvalue
eigenvaluerata2<-sum(eigenvalue)/length(eigenvalue)
eigenvaluerata2
count<-0
for(i in 1:length(eigenvalue)){
  if(eigenvalue[i]>eigenvaluerata2){
    count=count+1
  }
}
paste("banyak faktor yang sesuai ialah ",count)

# melihat loading pervariabel
factanal(factor=2, covmat = cor(scaleddata))

# melihat variabel mana yang tidak mampu memanifestasikan kedua faktor (drop variabel jika communality < 0.5)
variabel<-colnames(scaleddata)
communality<-c(1:length(variabel))
loading<-factanal(factor=2, covmat=cor(scaleddata))$loadings
for(i in 1:ncol(scaleddata)){
  temp<-0
  for( j in 1:count){
    temp<-temp+loading[i,j]^2
  }
  communality[i]<-round(temp,4)
}
communalitydf<-data.frame(variabel,communality)
communalitydf
?fa()
```
Analisis faktor yang dilakukan ialah analisis faktor untuk eksplorasi, sehingga asumsi multivariat normal tidak diperlukan. kemudian dilanjutkan dengan pengecekan apakah terdapat korelasi yang signifikan terhadap variabel (point pertanyaan) yang ada, setelah dilakkan uji bartlette spherical, didapat p-value yang kurang dari alfa 0.05, artinya memang benar terdapat korelasi di antara variabel.

kemudian dilakukan penentuan banyaknya faktor yaitu dengan menggunakan eigenvalue dari matriks korelasi. eigen value rata-rata ialah 1, karena terdapat dua buah eigen value yang lebih besar dari eigen value rata-rata, maka digunakan 2 buah faktor.

kemudian dilakukan analisis terhadap communality, jika communality suatu variabel manifes < 0.5, maka disarankan variabel tersebut dibuang karena sebenarnya variabel tersebut tidak berguna apabila ingin memanifestasikan faktor. dari hasil analisis terhadap communality, didapat variabel manifes yang >=0.5 hanya variabel q07, yaitu pernyataan tentang pengalaman menggunakan komputer. namun demi kesederhanaan, variabel yang digunakan dianggap penting untuk memanifestasikan faktor sehingga variabel yang terbukti tidak berguna akan dianggap berguna.

selanjutnya dilakukan analisis faktor menggunakan fungsi factoranal dengan factor=2, didapat loading masing masing variabel terhadap faktor. 
  * dari output fungsi, terlihat jika faktor 1 didominasi oleh variabel q01 dan q04, kedua variabel tersebut merupakan variabel variabel bersentimen negatif terhadap statistik. jadi bisa disarankan bahwa faktor pertama/ variabel laten pertama ialah "minat terhadap statistika".
  * dari output fungsi, terlihat jika faktor 2 didominasi oleh variabel q07 dan q06, kedua variabel tersebut merupakan variabel variabel bersentimen negatif terhadap komputer. jadi bisa disarankan bahwa faktor kedua/ variabel laten kedua ialah "minat terhadap komputer".
  

