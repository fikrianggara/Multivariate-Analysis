---
title: "Multivariate Normal Distribution"
author: "Fikri Septrian Anggara"
date: "3/6/2021"
output:
  html_document: default
  word_document: default
---
## buat fungsi untuk membuat gari kontur elips dan fungsi untuk menentukan apakah titik berada di dalam atau di luar elips.
```{r}
library(MASS)#package untuk geenerate data bivariate normal
library(mixtools)#package untuk generate plot elips
```

### A. Fungsi untuk membuat garis kontur elips dari data
#### bentuk dan ukuran garis kontur elips ditentukan oleh beberapa hal:
  * eigenvalue, menentukan arah elips
  * eigenvektor, menentukan besar elips
  * signifikansi(alfa), menentukan luas daerah elips
  * rata rata, menentukan titik pusat elips
eigenvalue dan eigenvektor diperoleh dari matriks varians-konvarians.

#### Fungsi varians-kovarians untuk mencari matriks varians-kovarians berdasarkan data
```{r}
fungsivarkov<-function(x){
  matvarkov<-matrix(nrow = ncol(x), ncol = ncol(x))
  z=1
  selisih=data.frame()
  xbar=NULL
  kaliselisih=NULL
  for (k in 1:ncol(x)){
    xbar[k]<-mean(x[,k])
    for(l in 1:nrow(x)){
    selisih[l,k]<-x[l,k]-xbar[k]
    }
  }
 
  for( k in 1:ncol(x)){
    for(j in 1:ncol(x)){
        for(i in 1:nrow(x)){
        kaliselisih[i]<-selisih[i,j]*selisih[i,k]
        }
      matvarkov[j,k]<-sum(kaliselisih)/(nrow(x)-1)
      kaliselisih=NULL
    }
  }
return(matvarkov)
}
```

#### Fungsi untuk menggambarkan elips kontur berdasarkan data
```{r}
kontur_elips<-function(data=data.frame(), alfa=0.05){
  if(ncol(data)!=2){
    stop("data harus memiliki 2 variat, tidak boleh lebih atau kurang")
  }
  else{
    myu<-c(mean(data[,1]),mean(data[,2]))
    sigma<-fungsivarkov(data)
  
  
    eigensigma<-eigen(sigma)
    chishitung<-round(qchisq(1-alfa,ncol(sigma)),2)
  
    sb1mayor<-myu+sqrt(chishitung*eigensigma$values[1])%*%eigensigma$vectors[,1]
    sb2mayor<-myu-sqrt(chishitung*eigensigma$values[1])%*%eigensigma$vectors[,1]
  
    sb1minor<-myu+sqrt(chishitung*eigensigma$values[2])%*%eigensigma$vectors[,2]
    sb2minor<-myu-sqrt(chishitung*eigensigma$values[2])%*%eigensigma$vectors[,2]
  
    judul=paste("elips kontur untuk alfa =",alfa)
    plot(data, xlab = names(data[1]), ylab= names(data[2]),main=judul)
    par(new=T)# untuk menggabungkan plot data dan elips, kode ini
    lines(ellipse(myu,sigma,alfa,col="red"))
    
  }
  
  
}

myu<-c(75,70)
sigma<-matrix(c(10,12,12,16), byrow = F, ncol=2)
alfa<-0.05

set.seed(123)
datdummy<-mvrnorm(n=200, mu=myu,Sigma = sigma)
datdummy<-data.frame(datdummy)
kontur_elips(datdummy, alfa)

set.seed(123)
#contoh menggunakan data dari dua distribusi yang berbeda
datdummy2<-data.frame(data_normal=rnorm(100,60,5.3), data_poisson=rpois(100,55))
kontur_elips(datdummy2,0.05)
```

### B. Fungsi untuk mengetahui apakah sebuah titik berada di luar atau di dalam daerah kontur elips dengan alfa tertentu.
```{r}
#input data harus berupa matriks
is_in_ellipse<-function(data=matrix(),myu=matrix(),sigma=matrix(),alfa=0.05){
  
  if(ncol(data)==1 && ncol(myu)==1){
  kiri<-t(data-myu)
  }
  
  else{
  kiri<-data-myu
  }
  
  kanan<-t(kiri)
  sigmainverse<-solve(sigma)
  titikkritis<-1-alfa
  variat<-ncol(kiri)
  
  chishitung<-round(qchisq(titikkritis,variat),2)
  
  ruaskanan<-chishitung
  ruaskiri<-round(kiri%*%sigmainverse%*%kanan,2)
  
  if(ruaskiri<=ruaskanan){
    keputusan<-paste("observasi berada di dalam elips. ",ruaskiri,"<=",ruaskanan)
    print(keputusan)
    return (TRUE)
  }
  
  else{
    keputusan<-paste("observasi berada di luar elips. ",ruaskiri,">",ruaskanan)
    print(keputusan)
    return (FALSE)
  }
  
}
myu<-matrix(c(75,70))
sigma<-matrix(c(10,12,12,16), byrow = F, ncol=2)
alfa<-0.05
data<-matrix(c(80,65))

is_in_ellipse(data,myu,sigma,alfa)
```


