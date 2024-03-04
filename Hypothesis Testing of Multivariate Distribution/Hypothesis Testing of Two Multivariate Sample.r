---
title: "Hypothesis Testing of Two Multivariate Sample"
author: "Fikri Septrian Anggara"
date: "3/16/2021"
output:
  html_document: default
  word_document: default
---
## Buatlah fungsi R untuk melakukan uji hipotesis dari vector rata-rata untuk satu sampel, dua sampel independen, dan dua sampel dependen (dalam satu fungsi).

#### Deskripsi fungsi 
* fungsi ini memerlukan 4 parameter yang wajib ada yaitu, vektor myu, data yang berbentuk matriks atau dataframe, indeks untuk sampel ke satu, dan indeks untuk sampel ke 2. 

* sigma diasumsikan didapat dari sampel, sehingga yang digunakan adalah t hitung yang kemudian ditransformasi ke f hitung. f hitung ini akan dibandingkan dengan f tabel. 

* parameter sampel1 dan sampel2 merupakan list indeks yang berguna untuk menandai kolom mana saja yang termasuk ke dalam sampel satu dan kolom mana saja yang masuk ke sampel 2. diasumsikan kedua list yang dimasukan berbeda sama sekali (jika terdapat kesamaan indeks, t hitung yang dihasilkan akan menghasilkan NA, karena determinan dari matriks kovarians adalah 0 (terdapat kolom yang tidak bebas linear) ).
```{r}
#fungsi ini memerlukan  input, vektor myu, matriks atau dataframe data dan signifikansi atau alfa. selain itu sigma diasumsikan didapat dari sampel, sehingga yang digunakan adalah t hitung. t hitung ini akan dibandingkan dengan t-hotelling tabel atau dikonversi ke f hitung. kemudian sigma yang diestimasi data sampel diasumsikan berbeda untuk kasus hipotesis dua sampel independen.

hipotesis_vektor_rata2<-function(myu=c(),data=matrix(),sampel1=c(),sampel2=c(),alfa=0.05){
  
  vektorrata2sampel1<-c()
  vektorrata2sampel2<-c()
  vektorrata2sampelpasangan<-c()
  satusampel<-data.frame()
  duasampelindenpenden<-data.frame()
  duasampeldependen<-data.frame()
  obj<-list()
  
  if(length(sampel1)!=length(sampel2)){
    stop("banyak variat tidak sama")
  } 
  
  if(length(myu)!=ncol(data)){
    stop("banyak myu tidak sama dengan banyak variabel data")
  }else {
    
  n<-nrow(data)
  vektor_rata2<-NULL
  variat<-ncol(data)
  
  for(i in 1:ncol(data)){
    vektor_rata2[i]<-mean(data[,i])
  }
  #=======================================================================================
  #kode di bawah untuk mencari t hitung dan f hitung untuk hipotesis satu vektor mean
  
  selisih<-t(vektor_rata2-myu)
  matvarkov<-cov(data)
  thitung<-n*selisih%*%solve(matvarkov)%*%t(selisih)
  
  fhitung<-(((n-1)-variat+1)/((n-1)*variat))*thitung
  d2<-n-1-variat+1
  ftabel<-qf(1-alfa,variat,d2)

  if(fhitung>ftabel){
    keputusan<-"tolak H0"
  } else{
    keputusan<-"gagal tolak H0"
  }
  
  satusampel<-data.frame(fhitung,ftabel,keputusan)
  #=======================================================================================
  #kode di bawah untuk mencari t hitung dan f hitung untuk hipotesis dua sampel independen
  
  datasampel1<-data[,sampel1]
  datasampel2<-data[,sampel2]
  indeks<-1
  for(i in sampel1){
    vektorrata2sampel1[indeks]<-mean(data[,i])
    indeks=indeks+1
  }
  indeks<-1
  for(i in sampel2){
    vektorrata2sampel2[indeks]<-mean(data[,i])
    indeks=indeks+1
  }
  selisih<-t(vektorrata2sampel1-vektorrata2sampel2)
  n<-nrow(datasampel1)
  variat<-ncol(datasampel1)
  
    #untuk sigma beda
    sigma1<-cov(datasampel1)
    sigma2<-cov(datasampel2)
    
    S<-(sigma1/n)+(sigma2/n)
    thitung<-n*selisih%*%solve(S)%*%t(selisih)
    fhitung<-(((n+n-variat-1)/((n+n-2)*variat)))*thitung
    d2<-n+n-variat-1
    ftabel<-qf(1-alfa,variat,d2)
    
    if(fhitung>ftabel){
      keputusan<-"tolak H0"
    } else{
      keputusan<-"gagal tolak H0"
    }
    temp<-data.frame(thitung,fhitung,ftabel,keputusan)
    
    #untuk sigma sama
    spool<-(1/(2*n-2))*((n-1)*sigma1+(n-1)*sigma2)
    thitung<-((n*n)/(n+n))*selisih%*%solve(spool)%*%t(selisih)
    fhitung<-(((n+n-variat-1)/((n+n-2)*variat)))*thitung
    d2<-n+n-variat-1
    ftabel<-qf(1-alfa,variat,d2)
    
    if(fhitung>ftabel){
      keputusan<-"tolak H0"
    } else{
      keputusan<-"gagal tolak H0"
    }
    temp<-rbind(temp,list(thitung,fhitung,ftabel,keputusan))
    row.names(temp)<-c("sigma beda", "sigma sama")
    duasampelindenpenden<-temp
  #=======================================================================================  
  #kode di bawah untuk mencari t hitung dan f hitung untuk sampel berpasangan
    
  d<-matrix(rep(0,ncol(datasampel1)*nrow(datasampel1)),ncol = ncol(datasampel1))
    
  for(i in 1:nrow(datasampel1)){
    for(j in 1:ncol(datasampel1)){
      d[i,j]<-datasampel1[i,j]-datasampel2[i,j]
    }
  }
  for(i in 1:ncol(datasampel1)){
    vektorrata2sampelpasangan[i]<-mean(d[,i])
  }
    
  vektorrata2sampelpasangan<-t(vektorrata2sampelpasangan)
  n<-nrow(d)
  sigmad<-cov(d)
  variat<-ncol(vektorrata2sampelpasangan)
  thitung<-n*vektorrata2sampelpasangan%*%solve(sigmad)%*%t(vektorrata2sampelpasangan)
  fhitung<-(((n-1)- variat +1)/((n-1)*variat))*thitung
  ftabel<-qf(1-alfa,variat,(n-1)-variat+1)
    
  if(fhitung>ftabel){
    keputusan<-"tolak H0"
  }else{
    keputusan<-"gagal tolak H0"
  }
  
  duasampeldependen<-data.frame(thitung=thitung,fhitung=fhitung,ftabel=ftabel,keputusan=keputusan)
  #=======================================================================================
  # return value berupa objek yang berisikan 3 dataframe
  # di mana masing masing data frame mewakili uji hipotesis satu vektor mean, dua sampel independen dan dua sampel dependen.
  # masing masing dataframe memiliki informasi seperti t hitung, f hitung, f tabel dan keputusan hipotesis
  
  obj<-list(satu_sampel=satusampel,dua_sampel_indenpenden=duasampelindenpenden,dua_sampel_dependen=duasampeldependen)
  return (obj)
  }
  
}

rata2<-rnorm(10)
x<-rnorm(300)
data<-matrix(x,byrow = F,ncol = 10)

x<-hipotesis_vektor_rata2(rata2,data,c(1:5),c(6:10),0.025)
x
```

