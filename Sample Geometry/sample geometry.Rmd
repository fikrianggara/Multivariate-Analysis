---
title: "sample geometry : vector deviation, vector projection"
author: "Fikri Septrian Anggara"
date: "2/27/2021"
output:
  html_document: default
  word_document: default
---

# 1. buatlah fungsi untuk mencari proyeksi vektor dan panjang proyeksi vektor x ke y
```{r}
# min-max feature scalling
normalisasi<-function(r){
  return((r-min(r))/(max(r)-min(r)))
}
#proyeksi vektor x pada y merupakan vektor y yang diskalakan sebesar (dotproduct x dan y dibagi panjang y kuadrat)
#panjang proyeksi vektor x pada y merupakan cosinus dari sudut yang dibentuk vektor x dan y, dikali panjang vektor x
x<-c(1:10)
y<-c(4:13)

#proyeksix1x2 merupakan fungsi yang digunakan untuk mencari proyeksi vektor dan panjang proyeksi vektor x1 pada x2
proyeksix1x2<-function(x,y){

  sumy<-0
  sumx<-0
  leny<-0
  lenx<-0
  proyeksivektor<-y
  panjangproyeksi<-0
  xy<-x*y
  dotprod<-0
  sumxy<-0
  
  #membuat jumlah kuadrat untuk vektor y
  for(i in 1:length(y)){
    sumy<-sumy+y[i]^2
    i=i+1
  }
  #membuat jumlah kuadrat untuk vektor x
  for(i in 1:length(x)){
    sumx<-sumx+x[i]^2
    i=i+1
  }
  
  #mencari panjang vektor x dan y
  leny<-sqrt(sumy)
  lenx<-sqrt(sumx)
  
  #mencari dot product dari vektor x dan y 
  for(i in 1:length(xy)){
    sumxy<-sumxy+xy[i]
    i=i+1
  }
  
  #mencari proyeksi vektor dan panjang proyeksi vektor
  proyeksivektor<-(sumxy/leny^2)*y
  panjangproyeksi<-sumxy/leny
  
  #membuat objek dengan atribut proyeksi vektor dan panjang proyeksinya sebagai nilai yang dikembalikan
  objek<-list(proyeksi_vektor=proyeksivektor,panjang_proyeksi=panjangproyeksi, panjang_vektorx1=lenx, panjang_vektorx2=leny)
  return (objek)
  
}

proyeksix1x2(x,y)

```

# 2. cari korelasi berdasarkan vektor deviasi
```{r}
#vektor deviasi=yi-xbar, di mana yi merupakan vektor observasi untuk variabel ke i, dan x bar-i rata-rata observasi dari variabel ke i
x<-c(1:10)
y<-c(4:13)

#vektordev merupakan fungsi yang berguna untuk mencari vektor deviasi dari suatu vektor
vektordev<-function(x){
  satu<-c(rep(1,length(x)))
  proyxsatu<-proyeksix1x2(x,satu)
  vektordeviasi<-x-proyxsatu$proyeksi_vektor
  
  return(vektordeviasi)
}

panjangvektor<-function(x){
  lenx<-0
  sumx<-0
  for(i in 1:length(x)){
    sumx<-sumx+x[i]^2
  }
  lenx<-sqrt(sumx)
  
  return(lenx)
}

z<-c(123,3,24321,43,4321,3434)
vektordev(z)
vektordev(y)

#korelasi dari dua variabel merupakan kosinus dari sudut dua buah vektor deviasi. kosinus dari dua vektor adalah dot product dari kedua vektor dibagi perkalian dari panjang kedua vektor tersebut.

#kovvektordev() merupakan fungsi yang digunakan untuk mencari kovarians dua variabel berdasarkan vektor deviasinya.
kovvektordev<-function(x,y){
  
  xy<-x*y
  ssx<-0
  ssy<-0
  ssxy<-0
  korelasi<-0
  
  #mencari jumlah kuadrat elemen vektor deviasi x
  for (i in 1:length(x)){
    ssx<-ssx+x[i]^2
    i<-i+1
  }
  #mencari jumlah kuadrat elemen vektor deviasi y
  for (i in 1:length(y)){
    ssy<-ssy+y[i]^2
    i<-i+1
  }
  
  #mencari jumlah perkalian elemen vektor deviasi x dan y
  for (i in 1:length(xy)){
    ssxy<-ssxy+xy[i]^2
  }
  
  korelasi<-sqrt(ssxy)/(sqrt(ssx)*sqrt(ssy))
  obj<-list(korelasi=korelasi,ssx1=ssx, ssx2=ssy, ssx1x2=ssxy)
  return (obj)
  
}

kovvektordev(vektordev(y),vektordev(x))
```
# latihan 3.4, berdasarkan data pada tabel 1.1 :
## a. tentukan proyeksi data x1 terhadap vektor 1
```{r}
#proyeksi vektor observasi terhadap vektor satu didapat menggunakan fungsi proyeksix1x2, di mana x1 merupakan data tabel 1.1, x2 merupakan vektor satu dengan 6 elemen
data3.4x1<-c(3497900,2485475, 1782875, 1725450, 1645575, 1469800)
satu<-c(rep(1,length(data3.4x1)))
data3.4x1norm<-normalisasi(data3.4x1)
proyeksix1x2(data3.4x1norm,satu)

var(data3.4x1norm)
var(data3.4x1)
```
## b. vektor deviasi data x1
```{r}
#vektor deviasi data tabel 1.1 didapat menggunakan fungsi vektordev
print("vektor deviasi: ")
vektordev(data3.4x1)
print("panjang vektor deviasi: ")
panjangvektor(vektordev(data3.4x1))

```
## c. buat segitiga yang dibentuk dari y1, x1*1, dan vektor deviasi y1
```{r}
print("hello")


```
## d. ulangi part a-c untuk variabel x2 pada data tabel 1.1
```{r}
data3.4x2<-c(0.623,0.593,0.512,0.5,0.463,0.395)
satux2<-c(rep(1,length(data3.4x2)))
proyeksix1x2(data3.4x2,satu)
print("vektor deviasi: ")
vektordev(data3.4x2)
print("panjang vektor deviasi: ")
panjangvektor(vektordev(data3.4x2))


```
## e. gambarkan vektor deviasi x1 dan x2, hitung sudutnya
```{r}

#fungsi sudutduavektor digunakan untuk menghitung kosinus dan besar sudut yang dibentuk oleh dua vektor, besar sudut 
sudutduavektor<-function(x,y){
  xy<-x*y
  sumxy<-0
  sumx<-0
  sumy<-0
  leny<-0
  lenx<-0
  
  #menghitung dot product vektor x dan y
  for (i in 1:length(xy)){
    sumxy<-sumxy+xy[i]
    i=i+1
  }
  
  #menghitung jumlah kuadrat untuk vektor y
  for(i in 1:length(y)){
    sumy<-sumy+y[i]^2
    i=i+1
  }
  
  #menghitung jumlah kuadrat untuk vektor x
  for(i in 1:length(x)){
    sumx<-sumx+x[i]^2
    i=i+1
  }
  
  #mencari panjang vektor x dan y
  leny<-sqrt(sumy)
  lenx<-sqrt(sumx)
  
  #mencari nilai kosinus
  cosxy<-sumxy/(lenx*leny)
  
  #nilai yang dikembalikan oleh fungsi acos memiliki satuan radian, dilakukan konversi ke satuan derajat
  sudut<-acos(cosxy)*180/pi
  obj<-list(kosinus=cosxy, sudut=sudut)
  
  return (obj)
}

sudutduavektor(data3.4x1norm,data3.4x2)
```

