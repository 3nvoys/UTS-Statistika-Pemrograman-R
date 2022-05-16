setwd("D:/perkuliahan/statistika/UTS")
tabel <- read.csv("data.csv", header = TRUE, sep = ";")

#=================<menghitung mean>===================== 

#menghitung xi*fi
tabel$xifi <- (tabel$xi)*(tabel$fi)

#total xifi
Txifi <- sum(tabel$xifi)

#menghitung mean
X <- Txifi/sum(tabel$fi)


#=================<menghitung median>===================== 

#batas kelas bawah
tabel$bkb <- with(tabel, nilai_min-0.5)
#batas kelas atas
tabel$bka <- with(tabel, nilai_max+0.5)

#menghitung tepi batas kelas median
b <- tabel[which.max(tabel$fi), "bkb"]

#menghitung interval
p <- tabel[1, "nilai_max"] - tabel[1, "nilai_min"]+1

#menghitung frekuensi kumulatif
fk <- tabel[which.max(tabel$fi)-1, "fi"]

#jumlah total frekuensi
n <- sum(tabel$fi)

F <- tabel[which.max(tabel$fi), "fi"]

#menghitung median
Me <- b + p*(((n/2)-fk)/F)


#=================<menghitung modus>===================== 

b1 <- tabel[which.max(tabel$fi), "fi"]-tabel[which.max(tabel$fi)-1, "fi"]

b2 <- tabel[which.max(tabel$fi), "fi"]-tabel[which.max(tabel$fi)+1, "fi"]

modus <- b + p * (b1/(b1+b2))

#=================<Range>===================== 

R <- max(tabel$xi)-min(tabel$xi)

#=================<Simpangan rata-rata>===================== 

SR <- (sum((tabel$fi)*(abs(tabel$xi - X)))) / (sum(tabel$fi))

#=================<Simpangan baku>===================== 

varian <- (sum((tabel$fi)*((tabel$xi - X)^2))) / (sum(tabel$fi))

SB <- sqrt(varian)

#=================<Menampilkan Hasil>===================== 

cat("Data Lama Penggunaan HP Dalam Sehari Mahasiswa Fasilkom (Jam)\n")
cat("\n")
print(tabel)
cat("\n")
cat("Mean : ", X, "\n")
cat("Median : ", Me, "\n")
cat("Modus : ", modus, "\n")
cat("Range : ", R, "\n")
cat("Simpangan Rata-Rata: ", SR,"\n")
cat("Simpangan Baku: ", SB,"\n")