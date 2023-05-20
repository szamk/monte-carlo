require(reshape2)
require(gghighlight)
require(pals)
require(RColorBrewer)

#Elterjedésre vonatkozó függvények leírása: adott évben eladott személyautók hány százaléka önvezető.
A <- matrix(nrow = 44, ncol = 4, byrow = TRUE)
A[,1] <- 2023:2066

A[,2] <- c(rep(0,3),seq(from = 0.3, by=3, length.out = 41))
A[,2] <- ifelse(A[,2]>100, 100, A[,2])

A[,3] <- c(rep(0,3),rep(0.3,3), rep(5,3), rep(12,3), rep(21,3),
           rep(32,3), rep(45,3), rep(60,3), rep(77,3), rep(94,3), rep(100,14))

A[1:3,4] <- rep(0,3)
for (i in 4:nrow(A)) {
  A[i,4] <- round(min(0.003 * exp(0.35*i)*100,100),2)
}


colnames(A) <- c("év","lineáris", "lépcsős", "exponenciális")
A[,2:4] <- A[,2:4]*0.01

# A mátrix 3 és 5 éves eltolása. 
A_3 <-matrix(nrow = 44, ncol = 4, byrow = TRUE) #3 éves lemaradás
colnames(A_3) <- c("év","lineáris", "lépcsős", "exponenciális")
A_3[,1] <- c(2023:2066)
A_3[1:3,2:4] <- 0
for (i in 4:nrow(A_3)){
  A_3[i,2:4] <- A[i-3,2:4]
}

A_5 <-matrix(nrow = 44, ncol = 4, byrow = TRUE) #5 éves lemaradás
colnames(A_5) <- c("év","lineáris", "lépcsős", "exponenciális")
A_5[,1] <- 2023:2066
A_5[1:5,2:4] <- 0
for (i in 6:nrow(A_5)){
  A_5[i,2:4] <- A[i-5,2:4]
}

#Önvezető arány a személygépjármű állományban. Németországi minta: nincs késleltetés, és az új autók aránya magasabb.
N <- A
N[1,2:4] <- N[1,2:4] * rnorm(1,0.07,0.007988)
for (i in 2:nrow(N)){
  for (j in 2:4){
    N[i,j] <- min(N[i,j]* rnorm(1,0.07,0.007988) + N[i-1,j],1)
  }
}

# A teljes személygépjármű állományban hogy fog alakulni az önvezető autók aránya az egyes elterjedési függvények esetén, ha az A mátrixban ismertetett 
# folyamatok  3 vagy 5 év késleltetéssel zajlanak le.
uj_jarmu <- 0.029969 # új személygépjárművek aránya
szoras <- 0.007988 

AV_3 <- matrix(nrow = 44, ncol = 4, byrow = TRUE)
colnames(AV_3) <- c("év","lineáris", "lépcsős", "exponenciális")
AV_3[,1] <- c(2023:2066)
AV_3[1,2:4] <- A_3[1,2:4] * rnorm(1,uj_jarmu,szoras)
for (i in 2:nrow(AV_3)){
  AV_3[i,2:4] <- A_3[i,2:4] * rnorm(1,uj_jarmu,szoras) + AV_3[i-1,2:4]
}
AV_3 <- ifelse(AV_3>1,1,AV_3)


AV_5 <- matrix(nrow = 44, ncol = 4, byrow = TRUE)
colnames(AV_5) <- c("év","lineáris", "lépcsős", "exponenciális")
AV_5[,1] <- c(2023:2066)
AV_5[1,2:4] <- A_5[1,2:4] * rnorm(1,uj_jarmu,szoras)
for (i in 2:nrow(AV_5)){
  AV_5[i,2:4] <- A_5[i,2:4] * rnorm(1,uj_jarmu,szoras) + AV_5[i-1,2:4]
}
AV_5 <- ifelse(AV_5>1, 1,AV_5)

####### Ár - ez egyben a kárnagyság növekedése is
ar <- seq(1.5,3,0.3)
ar_tendencia <- c(1,0.5,0,1.2)

####### Társadalmi hatás
kitettseg <- c(0.9,1,1.05,1.1,1.15)
kargyak_nav <- c(1,0.95,0.9)

####infláció
inf <- 0.033

##### egy kárra jutó átlagos kárfelhasználás
egy_kar <- 661974
karfelhasznalas <- c()
for (i in 1:44){
  karfelhasznalas[i] <- egy_kar*(1+inf)^(i+1)
}
v_kargyakorisag <- 0.0254

############################### MONTE CARLO ##############################
r <- 500000

tabla <- matrix(0,ncol = 4, nrow = r)
colnames(tabla) <- c("kar_db_hagyomanyos", "osszkar_hagyomanyos", "kar_db_av", "osszkar_av")

osszesito <- matrix(0,ncol = 8, nrow = 44)
colnames(osszesito) <- c("kar_db_hagyomanyos", "osszkar_hagyomanyos", "kar_db_av", "osszkar_av", "kargyak_also", "kargyak_felso", "osszkar_also", "osszkar_felso")

elterj <- list(N,AV_3,AV_5)

forgatokonyvek_m <- matrix(ncol=6, nrow = 12)
forgatokonyvek_m[,1] = c(rep(4,4), rep(2,4), rep(3,4))
forgatokonyvek_m[,2] = c(2,2,3,3,3,2,2,3,3,2,3,2)
forgatokonyvek_m[,3] = c(1,3,6,4,1,2,5,6,2,3,4,5)
forgatokonyvek_m[,4] = c(3,1,2,4,1,3,2,4,4,2,1,3)
forgatokonyvek_m[,5] = c(1,3,5,2,4,2,2,5,1,3,1,4)
forgatokonyvek_m[,6] = c(3,2,1,1,1,3,2,1,1,2,1,2)
colnames(forgatokonyvek_m) <- c("elterjedes", "eltolas", "ktg", "ktg_tend", "kitettseg", "tanulas")


tot_m <- matrix(0,nrow=44, ncol = 25)
tot_m[,1] < -2023:2066

tot_szam_m <- matrix(0,nrow=44, ncol = 13 )
tot_szam_m[,1] <- 2023:2066

tot_kar_m <- matrix(0,nrow=44, ncol = 13)
tot_kar_m[,1] <- 2023:2066

deltak_karnagy <- c()
deltak_kargyak <- c()
osszesitok <- list()

av_kargyak_kezdo <- 0.2
cel <- 0.05

kezdes <- Sys.time()
set.seed(13)

for (k in 1:12){
  
  tolas <- elterj[[forgatokonyvek_m[k,2]]]
  terj <- tolas[,forgatokonyvek_m[k,1]]
  nagysag <- ar[forgatokonyvek_m[k,3]]
  nagysag_tendencia <- ar_tendencia[forgatokonyvek_m[k,4]]
  kitett <- 1
  kargyak_nem <- 1
  av_kargyak <- av_kargyak_kezdo
  valtozik <- 0
  
  for (j in 1:44) {
    
    if (terj[j] >0.05) {
      valtozik <- valtozik + 1
      if (nagysag_tendencia <= 1){
        if (nagysag > (ar[forgatokonyvek_m[k,3]]-1)*nagysag_tendencia+1){
          nagysag <- nagysag - 
            (ar[forgatokonyvek_m[k,3]]-((ar[forgatokonyvek_m[k,3]]-1)*ar_tendencia[forgatokonyvek_m[k,4]]+1))/20
        }
      } else {
        if (nagysag < (ar[forgatokonyvek_m[k,3]]-1)*nagysag_tendencia+1){
          nagysag <- nagysag -
            (ar[forgatokonyvek_m[k,3]]-((ar[forgatokonyvek_m[k,3]]-1)*ar_tendencia[forgatokonyvek_m[k,4]]+1))/20
        }
      }
    }
    if (valtozik > 0 && valtozik < 9) {
      kitett <- kitett - (1 - kitettseg[forgatokonyvek_m[k,5]])/8
      kargyak_nem <- kargyak_nem - (1 - kargyak_nav[forgatokonyvek_m[k,6]])/8
    }
    
    if (j>=2) {
      av_kargyak <- av_kargyak - (av_kargyak_kezdo - cel)/43
    }
    
    for (i in 1:r){
      karszam <- rpois(1, v_kargyakorisag*kargyak_nem)
      karszam_av <- rpois(1, v_kargyakorisag*av_kargyak*kitett)
      tabla[i,1] <- karszam
      tabla[i,2] <- sum(rexp(karszam,1/(karfelhasznalas[j]*(terj[j]*nagysag + (1-terj[j])))))
      tabla[i,3] <- karszam_av
      tabla[i,4] <- sum(rexp(karszam_av,1/(karfelhasznalas[j]*(terj[j]*nagysag + (1-terj[j])))))
    }
    
    
    
    szoras_kargyak <- sqrt((1-terj[j])^2 * var(tabla[,1]) +
                             terj[j]^2 *var(tabla[,3]))
    atlag_kargyak <- (1-terj[j])*mean(tabla[,1]) + terj[j]*mean(tabla[,3])
    
    
    CI_kargyak <- c(atlag_kargyak - qnorm(1-0.05/2,0,1) * szoras_kargyak/sqrt(r),
                    atlag_kargyak + qnorm(1-0.05/2,0,1) * szoras_kargyak/sqrt(r))
    delta <- CI_kargyak[2]-atlag_kargyak
    deltak_kargyak[j] <- delta
    
    szoras_karnagy <- sqrt((1-terj[j])^2 * var(tabla[,2]) +
                             terj[j]^2 * var(tabla[,4]))
    
    atlag_karnagy <- (1-terj[j]) * mean(tabla[tabla[,2]>0,2]) +
      terj[j] * mean(tabla[tabla[,4]>0,4])
    
    CI_karnagy <- c(atlag_karnagy - qnorm(1-0.05/2,0,1) * szoras_karnagy/sqrt(r),
                    atlag_karnagy + qnorm(1-0.05/2,0,1) * szoras_karnagy/sqrt(r))
    
    delta_karnagy <- CI_karnagy[2] - atlag_karnagy
    deltak_karnagy[j] <- delta_karnagy
    
    osszesito[j,1:8] <- c(mean(tabla[,1]), mean(tabla[tabla[,2]>0,2]),
                          mean(tabla[,3]), mean(tabla[tabla[,4]>0,4]),
                          CI_kargyak[1], CI_kargyak[2],
                          CI_karnagy[1], CI_karnagy[2])
    osszesitok[[k]] <- osszesito
  }
  tot_kar_m[,k+1] <- osszesito[,2]*(1-terj) + osszesito[,4] * terj
  tot_szam_m[,k+1] <- osszesito[,1]*(1-terj) + osszesito[,3] * terj
  
}

tot_m[,2:(k+1)] <- tot_kar_m[,2:(k+1)]
tot_m[,(k+2):(2*k+1)] <- tot_szam_m[,2:(k+1)]

deltak <- matrix(0,nrow=44,ncol=24)
for (i in 1:12){
  for (j in 1:nrow(deltak)){
    deltak[j,i] <- (osszesitok[[i]][j,6] - osszesitok[[i]][j,5]) / 2
    deltak[j,i+12] <- (osszesitok[[i]][j,8] - osszesitok[[i]][j,7]) / 2
  }
}
summary(deltak[,1:12]) #mekkora hibával kell számolni a kárgyakoriság becslésénél (delta)
#bármely forgatókönyv esetén másfél ezrelékes pontosság 
summary(deltak[,13:24]) #mekkora hibával kell számolni a kárnagyság becslésénél
#forgatókönyvtől és évtől is erősen függ, de legrosszabb esetben is delta <2000


tot_kar <- as.data.frame(tot_kar_m)
tot_kar$V14 <- karfelhasznalas
names(tot_kar)[1] <- "év"
names(tot_kar)[2:13] <- 1:12
names(tot_kar)[14] <- "Nincs AV"

s <- 0
for (i in 2:13){
  s <- s + tot_kar[44,i]
}
elteres <- (s/12) / tot_kar[44,14] -1

karnagysagok <- melt(tot_kar, id.vars = 'év', variable.name = 'forgatókönyvek')
names(karnagysagok)[3] <- "átlagos_kárfelhasználás"
ggplot(karnagysagok, aes(év,átlagos_kárfelhasználás)) + geom_line(aes(colour = forgatókönyvek), lwd = 1.2)  +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "átlagos kárfelhasználás", breaks = seq(500000,6500000, by=500000)) +
  ggtitle("Egy kárra jutó átlagos kárfelhasználás alakulása") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold')) +
  scale_color_manual(values = as.vector(kelly(15))[-1])

tot_szam <- as.data.frame(tot_szam_m)
tot_szam$V14 <- rep(0.0254,44)
names(tot_szam)[1] <- "év"
names(tot_szam)[2:13] <- 1:12
names(tot_szam)[14] <- "Nincs AV" 

kargyakorisagok <- melt(tot_szam, id.vars = 'év', variable.name = "forgatókönyvek")
names(kargyakorisagok)[3] <- "kárgyakoriság"
ggplot(kargyakorisagok, aes(év,kárgyakoriság)) + geom_line(aes(colour = forgatókönyvek), lwd = 1.2)  +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "kárgyakoriság", breaks = seq(0,0.03, 0.005)) +
  ggtitle("Kárgyakoriság alakulása") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold')) +
  scale_color_manual(values = as.vector(kelly(15))[-1])

legkorabban <- c()
for (i in 2:13){
  for (j in 1:44){
    if (tot_szam[j,i] <= 0.0254*0.9){
      legkorabban[i-1] <- tot_szam[j,1]
      break
    }
  }
}


auto_aranyok <- melt(AV_3[,2:4], id.vars = "év", variable.name = "elterjedés")
auto_aranyok[,1] <- c(rep(seq(2023,2066,by=1),3))
names(auto_aranyok)[1] <- "év"
names(auto_aranyok)[2] <- "elterjedés"
ggplot(auto_aranyok, aes(év,value)) + geom_line(aes(colour = elterjedés), lwd = 1.2) +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "arány", labels = scales::percent, breaks=seq(0,1,0.1)) +
  ggtitle("Autonóm járművek magyarországi arányának becslése") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold'))


#nettó díjak alakulása:
tot_szamok <- tot_kar[,-1]*tot_szam[,-1]
for (i in 1:44){
  for (j in 1:13){
    tot_szamok[i,j] = tot_szamok[i,j]/tot_szamok[i,13]
  }
} 
utolso_ev <- tot_szamok[44,2:13]
s <-  0
for (i in 1:12){
  s = s + utolso_ev[[i]]
}
s <- s / 12
tot_szamok <- cbind(év = c(2023:2066), tot_szamok)

dijak <- melt(tot_szamok, id.vars="év", variable.name="forgatókönyv")
ggplot(dijak, aes(év, value)) + geom_line(aes(colour=forgatókönyv), lwd = 1) +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "", breaks=seq(0,1.5,0.25)) +
  ggtitle("Kárfelhasználás alakulása az autonóm járművek nélküli esethez viszonyítva") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold')) +
  scale_color_manual(values = as.vector(kelly(15))[-1])

osszehasonlitas_AV <- data.frame(év = c(2023:2066))
osszehasonlitas_NAV <- data.frame(év = c(2023:2066))
for (i in 1:12){
  osszehasonlitas_AV[,i+1] <- osszesitok[[i]][,3] * osszesitok[[i]][,4]
  osszehasonlitas_NAV[,i+1] <- osszesitok[[i]][,1] * osszesitok[[i]][,2]
}
osszehasonlitas_AV[,14:25] <- osszehasonlitas_NAV[,2:13]
for(i in 1:12){
  names(osszehasonlitas_AV)[i+1] <- paste0("AV_",as.character(i))
  names(osszehasonlitas_AV)[i+13] <- paste0("hagyományos_",as.character(i))
}

oh <- melt(osszehasonlitas_AV, id.vars="év", variable.name = "forgatókönyv")
ggplot(oh, aes(év,value)) + geom_line(aes(colour=forgatókönyv), lwd=1) +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "",limits=c(0,80000)) +
  ggtitle("Egy szerződésre jutó várható kárkifizetés") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold'))


aranyok <- c()
for (i in 2:13) {
  aranyok[i-1] <- mean(osszehasonlitas_AV[,i+12]/osszehasonlitas_AV[,i])
}
mean(aranyok)

###################################### érzékenységvizsgálat ##########################################
#ez a rész felülírja a változók eddigi értékeit!!!
r <- 500000

tabla <- matrix(0,ncol = 4, nrow = r)
colnames(tabla) <- c("kar_db_hagyomanyos", "osszkar_hagyomanyos", "kar_db_av", "osszkar_av")

osszesito <- matrix(0,ncol = 8, nrow = 44)
colnames(osszesito) <- c("kar_db_hagyomanyos", "osszkar_hagyomanyos", "kar_db_av", "osszkar_av", "kargyak_also", "kargyak_felso", "osszkar_also", "osszkar_felso")

elterj <- list(N,AV_3,AV_5)

forgatokonyvek_m <- matrix(ncol=6, nrow = 12)
forgatokonyvek_m[,1] = rep(3,12)
forgatokonyvek_m[,2] = rep(2,12)
forgatokonyvek_m[,3] = c(rep(3,9),1,5,6)
forgatokonyvek_m[,4] = c(rep(1,6), 2,3,4,rep(1,3))
forgatokonyvek_m[,5] = c(1,2,4,5,rep(2,8))
forgatokonyvek_m[,6] = c(rep(1,4),2,3,rep(1,6))
colnames(forgatokonyvek_m) <- c("elterjedes", "eltolas", "ktg", "ktg_tend", "kitettseg", "tanulas")


av_kargyak_vektor <- c()

tot_m <- matrix(0,nrow=44, ncol = 25)
tot_m[,1] < -2023:2066

tot_szam_m <- matrix(0,nrow=44, ncol = 13 )
tot_szam_m[,1] <- 2023:2066

tot_kar_m <- matrix(0,nrow=44, ncol = 13)
tot_kar_m[,1] <- 2023:2066

deltak_karnagy <- c()
deltak_kargyak <- c()
osszesitok <- list()

kezdes <- Sys.time()
set.seed(13)


for (k in 1:12){
  
  tolas <- elterj[[forgatokonyvek_m[k,2]]]
  terj <- tolas[,forgatokonyvek_m[k,1]]
  nagysag <- ar[forgatokonyvek_m[k,3]]
  nagysag_tendencia <- ar_tendencia[forgatokonyvek_m[k,4]]
  kitett <- 1
  kargyak_nem <- 1
  av_kargyak <- av_kargyak_kezdo
  valtozik <- 0
  
  for (j in 1:44) {
    
    if (terj[j] >0.05) {
      valtozik <- valtozik + 1
      if (nagysag_tendencia <= 1){
        if (nagysag > (ar[forgatokonyvek_m[k,3]]-1)*nagysag_tendencia+1){
          nagysag <- nagysag - 
            (ar[forgatokonyvek_m[k,3]]-((ar[forgatokonyvek_m[k,3]]-1)*ar_tendencia[forgatokonyvek_m[k,4]]+1))/20
        }
      } else {
        if (nagysag < (ar[forgatokonyvek_m[k,3]]-1)*nagysag_tendencia+1){
          nagysag <- nagysag -
            (ar[forgatokonyvek_m[k,3]]-((ar[forgatokonyvek_m[k,3]]-1)*ar_tendencia[forgatokonyvek_m[k,4]]+1))/20
        }
      }
    }
    
    if (valtozik > 0 && valtozik < 9) {
      kitett <- kitett - (1 - kitettseg[forgatokonyvek_m[k,5]])/8
      kargyak_nem <- kargyak_nem - (1 - kargyak_nav[forgatokonyvek_m[k,6]])/8
    }
    
    if (j>=2) {
      av_kargyak <- av_kargyak - (av_kargyak_kezdo - cel)/43
    }
    for (i in 1:r){
      karszam <- rpois(1, v_kargyakorisag*kargyak_nem)
      karszam_av <- rpois(1, v_kargyakorisag*av_kargyak*kitett)
      tabla[i,1] <- karszam
      tabla[i,2] <- sum(rexp(karszam,1/(karfelhasznalas[j]*(terj[j]*nagysag + (1-terj[j])))))
      tabla[i,3] <- karszam_av
      tabla[i,4] <- sum(rexp(karszam_av,1/(karfelhasznalas[j]*(terj[j]*nagysag + (1-terj[j])))))
    }
    
    
    
    szoras_kargyak <- sqrt((1-terj[j])^2 * var(tabla[,1]) +
                             terj[j]^2 *var(tabla[,3]))
    atlag_kargyak <- (1-terj[j])*mean(tabla[,1]) + terj[j]*mean(tabla[,3])
    
    
    CI_kargyak <- c(atlag_kargyak - qnorm(1-0.05/2,0,1) * szoras_kargyak/sqrt(r),
                    atlag_kargyak + qnorm(1-0.05/2,0,1) * szoras_kargyak/sqrt(r))
    delta <- CI_kargyak[2]-atlag_kargyak
    deltak_kargyak[j] <- delta
    
    szoras_karnagy <- sqrt((1-terj[j])^2 * var(tabla[,2]) +
                             terj[j]^2 * var(tabla[,4]))
    
    atlag_karnagy <- (1-terj[j]) * mean(tabla[tabla[,2]>0,2]) +
      terj[j] * mean(tabla[tabla[,4]>0,4])
    
    CI_karnagy <- c(atlag_karnagy - qnorm(1-0.05/2,0,1) * szoras_karnagy/sqrt(r),
                    atlag_karnagy + qnorm(1-0.05/2,0,1) * szoras_karnagy/sqrt(r))
    
    delta_karnagy <- CI_karnagy[2] - atlag_karnagy
    deltak_karnagy[j] <- delta_karnagy
    
    osszesito[j,1:8] <- c(mean(tabla[,1]), mean(tabla[tabla[,2]>0,2]),
                          mean(tabla[,3]), mean(tabla[tabla[,4]>0,4]),
                          CI_kargyak[1], CI_kargyak[2],
                          CI_karnagy[1], CI_karnagy[2])
    osszesitok[[k]] <- osszesito
  }
  tot_kar_m[,k+1] <- osszesito[,2]*(1-terj) + osszesito[,4] * terj
  tot_szam_m[,k+1] <- osszesito[,1]*(1-terj) + osszesito[,3] * terj
  
}

tot_m[,2:(k+1)] <- tot_kar_m[,2:(k+1)]
tot_m[,(k+2):(2*k+1)] <- tot_szam_m[,2:(k+1)]

deltak <- matrix(0,nrow=44,ncol=24)
for (i in 1:12){
  for (j in 1:nrow(deltak)){
    deltak[j,i] <- (osszesitok[[i]][j,6] - osszesitok[[i]][j,5]) / 2
    deltak[j,i+12] <- (osszesitok[[i]][j,8] - osszesitok[[i]][j,7]) / 2
  }
}
summary(deltak[,1:12])
summary(deltak[,13:24]) 

tot_kar <- as.data.frame(tot_kar_m)
tot_kar$V14 <- karfelhasznalas
names(tot_kar)[1] <- "év"
names(tot_kar)[2:13] <- 1:12
names(tot_kar)[14] <- "Nincs AV"

tot_szam <- as.data.frame(tot_szam_m)
tot_szam$V14 <- rep(0.0254,44)
names(tot_szam)[1] <- "év"
names(tot_szam)[2:13] <- 1:12
names(tot_szam)[14] <- "Nincs AV" 


sens_kitettseg <- tot_szam[,1:5]
names(sens_kitettseg)[1] <- "év"
names(sens_kitettseg)[2:5] <- c(0.9,1,1.1,1.15)

sens_kitett <- melt(sens_kitettseg, id.vars = 'év', variable.name = "kitettség")
ggplot(sens_kitett, aes(év,value)) + geom_line(aes(colour = kitettség), lwd = 1.5)  +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "kárgyakoriság", breaks = seq(0,0.03, 0.005)) +
  ggtitle("Kárgyakoriság és kitettség kapcsolata") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold')) +
  scale_color_manual(values = as.vector(kelly(15))[-1])



sens_tanulas <- tot_szam[,c(1,3,6,7)]
names(sens_tanulas)[1] <- "év"
names(sens_tanulas)[2:4] <- c("nincs", "enyhe", "erős")

sens_tan <- melt(sens_tanulas, id.vars = 'év', variable.name = "hatás")

ggplot(sens_tan, aes(év,value)) + geom_line(aes(colour = hatás), lwd = 1.5)  +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "kárgyakoriság", breaks = seq(0,0.03, 0.005)) +
  ggtitle("Kárgyakoriság és vezetési kultúra kapcsolata") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold'))

sens_arvalt <- tot_kar[,c(1,3,8,9,10)]
names(sens_arvalt)[1] <- "év"
names(sens_arvalt)[2:5] <- c("változatlan", "mérsékelt csökkenés", "erős csökkenés", "enyhe növekedés")

sens_trend <- melt(sens_arvalt, id.vars= "év", variable.name='árváltozás')
ggplot(sens_trend, aes(év,value)) + geom_line(aes(colour = árváltozás), lwd = 1.5)  +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "átlagos kárfelhasználás", breaks = seq(500000,7500000, by=500000)) +
  ggtitle("Egy kárra jutó átlagos kárfelhasználás alakulása az árváltozás függvényében") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold')) 

sens_ktg <- tot_kar[,c(1,3,11,12,13)]
names(sens_ktg)[1] <- "év"
names(sens_ktg)[2:5] <- c(2.1, 1.5, 2.7, 3)

sens_ar <- melt(sens_ktg, id.vars="év", variable.name = "Kezdeti_ár")

ggplot(sens_ar, aes(év,value)) + geom_line(aes(colour = Kezdeti_ár), lwd = 1.5)  +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "átlagos kárfelhasználás", breaks = seq(500000,7500000, by=500000)) +
  ggtitle("Egy kárra jutó átlagos kárfelhasználás alakulása a kezdeti ár függvényében") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold')) 

######################################################## érzékenységvizsgálat - önvezetők kárgyakorisága ###########################################

r <- 500000

tabla <- matrix(0,ncol = 4, nrow = r)
colnames(tabla) <- c("kar_db_hagyomanyos", "osszkar_hagyomanyos", "kar_db_av", "osszkar_av")

osszesito <- matrix(0,ncol = 8, nrow = 44)
colnames(osszesito) <- c("kar_db_hagyomanyos", "osszkar_hagyomanyos", "kar_db_av", "osszkar_av", "kargyak_also", "kargyak_felso", "osszkar_also", "osszkar_felso")

elterj <- list(N,AV_3,AV_5)

forgatokonyvek_m <- matrix(ncol=6, nrow = 12)
forgatokonyvek_m[,1] = rep(3,12)
forgatokonyvek_m[,2] = rep(2,12)
forgatokonyvek_m[,3] = c(rep(3,9),1,5,6)
forgatokonyvek_m[,4] = c(rep(1,6), 2,3,4,rep(1,3))
forgatokonyvek_m[,5] = c(1,2,4,5,rep(2,8))
forgatokonyvek_m[,6] = c(rep(1,4),2,3,rep(1,6))
colnames(forgatokonyvek_m) <- c("elterjedes", "eltolas", "ktg", "ktg_tend", "kitettseg", "tanulas")


av_kargyak_kezdok <- c(rep(0.4,3),rep(0.3,3),rep(0.2,3),rep(0.15,3))
celok <- c(0.15,0.1,0.05,0.15,0.1,0.05,0.15,0.1,0.05,0.15,0.1,0.05)


tot_m <- matrix(0,nrow=44, ncol = 25)
tot_m[,1] < -2023:2066

tot_szam_m <- matrix(0,nrow=44, ncol = 13 )
tot_szam_m[,1] <- 2023:2066

tot_kar_m <- matrix(0,nrow=44, ncol = 13)
tot_kar_m[,1] <- 2023:2066

deltak_karnagy <- c()
deltak_kargyak <- c()
osszesitok <- list()

kezdes <- Sys.time()
set.seed(13)


for (k in 1:12){
  
  tolas <- elterj[[forgatokonyvek_m[k,2]]]
  terj <- tolas[,forgatokonyvek_m[k,1]]
  nagysag <- ar[forgatokonyvek_m[k,3]]
  nagysag_tendencia <- ar_tendencia[forgatokonyvek_m[k,4]]
  kitett <- 1
  kargyak_nem <- 1
  av_kargyak <- av_kargyak_kezdok[k]
  valtozik <- 0
  
  for (j in 1:44) {
    
    if (terj[j] >0.05) {
      valtozik <- valtozik + 1
      if (nagysag_tendencia <= 1){
        if (nagysag > (ar[forgatokonyvek_m[k,3]]-1)*nagysag_tendencia+1){
          nagysag <- nagysag - 
            (ar[forgatokonyvek_m[k,3]]-((ar[forgatokonyvek_m[k,3]]-1)*ar_tendencia[forgatokonyvek_m[k,4]]+1))/20
        }
      } else {
        if (nagysag < (ar[forgatokonyvek_m[k,3]]-1)*nagysag_tendencia+1){
          nagysag <- nagysag -
            (ar[forgatokonyvek_m[k,3]]-((ar[forgatokonyvek_m[k,3]]-1)*ar_tendencia[forgatokonyvek_m[k,4]]+1))/20
        }
      }
    }
    if (valtozik > 0 && valtozik < 9) {
      kitett <- kitett - (1 - kitettseg[forgatokonyvek_m[k,5]])/8
      kargyak_nem <- kargyak_nem - (1 - kargyak_nav[forgatokonyvek_m[k,6]])/8
    }
    
    if (j>=2) {
      av_kargyak <- av_kargyak - (av_kargyak_kezdok[k] - celok[k])/43
    }
    for (i in 1:r){
      karszam <- rpois(1, v_kargyakorisag*kargyak_nem)
      karszam_av <- rpois(1, v_kargyakorisag*av_kargyak*kitett)
      tabla[i,1] <- karszam
      tabla[i,2] <- sum(rexp(karszam,1/(karfelhasznalas[j]*(terj[j]*nagysag + (1-terj[j])))))
      tabla[i,3] <- karszam_av
      tabla[i,4] <- sum(rexp(karszam_av,1/(karfelhasznalas[j]*(terj[j]*nagysag + (1-terj[j])))))
    }
    
    
    
    szoras_kargyak <- sqrt((1-terj[j])^2 * var(tabla[,1]) +
                             terj[j]^2 *var(tabla[,3]))
    atlag_kargyak <- (1-terj[j])*mean(tabla[,1]) + terj[j]*mean(tabla[,3])
    
    
    CI_kargyak <- c(atlag_kargyak - qnorm(1-0.05/2,0,1) * szoras_kargyak/sqrt(r),
                    atlag_kargyak + qnorm(1-0.05/2,0,1) * szoras_kargyak/sqrt(r))
    delta <- CI_kargyak[2]-atlag_kargyak
    deltak_kargyak[j] <- delta
    
    szoras_karnagy <- sqrt((1-terj[j])^2 * var(tabla[,2]) +
                             terj[j]^2 * var(tabla[,4]))
    
    atlag_karnagy <- (1-terj[j]) * mean(tabla[tabla[,2]>0,2]) +
      terj[j] * mean(tabla[tabla[,4]>0,4])
    
    CI_karnagy <- c(atlag_karnagy - qnorm(1-0.05/2,0,1) * szoras_karnagy/sqrt(r),
                    atlag_karnagy + qnorm(1-0.05/2,0,1) * szoras_karnagy/sqrt(r))
    
    delta_karnagy <- CI_karnagy[2] - atlag_karnagy
    deltak_karnagy[j] <- delta_karnagy
    
    osszesito[j,1:8] <- c(mean(tabla[,1]), mean(tabla[tabla[,2]>0,2]),
                          mean(tabla[,3]), mean(tabla[tabla[,4]>0,4]),
                          CI_kargyak[1], CI_kargyak[2],
                          CI_karnagy[1], CI_karnagy[2])
    osszesitok[[k]] <- osszesito
  }
  tot_kar_m[,k+1] <- osszesito[,2]*(1-terj) + osszesito[,4] * terj
  tot_szam_m[,k+1] <- osszesito[,1]*(1-terj) + osszesito[,3] * terj
  
}

tot_m[,2:(k+1)] <- tot_kar_m[,2:(k+1)]
tot_m[,(k+2):(2*k+1)] <- tot_szam_m[,2:(k+1)]

deltak <- matrix(0,nrow=44,ncol=24)
for (i in 1:12){
  for (j in 1:nrow(deltak)){
    deltak[j,i] <- (osszesitok[[i]][j,6] - osszesitok[[i]][j,5]) / 2
    deltak[j,i+12] <- (osszesitok[[i]][j,8] - osszesitok[[i]][j,7]) / 2
  }
}
summary(deltak[,1:12]) 
summary(deltak[,13:24]) 


tot_kar <- as.data.frame(tot_kar_m)
tot_kar$V14 <- karfelhasznalas
names(tot_kar)[1] <- "év"
names(tot_kar)[2:13] <- 1:12
names(tot_kar)[14] <- "Nincs AV"


tot_szam <- as.data.frame(tot_szam_m)
tot_szam$V14 <- rep(0.0254,44)
names(tot_szam)[1] <- "év"
names(tot_szam)[2:13] <- 1:12
names(tot_szam)[14] <- "Nincs AV" 


#nettó díjak alakulása:
tot_szamok <- tot_kar[,-1]*tot_szam[,-1]
for (i in 1:44){
  for (j in 1:13){
    tot_szamok[i,j] = tot_szamok[i,j]/tot_szamok[i,13]
  }
} 
utolso_ev <- tot_szamok[44,2:13]
s <-  0
for (i in 1:12){
  s = s + utolso_ev[[i]]
}
s <- s / 12
tot_szamok <- cbind(év = c(2023:2066), tot_szamok)

dijak <- melt(tot_szamok, id.vars="év", variable.name="forgatókönyv")
ggplot(dijak, aes(év, value)) + geom_line(aes(colour=forgatókönyv), lwd = 1) +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "", breaks=seq(0,1.5,0.25)) +
  ggtitle("Kárfelhasználás alakulása az autonóm járművek nélküli esethez viszonyítva") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold')) +
  scale_color_manual(values = as.vector(kelly(15))[-1])

osszehasonlitas_AV <- data.frame(év = c(2023:2066))
osszehasonlitas_NAV <- data.frame(év = c(2023:2066))
for (i in 1:12){
  osszehasonlitas_AV[,i+1] <- osszesitok[[i]][,3] * osszesitok[[i]][,4]
  osszehasonlitas_NAV[,i+1] <- osszesitok[[i]][,1] * osszesitok[[i]][,2]
}
for(i in 1:12){
  names(osszehasonlitas_AV)[i+1] <- paste0("AV_",as.character(i))
}


oh <- melt(osszehasonlitas_AV, id.vars="év", variable.name = "forgatókönyv")
ggplot(oh, aes(év,value)) + geom_line(aes(colour=forgatókönyv), lwd=1) +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "",limits=c(0,25000)) +
  ggtitle("Egy szerződésre jutó várható kárkifizetés") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold'))

osszehasonlitas_AV[,14:25] <- osszehasonlitas_NAV[,2:13]
for(i in 1:12){
  names(osszehasonlitas_AV)[i+1] <- paste0("AV_",as.character(i))
  names(osszehasonlitas_AV)[i+13] <- paste0("hagyományos_",as.character(i))
}

oh <- melt(osszehasonlitas_AV, id.vars="év", variable.name = "forgatókönyv")
ggplot(oh, aes(év,value)) + geom_line(aes(colour=forgatókönyv), lwd=1) +
  scale_x_continuous(name = "év", breaks = seq(2023,2066,by=3)) +
  scale_y_continuous(name = "",limits=c(0,80000)) +
  ggtitle("Egy szerződésre jutó várható kárkifizetés") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title =  element_text(size = 15, face = 'bold'))
