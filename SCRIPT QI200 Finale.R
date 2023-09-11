#Data analysis
#Librerie utili
library(limer)
library(dplyr) 
library(tidyr) 
library(pwr) 
library(nFactors) 
library(psych) 
library(GPArotation) 
library(plotrix)
library(corrtable)
library(table1)
library(car)
library(lsr)
library(readr)

#load dati
dati <- read_csv("dati.csv")
dati <- as.data.frame(dati)

# Rimozione NA
dati <- dati[which(is.na(dati$lastpage)==FALSE),] 

# Rename e ricodifica variabili
#1. Attention checks
dati <- dati %>% rename(mc1 = CONTMEDN2.MC1.) 
dati <- dati %>% rename(mc2 = ATCOS0.MC2.) 
dati <- dati %>% rename(mc3 = INT0.MC3.)

#2. Contatti
dati <- dati %>% rename(CONTMEDP.MEDPOS3. = CONTMEDP.MEDPOS4.)
dati <- dati %>% rename(CON_MED_F = CONTMEDF.MEDF.)
dati <- dati %>% rename(CON_IND_F = CONTINDF.INDF.)
dati <- dati %>% rename(CON_DIR_F = CONTDF.DF.)
dati <- dati %>% rename(CON_MED_AMB = CONTMEDAIUTARSI.MEDAMB.)

#3. Risposte variabile Int (Intenzione all'opposizione collettiva)
dati$mc3<- as.numeric(gsub("A", "", dati$mc3))
dati$INT0.INT1.<- as.numeric(gsub("A", "", dati$INT0.INT1.))
dati$INT0.INT2.<- as.numeric(gsub("A", "", dati$INT0.INT2.))
dati$INT0.INT3.<- as.numeric(gsub("A", "", dati$INT0.INT3.))
dati$INT0.INT4.<- as.numeric(gsub("A", "", dati$INT0.INT4.))

#5. Gender
dati[which(dati$SESSO%in%"0"), "SEX_dummy"]=0 #referente maschi
dati[which(dati$SESSO%in%"1"), "SEX_dummy"]=1 #comparazione
#non codifichiamo "altro" perchè è 1 sola persona e non rappresentativa

#6. Occupazione
unique(dati$Occupazione)
dati$Occup = dati$Occupazione

for (i in 1:length(dati$Occup)) {
  if (grepl("^Stud|^stud", dati$Occup[i])) {
    dati$Occup[i] = "Studenti"
  } else {
    dati$Occup[i] = "Comunità"
  }
}
print(dati$Occup)
table(dati$Occup)

# Filtro controllo attention checks
dati$contr_1 = dati$contr_2 = dati$contr_3 = 1 
dati[which(dati[, "mc1"] == 2),"contr_1"] = 0 
dati[which(dati[, "mc2"] == 4),"contr_2"] = 0 
dati[which(dati[, "mc3"] == 6),"contr_3"] = 0
dati$checks=100*rowMeans(dati[, c("contr_1","contr_2","contr_3")], na.rm=T)

# Filtro manipulation check: teniamo chi ha risposto correttamente al primo manipulation check 
dati <- dati[which(dati$mc1%in%"2"),]

#Item reverse
dati[, "SCB0.SCB4."] = 8 - dati[, "SCB0.SCB4."]

#Creazione vettori di scale
med_tot_items <- c("CONTMEDP.MEDPOS1.","CONTMEDP.MEDPOS2.","CONTMEDP.MEDPOS3.",
                   "CON_MED_AMB","CONTMEDN.MEDNEG1.","CONTMEDN.MEDNEG2.",
                   "CONTMEDN.MEDNEG3.","CONTMEDN2.MEDNEG4.")
ind_tot <- c("CONTINDVP.INDPOS1.","CONTINDVP.INDPOS2.","CONTINDVP.INDPOS3.","CONTINDVN.INDNEG1.","CONTINDVN.INDNEG2.",    
             "CONTINDVN.INDNEG3.")
dir_tot <- c("CONTDPOS.DPOS1." ,"CONTDPOS.DPOS2.","CONTDPOS.DPOS3.",        
             "CONTDNEG.DNEG1.","CONTDNEG.DNEG2." ,"CONTDNEG.DNEG3.")

#Medie scale
med_pos_items <- c("CONTMEDP.MEDPOS1.", "CONTMEDP.MEDPOS2.", "CONTMEDP.MEDPOS3.","CON_MED_AMB")
dati$Med_Pos <- rowMeans(dati[, med_pos_items])

med_neg_items <- c("CONTMEDN.MEDNEG1.", "CONTMEDN.MEDNEG2.", "CONTMEDN.MEDNEG3.", 
                   "CONTMEDN2.MEDNEG4.")
dati$Med_Neg <- rowMeans(dati[, med_neg_items])

ind_pos_items <- c("CONTINDVP.INDPOS1.", "CONTINDVP.INDPOS2.", "CONTINDVP.INDPOS3.")
dati$Ind_Pos <- rowMeans(dati[, ind_pos_items])

ind_neg_items <- c("CONTINDVN.INDNEG1.", "CONTINDVN.INDNEG2.", "CONTINDVN.INDNEG3.")
dati$Ind_Neg <- rowMeans(dati[, ind_neg_items])

dir_pos_items <- c("CONTDPOS.DPOS1.", "CONTDPOS.DPOS2.", "CONTDPOS.DPOS3.")
dati$Dir_Pos <- rowMeans(dati[, dir_pos_items])

dir_neg_items <- c("CONTDNEG.DNEG1.","CONTDNEG.DNEG2.","CONTDNEG.DNEG3.")
dati$Dir_Neg <- rowMeans(dati[, dir_neg_items])

jmc_items <- c("JMC0.JMC1.","JMC0.JMC2.",     
               "JMC0.JMC3.","JMC0.JMC4.","JMC0.JMC5.","JMC0.JMC6.",
               "JMC0.JMC7.","JMC0.JMC8.")
dati$JMC <- rowMeans(dati[, jmc_items])

rom_items <- c("ROM0.ROM1.","ROM0.ROM2.","ROM0.ROM3.","ROM0.ROM4.")
dati$ROM <- rowMeans(dati[, rom_items])

atcos_items <- c("ATCOS0.ATCOS1.","ATCOS0.ATCOS2.","ATCOS0.ATCOS3.","ATCOS0.ATCOS4.")
dati$ATCOS <- rowMeans(dati[, atcos_items])

scb_items <- c("SCB0.SCB1.","SCB0.SCB2.","SCB0.SCB3.","SCB0.SCB4.")
dati$SCB <- rowMeans(dati[, scb_items])

threat_items <- c("MIN0.THREAT1.", "MIN0.THREAT2.", "MIN0.THREAT3.", "MIN0.THREAT4.",
                  "MIN0.THREAT5.")
dati$THREAT <- rowMeans(dati[, threat_items])

him_items <- c("ONORE0.ONORE1.", "ONORE0.ONORE2.", "ONORE0.ONORE3.", "ONORE0.ONORE4.",
               "ONORE0.ONORE5.", "ONORE0.ONORE6.", "ONORE0.ONORE7.", "ONORE0.ONORE8.")
dati$HIM <- rowMeans(dati[, him_items])

int_items <- c ("INT0.INT1.", "INT0.INT2.", "INT0.INT3.", "INT0.INT4.")
dati$INT=rowMeans(dati[, int_items])

norm_atcos_items <- c("METAATT.META1.", "METAATT.META2.", "METAATT.META3.",
                      "METAATT.META4.")
dati$NORM_ATT <- rowMeans(dati[, norm_atcos_items])

#Analisi Descrittive

#1. Analisi scale
describe(dati[,c("Med_Pos","Med_Neg","Ind_Pos","Ind_Neg","Dir_Pos","Dir_Neg",           
                 "JMC","ROM","ATCOS","SCB","THREAT",            
                 "HIM","INT","NORM_ATT")])

#2. Analisi sociodemo
table1::table1(~ ETA + ISTR | Occup, data = dati, 
               digits = 1, caption = "Descrittive sociodemo", 
               overall = "Totale",
               na.rm = T,
               format.number = TRUE)

#3. Occupazione
table_occup = table(dati$Occup)
(prop.table(table_occup) %>% round(2) * 100)

#4. Istruzione
table_istr = table(dati$ISTR)
(prop.table(table_istr) %>% round(2) * 100)

#5. Genere
table_gen = table(dati$SESSO)
(prop.table(table_gen) %>% round(2) * 100)
gen_lbls <- c("Maschio", "Femmina", "Altro")
gen_pie <- pie3D(table(dati$SESSO), labels=gen_lbls, 
                 explode=0.1, radius = 0.9, 
                 col = c("#ddaa00", "brown", "pink"), 
                 main="Genere")

# Parallel Analysis & FA

#1. Scala contatto mediatico
ev <- eigen(cor(dati[, med_tot_items],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, med_tot_items]),var=ncol(dati[, med_tot_items]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, med_tot_items], nfactors = 2, rotate = "promax")
print(fa(dati[, med_tot_items], nfactors = 2, rotate = "promax"), sort = T, cut = .2)
fa.diagram(fa(dati[, med_tot_items], nfactors = 2, rotate = "promax"), main = "Analisi fattoriale: Contatto Mediatico")
#i fattori correlano tra di loro di .4, non sono indipendenti

#2. Scala Contatto indiretto
ev <- eigen(cor(dati[, ind_tot],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, ind_tot]),var=ncol(dati[, ind_tot]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, ind_tot], nfactors = 2, rotate = "promax")
print(fa(dati[, ind_tot], nfactors = 2, rotate = "promax"), sort = T, cut = .2)
fa.diagram(fa(dati[, ind_tot], nfactors = 2, rotate = "promax"), main = "Analisi fattoriale: Contatto Indiretto")
#i fattori correlano tra di loro di .6, non sono indipendenti

#3. Scala Contatto diretto
ev <- eigen(cor(dati[, dir_tot],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, dir_tot]),var=ncol(dati[, dir_tot]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
 
fa(dati[, dir_tot], nfactors = 2, rotate = "varimax")
print(fa(dati[, dir_tot], nfactors = 2, rotate = "varimax"), sort = T, cut = .2)

fa.diagram(fa(dati[, dir_tot], nfactors = 2, rotate = "varimax"), main = "Analisi fattoriale: Contatto Diretto")
#qui i fattori non correlano quindi sono indipendenti

#4. Scala Romanticizzazione
ev <- eigen(cor(dati[, rom_items],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, rom_items]),var=ncol(dati[, rom_items]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, rom_items], nfactors = 1, rotate = "varimax")
print(fa(dati[, rom_items], nfactors = 1, rotate = "varimax"), sort = T, cut = .2)

fa.diagram(fa(dati[, rom_items], nfactors = 1, rotate = "varimax"), main = "Analisi fattoriale: Romanticizzazione")

#5. Scala Atteggiamento vs COs
ev <- eigen(cor(dati[, atcos_items],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, atcos_items]),var=ncol(dati[, atcos_items]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, atcos_items], nfactors = 1, rotate = "varimax")
print(fa(dati[, atcos_items], nfactors = 1, rotate = "varimax"), sort = T, cut = .2)

fa.diagram(fa(dati[, atcos_items], nfactors = 1, rotate = "varimax"), main = "Analisi fattoriale: Atteggiamento vs COs")

#6. Scala Social Change Belief
ev <- eigen(cor(dati[, scb_items],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, scb_items]),var=ncol(dati[, scb_items]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, scb_items], nfactors = 1, rotate = "varimax")
print(fa(dati[, scb_items], nfactors = 1, rotate = "varimax"), sort = T, cut = .2)

fa.diagram(fa(dati[, scb_items], nfactors = 1, rotate = "varimax"), main = "Analisi fattoriale: Social Change Belief") #scb4 non satura nel fattore della scala,
#ma per un singolo item lo teniamo 

#7. Scala Minaccia
ev <- eigen(cor(dati[, threat_items],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, threat_items]),var=ncol(dati[, threat_items]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, threat_items], nfactors = 1, rotate = "varimax")
print(fa(dati[, threat_items], nfactors = 1, rotate = "varimax"), sort = T, cut = .2)

fa.diagram(fa(dati[, threat_items], nfactors = 1, rotate = "varimax"), main = "Analisi fattoriale: Minaccia")

#8. Scala Intenzione all'opposizione collettiva
ev <- eigen(cor(dati[, int_items],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, int_items]),var=ncol(dati[, int_items]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, int_items], nfactors = 1, rotate = "varimax")
print(fa(dati[, int_items], nfactors = 1, rotate = "varimax"), sort = T, cut = .2)

fa.diagram(fa(dati[, int_items], nfactors = 1, rotate = "varimax"), main = "Analisi fattoriale: Intenzione ad opporsi")

#9. Scala Atteggiamento normativo
ev <- eigen(cor(dati[, norm_atcos_items],use = "pairwise.complete.obs")) 
ap <- parallel(subject=nrow(dati[, norm_atcos_items]),var=ncol(dati[, norm_atcos_items]),
                rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa(dati[, norm_atcos_items], nfactors = 1, rotate = "varimax")
print(fa(dati[, norm_atcos_items], nfactors = 1, rotate = "varimax"), sort = T, cut = .2)

fa.diagram(fa(dati[, norm_atcos_items], nfactors = 1, rotate = "varimax"), main = "Analisi fattoriale: Atteggiamento normativo")

#Reliability

#1. Creazione vettori contatti totali con items frequenza
med_tot_items_rel <- c("CONTMEDP.MEDPOS1.","CONTMEDP.MEDPOS2.","CONTMEDP.MEDPOS3.",
                       "CON_MED_AMB","CONTMEDN.MEDNEG1.","CONTMEDN.MEDNEG2.",
                       "CONTMEDN.MEDNEG3.","CONTMEDN2.MEDNEG4.", "CON_MED_F")
ind_tot_rel <- c("CONTINDVP.INDPOS1.","CONTINDVP.INDPOS2.","CONTINDVP.INDPOS3.","CONTINDVN.INDNEG1.","CONTINDVN.INDNEG2.",    
                 "CONTINDVN.INDNEG3.", "CON_IND_F")
dir_tot_rel <- c("CONTDPOS.DPOS1." ,"CONTDPOS.DPOS2.","CONTDPOS.DPOS3.",        
                 "CONTDNEG.DNEG1.","CONTDNEG.DNEG2." ,"CONTDNEG.DNEG3.", "CON_DIR_F")

#2. Analisi reliability
psych::alpha(dati[, med_tot_items_rel])
psych::alpha(dati[, med_pos_items])
psych::alpha(dati[, med_neg_items])
psych::alpha(dati[, ind_tot_rel])
psych::alpha(dati[, ind_pos_items])
psych::alpha(dati[, ind_neg_items])
psych::alpha(dati[, dir_tot_rel])
psych::alpha(dati[, dir_pos_items])
psych::alpha(dati[, dir_neg_items])
psych::alpha(dati[, jmc_items])
psych::alpha(dati[, rom_items]) 
psych::alpha(dati[, atcos_items])
psych::alpha(dati[, scb_items])
psych::alpha(dati[, threat_items])
psych::alpha(dati[, him_items])
psych::alpha(dati[, int_items])
psych::alpha(dati[, norm_atcos_items])

#Correlation matrix
cor_matrix <- correlation_matrix(dati[,c( "Med_Pos", "Med_Neg", "Ind_Pos", "Ind_Neg", "Dir_Pos",  
                                          "Dir_Neg", "JMC", "ROM",  "ATCOS",  "SCB", "THREAT",  
                                          "HIM", "INT", "NORM_ATT")], 
                                 digits = 2, 
                                 use = "upper", 
                                 replace_diagonal = T)
cor_matrix

#--------------------------Modello Trav: replica ipotesi sul contatto di Trav

#1. Romanticizzazione
modello_rom_T= lm(ROM ~ Dir_Pos   +  Dir_Neg + CON_DIR_F + HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_rom_T) 
# rispetto a dati Trav troviamo Istruzione significativi
F2ROM1= 0.09655/(1-0.09655)
F2ROM1 #.10

#Assunzioni
par(mfrow = c(2,2))
plot(modello_rom_T) 
vif(modello_rom_T) 

#Beta
standardCoefs(modello_rom_T)

#2. Minaccia
modello_threat_T = lm(THREAT ~ Dir_Pos  +  Dir_Neg  + CON_DIR_F + HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_threat_T)
# rispetto a Trav troviamo HIM significativo, i maschi hanno punteggio + alto delle femmine di 0.13 e istruzione significativa
F2THREAT1= 0.06514/ (1- 0.06514)
F2THREAT1 #.07

#Assunzioni
par(mfrow = c(2,2))
plot(modello_threat_T) 
vif(modello_threat_T) 

#Beta
standardCoefs(modello_threat_T)

#3. Intenzione all'opposizione collettiva
modello_int_T= lm(INT ~ Dir_Pos  +  Dir_Neg  + CON_DIR_F + HIM + ROM + THREAT + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_int_T)
# rispetto a Trav Rom non è significativo! Dir_Neg è significativo! Le femmine hanno punteggio + alto dei maschi di 0.36
F2INT1=  0.1986 / (1-  0.1986 )
F2INT1 #.24

#Assunzioni
par(mfrow = c(2,2))
plot(modello_int_T)
vif(modello_int_T) 

#Beta
standardCoefs(modello_int_T)

#4. Contatto diretto positivo
modello_Dir_pos_T= lm(Dir_Pos ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Dir_pos_T) 
# HIM significativo come nel modello Trav
F2pos1= 0.05373/(1-0.05373)
F2pos1 #.056

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Dir_pos_T) 
vif(modello_Dir_pos_T)

#Beta
standardCoefs(modello_Dir_pos_T)

#5. Contatto diretto negativo 
modello_Dir_neg_T= lm(Dir_Neg ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Dir_neg_T) 
# HIM NON significativo come modello Trav
F2neg1= 0.04223/(1-0.04223)
F2neg1 #.04

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Dir_neg_T)
vif(modello_Dir_neg_T)

#Beta 
standardCoefs(modello_Dir_neg_T)

#Regressioni
#--------------------------------Modello QI200: Modelli Parziali
modello1= lm(INT ~ Med_Pos + Med_Neg + CON_MED_F, data=dati)
summary(modello1)

modello2= lm(INT ~ Ind_Pos + Ind_Neg + CON_IND_F, data=dati)
summary(modello2)

modello3= lm(NORM_ATT ~ Med_Pos + Med_Neg + CON_MED_F, data=dati)
summary(modello3)

modello4= lm(SCB ~ Med_Pos + Med_Neg  + CON_MED_F, data=dati)
summary(modello4)

modello5= lm(SCB ~ Ind_Pos + Ind_Neg  + CON_IND_F, data=dati)
summary(modello5)

modello6= lm(ATCOS ~ Med_Pos + Med_Neg + CON_MED_F, data=dati)
summary(modello6)

modellog= lm(ATCOS ~ Med_Pos, data=dati)
summary(modellog)

modellogg= lm(ATCOS ~ Med_Neg, data=dati)
summary(modellogg)

modello7= lm(JMC ~ Med_Pos + Med_Neg + CON_MED_F, data=dati)
summary(modello7)

modello8= lm(JMC ~ Ind_Pos + Ind_Neg + CON_IND_F, data=dati)
summary(modello8)

modello9= lm(THREAT ~ Med_Pos + Med_Neg + CON_MED_F, data=dati)
summary(modello9)

modello10= lm(THREAT ~ Ind_Pos + Ind_Neg + CON_IND_F, data=dati)
summary(modello10)

modello11= lm(ROM ~ Med_Pos + Med_Neg  + CON_MED_F, data=dati)
summary(modello11)

modello12= lm(ROM ~ Ind_Pos + Ind_Neg  + CON_IND_F, data=dati)
summary(modello12)

#--------------------------------Modello QI200: testiamo la nostra ipotesi

#1. Intenzione all'opposizione collettiva 
modelloINT= lm(INT ~ Med_Pos + Med_Neg + Ind_Pos  + Ind_Neg  + ROM + 
                 Dir_Pos + Dir_Neg + HIM + CON_MED_F + CON_IND_F + CON_DIR_F + 
                 JMC + ATCOS + SCB + THREAT + NORM_ATT + SEX_dummy + ETA + ISTR, data=dati)
summary(modelloINT)
# HIM, JMC, frequenza del diretto, med neg e Threat significativi. Anche qui ROM non predice intenzione.
# SCB e ATCOS non male ma non significativi. Femmine punteggio + alto dei maschi e significativo
F2INT= 0.255/ (1- 0.255)    #.28 togliendo tutto
F2INT #.34

#Assunzioni
par(mfrow = c(2,2))
plot(modelloINT)
vif(modelloINT)

#Beta
standardCoefs(modelloINT)

#2. Atteggiamento normativo 
modelloNORMATT= lm(NORM_ATT ~ Med_Pos + Med_Neg + Ind_Pos  + Ind_Neg  +  
                     Dir_Pos   +  Dir_Neg  + HIM + CON_MED_F + CON_IND_F + CON_DIR_F + SEX_dummy + ETA + ISTR, data=dati)
summary(modelloNORMATT)
#Ind pos e med pos significativi
F2META= 0.1015/ (1- 0.1015)
F2META #.11

# Assunzioni
par(mfrow = c(2,2))
plot(modelloNORMATT)
vif(modelloNORMATT) 

#Beta
standardCoefs(modelloNORMATT)

#3. Romanticizzazione
modelloROM= lm(ROM ~ Med_Pos + Med_Neg + Ind_Pos  + Ind_Neg  +  
                 Dir_Pos   +  Dir_Neg  + HIM + CON_MED_F + CON_IND_F + CON_DIR_F + SEX_dummy + ETA + ISTR, data=dati)
summary(modelloROM)
# b di mediatico e indiretto interessanti! HIM predice romanticizzazione, 
#l'Istruzione è significativa positiva!
F2ROM= 0.1631/ (1- 0.1631)
F2ROM #.19

#Assunzioni
par(mfrow = c(2,2))
plot(modelloROM)
vif(modelloROM) 

#Beta
standardCoefs(modelloROM)

#4. Social change belief 
modelloSCB= lm(SCB ~ Med_Pos + Med_Neg + Ind_Pos  + Ind_Neg  +  
                 Dir_Pos   +  Dir_Neg  + HIM + CON_MED_F + CON_IND_F + CON_DIR_F + SEX_dummy + ETA + ISTR, data=dati)
summary(modelloSCB)
# HIM come in modello Trava
F2SCB= 0.04475/ (1- 0.04475)
F2SCB #.04

# Assunzioni
par(mfrow = c(2,2))
plot(modelloSCB) 
vif(modelloSCB) 

#Beta
standardCoefs(modelloSCB)

#5. Atteggiamento 
modelloATCOS= lm(ATCOS ~ Med_Pos + Med_Neg + Ind_Pos  + Ind_Neg  +  
                   Dir_Pos   +  Dir_Neg  + HIM + CON_MED_F + CON_IND_F + CON_DIR_F + SEX_dummy + ETA + ISTR, data=dati)
summary(modelloATCOS)

# Dir Pos, ind pos e med pos significativ, HIM forte e significativo
F2ATCOS= 0.1414/ (1- 0.1414)
F2ATCOS #.16

#Assunzioni
par(mfrow = c(2,2))
plot(modelloATCOS)
vif(modelloATCOS) 

#Beta
standardCoefs(modelloATCOS)

#6. Justification of mafia collution 
modelloJMC= lm(JMC ~ Med_Pos + Med_Neg + Ind_Pos  + Ind_Neg  +  
                 Dir_Pos   +  Dir_Neg  + HIM + CON_MED_F + CON_IND_F + CON_DIR_F + SEX_dummy + ETA + ISTR, data=dati)
summary(modelloJMC)
# significativo e buono HIM, dir pos non male e Femmine punteggi più alti dei maschi di .22
F2JMC= 0.08414/ (1- 0.08414)
F2JMC #.09

#Assunzioni
par(mfrow = c(2,2))
plot(modelloJMC)
vif(modelloJMC) 

#Beta
standardCoefs(modelloJMC)

#7. Minaccia
modelloTHREAT= lm(THREAT ~ Med_Pos + Med_Neg + Ind_Pos  + Ind_Neg  +  
                    Dir_Pos + Dir_Neg  + HIM + CON_MED_F + CON_IND_F + CON_DIR_F + SEX_dummy + ETA + ISTR, data=dati)
summary(modelloTHREAT)
# HIM significativo, frequenza mediatico significativo ma basso, Femmine punteggi più bassi dei maschi di .13
# Istruzione significativa 
F2THREAT=  0.08844/ (1-  0.08844)
F2THREAT #.09

#Assunzioni
par(mfrow = c(2,2))
plot(modelloTHREAT)
vif(modelloTHREAT) 

#Beta
standardCoefs(modelloTHREAT)

#8. Contatto mediatico vicario positivo
modello_Med_pos_= lm(Med_Pos ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Med_pos_) 
# HIM predice mediatico pos
F2medpos= 0.02022/(1-0.02022)
F2medpos # .02

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Med_pos_) 
vif(modello_Med_pos_)

#Beta
standardCoefs(modello_Med_pos_)


#9. Contatto mediatico negativo 
modello_Med_neg_= lm(Med_Neg ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Med_neg_) 
# b di HIM non significativo, come nel contatto diretto negativo infatti
F2medneg= 0.003706/(1-0.003706)
F2medneg # .003 modello non worka

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Med_neg_) 
vif(modello_Med_neg_)

#Beta
standardCoefs(modello_Med_neg_)

#10. Contatto indiretto positivo 
modello_Ind_pos_= lm(Ind_Pos ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Ind_pos_)
# HIM significativo con Ind pos
F2indpos= 0.04883/(1-0.04883)
F2indpos # .05

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Ind_pos_) 
vif(modello_Ind_pos_)

#Beta
standardCoefs(modello_Ind_pos_)

#11. Contatto indiretto negativo 
modello_Ind_neg_= lm(Ind_Neg ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Ind_neg_) 
# qui HIM significativo e non male con ind Neg anche se effect size modello piccolo
F2indneg= 0.0277/(1-0.0277)
F2indneg # .02

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Ind_neg_) 
vif(modello_Ind_neg_)

#Beta
standardCoefs(modello_Ind_neg_)

#12. Contatto diretto positivo (ripetiamo i modelli per completezza)
modello_Dir_pos_= lm(Dir_Pos ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Dir_pos_) 
# HIM qui significativo come modello Trav e sex dummy
F2dirpos= 0.05373/(1-0.05373)
F2dirpos # .05

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Dir_pos_) 
vif(modello_Dir_pos_)

#Beta
standardCoefs(modello_Dir_pos_)

#13. Contatto diretto negativo 
modello_Dir_neg_= lm(Dir_Neg ~ HIM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello_Dir_neg_) 
# qui HIM NON significativo come modello Trav
F2dirneg= 0.04223/(1-0.04223)
F2dirneg # .04

#Assunzioni
par(mfrow = c(2,2))
plot(modello_Dir_neg_) 
vif(modello_Dir_neg_)

#Beta
standardCoefs(modello_Dir_neg_)




#-----------------------------------Modelli extra
#variabili mediatrici fra di loro
modello1= lm(ROM ~ JMC + ATCOS + SCB + THREAT + NORM_ATT + SEX_dummy + ETA + ISTR, data=dati)
summary(modello1) 

modello2= lm(JMC ~ ROM + ATCOS + SCB + THREAT + NORM_ATT + SEX_dummy + ETA + ISTR, data=dati)
summary(modello2)

modello3= lm(ATCOS ~ JMC + ROM + SCB + THREAT + NORM_ATT + SEX_dummy + ETA + ISTR, data=dati)
summary(modello3)

modello4= lm(SCB ~ JMC + ATCOS + ROM + THREAT + NORM_ATT + SEX_dummy + ETA + ISTR, data=dati)
summary(modello4)

modello5= lm(THREAT ~ JMC + ATCOS + SCB + ROM + NORM_ATT + SEX_dummy + ETA + ISTR, data=dati)
summary(modello5)

modello6= lm(NORM_ATT ~ JMC + ATCOS + SCB + THREAT + ROM + SEX_dummy + ETA + ISTR, data=dati)
summary(modello6)

#---------------------------------contatti fra di loro

modelloA= lm(Med_Pos ~ Med_Neg + Ind_Pos  + Ind_Neg  +  
                    Dir_Pos + Dir_Neg + CON_MED_F + CON_IND_F + CON_DIR_F, data=dati)
summary(modelloA)

modelloB= lm(Med_Neg ~ Med_Pos + Ind_Pos  + Ind_Neg  +  
               Dir_Pos + Dir_Neg + CON_MED_F + CON_IND_F + CON_DIR_F, data=dati)
summary(modelloB)

modelloC= lm(Ind_Pos ~ Med_Pos + Med_Neg  + Ind_Neg  +  
               Dir_Pos + Dir_Neg + CON_MED_F + CON_IND_F + CON_DIR_F, data=dati)
summary(modelloC)

modelloD= lm(Ind_Neg ~ Med_Pos + Med_Neg  + Ind_Pos  +  
               Dir_Pos + Dir_Neg + CON_MED_F + CON_IND_F + CON_DIR_F, data=dati)
summary(modelloD)

modelloE= lm(Dir_Pos ~ Med_Pos + Med_Neg  + Ind_Pos  +  
               Ind_Neg + Dir_Neg + CON_MED_F + CON_IND_F + CON_DIR_F, data=dati)
summary(modelloE)

modelloF= lm(Dir_Neg ~ Med_Pos + Med_Neg  + Ind_Pos  +  
               Ind_Neg + Dir_Pos + CON_MED_F + CON_IND_F + CON_DIR_F, data=dati)
summary(modelloF)
