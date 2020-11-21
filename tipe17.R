#Produk Amanah

#load data
amanah <- df_split[df_split$PRODUCT_CD=="17",]

#recoding
amanah$KEWARGANEGARAAN <- ifelse(amanah$KEWARGANEGARAAN=="2","1","1")
amanah$KEWARGANEGARAAN <- as.factor(amanah$KEWARGANEGARAAN)
#=========================================================================================
amanah$ASPEK_PEMASARAN <- ifelse(amanah$ASPEK_PEMASARAN=="Agen","1",
                                 ifelse(amanah$ASPEK_PEMASARAN=="Sewa","2",
                                        ifelse(amanah$ASPEK_PEMASARAN=="Jasa","3",
                                               ifelse(amanah$ASPEK_PEMASARAN=="Eceran","4",
                                                      ifelse(amanah$ASPEK_PEMASARAN=="Grosir","5",
                                                             ifelse(amanah$ASPEK_PEMASARAN=="Lainnya","1",
                                                                    "na"))))))
amanah$ASPEK_PEMASARAN <- as.factor(amanah$ASPEK_PEMASARAN)
#=========================================================================================
amanah$BIDANG_USAHA <- ifelse(amanah$BIDANG_USAHA=="Perdagangan","4",
                              ifelse(amanah$BIDANG_USAHA=="Perindustrian","3",
                                     ifelse(amanah$BIDANG_USAHA=="Jasa","2",
                                            ifelse(amanah$BIDANG_USAHA=="Pertanian","1", 
                                                   "na"))))
amanah$BIDANG_USAHA <- as.factor(amanah$BIDANG_USAHA)
#=========================================================================================
amanah$JENIS_USAHA <- ifelse(amanah$JENIS_USAHA=="Bahan Bangunan/Material","2",
                             ifelse(amanah$JENIS_USAHA=="Bengkel","2",
                                    ifelse(amanah$JENIS_USAHA=="Daging/Unggas/Ikan","3",
                                           ifelse(amanah$JENIS_USAHA=="Elektronik","1", 
                                                  ifelse(amanah$JENIS_USAHA=="Industri Rumahan","2", 
                                                         ifelse(amanah$JENIS_USAHA=="Mebel","2", 
                                                                ifelse(amanah$JENIS_USAHA=="Padi/Palawija","3", 
                                                                       ifelse(amanah$JENIS_USAHA=="Pakaian","3",     
                                                                              ifelse(amanah$JENIS_USAHA=="Peralatan Rumah Tangga","1", 
                                                                                     ifelse(amanah$JENIS_USAHA=="Rongsokan/Barang bekas","1",  
                                                                                            ifelse(amanah$JENIS_USAHA=="Rumah Makan","3",
                                                                                                   ifelse(amanah$JENIS_USAHA=="Sayur-Mayur/Buah-buahan","3",
                                                                                                          ifelse(amanah$JENIS_USAHA=="Sembako Kelontong","3",
                                                                                                                 ifelse(amanah$JENIS_USAHA=="Supplier","2",   
                                                                                                                        ifelse(amanah$JENIS_USAHA=="Transportasi","2",         
                                                                                                                               "na")))))))))))))))
amanah$JENIS_USAHA <- as.factor(amanah$JENIS_USAHA)
#=========================================================================================
amanah$JENIS_TEMPAT_USAHA <- ifelse(amanah$JENIS_TEMPAT_USAHA=="Gerobak berpindah","1",
                                    ifelse(amanah$JENIS_TEMPAT_USAHA=="Kios/Los/Lapak/Lahan","3",
                                           ifelse(amanah$JENIS_TEMPAT_USAHA=="Toko/Ruko","4",
                                                  ifelse(amanah$JENIS_TEMPAT_USAHA=="Warung/Tenda","2", 
                                                         "na"))))
amanah$JENIS_TEMPAT_USAHA <- as.factor(amanah$JENIS_TEMPAT_USAHA)
#=========================================================================================
amanah$LAMA_USAHA <- ifelse(amanah$LAMA_USAHA <= 1 ,"1",
                            ifelse(amanah$LAMA_USAHA > 1 & amanah$LAMA_USAHA <= 3,"2",
                                   ifelse(amanah$LAMA_USAHA > 3 & amanah$LAMA_USAHA <= 5,"3",
                                          ifelse(amanah$LAMA_USAHA > 5,"4", 
                                                 "na"))))
amanah$LAMA_USAHA <- as.factor(amanah$LAMA_USAHA)
#=========================================================================================
amanah$LAMA_BEKERJA <- ifelse(amanah$LAMA_BEKERJA <= 1 ,"1",
                              ifelse(amanah$LAMA_BEKERJA > 1 & amanah$LAMA_BEKERJA <= 3,"2",
                                     ifelse(amanah$LAMA_BEKERJA > 3 & amanah$LAMA_BEKERJA <= 5,"3",
                                            ifelse(amanah$LAMA_BEKERJA > 5,"4", 
                                                   "na"))))
amanah$LAMA_BEKERJA <- as.factor(amanah$LAMA_BEKERJA)
#=========================================================================================
amanah$STATUS_TEMPAT_USAHA <- ifelse(amanah$STATUS_TEMPAT_USAHA=="Kredit/Kontrak","2",
                                     ifelse(amanah$STATUS_TEMPAT_USAHA=="Milik Keluarga","3",
                                            ifelse(amanah$STATUS_TEMPAT_USAHA=="Milik Sendiri (Beli)","5",
                                                   ifelse(amanah$STATUS_TEMPAT_USAHA=="Milik Sendiri (Warisan)","4", 
                                                          ifelse(amanah$STATUS_TEMPAT_USAHA=="Sewa","1",                           
                                                                 "na")))))
amanah$STATUS_TEMPAT_USAHA <- as.factor(amanah$STATUS_TEMPAT_USAHA)
#=========================================================================================
amanah$RANGE_PENDAPATAN <- ifelse(amanah$RANGE_PENDAPATAN=="<= 2 Juta","1",
                                  ifelse(amanah$RANGE_PENDAPATAN=="> 2 Juta - 5 Juta","2",
                                         ifelse(amanah$RANGE_PENDAPATAN=="> 5 Juta - 10 Juta","3",
                                                ifelse(amanah$RANGE_PENDAPATAN=="> 10 Juta - 15 juta","4", 
                                                       ifelse(amanah$RANGE_PENDAPATAN=="> 15 Juta - 20 Juta","5",
                                                              ifelse(amanah$RANGE_PENDAPATAN=="> 20 Juta - 25 Juta","6",
                                                                     ifelse(amanah$RANGE_PENDAPATAN=="> 25 Juta - 50 Juta","7",
                                                                            ifelse(amanah$RANGE_PENDAPATAN=="> 50 Juta","8",
                                                                                   "na"))))))))
amanah$RANGE_PENDAPATAN <- as.factor(amanah$RANGE_PENDAPATAN)
#=========================================================================================
amanah$totalangsuran <- (amanah$SEWA_MODAL_MAKSIMAL/amanah$TENOR) + (amanah$UP/amanah$TENOR)
amanah$thp <- amanah$PENDAPATAN_BERSIH/((amanah$SEWA_MODAL_MAKSIMAL/amanah$TENOR) + (amanah$UP/amanah$TENOR))
amanah$thp <- ifelse(amanah$thp <= 1.5 ,"1",
                     ifelse(amanah$thp > 1.5 & amanah$thp <= 2,"2",
                            ifelse(amanah$thp > 2 & amanah$thp <= 3,"3",
                                   ifelse(amanah$thp > 3,"4", 
                                          "na"))))
amanah$thp <- as.factor(amanah$thp)
#=========================================================================================
amanah$JENIS_KELAMIN <- ifelse(amanah$JENIS_KELAMIN=="F","2",
                               ifelse(amanah$JENIS_KELAMIN=="L","1",
                                      ifelse(amanah$JENIS_KELAMIN=="P","2",
                                             ifelse(amanah$JENIS_KELAMIN=="N/A","2", 
                                                    "na"))))
amanah$JENIS_KELAMIN <- as.factor(amanah$JENIS_KELAMIN)
#=========================================================================================
amanah$STATUS_KAWIN <- ifelse(amanah$STATUS_KAWIN=="1","4",
                              ifelse(amanah$STATUS_KAWIN=="2","1",
                                     ifelse(amanah$STATUS_KAWIN=="3","2",
                                            ifelse(amanah$STATUS_KAWIN=="4","3", 
                                                   "na"))))
amanah$STATUS_KAWIN <- as.factor(amanah$STATUS_KAWIN)
#=========================================================================================
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  period$year
}
amanah$usia <- calc_age(amanah$TGL_LAHIR,amanah$TGL_KREDIT)
amanah$usia <- amanah$usia + 1 #karena periode pengamatan 1 thn

amanah$usia <- ifelse(amanah$usia <= 30 ,"1",
                      ifelse(amanah$usia > 30 & amanah$usia <= 40,"2",
                             ifelse(amanah$usia > 40 & amanah$usia <= 50,"3",
                                    ifelse(amanah$usia > 50,"4", 
                                           "na"))))
amanah$usia <- as.factor(amanah$usia)
#=========================================================================================
amanah$PENDIDIKAN <- ifelse(amanah$PENDIDIKAN=="10","1",
                            ifelse(amanah$PENDIDIKAN=="D1","3",
                                   ifelse(amanah$PENDIDIKAN=="D2","3",
                                          ifelse(amanah$PENDIDIKAN=="D3","3", 
                                                 ifelse(amanah$PENDIDIKAN=="S1","4", 
                                                        ifelse(amanah$PENDIDIKAN=="S2","4", 
                                                               ifelse(amanah$PENDIDIKAN=="S3","4", 
                                                                      ifelse(amanah$PENDIDIKAN=="SD","1",     
                                                                             ifelse(amanah$PENDIDIKAN=="SMA","3", 
                                                                                    ifelse(amanah$PENDIDIKAN=="SMP","2",  
                                                                                           "na"))))))))))
amanah$PENDIDIKAN <- as.factor(amanah$PENDIDIKAN)
#=========================================================================================
amanah$JENIS_PERUSAHAAN <- ifelse(amanah$JENIS_PERUSAHAAN=="1","3",
                                  ifelse(amanah$JENIS_PERUSAHAAN=="2","2",
                                         ifelse(amanah$JENIS_PERUSAHAAN=="3","1",
                                                "na")))
amanah$JENIS_PERUSAHAAN <- as.factor(amanah$JENIS_PERUSAHAAN)
#=========================================================================================
amanah$STATUS_KARYAWAN <- ifelse(amanah$STATUS_KARYAWAN=="1","2",
                                 ifelse(amanah$STATUS_KARYAWAN=="2","1",
                                        "na"))
amanah$STATUS_KARYAWAN <- as.factor(amanah$STATUS_KARYAWAN)
#=========================================================================================
amanah$SUMBER_DANA <- ifelse(amanah$SUMBER_DANA=="6","2",
                             ifelse(amanah$SUMBER_DANA=="1","3",
                                    ifelse(amanah$SUMBER_DANA=="2","5",
                                           ifelse(amanah$SUMBER_DANA=="3","1", 
                                                  ifelse(amanah$SUMBER_DANA=="4","4", 
                                                         ifelse(amanah$SUMBER_DANA=="5","2", 
                                                                "na"))))))
amanah$SUMBER_DANA <- as.factor(amanah$SUMBER_DANA)
#=========================================================================================
amanah$PROFESI <- ifelse(amanah$PROFESI=="IBU RUMAH TANGGA","2",
                         ifelse(amanah$PROFESI=="KARYAWAN SWASTA","3",
                                ifelse(amanah$PROFESI=="LAINNYA","1",
                                       ifelse(amanah$PROFESI=="NELAYAN","2", 
                                              ifelse(amanah$PROFESI=="PEGAWAI BUMN / BUMD","4", 
                                                     ifelse(amanah$PROFESI=="PELAJAR / MAHASISWA","1", 
                                                            ifelse(amanah$PROFESI=="PENSIUNAN","2", 
                                                                   ifelse(amanah$PROFESI=="PETANI","2",     
                                                                          ifelse(amanah$PROFESI=="PNS","3", 
                                                                                 ifelse(amanah$PROFESI=="PROFESIONAL","3",  
                                                                                        ifelse(amanah$PROFESI=="TNI / POLRI","1", 
                                                                                               ifelse(amanah$PROFESI=="WIRAUSAHA / PEDAGANG","4",        
                                                                                                      "na"))))))))))))
amanah$PROFESI <- as.factor(amanah$PROFESI)

#missing value
prop.table(table(is.na(amanah)))
missing_amanah <- round(as.data.frame(sapply(amanah, function(x) sum(is.na(x))/length(x))*100),2)
missing_amanah <- as.data.frame(cbind(rownames(missing_amanah),missing_amanah))
missing_amanah <- missing_amanah[missing_amanah$`sapply(amanah, function(x) sum(is.na(x))/length(x)) * 100` > 30 ,]

amanah <- amanah[ , !(names(amanah) %in% rownames(missing_amanah))]
amanah <- amanah[,grep('TGL', names(amanah), invert=TRUE)]

#jumlah fasilitas
cif <- as.data.frame(table(amanah$CIF))
amanah <- merge(amanah,cif,by.x="CIF", by.y="Var1", all.x=T)
colnames(amanah)[length(colnames(amanah))] <- "JumlahFasilitas"

#possible parameter
parameter <- c("TOTAL_TAKSIRAN",
               "TUJUAN_KREDIT",
               "JENIS_KELAMIN",
               "STATUS_KAWIN",
               "PENDIDIKAN",
               "TIPE_PENDAPATAN",
               "LAMA_BEKERJA",
               "JUMLAH_TANGGUNGAN",
               "TIPE_PENDUDUK",
               "SUMBER_DANA",
               "PROFESI",
               "usia",
               "JumlahFasilitas",
               "status")


amanah <- amanah[,parameter]
amanah <- amanah[!is.na(amanah$status),]


library(Hmisc)
#BIDANG_USAHA
amanah$BIDANG_USAHA <- Hmisc::impute(amanah$BIDANG_USAHA, mode)
amanah$BIDANG_USAHA <- as.character(amanah$BIDANG_USAHA)
amanah$BIDANG_USAHA <- as.factor(amanah$BIDANG_USAHA)
#ASPEK_PEMASARAN
amanah$ASPEK_PEMASARAN <- Hmisc::impute(amanah$ASPEK_PEMASARAN, mode)
amanah$ASPEK_PEMASARAN <- as.character(amanah$ASPEK_PEMASARAN)
amanah$ASPEK_PEMASARAN <- as.factor(amanah$ASPEK_PEMASARAN)
#JENIS_USAHA
amanah$JENIS_USAHA <- Hmisc::impute(amanah$JENIS_USAHA, mode)
amanah$JENIS_USAHA <- as.character(amanah$JENIS_USAHA)
amanah$JENIS_USAHA <- as.factor(amanah$JENIS_USAHA)
#JENIS_TEMPAT_USAHA
amanah$JENIS_TEMPAT_USAHA <- Hmisc::impute(amanah$JENIS_TEMPAT_USAHA, mode)
amanah$JENIS_TEMPAT_USAHA <- as.character(amanah$JENIS_TEMPAT_USAHA)
amanah$JENIS_TEMPAT_USAHA <- as.factor(amanah$JENIS_TEMPAT_USAHA)
#LAMA_USAHA
amanah$LAMA_USAHA <- Hmisc::impute(amanah$LAMA_USAHA, mode)
amanah$LAMA_USAHA <- as.character(amanah$LAMA_USAHA)
amanah$LAMA_USAHA <- as.factor(amanah$LAMA_USAHA)
#STATUS_TEMPAT_USAHA
amanah$STATUS_TEMPAT_USAHA <- Hmisc::impute(amanah$STATUS_TEMPAT_USAHA, mode)
amanah$STATUS_TEMPAT_USAHA <- as.character(amanah$STATUS_TEMPAT_USAHA)
amanah$STATUS_TEMPAT_USAHA <- as.factor(amanah$STATUS_TEMPAT_USAHA)
#RANGE_PENDAPATAN
amanah$RANGE_PENDAPATAN <- Hmisc::impute(amanah$RANGE_PENDAPATAN, mode)
amanah$RANGE_PENDAPATAN <- as.character(amanah$RANGE_PENDAPATAN)
amanah$RANGE_PENDAPATAN <- as.factor(amanah$RANGE_PENDAPATAN)
#PENDAPATAN_BERSIH
amanah$PENDAPATAN_BERSIH <- Hmisc::impute(amanah$PENDAPATAN_BERSIH, median)
amanah$PENDAPATAN_BERSIH <- as.character(amanah$PENDAPATAN_BERSIH)
amanah$PENDAPATAN_BERSIH <- as.numeric(amanah$PENDAPATAN_BERSIH)
#PENDIDIKAN
amanah$PENDIDIKAN <- Hmisc::impute(amanah$PENDIDIKAN, mode)
amanah$PENDIDIKAN <- as.character(amanah$PENDIDIKAN)
amanah$PENDIDIKAN <- as.factor(amanah$PENDIDIKAN)
#SUMBER_DANA
amanah$SUMBER_DANA <- Hmisc::impute(amanah$SUMBER_DANA, mode)
amanah$SUMBER_DANA <- as.character(amanah$SUMBER_DANA)
amanah$SUMBER_DANA <- as.factor(amanah$SUMBER_DANA)
#thp
amanah$thp <- Hmisc::impute(amanah$thp, mode)
amanah$thp <- as.character(amanah$thp)
amanah$thp <- as.factor(amanah$thp)
#GAJI
amanah$GAJI <- Hmisc::impute(amanah$GAJI, median)
amanah$GAJI <- as.character(amanah$GAJI)
amanah$GAJI <- as.numeric(amanah$GAJI)

amanah <- amanah[!is.na(amanah$status),]



#outlier, tidak perlu dilakukan karena sudah dikategorikan
#qnt <- quantile(amanah$LAMA_USAHA, probs=c(.25, .75), na.rm = T)
#caps <- quantile(amanah$LAMA_USAHA, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(amanah$LAMA_USAHA, na.rm = T)
#amanah$LAMA_USAHA[amanah$LAMA_USAHA < (qnt[1] - H)] <- caps[1]
#amanah$LAMA_USAHA[amanah$LAMA_USAHA > (qnt[2] + H)] <- caps[2]

#qnt <- quantile(amanah$PENDAPATAN_BERSIH, probs=c(.25, .75), na.rm = T)
#caps <- quantile(amanah$PENDAPATAN_BERSIH, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(amanah$PENDAPATAN_BERSIH, na.rm = T)
#amanah$PENDAPATAN_BERSIH[amanah$PENDAPATAN_BERSIH < (qnt[1] - H)] <- caps[1]
#amanah$PENDAPATAN_BERSIH[amanah$PENDAPATAN_BERSIH > (qnt[2] + H)] <- caps[2]

#qnt <- quantile(amanah$LAMA_BEKERJA, probs=c(.25, .75), na.rm = T)
#caps <- quantile(amanah$LAMA_BEKERJA, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(amanah$LAMA_BEKERJA, na.rm = T)
#amanah$LAMA_BEKERJA[amanah$LAMA_BEKERJA < (qnt[1] - H)] <- caps[1]
#amanah$LAMA_BEKERJA[amanah$LAMA_BEKERJA > (qnt[2] + H)] <- caps[2]

#qnt <- quantile(amanah$usia, probs=c(.25, .75), na.rm = T)
#caps <- quantile(amanah$usia, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(amanah$usia, na.rm = T)
#amanah$usia[amanah$usia < (qnt[1] - H)] <- caps[1]
#amanah$usia[amanah$usia > (qnt[2] + H)] <- caps[2]
#====end



amanah<- na.omit(amanah)
#cek proprosi
prop.table(table(amanah$status))
#0          1 
#0.08958064 0.91041936


library(DMwR)
set.seed(1)
amanah <- SMOTE(status ~ . , data = amanah)

library(knitr)
library(xgboost) # for xgboost
library(tidyverse)

amanah_num <- amanah %>%
  select_if(is.numeric)
amanah_num <- cbind(amanah_num,amanah$status)
colnames(amanah_num)[length(colnames(amanah_num))] <- "status"

amanah$status <- as.numeric(amanah$status)
amanah$status<-as.numeric(ifelse(amanah$status == "1", 0, 1))
row.names(amanah) = seq(1,nrow(amanah))
library(woe)
InfVal<-iv.mult(df = amanah,y = "status", TRUE)
InfVal <- InfVal[InfVal$InformationValue > 0.011,]

amanah_final <- amanah[,InfVal$Variable]
amanah_cat <- amanah_final %>%
  select_if(is.factor)
names(amanah_cat)
#multiko
library(GGally)
ggpairs(amanah_num)
library(mctest)
amanah_num$status <- as.numeric(amanah_num$status)
amanah_num$status<-as.numeric(ifelse(amanah_num$status == "1", 0, 1))
omcdiag(amanah_num[,!(names(amanah_num) %in% "status")],amanah_num$status)
colnames(amanah_num)
multiko <- imcdiag(amanah_num[,-c(4)],amanah_num$status) #final multiko
#checkpoint
multiko <- as.data.frame(multiko$x)
param <- names(multiko)
amanah_num <- amanah_num[,param]
amanah_num <- cbind(amanah_num,amanah$status)
colnames(amanah_num)[length(colnames(amanah_num))] <- "status"

#========================cek
mm <- glm(status ~ ., data=amanah_num, family = binomial(logit))
library(lmtest)
bptest(mm) #heteroscedaticyti
#studentized Breusch-Pagan test
#BP = 3310.6, df = 9, p-value < 2.2e-16
#This gives a very significant result and suggests that I have very significant heteroskedasticity in my dataset. Therefore I can reject the null hypothesis that my dataset is homoskedastic.
summary(mm) #model
cek <- as.data.frame(coef(summary(mm))[,'Pr(>|z|)'])
cek <- cbind(rownames(cek),cek)
cek <- cek[cek$`coef(summary(mm))[, "Pr(>|z|)"]`> 0.05,]
anova(mm,test='Chisq')
#=====================================================

#Final data
amanah_1 <- cbind(amanah_cat,amanah_num)
amanah <- amanah_1
#===================================

set.seed(1)
library(caret)
div_part_1 <- createDataPartition(y = amanah[,"status"], p = 0.7, list = F)

train_data <- amanah[div_part_1,] # 70% here
training <- train_data
write.csv(train_data,"train_produk17_final.csv", row.names = FALSE)
train_label <- train_data[,"status"]
train_data <- train_data[,!(names(train_data) %in% "status")]
training$status <- as.factor(training$status)
training$thp <- factor(training$thp,levels = c("1","2","3","4"))
prop.table(table(training$status))

test_data <- amanah[-div_part_1,] # rest of the 30% data goes here
testing <- test_data
write.csv(test_data,"test_produk17_final.csv", row.names = FALSE)
test_label <- test_data[,"status"]
test_data <- test_data[,!(names(test_data) %in% "status")]
testing$status <- as.factor(testing$status)
testing$thp <- factor(testing$thp,levels = c("1","2","3","4"))
prop.table(table(testing$status))

####XGBoost
library(Matrix)
library(xgboost)
library(ROCR)
library(mlr) 

train_data_mat <- createDummyFeatures(
training, target = "status")
train_data_mat <- as.data.frame(sapply(train_data_mat, as.numeric))
train_data_mat$status <- as.factor(train_data_mat$status)
train_data_mat$status <- ifelse(train_data_mat$status=="1",0,1)
train_data_mat$status <- as.factor(train_data_mat$status)
write.csv(train_data_mat[1:2,],"formatjson17.csv", row.names = FALSE)
traintask <- makeClassifTask (data = train_data_mat,target = "status", positive = 1)

test_data_mat <- createDummyFeatures(
  testing, target = "status")
test_data_mat <- as.data.frame(sapply(testing, as.numeric))
test_data_mat$status <- as.factor(test_data_mat$status)
test_data_mat$status <- ifelse(test_data_mat$status=="1",0,1)
test_data_mat$status <- as.factor(test_data_mat$status)
testtask <- makeClassifTask (data = test_data_mat,target = "status", positive = 1)

lrn <- makeLearner("classif.xgboost",predict.type = "prob")
lrn$par.vals <- list( objective="reg:logistic", eval_metric="auc", nrounds = 100)
params <- makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear"))
  ,makeIntegerParam("max_depth",lower = 3L,upper = 10L)
  ,makeNumericParam("min_child_weight",lower = 1L,upper = 10L)
  ,makeNumericParam("subsample",lower = 0.5,upper = 1)
  ,makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
  ,makeNumericParam("eta", lower = .1, upper = .5)
  ,makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
  ,makeNumericParam("gamma", lower = 0, upper = 3)
  ,makeNumericParam("alpha", lower = 0, upper = 1)
)
ctrl <- makeTuneControlRandom(maxit = 5L)
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)
xgmodel <- mlr::train(learner = lrn_tune,task = traintask)
pred_train <- predict(xgmodel, traintask)
cek <- pred_train$data$prob.1
View(pred_train$data)
train_label <- as.factor(train_label)
m1_pred <- ROCR::prediction(cek, train_label)
m1_perf <- ROCR::performance(m1_pred, "tpr", "fpr")

m1plot <- plot(m1_perf,
               avg="threshold",
               colorize=TRUE,
               lwd=1,
               main="Best Thresholds",
               print.cutoffs.at=seq(0, 1, by=0.5),
               text.adj=c(-0.5, 0.5),
               text.cex=0.5)
plot(m1_perf, lwd=2, colorize=TRUE, main="ROC : Basic Recuirsive Partitioning") # Model Performance
# Plot precision/recall curve
model_1_perf_precision <- ROCR::performance(m1_pred, measure = "prec", x.measure = "rec")
plot(model_1_perf_precision, main="Precision/recall curve")
# Plot accuracy as function of threshold
model_1_perf_acc <- ROCR::performance(m1_pred, measure = "acc")
plot(model_1_perf_acc, main="Accuracy as function of threshold")

m1AUROC_train <- round(ROCR::performance(m1_pred, measure = "auc")@y.values[[1]]*100, 2)
m1_KS_train <- round(max(attr(m1_perf,'y.values')[[1]]-attr(m1_perf,'x.values')[[1]])*100, 2)
m1_Gini_train <- (2*m1AUROC_train - 100)
cat("AUROC: ",m1AUROC_train,"\tKS: ", m1_KS_train, "\tGini:", m1_Gini_train, "\n")

opt1 <- InformationValue::optimalCutoff(train_label, pred_train$data[,4], optimiseFor = "misclasserror", returnDiagnostics = FALSE)

pred.resp <- ifelse(pred_train$data[,4] >= opt1, 1, 0)
pred.resp <- as.character(pred.resp)
pred.resp <- as.factor(pred.resp)
#  pred.resp <- as.numeric(pred.resp)
train_label <- as.factor(train_label)
cm_1 <- confusionMatrix(pred.resp, train_label)
saveRDS(xgmodel,"17_model_final.rds")
save(xgmodel, file = "model17.rda")

####Basic Recuirsive Partitioning
set.seed(44)
library(rpart)
model_2 <- rpart(status~.,data=train)

model_2_score <- predict(model_2,type='prob',train)
m2_pred <- prediction(model_2_score[,2],train$status)
m2_perf <- ROCR::performance(m2_pred,"tpr","fpr")
m2plot <- plot(m2_perf,
               avg="threshold",
               colorize=TRUE,
               lwd=1,
               main="Best Thresholds",
               print.cutoffs.at=seq(0, 1, by=0.5),
               text.adj=c(-0.5, 0.5),
               text.cex=0.5)
plot(m2_perf, lwd=2, colorize=TRUE, main="ROC : Basic Recuirsive Partitioning") # Model Performance
# Plot precision/recall curve
model_2_perf_precision <- ROCR::performance(m2_pred, measure = "prec", x.measure = "rec")
plot(model_2_perf_precision, main="Precision/recall curve")
# Plot accuracy as function of threshold
model_2_perf_acc <- ROCR::performance(m2_pred, measure = "acc")
plot(model_2_perf_acc, main="Accuracy as function of threshold")
m2AUROC_train <- round(ROCR::performance(m2_pred, measure = "auc")@y.values[[1]]*100, 2)
m2_KS_train <- round(max(attr(m2_perf,'y.values')[[1]]-attr(m2_perf,'x.values')[[1]])*100, 2)
m2_Gini_train <- (2*m2AUROC_train - 100)
cat("AUROC: ",m2AUROC_train,"\tKS: ", m2_KS_train, "\tGini:", m2_Gini_train, "\n")

opt2 <- InformationValue::optimalCutoff(train$status, model_2_score[,2], optimiseFor = "misclasserror", returnDiagnostics = FALSE)

pred.resp_2 <- ifelse(model_2_score[,2] >= opt2, 1, 0)
pred.resp_2 <- as.character(pred.resp_2)
pred.resp_2 <- as.factor(pred.resp_2)
#pred.resp_2 <- as.numeric(pred.resp_2)
train$status <- as.factor(train$status)
cm_2 <- confusionMatrix(pred.resp_2, train$status)
saveRDS(model_2,"17_model_2.rds")

###Logistic Reg
model_3 <- glm(status~.,data=train,family=binomial())
model_3 <- step(model_3)
model_3_score <- predict(model_3,type='response',train)
m3_pred <- prediction(model_3_score,train$status)
m3_perf <- ROCR::performance(m3_pred,"tpr","fpr")
m3plot <- plot(m3_perf,
               avg="threshold",
               colorize=TRUE,
               lwd=1,
               main="Best Thresholds",
               print.cutoffs.at=seq(0, 1, by=0.5),
               text.adj=c(-0.5, 0.5),
               text.cex=0.5)
plot(m3_perf, lwd=2, colorize=TRUE, main="ROC : Basic Recuirsive Partitioning") # Model Performance
# Plot precision/recall curve
model_3_perf_precision <- ROCR::performance(m3_pred, measure = "prec", x.measure = "rec")
plot(model_3_perf_precision, main="Precision/recall curve")
# Plot accuracy as function of threshold
model_3_perf_acc <- ROCR::performance(m3_pred, measure = "acc")
plot(model_3_perf_acc, main="Accuracy as function of threshold")
m3AUROC_train <- round(ROCR::performance(m3_pred, measure = "auc")@y.values[[1]]*100, 2)
m3_KS_train <- round(max(attr(m3_perf,'y.values')[[1]]-attr(m3_perf,'x.values')[[1]])*100, 2)
m3_Gini_train <- (2*m3AUROC_train - 100)
cat("AUROC: ",m3AUROC_train,"\tKS: ", m3_KS_train, "\tGini:", m3_Gini_train, "\n")

opt3 <- InformationValue::optimalCutoff(train$status, model_3_score, optimiseFor = "misclasserror", returnDiagnostics = FALSE)

pred.resp_3 <- ifelse(model_3_score >= opt3, 1, 0)
pred.resp_3 <- as.character(pred.resp_3)
pred.resp_3 <- as.factor(pred.resp_3)
#pred.resp_2 <- as.numeric(pred.resp_2)
train$status <- as.factor(train$status)
cm_3 <- confusionMatrix(pred.resp_3, train$status)
saveRDS(model_3,"17_model_3.rds")

#random_forest
library(randomForest)
model_4 <- randomForest(status ~., data = train,  ntree=40, mtry=2, nodesize=7,importance=TRUE)
model_4_score <- predict(model_4, newdata = train, type="prob")
m4_pred <- prediction(model_4_score[,2],train$status)
m4_perf <- ROCR::performance(m4_pred,"tpr","fpr")
m4plot <- plot(m4_perf,
               avg="threshold",
               colorize=TRUE,
               lwd=1,
               main="Best Thresholds",
               print.cutoffs.at=seq(0, 1, by=0.5),
               text.adj=c(-0.5, 0.5),
               text.cex=0.5)
plot(m4_perf, lwd=2, colorize=TRUE, main="ROC : random_forest") # Model Performance
# Plot precision/recall curve
model_4_perf_precision <- ROCR::performance(m4_pred, measure = "prec", x.measure = "rec")
plot(model_4_perf_precision, main="Precision/recall curve")
# Plot accuracy as function of threshold
model_4_perf_acc <- ROCR::performance(m4_pred, measure = "acc")
plot(model_4_perf_acc, main="Accuracy as function of threshold")
m4AUROC_train <- round(ROCR::performance(m4_pred, measure = "auc")@y.values[[1]]*100, 2)
m4_KS_train <- round(max(attr(m4_perf,'y.values')[[1]]-attr(m4_perf,'x.values')[[1]])*100, 2)
m4_Gini_train <- (2*m4AUROC_train - 100)
cat("AUROC: ",m4AUROC_train,"\tKS: ", m4_KS_train, "\tGini:", m4_Gini_train, "\n")

opt4 <- InformationValue::optimalCutoff(train$status, model_4_score[,2], optimiseFor = "misclasserror", returnDiagnostics = FALSE)

pred.resp_4 <- ifelse(model_4_score[,2] >= opt4, 1, 0)
pred.resp_4 <- as.character(pred.resp_4)
pred.resp_4 <- as.factor(pred.resp_4)
#pred.resp_2 <- as.numeric(pred.resp_2)
train$status <- as.factor(train$status)
cm_4 <- confusionMatrix(pred.resp_4, train$status)
saveRDS(model_4,"17_model_4.rds")

#SVM
library(kernlab)
model_5 <- ksvm(status ~ ., data = train, kernel = "vanilladot")

model_5_score <- predict(model_5, train, type="decision")
m5_pred <- prediction(model_5_score,train$status)
m5_perf <- ROCR::performance(m5_pred,"tpr","fpr")
m5plot <- plot(m5_perf,
               avg="threshold",
               colorize=TRUE,
               lwd=1,
               main="Best Thresholds",
               print.cutoffs.at=seq(0, 1, by=0.5),
               text.adj=c(-0.5, 0.5),
               text.cex=0.5)
plot(m5_perf, lwd=2, colorize=TRUE, main="ROC : SVM") # Model Performance
# Plot precision/recall curve
model_5_perf_precision <- ROCR::performance(m5_pred, measure = "prec", x.measure = "rec")
plot(model_5_perf_precision, main="Precision/recall curve")
# Plot accuracy as function of threshold
model_5_perf_acc <- ROCR::performance(m5_pred, measure = "acc")
plot(model_5_perf_acc, main="Accuracy as function of threshold")
m5AUROC_train <- round(ROCR::performance(m5_pred, measure = "auc")@y.values[[1]]*100, 2)
m5_KS_train <- round(max(attr(m5_perf,'y.values')[[1]]-attr(m5_perf,'x.values')[[1]])*100, 2)
m5_Gini_train <- (2*m5AUROC_train - 100)
cat("AUROC: ",m5AUROC_train,"\tKS: ", m5_KS_train, "\tGini:", m5_Gini_train, "\n")

opt5 <- InformationValue::optimalCutoff(train$status, model_5_score, optimiseFor = "misclasserror", returnDiagnostics = FALSE)

pred.resp_5 <- ifelse(model_5_score >= opt5, 1, 0)
pred.resp_5 <- as.character(pred.resp_5)
pred.resp_5 <- as.factor(pred.resp_5)
#pred.resp_2 <- as.numeric(pred.resp_2)
train$status <- as.factor(train$status)
cm_5 <- confusionMatrix(pred.resp_5, train$status)
saveRDS(model_5,"17_model_5.rds")
