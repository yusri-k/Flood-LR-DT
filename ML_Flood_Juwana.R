# set working directory
setwd("D:/Remote Sensing/PJ DAS (praktikum)/Flood")


install.packages("mlr")
install.packages("ROCR")

# load library
library(sp)
library(raster)
library(mlr)
library(ROCR)
library(rminer)

# persiapan environment dan load dat raster + rabel
rasterOptions(progress = "Text", tmpdir = paste0(getwd(), "/tmp"))

# ==== DATA PREPARATION ====
# load data table dan lihat nama pada setiap parameter
table.sample <- read.csv("training_sample_table.csv")
names(table.sample)

# load raster data yang digunakan sebagai predictor 
BL <- raster('BL50.tif')
DEM <- raster('DEM50.tif')
EDRiver <- raster('EDRiver50.tif')
NDVI <- raster('NDVI50.tif')
NDWI <- raster('NDWI50.tif')
PL <- raster('PL50.tif')
CH <- raster('CH50.tif')
SL <- raster('SL50.tif')
TWI <- raster('TWI50.tif')
DD <- raster('DD50.tif')

# samakan samakan cakupan wilayah seluruh data
DEM1 <- raster(vals=values(DEM),ext=extent(DEM),crs=crs(DEM),
               nrows=dim(DEM)[1],ncols=dim(DEM)[2])
SL1 <- raster(vals=values(SL),ext=extent(DEM),crs=crs(DEM),
               nrows=dim(DEM)[1],ncols=dim(DEM)[2])
TWI1 <- raster(vals=values(TWI),ext=extent(DEM),crs=crs(DEM),
               nrows=dim(DEM)[1],ncols=dim(DEM)[2])
BL1 <- raster(vals=values(BL),ext=extent(DEM),crs=crs(DEM),
              nrows=dim(DEM)[1],ncols=dim(DEM)[2])
PL1 <- raster(vals=values(PL),ext=extent(DEM),crs=crs(DEM),
              nrows=dim(DEM)[1],ncols=dim(DEM)[2])
NDVIcrop <- crop(NDVI,extent(DEM))
NDVI1 <- raster(vals=values(NDVIcrop),ext=extent(DEM),crs=crs(DEM),
                nrows=dim(DEM)[1],ncols=dim(DEM)[2])
NDWIcrop <- crop(NDWI,extent(DEM))
NDWI1 <- raster(vals=values(NDWIcrop),ext=extent(DEM),crs=crs(DEM),
                nrows=dim(DEM)[1],ncols=dim(DEM)[2])
CHcrop <- crop(CH,extent(DEM))
CH1 <- raster(vals=values(CHcrop),ext=extent(DEM),crs=crs(DEM),
                nrows=dim(DEM)[1],ncols=dim(DEM)[2])
EDRivercrop <- crop(EDRiver,extent(DEM))
EDRiver1 <- raster(vals=values(EDRivercrop),ext=extent(DEM),crs=crs(DEM),
              nrows=dim(DEM)[1],ncols=dim(DEM)[2])
DDcrop <- crop(DD,extent(DEM))
DD1 <- raster(vals=values(DDcrop),ext=extent(DEM),crs=crs(DEM),
                   nrows=dim(DEM)[1],ncols=dim(DEM)[2])

# cek data
BL1
DEM1
EDRiver1
NDVI1
NDWI1
PL1
CH1
SL1
TWI1
DD1

# stack raster layer dan rename layernya
predictor <- stack(BL1, DEM1, EDRiver1, NDVI1, NDWI1, PL1, CH1, SL1, TWI1, DD1)
names(predictor) <- c("BL", "Elev", "EDRiver", "NDVI", "NDWI", "PL", "CH","Slope", "TWI", "DD")
predictor
writeRaster(predictor, "predictor_stack.tif", format = "GTiff", datatype = "FLT4S", overwrite=TRUE)
plot(predictor)

# memisahkan data menjadi 2, training dan test data
spec <- c(train = .7, test = .3)
g <- sample(cut(seq(nrow(table.sample)), nrow(table.sample)*cumsum(c(0,spec)), label = names(spec)))
names((spec))
dframe.sp <- split(table.sample, g)
train <- as.data.frame(dframe.sp$train)
test <- as.data.frame(dframe.sp$test)

# ==== LOGISTICS REGRESSION ====
# membangun model logistic regression
lr.model <- glm(as.factor(TYPE) ~ ., family = binomial(), data = train)
summary(lr.model)

#membangun model logistic regression
prob.lr <- predict(lr.model, newdata=test, type="response")
pred.lr <- prediction(prob.lr, test$TYPE)
perf.lr <- ROCR::performance(pred.lr,"tpr","fpr")
auc.lr <- ROCR::performance(pred.lr, measure = "auc")
auc.lr <- auc.lr@y.values[[1]]
auc.lr

#aplikasikan model ke dalam raster data dan simpan hasilnya
predict.lr <- predict(predictor, model = lr.model, ext = extent(predictor), progress = "text",type = "response")
writeRaster(predict.lr, "Flood_Prediction.tif", format = "GTiff", datatype = "FLT4S")

mycol <- colorRampPalette(c("forestgreen","green","yellow", "orange", "red", "darkred"))
par(mar=c(5,5,4,2), mfrow=c(1,1))
plot(predict.lr, main="Peta kerawanan banjir DAS Juwana", col = mycol(50), ylab = "longitude", xlab = "latitude")

# ==== DECISSION TREE ====
library(rpart)
library(rpart.plot)
library(rminer)
library(caret)

#training model dengan menggunakan metode decision tree
dt.model <- rpart(factor(TYPE)~., data = train, method = 'class')

# membangun model dt
prob.dt <- predict(dt.model, newdata=test, type="prob")[,2]
pred.dt <- prediction(prob.dt, test$TYPE)
perf.dt <- ROCR::performance(pred.dt,"tpr","fpr")
auc.dt <- ROCR::performance(pred.dt, measure = "auc")
auc.dt <- auc.dt@y.values[[1]]
auc.dt

# plot AUC
plot(perf.lr, col = 'red', main = 'ROC Curve')
plot(perf.dt, add = TRUE, col = 'blue')
# Add a legend
legend(0.7, 0.4, legend=c("Logistics Regression", "Decission Tree"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
auc.lr
auc.dt

#plot model decision tree yang dihasilkan, gambar 4.3
par(mar = c(1,1,1,1))
rpart.plot(dt.model, uniform=TRUE, compress=TRUE)
rpart.plot(dt.model, extra = 4)

# evaluasi hasil dari model
dt.model

#klasifikasi dengan menggunakan raster data 
predict.dt <- predict(predictor, dt.model, ext= extent(predictor), progress = "text", type = "prob")

writeRaster(predict.dt, "pre_dt_Flood_Prediction.tif", format = "GTiff", datatype = "FLT4S", overwrite =TRUE)

plot(predict.dt)



