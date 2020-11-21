# script name:
# plumber.R

# set API title and description to show up in http://localhost:8000/__swagger__/

#' @apiTitle Run predictions for Credit Scoring Pegadaian
#' @apiDescription This API takes as CS Engine.

# load model
# this path would have to be adapted if you would deploy this
load("model17.rda")

#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return

#' predict model
#' @param id:numeric Id Pengajuan
#' @param STATUS_KAWIN:numeric Status Pernikahan
#' @param JENIS_KELAMIN Jenis Kelamin Calon Nasabah
#' @param usia Usia Calon Nasabah
#' @param SUMBER_DANA Sumber penghasilan nasabah
#' @param PENDIDIKAN Pendidikan terakhir nasabah
#' @param TIPE_PENDAPATAN Jenis sumber pendapatan calon nasabah
#' @param PROFESI Pekerjaan calon nasabah
#' @param TOTAL_TAKSIRAN Total Taksiran Agunan
#' @param PENDAPATAN_BERSIH Pendapatan Bersih Calon Nasabah
#' @param JUMLAH_TANGGUNGAN Jumlah Tanggungan
#' @param JumlahFasilitas Jumlah Fasilitas yang dimiliki
#' @get /predict
#' @html
#' @response 200 Returns the class 
calculate_prediction <- function(id,
                                 TIPE_PENDAPATAN,
                                 PROFESI,
                                 STATUS_KAWIN,
                                 JENIS_KELAMIN,
                                 usia,
                                 SUMBER_DANA,
                                 PENDIDIKAN,
                                 TOTAL_TAKSIRAN,
                                 JUMLAH_TANGGUNGAN,
                                 JumlahFasilitas) {

  # make data frame
  input_data_fac <- data.frame(TIPE_PENDAPATAN,PROFESI,STATUS_KAWIN,JENIS_KELAMIN,usia,SUMBER_DANA,PENDIDIKAN)
  
  input_data_fac$TIPE_PENDAPATAN <- factor(input_data_fac$TIPE_PENDAPATAN, levels = c( "1","2","3","4","5","6","7","8","9","10","11","12"))
  input_data_fac$PROFESI <- factor(input_data_fac$PROFESI,levels = c("1","2","3","4"))
  input_data_fac$STATUS_KAWIN <- factor(input_data_fac$STATUS_KAWIN,levels = c("1","2","3","4"))
  input_data_fac$JENIS_KELAMIN <- factor(input_data_fac$JENIS_KELAMIN,levels = c("1","2"))
  input_data_fac$usia <- factor(input_data_fac$usia, levels = c("1","2","3","4"))
  input_data_fac$SUMBER_DANA <- factor(input_data_fac$SUMBER_DANA,levels = c("1","2","3","4","5"))
  input_data_fac$PENDIDIKAN <- factor(input_data_fac$PENDIDIKAN,levels = c("1","2","3","4"))

  
  
  input_data_num <- data.frame(TOTAL_TAKSIRAN,JUMLAH_TANGGUNGAN,JumlahFasilitas)
  # and make sure they really are numeric
  input_data_num <- as.data.frame(t(sapply(input_data_num, as.numeric)))
  
  # combine into one data frame
  status <- "0"
  status <- as.character(status)
  status <- as.numeric(status)
  input_data <- cbind(input_data_num,status, input_data_fac)
  input_data$status <- factor(input_data$status,levels = c(0,1))
  library(mlr)
  dat <- createDummyFeatures(
    input_data, target = "status")
  dat$status <- as.numeric(dat$status)
  dat$status <- as.factor(dat$status)
  dat <- as.data.frame(dat)
  testtask <- makeClassifTask (data = dat,target = "status",positive = 1)
  
  # predict and return result
  pred_rf <- predict(xgmodel, testtask)
  paste("----------------\nTest case predicted to be",id,";",as.character(round(pred_rf$data$prob.1,4)), "\n----------------\n")
}
