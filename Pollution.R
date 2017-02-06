pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_full <- list.files(directory, full.names = TRUE) 
  
  dat <- data.frame()
  
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }
  
  mean(dat[, pollutant], na.rm = TRUE)
}


complete <- function(directory, id = 1:332) {
  files_full <- list.files(directory, full.names = TRUE)
  dat1 <- data.frame()
  
  for (i in id) {
    moni_i <- read.csv(files_full[i])
    nobs <- sum(complete.cases(moni_i))
    tmp <- data.frame(i, nobs)
    dat1 <- rbind(dat1, tmp)
  }
  
  colnames(dat1) <- c("id", "nobs")
  dat1
}


corr <- function(directory, threshold = 0) {
  files_full <- list.files(directory, full.names = TRUE)
  dat2 <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(files_full)) {
    moni_i <- read.csv(files_full[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      dat2 <- c(dat2, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  dat2
}
