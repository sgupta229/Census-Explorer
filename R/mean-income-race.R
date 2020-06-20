library(tidyverse)

#code to save race data from excel file 
#copy and paste each race table and run function
save_excel_data <- function(name) {
  data <- as_tibble(read.table(pipe("pbpaste"), sep = "\t", header = TRUE))
  cols <- c(1, 15)
  data <- data[-1,cols]
  colnames(data) <- c("year", "mean_income")
  data$mean_income <- as.numeric(gsub(",", "", data$mean_income))
  f = paste("data/mean-income-race/", name, sep = "")
  saveRDS(data, file = f)
}

save_excel_data("white")
readRDS(white, file = "data/mean-income-race/white")

save_excel_data("black")
readRDS(white, file = "data/mean-income-race/black")

save_excel_data("asian")
readRDS(white, file = "data/mean-income-race/asian")

