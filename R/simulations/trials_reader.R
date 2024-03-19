
source("R/methods/run_methods.R")

data_folder <- "data/trials"
# get all the files in the foldeer that contain _k_ and _s_
files.4 <- list.files(data_folder, pattern = "Scenario_4")
files.4 <- files.4[grepl("_k_", files.4)]
files.4
files.4

file.4 <- files.4[1]
file.4
file_name <- paste0(data_folder, "/", file.4)
data <- read.table(file_name, header = FALSE, sep = "," , nrow = 10)


df.control <- data.frame(data[data[,1] == "control",-1],stringsAsFactors = FALSE)
df.control <- data.frame(lapply(df.control, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
control_mean <- mean(colMeans(df.control))

df.treatment <- data.frame(data[data[,1] == "treatment",-1],stringsAsFactors = FALSE)
df.treatment <- data.frame(lapply(df.treatment, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
treatment_mean <- mean(colMeans(df.treatment))
mild <- 1*0.6 + 2*0.3 +3*0.1
moderate <- 1*0.3 + 2*0.6 +3*0.1
severe <- 1*0.2 + 2*0.3 +3*0.5

moderate/mild
severe/mild
treatment_mean/control_mean


  # get all the files in the foldeer that contain _k_ and _s_
files <- list.files(data_folder, pattern = "Scenario_2")
files <- files[grepl("_s_", files)]
files <- files[grepl("_k_", files)]
files
for(file in files) {
  # get the scenario name
  k <- strsplit(file, "_")[[1]][4]
  s <- strsplit(file, "_")[[1]][6]
  # remove .csv
  s <- substr(s, 1, nchar(s) - 4)

  print(k)
  print(s)

  # load data
  data <- read.csv(paste0(data_folder, "/", file))
  p_values <- get_values(data, "p_value")
  print(colMeans(p_values < alpha))
}


files.3 <- list.files(data_folder, pattern = "Scenario_3")
files.3 <- files.3[grepl("_l_", files.3)]
files.3 <- files.3[grepl("_k_", files.3)]
files.3

for(file.3 in files.3) {
  # get the scenario name
  k <- strsplit(file.3, "_")[[1]][4]
  l <- strsplit(file.3, "_")[[1]][6]
  # remove .csv
  s <- substr(l, 1, nchar(l) - 4)

  print(k)
  print(l)

}
files.4 <- list.files(data_folder, pattern = "Scenario_4")
files.4 <- files.4[grepl("_k_", files.4)]
files.4
for (file.4 in files.4) {
  # get the scenario name
  k <- strsplit(file.4, "_")[[1]][4]
  # remove .csv
  k <- substr(k, 1, nchar(k) - 4)

  print(k)

  # load data
  data <- read.csv(paste0(data_folder, "/", file.4))
  p_values <- get_values(data, "p_value")
  print(colMeans(p_values < alpha))
}


