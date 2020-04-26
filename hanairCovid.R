#Author: Hung N.I.
#Gmail: hung.ni96hn@gmail.com
######
#####
####
###
##
#
# Giai doan cach ly bat buoc toan xa hoi: 28/03/2020 - 22/04/2020
# Thoi diem bat dau khuyen khich han che tiep xuc: 18/03/2020
#________________________________________________________________

# Require library -----------------------------------------------
#####
library(worldmet)
library(lubridate)
library(plyr)
library(dplyr)
library(openair)
library(ggplot2)
library(rmweather)
library(ranger)
library(normalweatherr)
library(tibble) #rmw_do_all

setwd("D:/GitHub/hanair/Data")

# install.packages("doMC", repos="http://R-Forge.R-project.org")
# library(devtools)
# install_github("skgrange/enlightenr")
# install_github("skgrange/normalweatherr")

# Prepare raw PM2.5 data ----------------------------------------
#####
# Ha Noi data
airnow_hn_16 <- read.csv("http://dosairnowdata.org/dos/historical/Hanoi/2016/Hanoi_PM2.5_2016_YTD.csv", header = T)
airnow_hn_17 <- read.csv("http://dosairnowdata.org/dos/historical/Hanoi/2017/Hanoi_PM2.5_2017_YTD.csv", header = T)
airnow_hn_18 <- read.csv("http://dosairnowdata.org/dos/historical/Hanoi/2018/Hanoi_PM2.5_2018_YTD.csv", header = T)
airnow_hn_19 <- read.csv("http://dosairnowdata.org/dos/historical/Hanoi/2019/Hanoi_PM2.5_2019_YTD.csv", header = T)
airnow_hn_20 <- read.csv("http://dosairnowdata.org/dos/historical/Hanoi/2020/Hanoi_PM2.5_2020_YTD.csv", header = T)
# Ho Chi Minh data
airnow_hcm_16 <- read.csv("http://dosairnowdata.org/dos/historical/HoChiMinhCity/2016/HoChiMinhCity_PM2.5_2016_YTD.csv", header = T)
airnow_hcm_17 <- read.csv("http://dosairnowdata.org/dos/historical/HoChiMinhCity/2017/HoChiMinhCity_PM2.5_2017_YTD.csv", header = T)
airnow_hcm_18 <- read.csv("http://dosairnowdata.org/dos/historical/HoChiMinhCity/2018/HoChiMinhCity_PM2.5_2018_YTD.csv", header = T)
airnow_hcm_19 <- read.csv("http://dosairnowdata.org/dos/historical/HoChiMinhCity/2019/HoChiMinhCity_PM2.5_2019_YTD.csv", header = T)
airnow_hcm_20 <- read.csv("http://dosairnowdata.org/dos/historical/HoChiMinhCity/2020/HoChiMinhCity_PM2.5_2020_YTD.csv", header = T)

# Prepare raw weather data NOAA ---------------------------------
#####
# Noi Bai data
nb_met <- importNOAA(code = "488200-99999", year = 2016:2020)
# Tan Son Nhat data
tsn_met <- importNOAA(code = "489000-99999", year = 2016:2020)

# Prepare data Hysplit model ------------------------------------
#####
# Require Hysplit software installed
# Add function
getMet <- function (year = 2016:2020, month = 1:12, path_met = "D:/GitHub/hanair/TrajData/")
{
  for (i in seq_along(year))
  {
    for (j in seq_along(month))
    {
      download.file(url = paste0("ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/RP",
                                 year[i],
                                 sprintf("%02d", month[j]),
                                 ".gbl"),
                    destfile = paste0(path_met,
                                      "RP",
                                      year[i],
                                      sprintf("%02d", month[j]),
                                      ".gbl"),
                    mode = "wb")
    }
  }
}

read.files <- function(hours = 96, hy.path)
{
  # hours is the back trajectory time
  files <- Sys.glob("tdump*")
  output <- file('Rcombined.txt', 'w')
  for (i in files)
  {
    input <- readLines(i)
    input <- input[-c(1:7)]
    writeLines(input, output)
  }
  close(output)
  traj <- read.table(paste0(hy.path, "working/Rcombined.txt"), header = FALSE)
  traj <- subset(traj, select = -c(V2, V7, V8))
  traj <- rename(traj, c(V1 = "receptor",
                         V3 = "year",
                         V4 = "month",
                         V5 = "day",
                         V6 = "hour",
                         V9 = "hour.inc",
                         V10 = "lat",
                         V11 = "lon",
                         V12 = "height",
                         V13 = "pressure"))
  year <- traj$year[1]
  if (year < 50) traj$year <- traj$year + 2000 else traj$year <- traj$year + 1900
  traj$date2 <- with(traj, ISOdatetime(year, month, day, hour, min = 0, sec = 0,
                                       tz = "GMT"))
  traj$date <- traj$date2 - 3600 * traj$hour.inc
  traj
}

add.met <- function(month, Year, met, bat.file)
{
  if (month == 0)
  {
    month <- 12
    Year <- as.numeric(Year) - 1
  }
  if (month < 10) month <- paste("0", month, sep = "")
  write.table(paste("echo", met, " >>CONTROL"),
              bat.file,
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE,
              append = TRUE)
  x <- paste("echo RP", Year, month, ".gbl >>CONTROL", sep = "")
  write.table(x,
              bat.file, 
              col.names = FALSE,
              row.names = FALSE, 
              quote = FALSE, 
              append = TRUE)
}

procTraj <- function(lat = 21.221,
                     lon = 105.807,
                     year = 2016:2020,
                     name = "hanoi",
                     met = "D:/GitHub/hanair/TrajData/",
                     out = "D:/GitHub/hanair/TrajProc/",
                     hours = 96,
                     height = 12,
                     hy.path = "C:/hysplit4/")
{
  # hours is the back trajectory time e.g. 96 = 4-day back trajectory
  # height is start height (m)
  # year is period
  # lat,lon, name located
  lapply(c("openair", "plyr", "reshape2"), require, character.only = TRUE)
  setwd(paste0(hy.path, "working/"))
  path.files <- paste0(hy.path, "working/")
  bat.file <- paste0(hy.path, "working/test.bat")
  files <- list.files(path = path.files, pattern = "tdump")
  lapply(files, function(x) file.remove(x))
  start <- paste(year, "-01-01", sep = "")
  end <- paste(year, "-12-31 18:00", sep = "")
  dates <- seq(as.POSIXct(start, "GMT"), as.POSIXct(end, "GMT"), by = "3 hour")
  for (i in 1:length(dates))
  {
    year <- format(dates[i], "%y")
    Year <- format(dates[i], "%Y")
    month <- format(dates[i], "%m")
    day <- format(dates[i], "%d")
    hour <- format(dates[i], "%H")
    x <- paste("echo", year, month, day, hour, " >CONTROL")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE)
    x <- "echo 1 >>CONTROL"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- paste("echo", lat, lon, height, " >>CONTROL")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- paste("echo ", "-", hours, " >>CONTROL", sep = "")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- "echo 0       >>CONTROL
          echo 10000.0 >>CONTROL
          echo 3       >>CONTROL"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    months <- as.numeric(unique(format(dates[i], "%m")))
    months <- c(months, months + 1:2)
    months <- months - 1
    months <- months[months <= 12]
    if (length(months) == 2) months <- c(min(months) - 1, months)
    for (i in 1:3)
      add.met(months[i], Year, met, bat.file)
    x <- "echo ./ >>CONTROL"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- paste("echo tdump", year, month, day, hour, " >>CONTROL", sep = "")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- "c:\\hysplit4\\exec\\hyts_std"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    ## run the file
    system(paste0(hy.path, 'working/test.bat'))
  }
  traj <- read.files(hours, hy.path)
  file.name <- paste(out, name, Year, ".RData", sep = "")
  save(traj, file = file.name)
}

# Download meteorological file
getMet(year = 2016:2020, month = 1:12)

# Run model Hysplit
for (i in 2016:2020)
{
  procTraj(lat = 21.221,
           lon = 105.807,
           year = i,
           name = "hanoi",
           hours = 96,
           met = "D:/GitHub/hanair/TrajData/",
           out = "D:/GitHub/hanair/TrajProc/",
           hy.path = "C:/hysplit4/")
}

# Import data
traj_hn <- importTraj(site = "hanoi", year = 2016:2020, local = "D:/GitHub/hanair/TrajProc/")

# Compile data --------------------------------------------------
#####
# Ha Noi data
airnow_hn <- rbind(airnow_hn_16,
                   airnow_hn_17,
                   airnow_hn_18,
                   airnow_hn_19,
                   airnow_hn_20)
# Filter >0, QC = valid
airnow_hn_valid <- subset(airnow_hn,
                          airnow_hn$QC.Name == "Valid" &
                            airnow_hn$Raw.Conc. > 0)
# Change the column names
names(airnow_hn_valid)[3]  <- paste("date")
names(airnow_hn_valid)[11] <- paste("pm25.hn")
# Format date
airnow_hn_valid$date <- as.POSIXct(strptime(airnow_hn_valid$date,
                                            format = "%Y-%m-%d %I:%M %p",
                                            tz = "Asia/Ho_Chi_Minh"))

# Ho Chi Minh data
airnow_hcm <- rbind(airnow_hcm_16,
                    airnow_hcm_17,
                    airnow_hcm_18,
                    airnow_hcm_19,
                    airnow_hcm_20)
# Filter >0, QC = valid
airnow_hcm_valid <- subset(airnow_hcm,
                           airnow_hcm$QC.Name == "Valid" &
                             airnow_hcm$Raw.Conc. > 0)
# Change the column names
names(airnow_hcm_valid)[3]  <- paste("date")
names(airnow_hcm_valid)[11] <- paste("pm25.hcm")
# Format date
airnow_hcm_valid$date <- as.POSIXct(strptime(airnow_hcm_valid$date,
                                             format = "%Y-%m-%d %I:%M %p",
                                             tz = "Asia/Ho_Chi_Minh"))

# Format date NB weather data
nb_met$date <- as.POSIXct(strptime(nb_met$date,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "GMT"))
nb_met$date <- format(nb_met$date,
                      tz = "Asia/Ho_Chi_Minh")
nb_met$date <- as.POSIXct(strptime(nb_met$date,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "Asia/Ho_Chi_Minh"))

# Format date TSN weather data
tsn_met$date <- as.POSIXct(strptime(tsn_met$date,
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "GMT"))
tsn_met$date <- format(tsn_met$date,
                       tz = "Asia/Ho_Chi_Minh")
tsn_met$date <- as.POSIXct(strptime(tsn_met$date,
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "Asia/Ho_Chi_Minh"))
# Merge data
data_hn <- merge(airnow_hn_valid[,c(3,11)],
                 nb_met[,c(1,9:23)],
                 by = "date",
                 all.x = F,
                 all.y = F)

data_hcm <- merge(airnow_hcm_valid[,c(3,11)],
                  tsn_met[,c(1,9:23)],
                  by = "date",
                  all.x = F,
                  all.y = F)

# Analyze data - Cluster ----------------------------------------
#####
clust_hn <- trajCluster(traj_hn,
                        method = "Angle",
                        n.cluster = 6,
                        plot = TRUE,
                        type = "default",
                        cols = "Set1",
                        split.after = FALSE,
                        map.fill = T,
                        map.cols = "grey40",
                        map.alpha = 0.4,
                        projection = "lambert",
                        parameters = c(51, 51),
                        orientation = c(90, 0, 90),
                        by.type = FALSE,
                        origin = TRUE)

clust_dat_hn <- clust_hn$data
clust_dat_cut_hn <- clust_dat_hn[,c(6,12,16)]
# Format date
clust_dat_cut_hn$date <- format(clust_dat_cut_hn$date,
                                tz = "Asia/Ho_Chi_Minh")
clust_dat_cut_hn$date <- as.POSIXct(strptime(clust_dat_cut_hn$date,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "Asia/Ho_Chi_Minh"))
# Merge data
data_cl_hn <- merge(data_hn, subset(clust_dat_cut_hn, hour.inc == 0), by = "date")
data_cl_summ_hn <- ddply(data_cl_hn, .(cluster), numcolwise(mean), na.rm = TRUE)

clust_hn_plot <- ggplot(data_cl_summ_hn, aes(x= cluster, y = pm25.hn))+
  geom_bar(stat = "identity",
           fill = c("red","blue","green","purple","orange","yellow"))+
  theme_classic()


# Analyze data - rmweather --------------------------------------
#####
data_norm_hn <- mutate(data_cl_hn, cluster_num = substr(cluster,2,2))
data_norm_hn$cluster_num <- as.numeric(data_norm_hn$cluster_num)
data_norm_hn$date_unix <- as.numeric(data_norm_hn$date)
data_norm_hn$day_julian <- as.numeric(format(data_norm_hn$date, "%j"))
data_norm_hn$weekday <- as.numeric(format(data_norm_hn$date, "%u"))


# Prepare data for modelling
data_prepared_hn <- data_norm_hn %>%
  filter(!is.na(ws)) %>%
  dplyr::rename(value = pm25.hn) %>%
  rmw_prepare_data(na.rm = TRUE)

# Grow/train a random forest model and then create a meteorological normalised trend
set.seed(1)
list_normalised_hn <- rmw_do_all(data_prepared_hn,
                                 variables = c("date_unix",
                                               "day_julian",
                                               "weekday",
                                               "cluster_num",
                                               "air_temp",
                                               "RH",
                                               "wd",
                                               "ws"),
                                 n_trees = 300,
                                 n_samples = 300,
                                 verbose = TRUE)

# What units are in the list?
names(list_normalised_hn)

# Check model object's performance
rmw_model_statistics(list_normalised_hn$model)
model_statistics_normalised_hn <- rmw_model_statistics(list_normalised_hn$model)

# Plot variable importances
list_normalised_hn$model %>%
  rmw_model_importance() %>%
  rmw_plot_importance()

plot_model_normalised_hn <- list_normalised_hn$model %>%
  rmw_model_importance() %>%
  rmw_plot_importance()

# Check if model has suffered from overfitting
rmw_predict_the_test_set(model = list_normalised_hn$model,
                         df = list_normalised_hn$observations) %>%
  rmw_plot_test_prediction()

plot_predict_normalised_hn <- rmw_predict_the_test_set(model = list_normalised_hn$model,
                                                       df = list_normalised_hn$observations) %>%
  rmw_plot_test_prediction()

summary(list_normalised_hn)
summary(list_normalised_hn$normalised$value_predict)
summary(list_normalised_hn$observations$value)
summary(lm(list_normalised_hn$normalised$value_predict~
             list_normalised_hn$observations$value))
# How long did the process take?
list_normalised_hn$elapsed_times

# Plot normalised trend
rmw_plot_normalised(list_normalised_hn$normalised)
plot_rmw_normalised_hn <- rmw_plot_normalised(list_normalised_hn$normalised)

# Investigate partial dependencies, if variable is NA, predict all
data_pd_hn <- rmw_partial_dependencies(model = list_normalised_hn$model,
                                       df = list_normalised_hn$observations,
                                       variable = NA)
summary(data_pd_hn)
# Plot partial dependencies
data_pd_hn %>%
  filter(variable != "date_unix") %>%
  rmw_plot_partial_dependencies()

plot_partial_dependencies_hn <- data_pd_hn %>%
  filter(variable != "date_unix") %>%
  rmw_plot_partial_dependencies()

# Analyze data - rf ---------------------------------------------
#####

data_hn$date_unix <- as.numeric(data_hn$date)
data_hn$day_julian <- as.numeric(format(data_hn$date, "%j"))
data_hn$weekday <- as.numeric(format(data_hn$date, "%u"))
data_hn$hour <- as.numeric(format(data_hn$date, "%H"))
data_rf_hn <- mutate(data_hn, value = pm25.hn)

# Look at data
glimpse(data_rf_hn)

# Fraction of observations to form the training set. Default is 0.8 for an 80/20 % split for training and testing sets.
list_input_data_hn <- split_input_data(data_rf_hn, fraction=0.8)
names(list_input_data_hn)

variables <- c("wd", "ws", "air_temp", "RH", "day_julian", "date_unix", "weekday", "hour")

# Build the random forest model
set.seed(123)
model_rf_hn <- calculate_model(list_input_data_hn,
                               variables = variables,
                               mtry = 4,
                               nodesize = 3,
                               ntree=200,
                               model = "rf")

names(model_rf_hn)

# Good performance, r2 ~80 %
model_rf_hn$model

# Normalise for meteorology
testing_hn <- list_input_data_hn$testing

# Allow for parallel processing, a lot quicker
register_cores()

data_normalised_hn <- normalise_for_meteorology(model_rf_hn$model,
                                                testing_hn,
                                                variables = setdiff(variables, variables),
                                                n = 1000)


testing_final_hn <- merge(testing_hn, data_normalised_hn, by = "date")
names(testing_final_hn)
names(testing_final_hn)[23] <- paste("predictions")
names(testing_final_hn)[22] <- paste("observations")

scatterPlot(testing_final_hn,
            x = "observations",
            y = "predictions",
            col= "jet" ,
            method = "density")
plot_testing_final_hn <- scatterPlot(testing_final_hn,
                                     x = "observations",
                                     y = "predictions",
                                     col= "jet" ,
                                     method = "density")

timePlot(selectByDate(testing_final_hn, year = 2020),
         pollutant=c("observations","predictions"),
         group=TRUE)
plot_time_testing_hn <- timePlot(selectByDate(testing_final_hn, year = 2020),
                                 pollutant=c("observations","predictions"),
                                 group=TRUE)

timePlot(testing_final_hn,
         pollutant=c("observations","predictions"),
         group=TRUE)
plot_all_time_testing_hn <- timePlot(testing_final_hn,
                                     pollutant=c("observations","predictions"),
                                     group=TRUE)

modStats(testing_final_hn,
         mod = "predictions",
         obs = "observations",
         statistic = c("n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA"),
         type = "default",
         rank.name = NULL)



pm_lm <- lm(predictions ~ observations, data = testing_final_hn)
summary(pm_lm)

summary(testing_final_hn$predictions)
summary(testing_final_hn$observations)

# Save plot -------------------------------------------------------------------
#####

png(filename = "cluster_angle_num6.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
clust_hn$plot
dev.off()

png(filename = "cluster_barchart.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
clust_hn_plot
dev.off()

png(filename = "plot_model_normalised_hn.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
plot_model_normalised_hn
dev.off()

png(filename = "plot_predict_normalised_hn.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
plot_predict_normalised_hn
dev.off()

png(filename = "plot_rmw_normalised_hn.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
plot_rmw_normalised_hn
dev.off()

png(filename = "plot_partial_dependencies_hn.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
plot_partial_dependencies_hn
dev.off()



# RF
png(filename = "plot_testing_final_hn.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
plot_testing_final_hn
dev.off()

png(filename = "plot_time_testing_hn.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
plot_time_testing_hn
dev.off()

png(filename = "plot_all_time_testing_hn.png",
    width = 10,
    height = 9,
    units = "in",
    pointsize = 12,
    bg = "white", 
    res = 300)
plot_all_time_testing_hn
dev.off()
