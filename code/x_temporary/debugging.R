library(dplyr)
source("/global/home/users/yougsanghvi/global_suicide_dummy/code/y_utils/config.R")
df <- read.csv(get_era5_agg_yearly(1979))
glimpse(df)
