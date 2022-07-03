library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

## Objective: Analisis hubungan panjang jalan dengan tingkat kemacetan
## - banyak argumen yang mengatakan kalau panjang jalan tidak cukup 
##   untuk menampung kapasitas mobil
## - padahal dengan adanya jalan tersebut, hal itu yang menyebabkan kemacetan
## TODO 
## [x] Analisis perkembangan panjang jalan di ASEAN
## [] Analisis perkembangan panjang jalan di kota-kota ASEAN
## [] Analisis perkembangan indeks kemacetan
## [] Draft outline tulisan dalam markdown
## [] Tema plot khusus cerita kata data
## [] Template folder cerita kata data

# create function to convert
strip_and_convert <- function(x) {
  convert_list <- as.integer(gsub(" ", "", x)) # menghapus whitespace
  convert_list[is.na(convert_list)] <- 0 # mengganti nilai NA dengan 0
  return(convert_list)
}


#### Analisis perkembangan panjang jalan di ASEAN ####

path <- "~/Downloads/pivot.csv"
df <- read.csv(path, header = TRUE, skip = 1)

# convert string to integer save as list
df_convert <- lapply(df[,seq(2, dim(df)[2], 1)], strip_and_convert)

# concatenate with country and save to data frame
final_df <- cbind(country = df$country, data.frame(df_convert)) %>%
  gather(Tahun, road_length, -country) %>%
  mutate(year = as.integer(substr(Tahun, 2, 6))) 

final_df %>%
  filter(year < 2020) %>%
  ggplot(aes(x = year, 
             y = road_length, 
             group = country, 
             color = country)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2008:2019) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = "Tahun", 
       y = "Total Panjang Jalan", 
       color = "Negara ASEAN") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

#### Analisis perkembangan jumlah mobil di ASEAN ####

path_cars <- "~/Downloads/total_number_registered_road_motor_vehicles.csv"
df_cars <- read.csv(path_cars, skip = 1)

df_cars

df_cars_final <- lapply(df_cars[,seq(2, dim(df_cars)[2], 1)], 
       strip_and_convert) %>%
  data.frame() %>%
  cbind(country = df_cars$country, .) %>%
  gather(Tahun, number_of_cars, -country) %>%
  mutate(year = as.integer(substr(Tahun, 2, 6)))

df_cars_final %>%
  filter(year < 2020) %>%
  ggplot(aes(x = year, 
             y = number_of_cars, 
             group = country, 
             color = country)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2008:2019) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = "Tahun", 
       y = "Total Jumlah Mobil yang Tergistrasi", 
       color = "Negara ASEAN") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

left_join(df_cars_final, final_df, by = c("country", "year")) %>%
  select(country, year, road_length, number_of_cars) %>%
  filter(year < 2020) %>%
  ggplot(aes(x = road_length, 
             y = number_of_cars, 
             color = country)) +
  geom_point() +
  geom_line() +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::comma_format()) +
  theme_bw() +
  labs(x = "Panjang Jalan (Log 10)", 
       y = "Banyak nya Mobil (Log 10)", 
       color = "Negara ASEAN") +
  facet_wrap(.~country, 
             scales = "free")

## Semua negara ASEAN memiliki perkembangan jalan yang linear dengan pertumbuhan
## panjang jalan. Apa dampaknya dengan kemacetan?