library(tidyverse)
library(spotifyr)
library(httpuv)
library(fs)
library(lubridate)

#read in playlists

all <- dir_ls("Months") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols("Release Date" = col_character())) %>% 
  mutate(date = str_sub(source, start = 8, end = -5)) %>% 
  mutate(playlist_month = my(date)) %>% 
  separate(date, into = c("Month", "Year"), sep="_")

write_csv(all, "all.csv")



cleaned <- all %>% 
  janitor::clean_names() %>% 
  mutate(release_date = parse_date_time(release_date,c("ymd","dmy","Y")),
         release_year = year(release_date),
         )


long_genre <- cleaned %>% 
  separate_rows(genres, sep = ",")


long_genre %>% 
  filter(year == 2022) %>% 
  group_by(genres) %>% 
  summarize(n_songs = n()) %>% 
  arrange(desc(n_songs)) %>% 
  print(n = Inf)


twenty_twenty_two <- cleaned %>% 
  filter(year == 2022)

ggplot(twenty_twenty_two, aes(x = release_year))+
  geom_histogram() +
  theme_minimal()

twenty_twenty_two %>%
  group_by(artist_name_s) %>% 
  summarize(n_songs = n()) %>% 
  arrange(desc(n_songs))





########################################

# Sys.setenv(SPOTIFY_CLIENT_ID = '1e14468cae294aa1b6f5e239b2377890')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = '87ce32c141f144759d33e65f3f2961db')
# 
# access_token <- get_spotify_access_token()
# authorization_code <- get_spotify_authorization_code(scope = c(
# "playlist-modify-private",
# "playlist-read-collaborative",
# "playlist-read-private",
# "playlist-modify-public",
# "user-library-modify",
# "user-library-read"))
# 
# get_my_playlists()
# 
# get_playlist(playlist_id = "3pduCZMWXyK0L3jEW089SR")
# 
# https://open.spotify.com/playlist/3pduCZMWXyK0L3jEW089SR?si=ce3818cda0c342b1


