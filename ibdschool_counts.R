library(tuber)
library(tidyverse)
library(lubridate)
library(purrr)


yt_oauth(app_id = tuber_oauth_id, 
         app_secret = tuber_secret)

# stats on a single video
get_stats(video_id = "ezBvvgxjpqE")

# channel info
ibd_school <- list_channel_resources(filter = c(channel_id = "UCPv0O52oxpVo1el1_JsJNYg"), part="contentDetails")

# playlist info - we want playlist 4
playlists <- get_playlists(filter = c(channel_id = "UCPv0O52oxpVo1el1_JsJNYg"), part = "contentDetails", simplify = TRUE)

# Uploaded playlists:
playlist_id <- ibd_school$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos on the playlist
vids <- get_playlist_items(filter= c(playlist_id=playlist_id), max_results = 100) 

# Video ids
vid_ids <- as.vector(vids$contentDetails.videoId)
vid_ids_df <- as.data.frame(vid_ids)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
} 

# function to get titles for all vids
get_all_titles <- function(id) {
  get_video_details(video_id=id)$items[[1]]$snippet$localized$title
} 

# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats)
res_df <- do.call(rbind, lapply(res, data.frame))

head(res_df)

#get_video_details(video_id="ezBvvgxjpqE")

titles_df <- lapply(vid_ids, get_all_titles) %>% unlist() %>% as_tibble() %>% cbind(vid_ids_df)

res_df <- left_join(res_df, titles_df, c( "id"= "vid_ids")) 

# viewcounts OK up to now

res_df %>% 
  filter(str_detect(value, "^IBD")) %>% 
  purrr::set_names("id", "view_count", "like_count", "dislike_count", "fav_count", "comm_count", "title") %>% 
  relocate(title) %>% 
  mutate(across(ends_with("_count"), as.character)) %>%   mutate(across(ends_with("_count"), as.numeric))->
res_df

# total views
res_df %>% summarise(total_views = sum(view_count))
