external_menu () {
  rofi -dmenu -p "$1"
}

video_player() {
	mpv --ao=pulse "$@"
}

ytdl_pref="bestvideo[height<=720]+bestaudio/best[height<=720]"
sub_link_count=1

show_thumbnails=0
thumbnail_viewer="chafa"
#async_thumbnails=1

#interface="ext"
notify_playing=1
custom_scrape_search_exclude="recommended"
fancy_subs=1
invidious_instance=https://vid.puffyan.us
log_level=1
is_detach=0
search_result_type=all
nsfw=true
external_menu_len=50
