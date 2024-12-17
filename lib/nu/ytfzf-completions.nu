def history-clear [] {
  [search, watch]
}

def search-source [] {
  [args, prompt, hist, next, fn-args]
}

def features [] {
  [hd, subtitles, creative_commons, "3d", live, "4k", 360, location, hdr]
}

def thumbnail-quality [] {
  [maxres, maxresdefault, sddefault, high, medium, default,
    start # The first frame of the video (low quality)
    middle # The middle frame of the video (low quality)
    end # The end frame of the video (low quality)
  ]
}

def type [] { [video, playlist, channel, all] }

def video-duration [] { [short, long] }

def upload-date [] {
  [hour, today, week, month, year]
}

def scrapers [] {
  [
    youtube, Y # Use Invidious API with a search query
    youtube-channel # Scrapes a youtube channel with youtube
    invidious-channel # Scrapes a youtube channel with $invidious_instance
                      # When this scrape is active the search query is the
                      # link to a channel.
    video-recommended, R # Scrapes recommended videos from an invidious video link
    youtube-playlist     # Scrapes a youtube playlist
    invidious-playlist   # Scrapes a youtube playlist from invidious
    youtube-trending, T  # Use Invidious' API to get youtube trending
    multi, M             # Use ytfzf revursively to scrape multiple things with multiple different options
                         # Tabs:
                         #   gaming
                         #   music
                         #   movies
    youtube-subscriptions, S, SI # Scrape invidious for channels instead of youtube
    scrape-list, SL # uses your YTFZF_SCRAPELIST_FILE as scrape and search input
    peertube, P
    odysee, lbry, O
    history, H
    url, U               # Opens the url in the video player and exits
    u                    # Treats the url as an item, and will open fzf, or the external menu
    comments             # Scrapes the comments of a video link froom youtube
  ]
}
def interfaces [] {}
def url-handlers [] {}
def extensions [] {}
def information [] {
  echo [
    L, l, link,         # Prints the URL of the selected videos
    VJ, vj, video-json, # Prints the json of the selected videos
    J, j , json,        # Prints the json of all the videos in the search results
    F, f, format,       # Prints the video format being used
    R, r, raw           # Prints the data of the selected videos, as appears in the menu
  ]
}

def "format selection" [] {
  echo [normal, simple]
}

def preview-side [] {
  echo [
    left,
    right,
    up,
    down
  ]
}

def thumb-viewer [] {
  let default = [
    chafa
    chafa-16  # chafa with 16 colors
    chafa-tty # chafa with 4 colors
    catimg
    catimg-256 # catimg with 256 colors
    imv        # Good with tiling window managers
    mpv        # Similar to imv
    kitty      # For the kitty terminal
    swayimg    # Only works on the sway wayland compositor
  ]
  (if (YTFZF_THUMBNAIL_VIEWERS_DIR in (env).name) {
    ls $env.YTFZF_THUMBNAIL_VIEWERS_DIR | get name
  } else []) ++ $default
}

# TODO get sort functions/scripts names
def sort-name [] {
  []
}

def sort_by [] {
  [
    relevance
    rating # youtube only
    upload_date
    oldest_first # odysee only
    view_count # youtube only
  ]
}

# The search-query can also be read from stdin
export extern ytfzf [
  --help (-h)                      # Show this help text
  --download (-d)                      # Download the selected video(s)
  --audio-only (-m)                      # Only play audio
  --formats (-f)                      # Select a video format before playing
  --format-selection: string@"format selection" # The format selection screen type to use
  --format-sort: string   # The --format-sort to use in ytdl
  --video-pref: string    # Set the ytdl video format to pref
  --audio-pref: string    # Set the ytdl audio format to pref
  --ytdl-pref: string     # Set the ytdl format to pref
  --detach                # Detach the video player from the terminal
  -L                      # Alias for -I L
  --info-action: string   # The action to do when --info-wait is 1
  --info-wait: number     # Wheter or not to wait after printing info requested with
                          # -I or -L.
  --url-handler-opts: string # Opts to pass to the url handler, by default this will pass extra
                             # opts to mpv.
  --loop (-l)                      # Reopen the menu when the video stops playing
  --search-again (-s)                      # After closing fzf make another search
  -q                      # Use a search query from search history
  -L                      # Show the link of selected video(s)
  --auto-select (-a)      # Automatically select the first video
  --random-select (-r)    # Automatically select a random video
  --select-all (-A)       # Select all results
  --select (-S): string   # Automatically selects a specific video based on a given sed address,
                          # see ytfzf(1) for more info.
  --link-count (-n): number          # The amount of videos to select with -a and -r
  --scrape (-c): string@scrapers     # The scraper to use,
                                     #     See ytfzf(1) for a list of builtin scrapers
                                     #     you can use multiple scrapers by separating each with a comma, eg: youtube,odysee
  # --scrape+=scrapers # Same as -c, but keeps the default scrape as well.
  # --scraper-=scrapers # Removes scraper from list of scrapers to use
  --history (-H)                     # alias for -c H
  --show-thumbnails (-t)  # Show thumbnails
  --thumb-viewer: string@thumb-viewer # Use program for displaying thumbnails
  --async-thumbnails      # Whether or not to download thumbnails asynchonously
  --skip-thumb-download   # Whether or not to skip the thumbnail download
  --notify-playing        # Show notifications when a video is about to be played,
                          # and other notifications relating to playing videos
  --preview-side: string@preview-side  # The preview side in fzf
  -T: string              # The program to use for displaying thumbnails.
  --interface (-i): string@interfaces   # The interface to use (default: text)
  --external-menu (-D)                      # Alias for -i ext
  --url-handler (-u): string@url-handlers # The program to use for handling urls (deafult: multimedia_player)
  --ext (-e): string@extensions   # Load an extention
  -I: string@information  # Instead of playing the selected video(s), get information about them.
                          #     Options can be separated with a comma, eg: L,R
                          #     Options for info:
                          #       L:         print the link of the video
                          #       VJ:        print the json of the video
                          #       J:         print the json of all videos shown in the search
                          #       R:         print the data of the selected videos, as appears in the menu
                          #       F:         print the selected video format
  --history-clear (-x): string@history-clear                      # Clear search and watch history (use --history-clear=<search|watch> to specify 1)
  --disable-submenus      # Whether or not to disable submenus, which are menus for things like playlists and channels
  --version               # Get the current version
  --version-all           # Get the current version of ytfzf, and required dependencies
  --sort                  # Sort videos (after scraping) by upload date
  --sort-name: string     # Calls a function set in YTFZF_CONFIG_FILE or sources a script in YTFZF_SORT_NAMES_DIR if it exists.
  --fancy-subs            # Whether or not to have a separator between each subscription
  --disable-back          # Whether or not to disable the back button in submenus
  --disable-submenus      # Whether or not to disable submenus
  --keep-vars             # Whether or not options passed into ytfzf also get passed into submenus
  --submenu-opts: string  # The opts to use in the submenu
  --multi-search # Whether or not to use multi search
  --force-youtube # When using the youtube scraper, convert the invidious links to youtube links before playing/downloading
# Scraper Options
#   Currently, --video-duration, --type, --thumbnail-quality, and
#   --features only applies to the scrape: youtube/Y
  --pages: number # Amount of pages to scrape on youtube/invidious, and the comments scraper
  --pages-start: string # The page to start on
  --max-threads: number # Amount of threads that can be used to scrape youtube search, playlists, and channels
  --single-threaded
  --odysee-video-count: number
  --nsfw
  --sort-by: string@sort_by
  --upload-date: string@upload-date # Works with youtube/Y and odysee/O
  --video-duration: string@video-duration # Whether or not to search for long or short videos
  --type: string@type # The type of results to get
  --thumbnail-quality (-T): string@thumbnail-quality # Select the quality of the thumbnails
  --features: string@features # The features to have on a video as CSV
  --region # The region (country code) to search
  --ii: string # Use a different invidious instance
  --invidious-instance: string # Use a different invidious instance
  --rii
  --refresh-inv-instance
  --available-inv-instances # Prints the invidious instances that may be used and exits
  --channel-link # Converts channel links from 'https://youtube.com/c/name' to 'https://youtube.com/channel/id'
  --search-source: string@search-source # The source to use for the search query
  --keep-cache # Whether or not to keep cache after ytfzf exists
  --ytdl-opts: string # Pass command-line options to youtube-dl when downloading
  --ytdl-path: path # Specify the path to youtube-dl (or similar) for downloading
  --list-addons # Lists all addons and exits
  --thumbnail-log # Sets the file to log thumbnail debug info to
  ...query
]
