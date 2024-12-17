# author: goiabae <goiabae@protonmail.com>
# written against animdl 1.7.11

export extern animdl [
  --version
  --disable-update (-x)
  --help
]

export extern "animdl download" []

# Stream the stream links to the stdout stream for external usage
export extern "animdl grab" [
  --range (-r): string # Select ranges of anime.
  --index: int # Index for the auto flag.
  --log-level: int # Set the integer log level.
  # FIXME: nushell doesn't allow short flags longer than one character
  # -ll: int # Set the integer log level.
  --log-file: path # Set a log file to log everything to.
  --help # Show this message and exit.
  query
]

export extern "animdl schedule" []

def "nu-complete animdl search providers" [] {
  [ 9anime animekaizoku allanime animepahe animeout
    animixplay crunchyroll kamyroll kawaiifu gogoanime
    haho hentaistream marin twist yugen zoro
  ]
}

# Search for an anime in the provider
export extern "animdl search" [
  --provider (-p): string@"nu-complete animdl search providers" # Provider to search in
  --log-level: int # Set the integer log level.
  # FIXME: nushell doesn't allow short flags longer than one character
  # -ll: int # Set the integer log level.
  --log-file: path # Set a log file to log everything to.
  --json (-j) # Output as json
  --help # Show this message and exit.
  query
]


def "nu-complete animdl stream players" [] {
  [ mpv iina vlc celluloid ffplay android ]
}

# Stream your favorite anime by query
export extern "animdl stream" [
  --quality (-q): string # Use quality strings.
  --special (-s): string # Special range selection.
  --range (-r): string # Select ranges of anime.
  --player (-p): string@"nu-complete animdl stream players" # Select which player to play from. (default mpv)
  --player-opts: string # Arguments that are to be passed to the player call
  --index: int # Index for the auto flag.
  --log-level: int # Set the integer log level.
  # FIXME: nushell doesn't allow short flags longer than one character
  # -ll: int # Set the integer log level.
  --log-file: path # Set a log file to log everything to.
  --help # Show this message and exit.
  query
]

export extern "animdl update" []
