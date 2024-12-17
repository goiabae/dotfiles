use ~/lib/nu/xdg.nu

def sfeed-parse [] {
  $in
  | from tsv --noheaders
  | rename timestamp title link content content-type id author enclosure category
}

export def feed [feed: string] {
  xdg cache-home
  | path join feeds $feed
  | ls $in
  | each { |file|
    open $file.name | sfeed-parse
  }
  | flatten
}

export def view [
  feed: string
  --plumber (-p): string
  --read_cache (-c): path
] {
  $env.SFEED_PLUMBER = ($plumber | default "/bin/xdg-open")
  $env.SFEED_URL_FILE = ($read_cache | default (xdg cache-home | path join feeds $"($feed).read.list"))
	$env.SFEED_YANKER = "xclip -r -selection clipboard"
  xdg cache-home
  | path join feeds $feed "*"
  | glob $in
  | sfeed_curses ...$in
}

export def list [] {
	ls -s ~/app/feeds
	| get name
	| str replace '.sh' ''
}

export def update [] {
  let feed = $in
  echo ~/app/feeds
  | path join $"($feed).sh"
  | path expand
  | sfeed_update $in
}
