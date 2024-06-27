# made for nushell version 0.90.1

let dark_theme = {
    separator: white
    leading_trailing_space_bg: { attr: n }
    header: green_bold
    empty: white
    bool: white
    int: white
    filesize: white
    duration: white
    date: white
    range: white
    float: white
    string: white
    nothing: white
    binary: white
    cellpath: white
    row_index: green_bold
    record: white
    list: white
    block: white
    hints: dark_gray
    search_result: { bg: red fg: white }

    shape_binary: white
    shape_block: white
    shape_bool: purple
    shape_closure: white
    shape_custom: white
    shape_datetime: cyan
    shape_directory: green
    shape_external: white
    shape_externalarg: white
    shape_external_resolved: yellow
    shape_filepath: green
    shape_flag: blue
    shape_float: purple
    shape_garbage: { fg: white bg: red attr: b}
    shape_globpattern: cyan
    shape_int: purple
    shape_internalcall: yellow
    shape_keyword: red
    shape_list: white
    shape_literal: white
    shape_match_pattern: white
    shape_matching_brackets: { attr: n }
    shape_nothing: purple
    shape_operator: cyan
    shape_and: cyan
    shape_or: cyan
    shape_pipe: white
    shape_range: white
    shape_record: white
    shape_redirection: red
    shape_signature: white
    shape_string: green
    shape_string_interpolation: cyan
    shape_table: white
    shape_variable: yellow
    shape_vardecl: yellow
}

$env.config = {
  color_config: $dark_theme
  bracketed_paste: true
  buffer_editor: "emacs"
  edit_mode: emacs
  float_precision: 2
  footer_mode: 20
  render_right_prompt_on_last_line: false
  shell_integration: true
  show_banner: false
  use_grid_icons: false
  use_ansi_coloring: true
  highlight_resolved_externals: true

  cursor_shape: { emacs: block }

  ls: {
    use_ls_colors: true
    clickable_links: true
  }

  rm: { always_trash: true }

  table: {
    mode: none
    index_mode: auto
    show_empty: true
    trim: {
      methodology: wrapping
      wrapping_try_keep_words: true
      truncating_suffix: "..."
    }
  }

  explore: {
    help_banner: true
    exit_esc: false
    command_bar_text: '#C4C9C6'
    status_bar_background: { fg: '#1D1F21' bg: '#C4C9C6' }
    highlight: { bg: 'yellow' fg: 'black' }

    status: {}
    try: {}

    table: {
      split_line: '#404040'

      cursor: true

      line_index: true
      line_shift: true
      line_head_top: true
      line_head_bottom: true

      show_head: true
      show_index: true
    }

    config: { cursor_color: { bg: 'yellow' fg: 'black' } }
  }

  history: {
    max_size: 10000
    sync_on_enter: false
    file_format: "plaintext"
    isolation: false
  }

  completions: {
    case_sensitive: false
    quick: true
    partial: true
    algorithm: "prefix"
    external: { enable: true, max_results: 100, completer: null }
  }

  filesize: {
    metric: true
    format: "auto"
  }

  hooks: { env_change: { PWD: [{ |before, after|
    if not (which direnv | is-empty) {
      direnv export json | from json | default {} | load-env
    }
  }]}}

  menus: [{
      name: completion_menu
      only_buffer_difference: false
      marker: "| "
      type: {
        layout: columnar
        columns: 4
        col_width: 20
        col_padding: 2
      }
      style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
      }
    }
    {
      name: history_menu
      only_buffer_difference: true
      marker: "? "
      type: {
        layout: list
        page_size: 10
      }
      style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
      }
    }
  ]
  keybindings: [
    {
      name: completion_menu
      modifier: none
      keycode: tab
      mode: emacs
      event: {
        until: [
          { send: menu name: completion_menu }
          { send: menunext }
        ]
      }
    }
    {
      name: completion_previous
      modifier: shift
      keycode: backtab
      mode: [emacs, vi_normal, vi_insert]
      event: { send: menuprevious }
    }
    {
      name: history_menu
      modifier: control
      keycode: char_r
      mode: emacs
      event: { send: menu name: history_menu }
    }
    {
      name: undo_or_previous_page
      modifier: control
      keycode: char_z
      mode: emacs
      event: {
        until: [
          { send: menupageprevious }
          { edit: undo }
        ]
       }
    }
    {
      name: paste
      modifier: control
      keycode: char_y
      mode: emacs
      event: { edit: pastecutbufferbefore }
    }
    {
      name: cut
      modifier: control
      keycode: char_w
      mode: emacs
      event: { edit: cutfromstart }
    }
    {
      name: cut-line
      modifier: control
      keycode: char_k
      mode: emacs
      event: { edit: cutfromlinestart }
    }
    {
      name: move_to_line_start
      modifier: control
      keycode: char_a
      mode: emacs
      event: { edit: movetolinestart }
    }
    {
      name: move_to_line_end
      modifier: control
      keycode: char_e
      mode: emacs
      event: { edit: movetolineend }
    }
    {
      name: delete_one_word_backward
      modifier: control
      keycode: backspace
      mode: emacs
      event: { edit: backspaceword }
    }
  ]
}

use completions.nu *
use xdg.nu
use unix.nu
use x11.nu *
use yt.nu
use xbps.nu
use wrappers.nu *
use music.nu
use nix-locate.nu

def "get random" [] { get (random int 0..(($in | length) - 1)) }

def 'from list' [] { lines }

def "from xbps-repodata" [] {
	zstdcat '-'
	| tar --extract --to-stdout --file '-' index.plist
	| jc --plist
	| from json
}

# parses an ipv6 or ipv4 string into a string of digits
def "from ip" [] -> string { python3 -c $"import ipaddress; print\(int\(ipaddress.ip_address\('($in)'\)\)\)" }

def "secret token" [name] {
	let res = ^secret token $name | complete
	if $res.exit_code == 0 {
		$res.stdout | str trim -r
	} else error make {
		msg: "Secret not found."
	}
}

def ip-geolocation [
	ip: string # ip is a string of digits
] -> list<any> {
	let db = xdg data-home  | path join geolite2-city-ipv6-num.csv
	if not ($db | path exists) {
		cd (xdg data-home)
		http get https://cdn.jsdelivr.net/npm/@ip-location-db/geolite2-city-7z/geolite2-city-ipv6-num.csv.7z | save geolite2-city-ipv6-num.csv.7z
		7z x geolite2-city-ipv6-num.csv.7z
	}

	open --raw $db
	| from csv --no-infer --noheaders
	| rename start end country state1 state2 city postcode lat lon timezone
	| where { |it| ($it.start <= $ip) and ($it.end >= $ip) }
	| first
}

def weather [--city (-c): string] -> list<any> {
	let ip = http get https://6.ident.me | from ip
	let geo = ip-geolocation $ip
	http get (do { |x| $x | url join } {
		scheme: https
		host: api.openweathermap.org
		path: /data/2.5/weather
		params: {
			lat: $geo.lat
			lon: $geo.lon
			units: metric
			appid: (secret token openweathermap)
		}
	})
}

module anime-season {
	def season-type [] {
		[ WINTER, SPRING, SUMMER, FALL ]
	}

	export def main [year: int, season: string@season-type ] {
		let query = open ($nu.default-config-dir | path join anime.gql)

		const headers = {
			Content-Type: "application/json"
			Accept: "application/json"
		}

		mut as = []
		mut idx = 0 # page index

		loop {
			let body = {
				query: $query
				variables: {
					page: $idx
					season: $season
					year: $year
				}
			}

			let res = http post -H $headers https://graphql.anilist.co ($body | to json)
			if ($res.data.Page.media | is-empty) { break }

			$as = ($as | append (
				$res.data.Page.media | update relations.nodes { |row|
					$row.relations.nodes | filter { |it| $it.type == ANIME }
				}
			))
			$idx = $idx + 1
		}

		echo $as
	}
}

use anime-season

def "firefox tabs" [] -> list<any> {
  cd ~/.mozilla/firefox
  let default_profile = (open installs.ini | jc --ini | from json | values | get default.0)
  let info = (lz4jsoncat  $'($default_profile)/sessionstore-backups/recovery.jsonlz4' | from json)
  $info.windows
  | reduce -f [] { |w, acc|
    $w.tabs
    | each { |tab|
      $tab
      | get entries
      | select url title
      | last
    }
    | append $acc
  }
}

def "emacs eval" [exp: string, --server_socket (-s): path] -> string {
  let socket = (
    $server_socket
    | default (xdg runtime-dir | path join emacs server)
  )
  do { emacsclient -s $socket -e $exp }
  | complete
  | if $in.exit_code != 0 {
    error make {
      msg: $"Couldn't connect with emacs server: ($in.stderr)"
    }
  } else { $in.stdout }
}

# cookie clicker
def next-ascension [--pretty (-p)] -> list<any> {
  into string
  | ^next-ascension # J script
  | lines
  | first
  | split column ' ' chips cookies
  | first
  | if not $pretty {
    into int chips | into float cookies
  } else {
    $in
  }
}

def inv-instances [] -> list<any> {
	http get "https://api.invidious.io/instances.json" | each { get 1 }
}

def tv [] {
	use iptv.nu

	let user = (iptv default-user)
	let file = (xdg cache-home | path join iptv.json)

	if not ($file | path exists) or (ls -l $file | get modified | first | (date now) - $in | $in > 4wk) {
		iptv get-live-streams $user.name $user.passwd
		| to json
		| save --force $file
	}

	let streams = (open $file | select stream_id name)
	let sel = (try { $streams | input list -f } catch { return })

	$sel
	| iptv m3u-url $user.name $user.passwd $in.stream_id
	| mpv '--user-agent=TiviMate/4.7.0 (Rockchip RPCplus; Android 7.1.2)' $in
}

# a bug prevents this from being an alias
def yt [] {
	use sfeed.nu
	sfeed view -p (which player | first | get path) youtube
}

def pomo [--work(-w): duration, --break(-b): duration] {
	let work = ($work | default 25min)
	let break = ($break | default 15min)
	let langs = (espeak --voices | detect columns | get language)
	mut left = 1hr

	loop {
		let lang = ($langs | get random)

		termdown -c 10 -v $lang ($work | into string)
		notify-send "pomo: work done. go relax" $"($work | format duration min) have passed"

		$left = ($left - $work)
		if ($left <= 0sec) {
			notify-send "pomo: session completed" $"A total of (1hr | format duration min) of work has passed"
			break
		}

		termdown -c 10 -v $lang ($break | into string)
		notify-send "pomo: break's up" $"($break | format duration min) have passed"
	}
}

def album-tracks [] -> list<any> {
	use mbz.nu
	let author = input 'artist name: '
	let artist = mbz search artist $author | input list 'select artist: '
	let group = mbz artist release-groups $artist.id | input list 'select release-group: '
	let release = mbz release-group releases $group.id | input list 'select release: '
	mbz release recordings $release.id
}

alias surch = xbps search
alias wget = ^wget --hsts-file ($env.XDG_DATA_HOME | path join wget.hist)
alias mitmproxy = ^mitmproxy --set $"confdir=($env.XDG_CONFIG_HOME)/mitmproxy"
alias mitmweb = ^mitmweb --set $"confdir=($env.XDG_CONFIG_HOME)/mitmproxy"
alias adb = with-env [HOME $env.ANDROID_USER_HOME] { ^adb }
