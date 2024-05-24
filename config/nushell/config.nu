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

def weather [--city (-c): string] {
  let tab = [
    [name           id     ];
    ["Toledo"       3446370]
    ["Sao Paulo"    3448439]
    ["Paranapanema" 3455061]
  ]
  let i = (if $city == null { "Toledo" } else { $city })
  let id = ($tab | where name == $i | first | get id)
  let appid = "5f3a866fcadbf0e0615e100650378d72"
  http get $"http://api.openweathermap.org/data/2.5/weather?id=($id)&units=metric&appid=($appid)"
}

def "get random" [] {
  $in | get (random int 0..(($in | length) - 1))
}

def www [...query] {
  let search_engine = "https://lite.duckduckgo.com/lite/?q="
  w3m $"($search_engine)($query | str join '+')"
}


def 'mal anime season' [year: string, season: string] {
  let resp = (http get $"https://myanimelist.net/anime/season/($year)/($season)")
  let categories = ($resp | query web --as-html --query '.seasonal-anime-list')
  let seasonal_anime = (
    $categories | each { |cat|
      $cat
      | query web --as-html --query '.seasonal-anime'
      | each { |anime|
        { release: ($anime | query web --query '.info .item' | first | into datetime)
            title: ($anime | query web --query '.title-text' | str trim | first)
            score: ($anime | query web --query '.information .score' | str trim | first)
          members: ($anime | query web --query '.information .member' | str trim | first)
            cover: (do {
              let src = ($anime | query web --query 'img' --attribute 'src')
              let data_src = ($anime | query web --query 'img' --attribute 'data-src')
              $src | append $data_src | where $it != '' | first
            })
         synopsis: ($anime | query web --query '.synopsis p' | first | str replace '\n\(Source: .*\)' '')
          # studio, source themes -> .synopsis .properties |> each :key [0] :value [1]
        }
      }
      | insert category ($cat | query web --query '.anime-header' | first)
    }
    | flatten
    | str downcase category
  )
  echo $seasonal_anime
}


def 'from list' [] { $in | lines }

module firefox {
  export def tabs [] {
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
}

use firefox

def "emacs eval" [exp: string, --server_socket (-s): path]  {
  let socket = (
    $server_socket
    | default (xdg runtime-dir | path join emacs server)
  )
  do { emacsclient -s $socket -e $exp }
  | complete
  | if $in.exit_code != 0 {
    error make {
      msg: "Couldn't connect with emacs server"
    }
  } else { $in.stdout }
}

def next-ascension [--pretty (-p)] {
  into string
  | ^next-ascension
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

def inv-instances [] {
	let url = {
		scheme: https
		host: api.invidious.io
		path: /instances.json
	}
	$url
	| url join
	| http get $in
	| each { get 1 }
}

def s [] { ls | grid --color }

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

def "from xbps-repodata" [] {
	zstdcat '-'
	| tar --extract --to-stdout --file '-' index.plist
	| jc --plist
	| from json
}

use mbz.nu
def album-tracks [] {
  let author = input 'artist name: '
  let as = mbz search artist $author
  let rgs = mbz artist release-groups ($as | input list 'select artist' | get id)
  let rs = mbz release-group releases ($rgs | input list 'select release-group' | get id)
  mbz release recordings ($rs | input list 'select release' | get id)
}
alias surch = xbps search
alias wget = ^wget --hsts-file ($env.XDG_DATA_HOME | path join wget.hist)
alias mitmproxy = ^mitmproxy --set $"confdir=($env.XDG_CONFIG_HOME)/mitmproxy"
alias mitmweb = ^mitmweb --set $"confdir=($env.XDG_CONFIG_HOME)/mitmproxy"
alias adb = with-env [HOME $env.ANDROID_USER_HOME] { ^adb }
