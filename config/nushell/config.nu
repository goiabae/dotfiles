# for more information on themes see
# https://www.nushell.sh/book/coloring_and_theming.html
let dark_theme = {
    separator: white
    leading_trailing_space_bg: { attr: n }
    header: green_bold
    empty: blue
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

    shape_and: purple_bold
    shape_binary: purple
    shape_block: white
    shape_bool: purple
    shape_closure: green_bold
    shape_custom: green
    shape_datetime: cyan
    shape_directory: green
    shape_external: white
    shape_externalarg: white
    shape_filepath: green
    shape_flag: blue
    shape_float: purple
    shape_garbage: { fg: "#FFFFFF" bg: "#FF0000" attr: b}
    shape_globpattern: cyan
    shape_int: purple
    shape_internalcall: yellow
    shape_list: white
    shape_literal: white
    shape_match_pattern: green
    shape_matching_brackets: { attr: n }
    shape_nothing: light_cyan
    shape_operator: white
    shape_or: purple_bold
    shape_pipe: purple_bold
    shape_range: cyan
    shape_record: white
    shape_redirection: purple_bold
    shape_signature: blue
    shape_string: green
    shape_string_interpolation: cyan
    shape_table: white
    shape_variable: yellow
    shape_vardecl: purple
}

let light_theme = {
    separator: dark_gray
    leading_trailing_space_bg: { attr: n }
    header: green_bold
    empty: blue
    bool: dark_gray
    int: dark_gray
    filesize: dark_gray
    duration: dark_gray
    date: dark_gray
    range: dark_gray
    float: dark_gray
    string: dark_gray
    nothing: dark_gray
    binary: dark_gray
    cellpath: dark_gray
    row_index: green_bold
    record: white
    list: white
    block: white
    hints: dark_gray

    shape_garbage: { fg: "#FFFFFF" bg: "#FF0000" attr: b}
    shape_binary: purple_bold
    shape_bool: light_cyan
    shape_int: purple_bold
    shape_float: purple_bold
    shape_range: yellow_bold
    shape_internalcall: cyan_bold
    shape_external: cyan
    shape_externalarg: green_bold
    shape_literal: blue
    shape_operator: yellow
    shape_signature: green_bold
    shape_string: green
    shape_string_interpolation: cyan_bold
    shape_datetime: cyan_bold
    shape_list: cyan_bold
    shape_table: blue_bold
    shape_record: cyan_bold
    shape_block: blue_bold
    shape_filepath: cyan
    shape_directory: green
    shape_globpattern: cyan_bold
    shape_variable: purple
    shape_flag: blue_bold
    shape_custom: green
    shape_nothing: light_cyan
    shape_matching_brackets: { attr: n }
}

# The default config record. This is where much of your global
# configuration is setup.
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

  cursor_shape: {
    emacs: block
    vi_insert: line
    vi_normal: block
  }

  ls: {
    use_ls_colors: true
    clickable_links: true
  }

  rm: {
    always_trash: true
  }

  cd: {
    abbreviations: true
  }

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

    status: {
      # warn: {bg: 'yellow', fg: 'blue'}
      # error: {bg: 'yellow', fg: 'blue'}
      # info: {bg: 'yellow', fg: 'blue'}
    }

    try: {
      # border_color: 'red'
      # highlighted_color: 'blue'
      # reactive: false
    }

    table: {
      split_line: '#404040'

      cursor: true

      line_index: true
      line_shift: true
      line_head_top: true
      line_head_bottom: true

      show_head: true
      show_index: true

      # selected_cell: {fg: 'white', bg: '#777777'}
      # selected_row: {fg: 'yellow', bg: '#C1C2A3'}
      # selected_column: blue

      # padding_column_right: 2
      # padding_column_left: 2

      # padding_index_left: 2
      # padding_index_right: 1
    }

    config: {
      cursor_color: { bg: 'yellow' fg: 'black' }

      # border_color: white
      # list_color: green
    }
  }


  history: {
    max_size: 10000
    sync_on_enter: false
    file_format: "plaintext"
    history_isolation: true
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

  hooks: {
    pre_prompt:    [{ || null }]
    pre_execution: [{ || null }]
    env_change: { PWD: [{ |before, after| null }] }
    display_output: { ||
      # if (term size).columns >= 100 {
      #   with-env [config ($env.config | update table.mode "rounded")] {
      #     table -e
      #   }
      # } else {
      #   $in
      # }
    }
    command_not_found: { || null }
  }

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
    {
      name: help_menu
      only_buffer_difference: true
      marker: "? "
      type: {
        layout: description
        columns: 4
        col_width: 20
        col_padding: 2
        selection_rows: 4
        description_rows: 10
      }
      style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
      }
    }
    {
      name: commands_menu
      only_buffer_difference: false
      marker: "# "
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
      source: { |buffer, position|
        $nu.scope.commands
        | where command =~ $buffer
        | each { |it| {value: $it.command description: $it.usage} }
      }
    }
    {
      name: vars_menu
      only_buffer_difference: true
      marker: "# "
      type: {
        layout: list
        page_size: 10
      }
      style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
      }
      source: { |buffer, position|
        $nu.scope.vars
        | where name =~ $buffer
        | sort-by name
        | each { |it| {value: $it.name description: $it.type} }
      }
    }
    {
      name: commands_with_description
      only_buffer_difference: true
      marker: "# "
      type: {
        layout: description
        columns: 4
        col_width: 20
        col_padding: 2
        selection_rows: 4
        description_rows: 10
      }
      style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
      }
      source: { |buffer, position|
        $nu.scope.commands
        | where command =~ $buffer
        | each { |it| {value: $it.command description: $it.usage} }
      }
    }
  ]
  keybindings: [
    {
      name: completion_menu
      modifier: none
      keycode: tab
      mode: emacs # Options: emacs vi_normal vi_insert
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
      name: next_page
      modifier: control
      keycode: char_x
      mode: emacs
      event: { send: menupagenext }
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
      name: yank
      modifier: control
      keycode: char_y
      mode: emacs
      event: {
        until: [
          {edit: pastecutbufferafter}
        ]
      }
    }
    {
      name: unix-line-discard
      modifier: control
      keycode: char_u
      mode: [emacs, vi_normal, vi_insert]
      event: {
        until: [
          {edit: cutfromlinestart}
        ]
      }
    }
    {
      name: kill-line
      modifier: control
      keycode: char_k
      mode: [emacs, vi_normal, vi_insert]
      event: {
        until: [
          {edit: cuttolineend}
        ]
      }
    }
    # Keybindings used to trigger the user defined menus
    {
      name: commands_menu
      modifier: control
      keycode: char_t
      mode: [emacs, vi_normal, vi_insert]
      event: { send: menu name: commands_menu }
    }
    {
      name: vars_menu
      modifier: alt
      keycode: char_o
      mode: [emacs, vi_normal, vi_insert]
      event: { send: menu name: vars_menu }
    }
    {
      name: commands_with_description
      modifier: control
      keycode: char_s
      mode: [emacs, vi_normal, vi_insert]
      event: { send: menu name: commands_with_description }
    }
  ]
}

use ~/lib/nu/completions.nu *
use ~/lib/nu/xdg.nu
use ~/lib/nu/sfeed.nu
use ~/lib/nu/unix.nu
use ~/lib/nu/x11.nu *
use ~/lib/nu/yt.nu
use ~/lib/nu/xbps.nu

register ~/bin/nu.d/nu_plugin_query
register ~/bin/nu.d/nu_plugin_formats

def weather [--city (-c): string] {
  let tab = [
    [name           id     ];
    ["Toledo"       3446370]
    ["Sao Paulo"    3448439]
    ["Paranapanema" 3455061]
  ]
  let i = (if $city == $nothing { "Toledo" } else { $city })
  let id = ($tab | where name == $i | first | get id)
  let appid = "5f3a866fcadbf0e0615e100650378d72"
  http get $"http://api.openweathermap.org/data/2.5/weather?id=($id)&units=metric&appid=($appid)"
}

def "get random" [] {
  $in | get (random integer 0..(($in | length) - 1))
}

def www [...query] {
  let search_engine = "https://lite.duckduckgo.com/lite/?q="
  w3m $"($search_engine)($query | str join '+')"
}

def 'flatpak search' [query: string] {
  ^flatpak search $query
  | from tsv --noheaders
  | rename name description id version branch remote
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
            cover: ($anime | query web --query 'img' --attribute 'src' | first)
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

def free [] {
  ^free
  | lines
  | skip
  | parse -r '(?P<name>[[:alpha:]]*):\s+(?P<total>[0-9]*)\s+(?P<used>[0-9]*)\s+(?P<free>[0-9]*)\s+(?P<shared>[0-9]*)\s+(?P<buff>[0-9]*)\s+(?P<available>[0-9]*)'
  | rename name total used free shared buff available
  | str downcase name
}


def 'from list' [] { $in | lines }

def fc-list [] {
  ^fc-list | lines | parse '{path}: {name}:style={style}'
}

def 'firefox tabs' [] {
  cd ~/.mozilla/firefox
  let default_profile = (open installs.ini | rotate | first | get column0.Default)
  let info = (lz4jsoncat  $'($default_profile)/sessionstore-backups/recovery.jsonlz4' | from json)
  $info.windows | each { |window|
    $window.tabs.entries | flatten | select url title
  }
  | first
}

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
  } else $in.stdout
}

def "org-roam nodes" [] {
  emacs eval 'org-roam-db-location'
  | str trim -r
  | str replace -a '"' ''
  | open $in
  | get nodes
}

def "org-roam random-node" [] {
  org-roam nodes
  | get random
  | select title file
  | str replace -a '"' '' title file
}

def accountances [] {
	cd ~/doc/pdf/accountances/
	fd .
	| lines
	| skip
	| where not ($it | str ends-with '/')
	| each { |path|
		 $path
		| path basename
		| parse "{stem}.{ext}"
		| $in.0.stem
		| into datetime
		| {path: $path, expire: $in}
	}
}

def 'music' [] {
  let file = '~/doc/table/music.csv'
  open --raw $file | from csv --no-infer | into int score revisions
}

def 'music add' [] {
  let entry = ($in | default {
    author: (input 'Enter author (+ separated list): ')
    title: (input 'Enter title: ')
    type: ([track list] | input list 'Select type: ')
    score: (input 'Enter score (0-10 nat): ' | into decimal)
    revisions: 0
    added: (date now | date format '%Y-%m-%d')
    modified: (date now | date format '%Y-%m-%d')
  })
  let file = '~/doc/table/music.3.csv'
  let mus = (open $file)
  if ($entry | select author title) not-in ($mus | select author title) {
    $mus | append $entry | save --force $file
  }
}

def 'music review' [] {
  let input = $in
  let file = '~/doc/table/music.3.csv'
  let mus = (open $file)
  let res = (
    $mus
    | select author title
    | enumerate
    | where $it.item == $input
  )
  if ($res | is-empty) {
    return
  }
  let index = ($res | first | get index)
  let score = (input 'Enter new score: ')
  let prev = ($mus | get $index)
  $mus
  | update $index (
    $prev
    | update revisions ($prev.revisions + 1)
    | update score $score
  )
  | save --force $file
}

def next-ascension [--pretty (-p)] {
  into string
  | ^next-ascension
  | lines
  | first
  | split column ' ' chips cookies
  | first
  | if not $pretty {
    into int chips | into decimal cookies
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

alias surch = xbps search
alias wget = ^wget --hsts-file ($env.XDG_DATA_HOME | path join wget.hist)
alias yt = sfeed view -p /bin/mpv youtube
def s [] { ls | grid --color }

use ~/lib/nu/iptv.nu

def tv [] {
	let user = (iptv default-user)
	iptv get-live-streams $user.name $user.passwd
	| select stream_id name
	| input list -f
	| iptv m3u-url $user.name $user.passwd $in.stream_id
	| mpv $in
}
