# made for nushell version 0.104.0

let theme = {
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
    shape_garbage: { fg: white bg: red attr: b }
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
    shape_raw_string: green
}

$env.config.buffer_editor = "emacs"
$env.config.show_banner = false
$env.config.highlight_resolved_externals = true
$env.config.color_config = $theme
$env.config.cursor_shape.emacs = "block"
$env.config.rm.always_trash = true
$env.config.table.mode = "none"
$env.config.table.index_mode = "auto"
$env.config.display_errors.exit_code = true
$env.config.filesize.unit = 'metric'

$env.config.hooks.env_change = {
  PWD: [{ |before, after|
    if not (which direnv | is-empty) {
      direnv export json | from json | default {} | load-env
    }
  }]
}

$env.config.keybindings ++= [{
  name: cut_to_end
  modifier: control
  keycode: char_k
  mode: emacs
  event: { edit: cuttoend }
}]

use completions.nu *
use xdg.nu
use unix.nu
use x11.nu *
use yt.nu
use xbps.nu
use wrappers.nu *
use music_new.nu *
use nix-locate.nu
use sfeed.nu

def "get random" [] { get (random int 0..(($in | length) - 1)) }

def 'from list' [] { lines }

def lsblk [] { ^lsblk --json | from json }

def "secret token" [name] {
	let res = ^secret token $name | complete
	if $res.exit_code == 0 {
		$res.stdout | str trim -r
	} else error make {
		msg: "Secret not found."
	}
}

# a bug prevents this from being an alias
def yt [] {
	sfeed view -p (which player | first | get path) youtube
}

def pomodoro [
  --work(-w): duration, # duration of work cycles
	--break(-b): duration # duration of break cycles
	--total(-t): duration # total work time
] {
	let work = $work | default 25min
	let break = $break | default 15min
	let langs = espeak --voices | detect columns | get language
	mut left = $total | default 1hr

	loop {
		let lang = $langs | get random

		termdown -c 10 -v $lang ($work | into string)
		notify-send "pomo: work done. go relax" $"($work | format duration min) have passed"

		$left = $left - $work
		if ($left <= 0sec) {
			notify-send "pomo: session completed" $"A total of ($total | default 1hr | format duration min) of work has passed"
			break
		}

		termdown -c 10 -v $lang ($break | into string)
		notify-send "pomo: break's up" $"($break | format duration min) have passed"
	}
}

def album-tracks []: nothing -> list<any> {
	use mbz.nu
	let author = input 'artist name: '
	let artist = mbz search artist $author | input list 'select artist: '
	let group = mbz artist release-groups $artist.id | input list 'select release-group: '
	let release = mbz release-group releases $group.id | input list 'select release: '
	mbz release recordings $release.id
}

$env.DZR_CBC = secret token dzr

alias wget = ^wget --hsts-file ($env.XDG_DATA_HOME | path join wget.hist)
alias mitmproxy = ^mitmproxy --set $"confdir=($env.XDG_CONFIG_HOME)/mitmproxy"
alias mitmweb = ^mitmweb --set $"confdir=($env.XDG_CONFIG_HOME)/mitmproxy"
alias adb = with-env { HOME: $env.ANDROID_USER_HOME } { ^adb }
alias pomo = pomodoro -w 10min -b 10min -t 90min

source ./stuff.nu
