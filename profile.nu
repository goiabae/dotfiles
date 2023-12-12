use ~/lib/nu/unix.nu
use ~/lib/nu/xdg.nu

def 'stringy' [
  --assignment(-a): string
  --separator(-s): string
  rec
] {
  let vals = (if $assignment == null { $rec } else {
    $rec
    | transpose key value
    | each { |row| $row.key + $assignment + $row.value }
  })
  $vals | str join $separator
}

let uid = (unix user | first | get uid | into string)

mut a  = {}

$a.XDG_CONFIG_HOME = ($nu.home-path | path join config)
$a.XDG_DATA_HOME   = ($nu.home-path | path join var)
$a.XDG_STATE_HOME  = ($a.XDG_DATA_HOME | path join state)
$a.XDG_CACHE_HOME  = ($a.XDG_DATA_HOME | path join cache)

$a.XDG_RUNTIME_DIR = ($env.XDG_RUNTIME_DIR? | default (echo /run/user | path join $uid))
$a.XDG_CURRENT_DESKTOP = 'Unity'

$a.XDG_DATA_DIRS = (
	$env.XDG_DATA_DIRS?
	| default "/usr/local/share:/usr/share"
	| split row (char env_sep)
	| append [
		/var/lib/flatpak/exports/share
		($a.XDG_DATA_HOME | path join flatpak/exports/share)
		($a.XDG_DATA_HOME)
	]
	| str join (char env_sep)
)

def ansi-color [color] {
  ansi $color | split chars | skip 2 | drop | str join
}

$a.GCC_COLORS = (stringy -a '=' -s (char env_sep) {
  error:   (ansi-color red_bold)
  warning: (ansi-color magenta_bold)
  note:    (ansi-color cyan_bold)
  caret:   (ansi-color green_bold)
  locus:   (ansi-color attr_bold)
  quote:   (ansi-color attr_bold)
})

$a.SQLITE_HISTORY = ($a.XDG_DATA_HOME | path join sqlite.hist)
$a.NODE_REPL_HISTORY = ($a.XDG_DATA_HOME | path join node.hist)
$a.LESSHISTFILE = ($a.XDG_DATA_HOME | path join less.hist)
$a.HISTFILE = ($a.XDG_DATA_HOME | path join bash.hist)
$a.HISTCONTROL = "ignoreboth"
$a.HISTSIZE = 100 # max lines on memory
$a.HISTFILESIZE = 2000 # max lines on disk

$a.INPUTRC = ($a.XDG_CONFIG_HOME | path join readline/inputrc)

$a.CARGO_HOME = ($nu.home-path | path join app cargo)
$a.RUSTUP_HOME = ($nu.home-path | path join app rustup)
$a.GOPATH = ($nu.home-path | path join app go)
$a.NPM_CONFIG_USERCONFIG = ($a.XDG_CONFIG_HOME | path join npm/npmrc)
$a.GNUPGHOME = ($a.XDG_DATA_HOME | path join gnupg)
$a.ROSWELL_HOME = ($nu.home-path | path join app roswell)

$a.JULIA_DEPOT_PATH = (
	$env.JULIA_DEPOT_PATH?
	| if $in != null { split row (char env_sep) }
	| append ($a.XDG_DATA_HOME | path join julia)
	| str join (char env_sep)
)

$a.OPAMROOT = ($a.XDG_DATA_HOME | path join opam)

# Both configuration and state data are stored in OPAMROOT,
# so this solution is not fully compliant.
$a.OPAM_SWITCH_PREFIX = ($a.OPAMROOT | path join default)

$a.PYTHONSTARTUP = ($a.XDG_CONFIG_HOME | path join python conf.py)
$a.GRADLE_USER_HOME = ($a.XDG_DATA_HOME | path join gradle)
$a.W3M_DIR = ($a.XDG_DATA_HOME | path join w3m)
$a.ANDROID_HOME = ($a.XDG_DATA_HOME | path join android)

# this is causing problems, specially with Qt shared objects, indicating `undefined symbol`s
# export LD_LIBRARY_PATH=/usr/lib:/usr/lib32:/usr/local/lib:$HOME/lib/c

$a.CAML_LD_LIBRARY_PATH = ([
  ($nu.home-path | path join lib/ocaml/stublibs/)
  ($a.OPAM_SWITCH_PREFIX | path join lib/stublibs)
  /usr/lib/ocaml/stublibs
  /usr/lib/ocaml
] | str join (char env_sep))

$a.OCAML_TOPLEVEL_PATH = ($a.OPAM_SWITCH_PREFIX | path join lib toplevel)

let python_version = (
  python3 --version
  | parse "Python {major}.{minor}.{patch}"
  | first
  | $"python($in.major).($in.minor)"
)

# $a.PYTHONPATH = (
#   ls /usr/lib/python3.*
#   | get name
#   | append ($nu.home-path | path join lib $python_version site-packages)
#   | str join (char env_sep)
# )

$a.IPFS_PATH = ($nu.home-path | path join app ipfs)
$a.WWW_HOME = ($a.XDG_DATA_HOME | path join w3m)
$a.SSB_HOME = ($a.XDG_DATA_HOME | path join zoom)
$a.YTFZF_SYSTEM_ADDON_DIR = ($home | path join lib sh ytfzf addons)

$a.EDITOR = "editor"
$a.VISUAL = "editor"
$a.BROWSER = "xdg-open"
$a.GIT_PAGER = "delta"

$a.WGETRC = ($a.XDG_CONFIG_HOME | path join wgetrc)
$a.XINITRC = ($a.XDG_CONFIG_HOME | path join X11/xinitrc)
$a.XSERVERRC = ($a.XDG_CONFIG_HOME | path join X11/xserverrc)
$a.GTK2_RC_FILES = ($a.XDG_CONFIG_HOME | path join gtk-2.0/gtkrc)

$a.RIPGREP_CONFIG_PATH = ($a.XDG_CONFIG_HOME | path join ripgrep/config)
$a.ANIMDL_CONFIG = ($a.XDG_CONFIG_HOME | path join animdl/config.yml)
$a.DOT_SAGE = ($a.XDG_CONFIG_HOME | path join sage)

$a.GPG_TTY = (unix tty)
#$a.QT_QPA_PLATFORM = 'wayland;xcb'
$a.MOZ_ENABLE_WAYLAND = '1'
$a.YTFZF_PREFIX = "bestvideo[height<=720]+bestaudio/best[height<=720]"
$a.SYS_LOG_FILE = ($nu.home-path | path join data/msg.txt)
$a.GRIM_DEFAULT_DIR = ($nu.home-path | path join img/screenshot)
$a.QT_STYLE_OVERRIDE = "kvantum"
$a.TERMINFO = ($a.XDG_DATA_HOME | path join terminfo)
$a.WINEPREFIX = ($a.XDG_DATA_HOME | path join wineprefixes/default)
$a.TERMINFO_DIRS = ([
  ($a.XDG_DATA_HOME | path join terminfo)
  /usr/share/terminfo
] | str join (char env_sep))
$a.PASSWORD_STORE_DIR = ($a.XDG_DATA_HOME | path join pass)

# See Xcursor(3) of libXcursor for default value
$a.XCURSOR_PATH = ([
  /usr/share/icons
  ($a.XDG_DATA_HOME | path join icons)
] | str join (char env_sep))

$a.ICEAUTHORITY = ($a.XDG_CACHE_HOME | path join ICEauthority)

$a.XAUTHORITY = ($env.XAUTHORITY? | default ($a.XDG_RUNTIME_DIR | path join Xauthority))

$a.XKB_DEFAULT_LAYOUT = "us(intl)" # ,br(abnt2)
$a.XKB_DEFAULT_OPTIONS = (stringy -a (char env_sep) -s ',' {
  grp: shifts_toggle
  caps: swapescape
  compose: ralt
})

let opam_switch_prefix = $a.OPAM_SWITCH_PREFIX

$a.MANPATH = (do {
	let default = (
		open /etc/man.conf
		| lines
		| find manpath
		| parse "manpath {path}"
		| get path
	)

	$default
	| append [
		($nu.home-path | path join doc/man)
		($opam_switch_prefix | path join man)
		/opt/texlive/2023/texmf-dist/doc/man
	]
	| str join (char env_sep)
})

$a.PATH = (
	$env.PATH
	| append [
		($a.CARGO_HOME | path join bin)
		/opt/texlive/2023/bin/x86_64-linux
		($a.OPAM_SWITCH_PREFIX | path join bin)
		($nu.home-path | path join $a.ROSWELL_HOME bin)
	]
	| str join (char env_sep)
)

# SECRETS
$a.DZR_CBC = "g4el58wc0zvf9na1"

let e = $a

def main [] {}

def "main json" [] {
	print ($e | to json)
}

def "main shell" [] {
	$e
	| transpose
	| each { |it| $'export ($it.column0)="($it.column1)"' }
	| str join (char newline)
	| print $in
}
