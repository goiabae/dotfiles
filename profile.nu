# written for nushell 0.90.1

use ~/lib/nu/unix.nu

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
$a.GDBHISTFILE = ($a.XDG_CONFIG_HOME | path join gdb .gdb_history)
$a.RLWRAP_HOME = ($a.XDG_DATA_HOME | path join rlwrap)

$a.INPUTRC = ($a.XDG_CONFIG_HOME | path join readline/inputrc)

$a.CARGO_HOME = ($nu.home-path | path join app cargo)
$a.RUSTUP_HOME = ($nu.home-path | path join app rustup)
$a.GOPATH = ($nu.home-path | path join app go)
$a.NPM_CONFIG_USERCONFIG = ($a.XDG_CONFIG_HOME | path join npm/npmrc)
$a.ROSWELL_HOME = ($nu.home-path | path join app roswell)
$a.R_HOME = ($nu.home-path | path join app 'R')

$a.JULIA_DEPOT_PATH = (
	$env.JULIA_DEPOT_PATH?
	| if $in != null { split row (char env_sep) }
	| append ($a.XDG_DATA_HOME | path join julia)
	| str join (char env_sep)
)

$a.OPAMROOT = ($a.XDG_DATA_HOME | path join opam)
$a.NUGET_PACKAGES = ($a.XDG_CACHE_HOME | path join nuget packages)
$a.DVDCSS_CACHE = ($a.XDG_DATA_HOME | path join dvdcss)

$a.OPAM_SWITCH_PREFIX = ($a.OPAMROOT | path join default)

$a.PYTHONSTARTUP = ($a.XDG_CONFIG_HOME | path join python conf.py)
$a.GRADLE_USER_HOME = ($a.XDG_DATA_HOME | path join gradle)

$a.CAML_LD_LIBRARY_PATH = ([
  ($nu.home-path | path join lib/ocaml/stublibs/)
  ($a.OPAM_SWITCH_PREFIX | path join lib/stublibs)
  /usr/lib/ocaml/stublibs
  /usr/lib/ocaml
] | str join (char env_sep))

$a.OCAML_TOPLEVEL_PATH = ($a.OPAM_SWITCH_PREFIX | path join lib toplevel)

$a.GNUPGHOME = ($a.XDG_DATA_HOME | path join gnupg)

$a.W3M_DIR = ($a.XDG_DATA_HOME | path join w3m)
$a.ANDROID_HOME = ($a.XDG_DATA_HOME | path join android)
$a.ANDROID_USER_HOME = ($a.XDG_DATA_HOME | path join android)
$a.DOTNET_ROOT = ($nu.home-path | path join app dotnet latest)
$a.MINETEST_USER_PATH = ($a.XDG_DATA_HOME | path join minetest)
$a.OMNISHARPHOME = ($a.XDG_CONFIG_HOME | path join omnisharp)
$a._JAVA_OPTIONS = $"-Djava.util.prefs.userRoot=($a.XDG_CONFIG_HOME | path join java)"
$a.ERRFILE = ($a.XDG_CACHE_HOME | path join X11 xsession-errors)
$a.LEIN_HOME = ($a.XDG_DATA_HOME | path join lein)

$a.KODI_DATA = ($a.XDG_DATA_HOME | path join kodi)

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
$a.SYS_LOG_FILE = ($nu.home-path | path join data/msg.txt)
$a.GRIM_DEFAULT_DIR = ($nu.home-path | path join img/screenshot)
$a.QT_STYLE_OVERRIDE = "kvantum"
$a.TERMINFO = ($a.XDG_DATA_HOME | path join terminfo)
$a.DOTNET_CLI_TELEMETRY_OPTOUT = '1'
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
		($nu.home-path | path join '.nix-profile' bin)
		($a.CARGO_HOME | path join bin)
		/opt/texlive/2023/bin/x86_64-linux
		($a.OPAM_SWITCH_PREFIX | path join bin)
		($nu.home-path | path join $a.ROSWELL_HOME bin)
		($nu.home-path | path join .dotnet tools)
		($a.XDG_DATA_HOME | path join npm bin)
	]
	| append (luarocks-5.1 path --lr-bin | split row (char env_sep))
	| str join (char env_sep)
)

# SECRETS
$a.DZR_CBC = (secret token dzr | str trim -r)

$a.GTK_IM_MODULE = "ibus"
$a.QT_IM_MODULE = "ibus"
$a.XMODIFIERS = "@im=ibus"

$a.XBPS_DISTDIR = "/store/source/void-packages"

$a.SFEED_AUTOCMD = "to"

$a.LUA_PATH = (luarocks-5.1 path --lr-path | str trim -r)
$a.LUA_CPATH = (luarocks-5.1 path --lr-cpath | str trim -r)

$a.GVIMINIT = 'let $MYGVIMRC="$XDG_CONFIG_HOME/vim/gvimrc" | source $MYGVIMRC'
$a.VIMINIT = 'let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'

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
