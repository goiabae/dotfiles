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
let home = $nu.home-path

# default applications
let EDITOR = "editor"
let VISUAL = "editor"
let BROWSER = "xdg-open"

# for gnupg-agent to work properly
let GPG_TTY = (unix tty)
let HISTSIZE = "1000"
let GIT_PAGER = "delta"

def ansi-color [color] {
  ansi $color | split chars | skip 2 | drop | str join
}

let GCC_COLORS = (stringy -a '=' -s ':' {
  error: (ansi-color red_bold)
  warning: (ansi-color magenta_bold)
  note: (ansi-color cyan_bold)
  caret: (ansi-color green_bold)
  locus: (ansi-color attr_bold)
  quote: (ansi-color attr_bold)
})

let CARGO_HOME = ($home | path join app cargo)
let RUSTUP_HOME = ($home | path join app rustup)
let YTFZF_SYSTEM_ADDON_DIR = ($home | path join lib sh ytfzf addons)
let GOPATH = ($home | path join app go)

let-env XDG_CONFIG_HOME = ($home | path join config)
let-env XDG_DATA_HOME   = ($home | path join var)
let-env XDG_STATE_HOME  = (xdg data-home | path join state)
let-env XDG_CACHE_HOME  = (xdg data-home | path join cache)

let-env XDG_RUNTIME_DIR = (echo /run/user | path join $uid)
let XDG_CURRENT_DESKTOP = 'Unity'
let XDG_DATA_DIRS = (stringy -s ':' [
  /usr/local/share
  /usr/share
  /usr/share/local
  /var/lib/flatpak/exports/share
  (xdg data-home | path join flatpak/exports/share)
  (xdg data-home)
])

let XAUTHORITY = (xdg runtime-dir | path join Xauthority)
#let QT_QPA_PLATFORM = 'wayland;xcb'
let MOZ_ENABLE_WAYLAND = '1'

# don't put duplicate lines or lines starting with spaces on history
let HISTCONTROL = "ignoreboth"
let SYS_LOG_FILE = ($home | path join data/msg.txt)
let YTFZF_PREFIX = "bestvideo[height<=720]+bestaudio/best[height<=720]"
let HISTFILESIZE = "2000"
let GRIM_DEFAULT_DIR = ($home | path join img/screenshot)
let QT_STYLE_OVERRIDE = "kvantum"
let XKB_DEFAULT_LAYOUT = "us(intl)" # ,br(abnt2)

# move things away from $HOME
let WGETRC = (xdg config-home | path join wgetrc)
let INPUTRC = (xdg config-home | path join readline/inputrc)
let XINITRC = (xdg config-home | path join X11/xinitrc)
let DOT_SAGE = (xdg config-home | path join sage)
let XSERVERRC = (xdg config-home | path join X11/xserverrc)
let GTK2_RC_FILES = (xdg config-home | path join gtk-2.0/gtkrc)
let RIPGREP_CONFIG_PATH = (xdg config-home | path join ripgrep/config)
let NPM_CONFIG_USERCONFIG = (xdg config-home | path join npm/npmrc)
let ANIMDL_CONFIG = (xdg config-home | path join animdl/config.yml)

let WWW_HOME = (xdg data-home | path join w3m)
let SSB_HOME = (xdg data-home | path join zoom)
let TERMINFO = (xdg data-home | path join terminfo)
let HISTFILE = (xdg data-home | path join bash.hist)
let IPFS_PATH = ($home | path join app ipfs)
let WINEPREFIX = (xdg data-home | path join wineprefixes/default)
let LESSHISTFILE = (xdg data-home | path join less.hist)
let TERMINFO_DIRS = ([
  (xdg data-home | path join terminfo)
  /usr/share/terminfo
] | str join ':')

let SQLITE_HISTORY = (xdg data-home | path join sqlite.hist)
let OPAMROOT = (xdg data-home | path join opam)
let NODE_REPL_HISTORY = (xdg data-home | path join node.hist)
let GNUPGHOME = (xdg data-home | path join gnupg)
let PASSWORD_STORE_DIR = (xdg data-home | path join pass)

let JULIA_DEPOT_PATH = ([
  (xdg data-home | path join julia)
  $env.JULIA_DEPOT_PATH?
] | str join ':')

let XCURSOR_PATH = ([
  /usr/share/icons
  (xdg data-home | path join icons)
] | str join ':')

let ICEAUTHORITY = (xdg cache-home | path join ICEauthority)

# this is causing problems, specially with Qt shared objects, indicating `undefined symbol`s
# export LD_LIBRARY_PATH=/usr/lib:/usr/lib32:/usr/local/lib:$HOME/lib/c

# Both configuration and state data are stored in OPAMROOT,
# so this solution is not fully compliant.
let OPAM_SWITCH_PREFIX = ($OPAMROOT | path join default)

let XKB_DEFAULT_OPTIONS = (stringy -a ':' -s ',' {
  grp: shifts_toggle
  caps: swapescape
  compose: ralt
})

let MANPATH = ([
  /usr/local/share/man
  /usr/share/man
  ($home | path join doc/man)
  ($home | path join .opam/default/man)
  /opt/texlive/2023/texmf-dist/doc/man
] | str join ':')

let CAML_LD_LIBRARY_PATH = ([
  ($home | path join lib/ocaml/stublibs/)
  ($OPAM_SWITCH_PREFIX | path join lib/stublibs)
  /usr/lib/ocaml/stublibs
  /usr/lib/ocaml
] | str join ':')

let OCAML_TOPLEVEL_PATH = ($OPAM_SWITCH_PREFIX | path join lib toplevel)

let python_version = (
  python3 --version
  | parse "Python {major}.{minor}.{patch}"
  | first
  | $"python($in.major).($in.minor)"
)

let PYTHONPATH = ([
  (echo /usr/lib | path join $python_version)
  ($home | path join lib $python_version site-packages)
] | str join ':')

let PYTHONSTARTUP = (xdg config-home | path join python conf.py)

let PATH = (
  $env.PATH
  | append ($CARGO_HOME | path join bin)
  | append /opt/texlive/2023/bin/x86_64-linux
  | append ($OPAM_SWITCH_PREFIX | path join bin)
  | append ($home | path join .roswell bin)
  | str join ':'
)

# SECRETS
let DZR_CBC = "g4el58wc0zvf9na1"

let W3M_DIR = (xdg data-home | path join w3m)
let GRADLE_USER_HOME = (xdg data-home | path join gradle)
let ANDROID_HOME = (xdg data-home | path join android)

let e = {
  EDITOR: $EDITOR
  VISUAL: $VISUAL
  BROWSER: $BROWSER

  GPG_TTY: $GPG_TTY
  MANPATH: $MANPATH
  GIT_PAGER: $GIT_PAGER
  GCC_COLORS: $GCC_COLORS
  CARGO_HOME: $CARGO_HOME
  RUSTUP_HOME: $RUSTUP_HOME
  YTFZF_SYSTEM_ADDON_DIR: $YTFZF_SYSTEM_ADDON_DIR
  OCAML_TOPLEVEL_PATH: $OCAML_TOPLEVEL_PATH
  GOPATH: $GOPATH

  # XDG Base Directory
  XDG_DATA_HOME: (xdg data-home)
  XDG_STATE_HOME: (xdg state-home)
  XDG_CACHE_HOME: (xdg cache-home)
  XDG_CONFIG_HOME: (xdg config-home)
  XDG_RUNTIME_DIR: (xdg runtime-dir)
  XDG_CURRENT_DESKTOP: $XDG_CURRENT_DESKTOP
  XDG_DATA_DIRS: $XDG_DATA_DIRS

  XAUTHORITY: $XAUTHORITY
  #QT_QPA_PLATFORM: $QT_QPA_PLATFORM
  MOZ_ENABLE_WAYLAND: $MOZ_ENABLE_WAYLAND

  # History
  HISTSIZE: $HISTSIZE
  HISTCONTROL: $HISTCONTROL
  HISTFILESIZE: $HISTFILESIZE
  HISTFILE: $HISTFILE
  LESSHISTFILE: $LESSHISTFILE
  SQLITE_HISTORY: $SQLITE_HISTORY
  NODE_REPL_HISTORY: $NODE_REPL_HISTORY

  SYS_LOG_FILE: $SYS_LOG_FILE
  YTFZF_PREFIX: $YTFZF_PREFIX
  GRIM_DEFAULT_DIR: $GRIM_DEFAULT_DIR
  QT_STYLE_OVERRIDE: $QT_STYLE_OVERRIDE
  XKB_DEFAULT_LAYOUT: $XKB_DEFAULT_LAYOUT
  XKB_DEFAULT_OPTIONS: $XKB_DEFAULT_OPTIONS
  WGETRC: $WGETRC
  INPUTRC: $INPUTRC
  XINITRC: $XINITRC
  DOT_SAGE: $DOT_SAGE
  XSERVERRC: $XSERVERRC
  GTK2_RC_FILES: $GTK2_RC_FILES
  RIPGREP_CONFIG_PATH: $RIPGREP_CONFIG_PATH
  NPM_CONFIG_USERCONFIG: $NPM_CONFIG_USERCONFIG
  ANIMDL_CONFIG: $ANIMDL_CONFIG
  WWW_HOME: $WWW_HOME
  SSB_HOME: $SSB_HOME
  TERMINFO: $TERMINFO
  IPFS_PATH: $IPFS_PATH
  WINEPREFIX: $WINEPREFIX
  TERMINFO_DIRS: $TERMINFO_DIRS
  OPAMROOT: $OPAMROOT
  GNUPGHOME: $GNUPGHOME
  PASSWORD_STORE_DIR: $PASSWORD_STORE_DIR
  JULIA_DEPOT_PATH: $JULIA_DEPOT_PATH
  XCURSOR_PATH: $XCURSOR_PATH
  ICEAUTHORITY: $ICEAUTHORITY
  OPAM_SWITCH_PREFIX: $OPAM_SWITCH_PREFIX
  CAML_LD_LIBRARY_PATH: $CAML_LD_LIBRARY_PATH
  PYTHONPATH: $PYTHONPATH
  PATH: $PATH
  DZR_CBC: $DZR_CBC
  W3M_DIR: $W3M_DIR
  PYTHONSTARTUP: $PYTHONSTARTUP
  GRADLE_USER_HOME: $GRADLE_USER_HOME
  ANDROID_HOME: $ANDROID_HOME
}

$e | to json
