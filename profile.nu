let uid = (id -u | str trim -r)
let home = $nu.home-path

# default applications
let EDITOR = "emacs"
let VISUAL = "emacs --no-window-system"
let BROWSER = "luakit"

# for gnupg-agent to work properly
let GPG_TTY = (tty | str trim -r)
let HISTSIZE = "1000"
let GIT_PAGER = "delta"
let GCC_COLORS = ([
  "error=01;31"
  "warning=01;35"
  "note=01;36"
  "caret=01;32"
  "locus=01"
  "quote=01"
] | str join ':')

let CARGO_HOME = ($home | path join "app/cargo")
let RUSTUP_HOME = ($home | path join "app/rustup")
let YTFZF_SYSTEM_ADDON_DIR = ($home | path join lib/sh/ytfzf/addons)
let OCAML_TOPLEVEL_PATH = ($home | path join lib/ocaml/toplevel)
let GOPATH = ($home | path join app/go/)

let XDG_CONFIG_HOME = ($home | path join config)
let XDG_DATA_HOME   = ($home | path join var)
let XDG_STATE_HOME  = ($XDG_DATA_HOME | path join state)
let XDG_CACHE_HOME  = ($XDG_DATA_HOME | path join cache)

let XDG_RUNTIME_DIR = $"/run/user/($uid)"
let XDG_CURRENT_DESKTOP = 'Unity'
let XDG_DATA_DIRS = (
  [ /usr/local/share
    /usr/share
    /usr/share/local
    /var/lib/flatpak/exports/share
    $"($XDG_DATA_HOME)/flatpak/exports/share"
    $"($XDG_DATA_HOME)"
  ]
  | str join ':'
)

let XAUTHORITY = ($XDG_RUNTIME_DIR | path join "Xauthority")
#let QT_QPA_PLATFORM = 'wayland;xcb'
let MOZ_ENABLE_WAYLAND = '1'
# don't put duplicate lines or lines starting with spaces on history
let HISTCONTROL = "ignoreboth"
let SYS_LOG_FILE = ($home | path join data/msg.txt)
let YTFZF_PREFIX = "bestvideo[height<=720]+bestaudio/best[height<=720]"
let HISTFILESIZE = "2000"
let EMACS_SESSION = "visual+lang"
let GRIM_DEFAULT_DIR = ($home | path join img/screenshot)
let QT_STYLE_OVERRIDE = "kvantum"
let XKB_DEFAULT_LAYOUT = "us(intl)" # ,br(abnt2)
let EMACS_SERVER_FILE = $"/run/user/($uid)/emacs/emacsd"
let EMACS_SOCKET_NAME = $"/run/user/($uid)/emacs/emacsd"

# move things away from $HOME
let WGETRC                = ($XDG_CONFIG_HOME | path join wgetrc)
let INPUTRC               = ($XDG_CONFIG_HOME | path join readline/inputrc)
let XINITRC               = ($XDG_CONFIG_HOME | path join X11/xinitrc)
let DOT_SAGE              = ($XDG_CONFIG_HOME | path join sage)
let XSERVERRC             = ($XDG_CONFIG_HOME | path join X11/xserverrc)
let GTK2_RC_FILES         = ($XDG_CONFIG_HOME | path join gtk-2.0/gtkrc)
let RIPGREP_CONFIG_PATH   = ($XDG_CONFIG_HOME | path join ripgrep/config)
let NPM_CONFIG_USERCONFIG = ($XDG_CONFIG_HOME | path join npm/npmrc)
let ANIMDL_CONFIG         = ($XDG_CONFIG_HOME | path join animdl/config.yml)

let WWW_HOME           = ($XDG_DATA_HOME | path join w3m)
let SSB_HOME           = ($XDG_DATA_HOME | path join zoom)
let TERMINFO           = ($XDG_DATA_HOME | path join terminfo)
let HISTFILE           = ($XDG_DATA_HOME | path join bash.hist)
let IPFS_PATH          = ($XDG_DATA_HOME | path join ipfs)
let WINEPREFIX         = ($XDG_DATA_HOME | path join wineprefixes/default)
let LESSHISTFILE       = ($XDG_DATA_HOME | path join less.hist)
let TERMINFO_DIRS      = ($XDG_DATA_HOME | path join terminfo:/usr/share/terminfo)

let SQLITE_HISTORY     = ($XDG_DATA_HOME | path join sqlite.hist)
let OPAMROOT           = ($XDG_DATA_HOME | path join opam)
let NODE_REPL_HISTORY  = ($XDG_DATA_HOME | path join node.hist)
let GNUPGHOME          = ($XDG_DATA_HOME | path join gnupg)
let PASSWORD_STORE_DIR = ($XDG_DATA_HOME | path join pass)
let JULIA_DEPOT_PATH   = $"($XDG_DATA_HOME | path join julia):($env | get -i JULIA_DEPOT_PATH)"
let XCURSOR_PATH = $"/usr/share/icons:($XDG_DATA_HOME | path join icons)"


let ICEAUTHORITY = ($XDG_CACHE_HOME | path join ICEauthority)

# this is causing problems, specially with Qt shared objects, indicating `undefined symbol`s
# export LD_LIBRARY_PATH=/usr/lib:/usr/lib32:/usr/local/lib:$HOME/lib/c

# Both configuration and state data are stored in OPAMROOT, so this solution is not fully compliant.
let OPAM_SWITCH_PREFIX = ($OPAMROOT | path join default)

let XBPS_DISTDIR = "/store/source/void-packages"
let XBPS_SRCPKGDIR = ($XBPS_DISTDIR | path join srcpkgs)

let XKB_DEFAULT_OPTIONS = ([
  "grp:shifts_toggle"
  "caps:swapescape"
  "compose:ralt"
] | str join ',')

let MANPATH = ([
  /usr/local/share/man
  /usr/share/man
  ($home | path join "doc/man")
  ($home | path join ".opam/default/man")
] | str join ':')

let CAML_LD_LIBRARY_PATH = ([
  ($home | path join lib/ocaml/stublibs/)
  /usr/lib/ocaml/stublibs
  /usr/lib/ocaml
] | str join ':')

let PYTHONPATH = ([
  /usr/lib/python3.10
  ($home | path join lib/python3.10/site-packages)
] | str join ':')

let PYTHONSTARTUP = "/etc/python/pythonrc"

let PATH = (
  $env.PATH
  | append ($CARGO_HOME | path join "bin")
  | str join ':'
)

# SECRETS
let DZR_CBC = "g4el58wc0zvf9na1"

let W3M_DIR = ($XDG_DATA_HOME | path join w3m)
let GRADLE_USER_HOME = ($XDG_DATA_HOME | path join gradle)
let ANDROID_HOME = ($XDG_DATA_HOME | path join android)

let e = {
  EDITOR: $EDITOR
  VISUAL: $VISUAL
  BROWSER: $BROWSER
  GPG_TTY: $GPG_TTY
  MANPATH: $MANPATH
  HISTSIZE: $HISTSIZE
  GIT_PAGER: $GIT_PAGER
  GCC_COLORS: $GCC_COLORS
  CARGO_HOME: $CARGO_HOME
  RUSTUP_HOME: $RUSTUP_HOME
  YTFZF_SYSTEM_ADDON_DIR: $YTFZF_SYSTEM_ADDON_DIR
  OCAML_TOPLEVEL_PATH: $OCAML_TOPLEVEL_PATH
  GOPATH: $GOPATH
  XDG_DATA_HOME: $XDG_DATA_HOME
  XDG_STATE_HOME: $XDG_STATE_HOME
  XDG_CACHE_HOME: $XDG_CACHE_HOME
  XDG_CONFIG_HOME: $XDG_CONFIG_HOME
  XDG_RUNTIME_DIR: $XDG_RUNTIME_DIR
  XDG_CURRENT_DESKTOP: $XDG_CURRENT_DESKTOP
  XDG_DATA_DIRS: $XDG_DATA_DIRS
  XAUTHORITY: $XAUTHORITY
  #QT_QPA_PLATFORM: $QT_QPA_PLATFORM
  MOZ_ENABLE_WAYLAND: $MOZ_ENABLE_WAYLAND
  HISTCONTROL: $HISTCONTROL
  SYS_LOG_FILE: $SYS_LOG_FILE
  YTFZF_PREFIX: $YTFZF_PREFIX
  HISTFILESIZE: $HISTFILESIZE
  EMACS_SESSION: $EMACS_SESSION
  GRIM_DEFAULT_DIR: $GRIM_DEFAULT_DIR
  QT_STYLE_OVERRIDE: $QT_STYLE_OVERRIDE
  XKB_DEFAULT_LAYOUT: $XKB_DEFAULT_LAYOUT
  EMACS_SERVER_FILE: $EMACS_SERVER_FILE
  EMACS_SOCKET_NAME: $EMACS_SOCKET_NAME
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
  HISTFILE: $HISTFILE
  IPFS_PATH: $IPFS_PATH
  WINEPREFIX: $WINEPREFIX
  LESSHISTFILE: $LESSHISTFILE
  TERMINFO_DIRS: $TERMINFO_DIRS
  SQLITE_HISTORY: $SQLITE_HISTORY
  OPAMROOT: $OPAMROOT
  NODE_REPL_HISTORY: $NODE_REPL_HISTORY
  GNUPGHOME: $GNUPGHOME
  PASSWORD_STORE_DIR: $PASSWORD_STORE_DIR
  JULIA_DEPOT_PATH: $JULIA_DEPOT_PATH
  XCURSOR_PATH: $XCURSOR_PATH
  ICEAUTHORITY: $ICEAUTHORITY
  OPAM_SWITCH_PREFIX: $OPAM_SWITCH_PREFIX
  XBPS_DISTDIR: $XBPS_DISTDIR
  XBPS_SRCPKGDIR: $XBPS_SRCPKGDIR
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
