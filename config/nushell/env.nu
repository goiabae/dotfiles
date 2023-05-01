# Nushell Environment Config File

def short-path [] {
  let p = (pwd | str trim -r | path split)
  let p = (if ($p | take 3 | $in == ($nu.home-path | path split)) {
    $p | skip 3 | prepend '~'
  } else {
    $p
  })
  $p
  | drop
  | each { |dir| $dir | str substring 0..1 }
  | append ($p | last)
  | path join
}

def prompt-overlays [] {
  let overlays = (overlay list | where $it != zero)
  if ($overlays | length | $in > 0) {
    [ (ansi yellow_reverse)
      ($overlays | str join ' ')
      (ansi reset)
      ' '
    ] | str join
  } else ""
}

def create_left_prompt [] {
  [ (ansi blue)
    (whoami | str trim -r)
    (ansi reset)
    "@"
    (ansi lrb)
    (hostname | str trim -r)
    (ansi reset)
  ] | str join
}

def create_right_prompt [] {
  let r = (do -i { git branch | complete })
  [(ansi green) (short-path) (ansi reset)]
  | if ($r.exit_code != 128) {
    $in | append [
      "@"
      (ansi cb)
      ($r.stdout | sed --quiet --regexp-extended 's/\*\s(.*)/\1/p' | tr -d "\n")
      (ansi reset)
    ]
  } else $in
  | str join
}

# Use nushell functions to define your right and left prompt
let-env PROMPT_COMMAND = { || create_left_prompt }
let-env PROMPT_COMMAND_RIGHT = { || create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
let-env PROMPT_INDICATOR = { || [(ansi yb) "|> " (prompt-overlays)] | str join }
let-env PROMPT_INDICATOR_VI_INSERT = { || ": " }
let-env PROMPT_INDICATOR_VI_NORMAL = { || "|> " }
let-env PROMPT_MULTILINE_INDICATOR = { || "::: " }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
let-env ENV_CONVERSIONS = {
  "PATH": {
    from_string: { |s| $s | split row (char esep) }
    to_string: { |v| $v | path expand | str join (char esep) }
  }
  "Path": {
    from_string: { |s| $s | split row (char esep) }
    to_string: { |v| $v | path expand | str join (char esep) }
  }
}

# Directories to search for scripts when calling source or use
#
# By default, <nushell-config-dir>/scripts is added
let-env NU_LIB_DIRS = [
  ($nu.config-path | path dirname | path join 'scripts')
]

# Directories to search for plugin binaries when calling register
#
# By default, <nushell-config-dir>/plugins is added
let-env NU_PLUGIN_DIRS = [
  ($nu.config-path | path dirname | path join 'plugins')
  $"($nu.home-path)/exe/nu.d"
]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# let-env PATH = ($env.PATH | split row (char esep) | prepend '/some/path')

let-env XBPS_SRC_CONFIG = $"($env.XDG_CONFIG_HOME)/xbps-src/config.ini"

let-env YTFZF_SYSTEM_ADDON_DIR = $"($nu.home-path)/lib/sh/ytfzf"

nu ~/profile.nu | from json | load-env
