def short-path [] {
  let p = ($env.PWD | path split)
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

def create_left_prompt [] {
  [ (ansi blue)
    (whoami | str trim -r)
    (ansi reset)
    "@"
    (ansi light_red_bold)
    (hostname | str trim -r)
    (ansi reset)
  ] | str join
}

def create_right_prompt [] {
  let branch_str = (
    do -i { git branch --show-current | complete }
    | if $in.exit_code != 128 {
      ["@" (ansi cyan_bold) ($in.stdout | str trim -r) (ansi reset)] | str join
    } else ''
  )
  [(ansi green) (short-path) (ansi reset) $branch_str] | str join
}

# Closures that generate prompt strings
let-env PROMPT_COMMAND = { create_left_prompt }
let-env PROMPT_COMMAND_RIGHT = { create_right_prompt }

let-env PROMPT_INDICATOR = $"(ansi yb)|> (ansi reset)"
let-env PROMPT_INDICATOR_VI_INSERT = ": "
let-env PROMPT_INDICATOR_VI_NORMAL = "|> "
let-env PROMPT_MULTILINE_INDICATOR = "::: "

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
