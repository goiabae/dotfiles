let uid = (id -u | str trim -r)
let home = $nu.home-path

let e = {
  XDG_DATA_HOME:   $'($home)/data',
  XDG_STATE_HOME:  $"($home)/data/state",
  XDG_CACHE_HOME:  $"($home)/data/cache",
  XDG_CONFIG_HOME: $"($home)/config",

  XDG_RUNTIME_DIR: $"/tmp/run/user/($uid)",
  XDG_CURRENT_DESKTOP: Unity,

  QT_QPA_PLATFORM: 'wayland;xcb',
  MOZ_ENABLE_WAYLAND: '1'
}

let e = (
  $e
  | insert XDG_DATA_DIRS (
    [ /usr/local/share
      /usr/share
      /usr/share/local
      /var/lib/flatpak/exports/share
      $"($e.XDG_DATA_HOME)/flatpak/exports/share"
      $"($e.XDG_DATA_HOME)"
    ]
    | str join ':'
  )
)

echo $e | to json
