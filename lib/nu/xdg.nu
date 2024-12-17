export def config-home [] {
  $env.XDG_CONFIG_HOME? | default ~/.config
}

export def cache-home [] {
  $env.XDG_CACHE_HOME? | default ~/.cache
}

export def runtime-dir [] {
  $env.XDG_RUNTIME_DIR? | default /tmp
}

export def data-home [] {
  $env.XDG_DATA_HOME? | default ~/.local/share
}

export def state-home [] {
  $env.XDG_STATE_HOME? | default ~/.local/state
}
