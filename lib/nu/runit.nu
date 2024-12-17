def 'from status' [] {
  let input = ($in | into binary)
  # FIXME: as of 0.71, nushell doesn't have a way to specify if the
  # input binary data to `into int' is signed or unsigned, always
  # assuming unsigned. This simply skeeps the first 4 bytes,
  # effectively convert it from a signed 64 bit integer to unsigned 32
  # bit integer
  let time = ($input | skip 4 | take 4 | into int)
  let pid = ($input | skip 12 | take 4 | into int --little-endian)
  let status = (
      $input
    | get 19
    | if $in == 0 {
      "down"
    } else if $in == 1 {
      "up"
    } else if $in == 2 {
      "exit"
    } else "unknown"
  )

  { time: $time, pid: $pid, status: $status }
}

# outputs a information record of each service in dir
export def info [dir] {
  $in
  | each { |service|
      open --raw $"($dir)/($service)/supervise/status"
    | from status
    | insert name $service
  }
}

# outputs a list of runsvdir directories
export def dirs [] {
    ps
  | where name =~ runsvdir
  | get pid
  | each { |id|
      open --raw $"/proc/($id)/cmdline"
    | tr '\0' '\n'
    | lines
    | skip
    | where $it != "-P"
    | first
  }
}

# input is a directory
export def services [] {
    ls $in
  | get name
  | path basename
}

export def up [dir, service] {
  echo 'u' | save $'($dir)/($service)/supervise/control'
}

export def down [dir, service] {
  echo 'd' | save $'($dir)/($service)/supervise/control'
}

export def restart [dir, service] {
  down $dir $service
  up   $dir $service
}

# TODO: implement once exit
