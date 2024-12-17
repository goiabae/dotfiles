export def channels [] {
  xfconf-query --list
  | lines
  | skip
  | str trim
}

export def properties [channel: string] {
  xfconf-query --channel $channel --list
  | lines
}

export def "property get" [channel: string, property: path] {
  xfconf-query --channel $channel --property $property
}
