export def free [] {
  ^free
  | lines
  | skip
  | parse -r '(?P<name>[[:alpha:]]*):\s+(?P<total>[0-9]*)\s+(?P<used>[0-9]*)\s+(?P<free>[0-9]*)\s+(?P<shared>[0-9]*)\s+(?P<buff>[0-9]*)\s+(?P<available>[0-9]*)'
  | rename name total used free shared buff available
  | str downcase name
}

export def fc-list [] {
  ^fc-list | lines | parse '{path}: {name}:style={style}'
}

export def 'flatpak search' [query: string] {
  ^flatpak search $query
  | from tsv --noheaders
  | rename name description id version branch remote
}
