def 'into pkg' [] {
  { name: (xbps-uhelper getpkgname $in | str trim -r)
    version: (xbps-uhelper getpkgversion $in | str trim -r)
    revision: (xbps-uhelper getpkgrevision $in | into int)
  }
}

export def install [...pkgs] {
  sudo xbps-install $pkgs
}

export def sync [] {
  sudo xbps-install -S
}

export def upgrade [pkg: string] {
  sudo xbps-install -u $pkg
}

export def remove [pkg: string] {
  sudo xbps-remove $pkg
}

export def list [] {
  xbps-query -l
  | str trim
  | lines
  | parse '{status} {pkg} {description}'
}

export def 'list repos' [] {
  xbps-query --list-repos
  | sed 's/^ //g'
  | lines
  | parse '{size} {url} {signed}'
}

export def 'list on-hold' [] { xbps-query --list-hold-pkgs | lines }
export def 'list repo-locked' [] { xbps-query --list-repolock-pkgs }
export def 'list manual' [] { xbps-query --list-manual-pkgs | lines }
export def 'list orphan' [] { xbps-query --list-orphans | lines }

export def ownedby [pattern: string] {
  xbps-query --ownedby $pattern
  | lines
  | parse '{pkg}: {path} {type}'
}

export def show [pkg: string] {
  xbps-query --show $pkg | if $in != '' {
    $in | str replace --all (char tab) '- ' | str replace 'custom:' 'Custom' | from yaml
  } else null
}

export def search [pattern: string] {
  xbps-query --search $pattern
  | detect columns -c 2.. --no-headers
  | rename status pkgref description
}

export def cat [file: string, pkg: string] {
  xbps-query --cat $file $pkg
}

export def files [pkg: string] {
  xbps-query --files $pkg
  | lines
  | split column ' -> '
  | rename name target
}

export def deps [--reverse (-r), pkg: string] {
  if $reverse {
    xbps-query --revdeps $pkg
  } else {
    xbps-query --deps $pkg
  }
  | lines
}

export def locate [pattern: string] {
  xlocate $pattern
  | lines
  | str replace '\s+' ' '
  | parse '{pkgref} {tmp}'
  | update tmp { |row| $row.tmp | split column ' -> ' name target | first }
  | flatten tmp
  | default null target
}
