export def search [query: string] {
  let res = (http get -f $"https://coolrom.com.au/search?q=($query | url encode)")
  $res.body
  | query web -a href -q a
  | find -r /roms/.*/.*/.*\.php
  | parse "/roms/{platform}/{id}/{name}.php"
}

export def get-link [game: record<platform: string, id: string, name: string>] {
  let res = (http get -f $"https://coolrom.com.au/roms/($game.platform)/($game.id)/($game.name).php")
  $res.body
  | query web -q 'script'
  | find dl.coolrom.com
  | lines
  | find dl.coolrom.com
  | first
  | str replace '.*(https://dl.coolrom.com.au/dl/.*/.*/.*/).*' '${1}'
}
