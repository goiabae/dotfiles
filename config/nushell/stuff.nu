def "from xbps-repodata" [] {
	zstdcat '-'
	| tar --extract --to-stdout --file '-' index.plist
	| jc --plist
	| from json
}

# parses an ipv6 or ipv4 string into a string of digits
def "from ip" []: string -> string { python3 -c $"import ipaddress; print\(int\(ipaddress.ip_address\('($in)'\)\)\)" }


def ip-geolocation [
	ip: string # ip is a string of digits
]: nothing -> list<any> {
	let db = xdg data-home  | path join geolite2-city-ipv6-num.csv
	if not ($db | path exists) {
		cd (xdg data-home)
		http get https://cdn.jsdelivr.net/npm/@ip-location-db/geolite2-city-7z/geolite2-city-ipv6-num.csv.7z | save geolite2-city-ipv6-num.csv.7z
		7z x geolite2-city-ipv6-num.csv.7z
	}

	open --raw $db
	| from csv --no-infer --noheaders
	| rename start end country state1 state2 city postcode lat lon timezone
	| where { |it| ($it.start <= $ip) and ($it.end >= $ip) }
	| first
}

def weather [--city (-c): string]: nothing -> list<any> {
	let ip = http get https://6.ident.me | from ip
	let geo = ip-geolocation $ip
	http get (do { |x| $x | url join } {
		scheme: https
		host: api.openweathermap.org
		path: /data/2.5/weather
		params: {
			lat: $geo.lat
			lon: $geo.lon
			units: metric
			appid: (secret token openweathermap)
		}
	})
}

module anime-season {
	def season-type [] {
		[ WINTER, SPRING, SUMMER, FALL ]
	}

	export def main [year: int, season: string@season-type ] {
		let query = open ($nu.default-config-dir | path join anime.gql)

		const headers = {
			Content-Type: "application/json"
			Accept: "application/json"
		}

		mut as = []
		mut idx = 0 # page index

		loop {
			let body = {
				query: $query
				variables: {
					page: $idx
					season: $season
					year: $year
				}
			}

			let res = http post -H $headers https://graphql.anilist.co ($body | to json)
			if ($res.data.Page.media | is-empty) { break }

			$as = ($as | append (
				$res.data.Page.media | update relations.nodes { |row|
					$row.relations.nodes | filter { |it| $it.type == ANIME }
				}
			))
			$idx = $idx + 1
		}

		echo $as
	}
}

use anime-season

const firefox_path = $nu.home-path | path join .mozilla/firefox

def "firefox tabs" []: nothing -> list<any> {
	if not ($firefox_path | path join profiles.ini | path exists) {
		error make { msg: "no firefox profiles.ini found" }
	}
	let profiles = $firefox_path | path join profiles.ini | open $in | jc --ini | from json
	let default_profile = $profiles | values | where Default? == '1' | first | get Path
	# FIXME: NixOS doesn't have a lz4json package
  let info = (lz4jsoncat  $'($default_profile)/sessionstore-backups/recovery.jsonlz4' | from json)
  $info.windows
  | reduce -f [] { |w, acc|
    $w.tabs
    | each { |tab|
      $tab
      | get entries
      | select url title
      | last
    }
    | append $acc
  }
}

def "firefox profiles" []: nothing -> list<any> {
  open .mozilla/firefox/profiles.ini
	| jc --ini
	| from json
	| transpose
	| where column0 =~ Profile[0-9]*
	| get column1
	| update IsRelative { |row| $row.IsRelative == '1' }
	| update Default { |row| $row.Default == '1' }
	| update Path { |row| echo ~/.mozilla/firefox/ | path join $row.Path }
	| rename name relative path default
}

def "firefox history" [profile: string] {
  let file = echo ~/.mozilla/firefox | path join $profile places.sqlite
  open $file | get moz_places
}

def "emacs eval" [exp: string, --server_socket (-s): path]: nothing -> string {
  let socket = (
    $server_socket
    | default (xdg runtime-dir | path join emacs server)
  )
  do { emacsclient -s $socket -e $exp }
  | complete
  | if $in.exit_code != 0 {
    error make {
      msg: $"Couldn't connect with emacs server: ($in.stderr)"
    }
  } else { $in.stdout }
}

# cookie clicker
def next-ascension [--pretty (-p)]: int -> list<any> {
  into string
  | ^next-ascension # J script
  | lines
  | first
  | split column ' ' chips cookies
  | first
  | if not $pretty {
    into int chips | into float cookies
  } else {
    $in
  }
}

def inv-instances []: nothing -> list<any> {
	http get "https://api.invidious.io/instances.json" | each { get 1 }
}

def tv [] {
	use iptv.nu

	let user = (iptv default-user)
	let file = (xdg cache-home | path join iptv.json)

	if not ($file | path exists) or (ls -l $file | get modified | first | (date now) - $in | $in > 4wk) {
		iptv get-live-streams $user.name $user.passwd
		| to json
		| save --force $file
	}

  if (open $file | from json | get user_info.auth | $in == 0) {
		 error make { msg: "Authentication failed" }
  }

	let streams = (open $file | select stream_id name)
	let sel = (try { $streams | input list -f } catch { return })

	$sel
	| iptv m3u-url $user.name $user.passwd $in.stream_id
	| mpv '--user-agent=TiviMate/4.7.0 (Rockchip RPCplus; Android 7.1.2)' $in
}
