const domain = 'musicbrainz.org'
const version = 2

# extracts the value from a table whose row has the column tag with name $tag as
# if it were a record
def get-tag [tag] {
	group-by tag | get $tag | first | get content
}

export def "artist release-groups" [artist: string] {
	let url = {
		scheme: https
		host: $domain
		path: (['/' ws $version release-group] | path join)
		params: {
			artist: $artist
		}
	}
	http get -fr ($url | url join)
	| get body
	| from xml
	| get content.0.content
	| each { |rg|
		$rg.attributes
    | insert title (
			$rg.content | group-by tag | get title.0.content.0.content
		)
	}
}

export def "release-group releases" [rg: string] {
	let url = {
		scheme: https
		host: $domain
		path: (['/' ws $version release-group $rg] | path join)
		params: {
			inc: releases
		}
	}
	http get -fr ($url | url join)
	| get body
	| from xml
	| get content.0.content
	| get-tag release-list
	| each { |release|
		$release.content.0
		| select tag content.0.content
		| rename tag value
		| group-by tag
		| insert id $release.attributes.id
	}
}

export def "release recordings" [release: string] {
	let url = {
		scheme: https
		host: $domain
		path: (['/' ws $version release $release] | path join)
		params: {
			inc: recordings
		}
	}

	let process_resp = { |resp|
		$resp
		| from xml
		| get content.0.content
		| get-tag medium-list
			# a release can have multiple mediums, like being split accross multiple CDs
			| par-each { |medium|
			| get content
			| get-tag track-list
			| get content
			| par-each { |it|
				$it
				| get-tag recording
				| get-tag title
				| get 0.content
			}
		}
		| flatten
	}

	# query xml doesn't work (see #8624)
	http get -r ($url | url join) | do $process_resp $in
}

export def "search artist" [artist: string] {
	let url = {
		scheme: https
		host: $domain
		path: (['/' ws $version artist ''] | path join)
		params: {
			query: $"artist:($artist | url encode)"
		}
	}
	http get -f ($url | url join)
	| get body.content.0.content
	| each { |art|
	  { name: ($art.content | get-tag name | get 0.content)
		, id: $art.attributes.id
		}
	}
}

export def "search track" [ artist_name: string, title: string ] {
	let url = {
		scheme: https
		host: $domain
		path: (['/' ws $version recording ''] | path join)
		params: {
			query: ($"recording:($title) AND artistname:($artist_name)" | url encode)
		}
	}
	let res = http get -f --headers [Accept "application/json"] ($url | url join)
	$res.body.recordings | each { |rec|
		$rec | {
			id: $in.id
			title: $in.title
			score: $in.score
			length: ($in.length? | default 0 | into duration --unit ms)
			release: ($in.first-release-date? | default "Unknown date")
			author: ($in.artist-credit | get name | str join ' + ')
		}
	}
}

export def "search list" [artist_name: string, title: string] {
	let url = {
		scheme: https
		host: $domain
		path: (['/' ws $version release-group ''] | path join)
		params: {
			query: ($"release:($title) AND artistname:($artist_name)" | url encode)
		}
	}
	let res = http get -f --headers [Accept "application/json"] ($url | url join)
	$res.body.release-groups | each { |rec|
	  $rec
    | select id title score first-release-date? artist-credit.0.name
		| rename id title score release author
	}
}

def is_valid_mbid [str: string] {
	$str =~ '[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}'
}

export def "fetch-cover" [mbid: string, type: string] -> path {
	if $type != release-group {
		error make { msg: "Only release-group fetching is implemented" }
	}
	let res = http get -f $"https://coverartarchive.org/release-group/($mbid)/front"
	if ($res.headers.response | where name == content-type | first | get value) != 'image/jpeg' {
		error make { msg: "Format not implemented" }
	}
	let filename = mktemp -t --suffix .jpeg
	$res.body | save -f $filename
	echo $filename
}
