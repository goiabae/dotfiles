def agent [] { echo IPTVSmartersPlayer }

export def get-stream-domain [user, passwd] {
	let url = "http://bkimport.xyz/player_api.php"
	let res = (
		http post -f
		-H [Accept-Encoding gzip]
		-t application/x-www-form-urlencoded
		$url { username: $user password: $passwd }
	)

	let stream_domain = $res.body.server_info.url
	echo $stream_domain
}

export def get-live-categories [user, passwd] {
	let url = "http://bkimport.xyz/player_api.php"
	let res = (
		http post -f
		-H [Accept-Encoding gzip]
		-t application/x-www-form-urlencoded
		$url { username: $user password: $passwd action: get_live_categories }
	)

	let categories = (
		$res.body | select category_id category_name | rename id name
	)
	echo $categories
}

export def get-live-streams [user, passwd] {
	let url = "http://bkimport.xyz/player_api.php"
	let hs = [Accept-Encoding gzip] # headers
	let ct = "application/x-www-form-urlencoded" # content-type
	let payload = { username: $user password: $passwd action: get_live_streams }

	let res = (
		http post -f -H $hs -t $ct $url $payload
	)

	let streams = $res.body
	echo $streams
}

export def m3u-url [user, passwd, stream_id] {
	{ scheme: http
	  host: connecttvapp.xyz
	  port: 80
	  path: ("" | path join / live $user $passwd $"($stream_id).m3u8")
	}
	| url join
}

export def default-user [] {
	secret json iptv | from json
}
