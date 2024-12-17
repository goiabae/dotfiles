use ~/lib/nu/network.nu

export def public-ip [] {
	let res = (http get -f https://ipinfo.io/ip)
	echo $res.body
}

export def lan-ips [] {
	network interfaces
	| where operstate == UP and link_type == ether
	| each { |it|
		$it.addr_info
		| where family == inet
		| first
		| get local
	}
}
