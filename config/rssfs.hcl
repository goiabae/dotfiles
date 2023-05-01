mountpoint = "/home/goiabae/net/rssfs"
category "test category" {
	feed {
		url = "https://blog.golang.org/feed.atom"
		showlink = true
		plaintext = false
		cache = true
		cachemins = 5
	}
}
feed {
	url = "https://this.does.not.exist/all.other.parameters.are.optional"
}
