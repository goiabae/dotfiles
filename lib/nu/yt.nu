const invidious_domains = [
	"vid.puffyan.us"
	"yewtu.be"
]

export def "play video" [id: string] {
  mpv $"https://www.youtube.com/watch?v=($id)"
}

export def "play music" [] {
  let input = $in
  let query = $"($input.author) ($input.title)"
  ytfzf $query
}

export def "video-info" [--info: list, id: string] {
  let fields = ($info | default [
    videoId, title, authorId,
    author, published, viewCount,
    lengthSeconds, likeCount, dislikeCount, keywords
  ] | str join ',')
  http get $"https://($invidious_domains.1)/api/v1/videos/($id)?fields=($fields)"
}

export def "comments" [id: string, --continuation: string] {
	echo {
		scheme: https
		host: $invidious_domains.1
		path: ([ '/' 'api' 'v1' 'comments' $id ] | path join)
		params: {}
	}
	| if $continuation != null {
		upsert params.continuation $continuation
	} else $in
	| url join
	| http get $in
	| get comments
	| select author likeCount replies? content
	| sort-by -r likeCount
	| update replies { |row|
		if $row.replies? != null {
			comments $id --continuation $row.replies.continuation
		}
	}
}

export def "search-enqueue" [] {
  $in | each { |it| ytfzf -D --url-handler=mpvq --detach $it }
}
