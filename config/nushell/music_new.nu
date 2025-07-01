const db_path = $nu.home-path | path join "net/sync/music.db"

def types [] {
	echo ["list", "track"]
}

def add_author [name] {
	let last_author_idx = open $db_path | query db "select max(author_id) as last_author_id from author;" | get last_author_id | first | if $in == null { -1 } else { $in }
	open $db_path | query db "insert into author (author_id, name) values (:idx, :name);" -p { idx: ($last_author_idx + 1), name: $name }
}

export def "music add" [authors_str, title, type, score: int, tags: list<string> = []]: nothing -> int {
	# FIXME check for duplicates

	if $type not-in (types) {
		error make { msg: "Invalid type" }
	}

	let maybe_previous = music | where authors == $authors_str and title == $title and type == $type
	if ($maybe_previous | is-not-empty) {
		let last_modified = $maybe_previous | last | get modified | into datetime | (date now) - $in | format duration day
		error make { msg: $"Entry already exists. Was modified ($last_modified) ago" }
	}

	let last_music_idx = open $db_path | query db "select max(music_id) as last_music_id from music_new;" | get last_music_id | first | if $in == null { -1 } else { $in }
	let music_id = $last_music_idx + 1
	let now = date now | format date '%Y-%m-%d'
	let params = { music_id: $music_id, title: $title, type: $type, score: $score, revisions: 1, added: $now, modified: $now }
	open $db_path | query db "insert into music_new (music_id, title, type, score, revisions, added, modified) values (:music_id, :title, :type, :score, :revisions, :added, :modified);" -p $params

	let authors = $authors_str | split row ' + '

	for author in $authors {
		let res = open $db_path | query db "select author_id from author where name = :name;" -p { name: $author }
		let author_exists = $res | is-not-empty

		if not $author_exists {
			add_author $author
		}

		let author_id = open $db_path | query db "select author_id from author where name = :name;" -p { name: $author } | first | get author_id
		open $db_path | query db "insert into music_authors (music_id, author_id) values (:music_id, :author_id);" -p { music_id: $music_id, author_id: $author_id }
	}

	for tag in $tags {
		music add tag ($last_music_idx) $tag
	}

	$music_id
}

export def "music todo add" [authors_str, title, type] {
	# FIXME check for duplicates

	if $type not-in (types) {
		error make { msg: "Invalid type" }
	}

	let last_music_idx = open $db_path | query db "select max(music_id) as last_music_id from music_new;" | get last_music_id | first | if $in == null { -1 } else { $in }
	let params = { music_id: ($last_music_idx + 1), title: $title, type: $type, score: null, revisions: 1, added: null, modified: null }
	open $db_path | query db "insert into music_new (music_id, title, type, score, revisions, added, modified) values (:music_id, :title, :type, :score, :revisions, :added, :modified);" -p $params

	let authors = $authors_str | split row ' + '

	for author in $authors {
		let res = open $db_path | query db "select author_id from author where name = :name;" -p { name: $author }
		let author_exists = $res | is-not-empty

		if not $author_exists {
			add_author $author
		}

		let author_id = open $db_path | query db "select author_id from author where name = :name;" -p { name: $author } | first | get author_id
		open $db_path | query db "insert into music_authors (music_id, author_id) values (:music_id, :author_id);" -p { music_id: ($last_music_idx + 1), author_id: $author_id }
	}
}

export def "music" [] {
	open $db_path | query db "select mn.music_id, group_concat(a.name, ' + ') as authors, mn.title, mn.type, mn.score, mn.added, mn.modified from music_new mn inner join music_authors ma on mn.music_id = ma.music_id inner join author a on a.author_id = ma.author_id group by mn.music_id;"
}

export def "music todo" [] {
	open $db_path | query db "select mn.music_id, group_concat(a.name, ' + ') as authors, mn.title, mn.type from music_new mn inner join music_authors ma on mn.music_id = ma.music_id inner join author a on a.author_id = ma.author_id where mn.score is null group by mn.music_id;"
}

export def "music of-author" [author: string] {
	open $db_path | query db "select mn.music_id, group_concat(a.name, ' + ') as authors, mn.title, mn.type, mn.score, mn.added, mn.modified from music_new mn inner join music_authors ma on mn.music_id = ma.music_id inner join author a on a.author_id = ma.author_id where a.name = :author group by mn.music_id;" -p { author: $author }
}

export def "music todo done" [entry, score: int] {
	let now = date now | format date '%Y-%m-%d'
	open $db_path | query db "update music_new set score = :score, added = :now, modified = :now where music_id = :music_id;" -p { score: $score, now: $now, music_id: $entry.music_id }
}

export def "music delete" [music_id: int] {
  open $db_path | query db "delete from music_authors where music_id = :music_id;" -p { music_id: $music_id }
  open $db_path | query db "delete from music_new where music_id = :music_id;" -p { music_id: $music_id }
}

export def "music review" [music_id: int, score: int] {
	let today = date now | format date '%Y-%m-%d'
	open $db_path | query db $"update music_new set modified = :today, score = :score where music_id = :music_id;" -p { today: $today, score: $score, music_id: $music_id }
}

export def "music all tags" [] {
	open $db_path | query db "select * from tag;"
}

export def "music tags" [music_id] {
	open $db_path | query db "select t.tag_id, t.name from tag t inner join music_tags mt on t.tag_id = mt.tag_id inner join music_new m on mt.music_id = m.music_id where m.music_id = :music_id;" -p { music_id: $music_id }
}

export def "music create tag" [name: string, parent?: int] {
	let last_tag_id = open $db_path | query db "select max(tag_id) as last_tag_id from tag;" | get last_tag_id | first | if $in == null { -1 } else { $in }
	if $parent != null {
		if (music all tags | where tag_id == $parent | is-empty) {
			error make { msg: "parent tag doesn't exist" }
		}

		open $db_path | query db "insert into tag (tag_id, name, parent_tag_id) values (:tag_id, :name, :parent_tag_id);" -p { tag_id: ($last_tag_id + 1), name: $name, parent_tag_id: $parent }
	} else {
		open $db_path  | query db "insert into tag (tag_id, name, parent_tag_id) values (:tag_id, :name, NULL);" -p { tag_id: ($last_tag_id + 1), name: $name }
	}

	($last_tag_id + 1)
}

export def "music add tag" [music_id: int, name: string] {
	if (music | where music_id == $music_id | is-empty) {
		error make { msg: $"music with id (music_id) doesn't exist" }
	}

	let music_id = music | where music_id == $music_id | first | get music_id

	let tag_id = if (music all tags | where name == $name | is-empty) {
		music create tag $name
	} else {
		music all tags | where name == $name | first | get tag_id
	}

	open $db_path | query db "insert into music_tags (music_id, tag_id) values (:music_id, :tag_id);" -p { music_id: $music_id, tag_id: $tag_id }
}
