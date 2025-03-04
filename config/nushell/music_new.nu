const db_path = $nu.home-path | path join "net/sync/music.db"

def add_author [name] {
	let last_author_idx = open $db_path | query db "select max(author_id) as last_author_id from author;" | get last_author_id | first | if $in == null { -1 } else { $in }
	open $db_path | query db "insert into author (author_id, name) values (:idx, :name);" -p { idx: ($last_author_idx + 1), name: $name }
}

export def "music add" [authors_str, title, type, score: int] {
	# FIXME check for duplicates

	let last_music_idx = open $db_path | query db "select max(music_id) as last_music_id from music_new;" | get last_music_id | first | if $in == null { -1 } else { $in }
	let now = date now | format date '%Y-%m-%d'
	let params = { music_id: ($last_music_idx + 1), title: $title, type: $type, score: $score, revisions: 1, added: $now, modified: $now }
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

export def "music todo add" [authors_str, title, type] {
	# FIXME check for duplicates

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
