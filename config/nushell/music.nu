use mbz.nu

const db = ("~/doc/table/music.db" | path expand)

export def main [] { open $db | query db "select * from music;" }

export def add-mbid [entry] {
	let today = date now | format date '%Y-%m-%d'
	print $"Updating ($entry.title) by ($entry.author)"
	let tracks = match $entry.type {
		  "track" => { mbz search track $entry.author $entry.title }
		  "list" =>  { mbz search list $entry.author $entry.title }
		}
	if ($tracks | is-empty) { return }
  let exact = $tracks | filter { |it| $it.author == $entry.author and $it.title == $entry.title and $it.score == 100 }
	let id = if ($exact | is-not-empty) {
		$exact.0.id
  } else {
		print ($tracks | merge (0..($in | length) | wrap index))
		$tracks | get (input 'Enter index: ' | into int) | get id
  }
	open $db | query db $"
	  update music
		set mbid = '($id)',
		    modified = '($today)'
		where author = \"($entry.author)\"
		  and title = \"($entry.title)\";
  "
}

export def add [author, title, type, --force(-f)] {
	let score = input 'Enter score: '
	let today = date now | format date '%Y-%m-%d'
	let mbid = do {
		let ids = match $type {
		  "track" => { mbz search track $author $title }
		  "list" =>  { mbz search list $author $title }
		}
		if ($ids | is-empty) {
			if not $force {
			  print $"No MBID found this ($type)"
			  return null
			} else {
				""
			}
		} else {
		  print $ids
			try {
				$ids | get (input 'Enter index: ' | into int) | get id
			} catch {
				""
			}
		}
	}
	if $mbid == null { return }
	let query = "insert into music values (:author, :title, :type, :score, 0, :add, :mod, :mbid, '');"
	(
		open $db
		| query db $query --params {
		  author: $author,
		  title: $title,
		  type: $type,
		  score: $score,
		  add: $today,
		  mod: $today,
		  mbid: $mbid
		}
	)
}

export def review [entry] {
	let today = date now | format date '%Y-%m-%d'
	let score = input 'Enter score: ' | into float
	open $db | query db $"update music set modified = '($today)', score = ($score) where title = '($entry.title)' and author = '($entry.author)';"
}

export def "todo add" [author: string, title: string, type: string] {
	open $db
	| query db "insert into todo values (:author, :title, :type);" --params { author: $author title: $title type: $type }
	| null
}

export def todo [] { open $db | query db "select * from todo;" }

export def "todo done" [entry, --force(-f)] {
	open $db | query db "delete from todo where author = :author and title = :title and type = :type;" -p $entry
	let author = $entry.author
	let title = $entry.title
	let type = $entry.type
	if $force {
		add --force $author $title $type
	} else {
		add $author $title $type
	}
}
