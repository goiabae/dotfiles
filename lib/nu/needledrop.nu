export def process-wtr [] {
  let tracks = $in | lines | parse "{authors} - {title}" | update authors { |row| $row.authors | split row ' + ' }
  let min_score = 70

  let not_to_add = ($tracks.authors | flatten | uniq) | each { |author|
    let m = music of-author $author
    if ($m.score | where $it != null | is-not-empty) {
      echo { author: $author, avg: ($m.score | where $it != null | math avg) }
    } else {
      echo { author: $author, avg: null }
    }
  } | where avg != null and avg < $min_score

  let to_add = $tracks | where { |row|
    let will_add = $row.authors | any { |a| $a in $not_to_add } | not $in
    let dice_roll = random dice --sides 2 | first | $in == 1
    $will_add or $dice_roll
  } | update authors { |row| $row.authors | str join ' + ' }

  for t in $to_add {
    print $"Adding ($t.title) by ($t.authors)"
    music todo add $t.authors $t.title track
  }
}
