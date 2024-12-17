def replace-csv-with-list [] {
  $in
  | split chars
  | reduce -f {quote: false, sofar: []} {|it, acc|
    if $it == ',' {
      $acc | update sofar ($acc.sofar | append (if $acc.quote { ' ' } else { ',' } ))
    } else if $it == '"' {
      $acc
      | update sofar ($acc | get sofar | append (if $acc.quote {']'} else '['))
      | update quote (not $acc.quote)
    } else {
      $acc | update sofar ($acc | get sofar | append $it)
    }
  }
  | get sofar
  | str join
}

def 'parse-attributes' [] {
  $in
  | replace-csv-with-list
  | split row ','
  | parse '{key}={value}'
}

export def 'from m3u8' [] {
  let input = ($in)
  # remove leading spaces, put values on a single line, remove comments, remove empty lines
  let treated = ($input | sed -E 's/^ *(.*)/\1/' | str replace -a ' *\\\n' '' | sed -e '/^# .*$/d' -e '/^$/d' | str trim -r)
  let t = ($treated | split row '#' | each {|it| let x = ($it | lines); let y = ($x.0 | split column ':'); let z = ($x | skip | wrap column3); $y | merge $z } | flatten | rename tag value uri | default null value | default null uri)
  echo $t
  | each { |it|
    $it
    | if $it.tag == 'EXTM3U' {
        $it
    } else if $it.tag == 'EXT-X-DISCONTINUITY' {
        $it
    } else if $it.tag == 'EXT-X-ENDLIST' {
        $it
    } else if $it.tag == 'EXT-X-INDEPENDENT-SEGMENTS' {
        $it
    } else (
        $it | update value (
          $it
          | if $it.tag == 'EXT-X-VERSION' {
              $it.value | into int
          } else if $it.tag == 'EXTINF' {
              # FIXME: if the compatibility version number is less than 3,
              # durations MUST be integers.
              $it.value
              | parse '{duration},{title}'
              | first
              | update duration ($in.duration | into decimal)
          } else if $it.tag == 'EXT-X-BYTERANGE' {
              # Use of the EXT-X-BYTERANGE tag REQUIRES a compatibility
              # version number of 4 or greater. Use of the EXT-X-BYTERANGE
              #tag REQUIRES a compatibility version number of 4 or greater.
              $it.value
              | parse -r '([0-9]*)(@[0-9]*)?'
              | first
              | reject Capture2
              | rename n o
              | update n ($in.n | into int)
              | update o ($in | if $in.o != '' { $in.o | into int } else null)
          } else if $it.tag == 'EXT-X-KEY' {
              $it.value
              | split row ','
              | parse '{key}={value}'
              | each { |it|
                  # TODO parse individual values
                  $it
              }
              | transpose -ir
              | first
          } else if $it.tag == 'EXT-X-MAP' {
              $it.value
              | split row ','
              | parse '{key}={value}'
              | each { |it|
                  # TODO parse individual values
                  $it
              }
              | transpose -ir
              | first
          } else if $it.tag == 'EXT-X-PROGRAM-DATE-TIME' {
              $it.value | into datetime
          } else if $it.tag == 'EXT-X-DATERANGE' {
              $it.value
              | split row ','
              | parse '{key}={value}'
              | each { |it|
                  # TODO parse individual values
                  if $it.key == 'START-DATE' {
                    $it | update value ($it.value | into datetime)
                  } else if $it.key == 'END-DATE' {
                    $it | update value ($it.value | into datetime)
                  } else if $it.key == 'DURATION' {
                    $it | update value ($it.value | into decimal)
                  } else if $it.key == 'PLANNED-DURATION' {
                    $it | update value ($it.value | into decimal)
                  } else $it
              }
              | transpose -ir
              | first
          } else if $it.tag == 'EXT-X-TARGETDURATION' {
              $it.value | into int
          } else if $it.tag == 'EXT-X-MEDIA-SEQUENCE' {
              $it.value | into int
          } else if $it.tag == 'EXT-X-DISCONTINUITY-SEQUENCE' {
              $it.value | into int
          } else if $it.tag == 'EXT-X-PLAYLIST-TYPE' {
              $it.value
          } else if $it.tag == 'EXT-X-I-FRAMES-ONLY' {
              $it.value
          } else if $it.tag == 'EXT-X-MEDIA' {
              # TODO
              $it.value
          } else if $it.tag == 'EXT-X-STREAM-INF' {
              $it.value
              | parse-attributes
              | each { |it|
                  if $it.key == "RESOLUTION" {
                    $it
                    | update value (
                        $it.value
                        | parse '{width}x{height}'
                        | first
                        | update width ($in.width | into int)
                        | update height ($in.height | into int)
                    )
                  } else if $it.key == "CODECS" {
                    $it | update value ($it.value | from nuon)
                  } else if $it.key == "BANDWIDTH" {
                    $it | update value ($it.value | into int)
                  } else if $it.key == "AVERAGE-BANDWIDTH" {
                    $it | update value ($it.value | into int)
                  } else if $it.key == "FRAME-RATE" {
                    $it | update value ($it.value | into decimal)
                  } else $it
              }
              | transpose -ir
              | first
          } else if $it.tag == 'EXT-X-I-FRAME-STREAM-INF' {
              $it.value | parse-attributes
          } else if $it.tag == 'EXT-X-SESSION-DATA' {
              $it.value | parse-attributes
          } else if $it.tag == 'EXT-X-SESSION-KEY' {
              $it.value | parse-attributes
          } else if $it.tag == 'EXT-X-START' {
              $it.value
              | parse-attributes
              | each { |it|
                  if $it.key == 'TIME-OFFSET' {
                    $it | update value ($it.value | into decimal)
                  } else $it
              }
              | transpose -ir
              | first
          } else $it.value
        )
    )
  }
}
