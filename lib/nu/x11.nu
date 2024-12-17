export def 'clipboard paste' [] { xclip -o -sel clip }

export def 'clipboard copy' [] { $in | xclip -i -sel clip }

export def "window info" [] {
  let text = (xwininfo | tail -n +8 | head -n -2 | sed -e 's/^[ \t]*//' | from yaml)
  { depth: $text.Depth
  , width: $text.Width
  , y: { relative: $text."Relative upper-left Y", absolute: $text."Absolute upper-left Y" }
  , x: { relative: $text."Relative upper-left X", absolute: $text."Absolute upper-left X" }
  , state:
    { saveUnder: $text."Save Under State"
    , gravity: { bit: $text."Bit Gravity State", window: $text."Window Gravity State" }
    , backingStore: $text."Backing Store State"
    , map: $text."Map State"
    , overrideRedirect: $text."Override Redirect State"
    }
  , class: $text.Class
  , colormap: $text.Colormap
  , height: $text.Height
  , visualClass: $text."Visual Class"
  , visual: $text.Visual
  , borderWidth: $text."Border width"
  }
}
