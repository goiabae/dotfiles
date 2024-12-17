# author: goiabae <goiabae@protonmail.com>
# written against termdown 1.17.0

def 'fonts' [] {
  [
    3-d
    3x5
    5lineoblique
    acrobatic
    alligator
    alligator2
    alphabet
    avatar
    banner
    banner3-D
    banner3
    banner4
    barbwire
    basic
    bell
    big
    bigchief
    binary
    block
    bubble
    bulbhead
    calgphy2
    caligraphy
    catwalk
    chunky
    coinstak
    colossal
    computer
    contessa
    contrast
    cosmic
    cosmike
    cricket
    cyberlarge
    cybermedium
    cybersmall
    diamond
    digital
    doh
    doom
    dotmatrix
    drpepper
    eftichess
    eftifont
    eftipiti
    eftirobot
    eftitalic
    eftiwall
    eftiwater
    epic
    fender
    fourtops
    fuzzy
    goofy
    gothic
    graffiti
    hollywood
    invita
    isometric1
    isometric2
    isometric3
    isometric4
    italic
    ivrit
    jazmine
    jerusalem
    katakana
    kban
    larry3d
    lcd
    lean
    letters
    linux
    lockergnome
    madrid
    marquee
    maxfour
    mike
    mini
    mirror
    mnemonic
    morse
    moscow
    nancyj-fancy
    nancyj-underlined
    nancyj
    nipples
    ntgreek
    o8
    ogre
    pawp
    peaks
    pebbles
    pepper
    poison
    puffy
    pyramid
    rectangles
    relief
    relief2
    rev
    roman
    rot13
    rounded
    rowancap
    rozzo
    runic
    runyc
    sblood
    script
    serifcap
    shadow
    short
    slant
    slide
    slscript
    small
    smisome1
    smkeyboard
    smscript
    smshadow
    smslant
    smtengwar
    speed
    stampatello
    standard
    starwars
    stellar
    stop
    straight
    tanja
    tengwar
    term
    thick
    thin
    threepoint
    ticks
    ticksslant
    tinker-toy
    tombstone
    trek
    tsalagi
    twopoint
    univers
    usaflag
    weird
  ]
}

# Starts a countdown to or from TIME. If TIME is not given, termdown will operate in stopwatch mode and count forward.
export extern termdown [
  --alt-format (-a) # Use colon-separated time format
  --blink (-b) # Flash terminal at end of countdown
  --no-bell (-B) # Don't ring terminal bell at end of countdown
  --critical (-c): int # Draw final N seconds in red and announce them individually with --voice or --exec-cmd (defaults to 3)
  --font (-f): string@fonts # Choose from http://www.figlet.org/examples.html
  --voice-prefix (-p): string # Add TEXT to the beginning of --voice and --exec annunciations (except per-second ones)
  --quit-after (-q): int #  Quit N seconds after countdown (use with -b or -t) or terminate stopwatch after N seconds
  --no-seconds (-s) # Don't show seconds (except for last minute of countdown and first minute of stopwatch)
  --text (-t): string # Text to display at end of countdown
  --title (-T): string # Text to display on top of countdown/stopwatch
  --no-window-title (-W) # Don't update terminal title with remaining/elapsed time
  --voice (-v): string # Spoken countdown (at fixed intervals with per-second annunciations starting at --critical
  --outfile (-o): path # File to write current remaining/elapsed time to
  --exec-cmd: string # Runs CMD every second. '{0}' and '{1}' in CMD will be replaced with the remaining/elapsed number of seconds and a more sparse annunciation as in --voice, respectively.
  --no-figlet # Don't use ASCII art for display
  --no-text-magic # Don't try to replace non-ASCII characters (use with -t)
  --version # Show version and exit
  --time # Show current time instead of countdown/stopwatch
  --time-format (-Z): string # Format for --time (defaults to "%H:%M:%S", ignores --no-seconds)
  time # For example: 10, '1h 5m 30s', '12:00', '2020-01-01', '2020-01-01 14:00 UTC'
]
