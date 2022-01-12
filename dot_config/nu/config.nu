# -- FUNCTIONS

# Enter and list directory
def el [dir] {
  cd $dir
  ls
}

def ~ [] {
  $nu.env.HOME
}

# returns true if user == root, else false
def is_root [] {
  (id -u | str to-int) == 0
}

# implement the ternary operator
def ? [b1 v1 v2] {
  if $b1 == $true {echo $v1} {echo $v2}
}

def prompt-pwd [] {
  let path = (pwd | split row "/")
  let home = ($nu.env.HOME | split row "/")
  if ($path | length) > 1 {
    if ($home | reduce { $it in $path }) {
      let path-without-home = ($path | skip ($home | length))
      if ($path-without-home | wrap | compact | length) > 0 {
        let parent = ($path | skip ($home | length) | drop)
        if ($parent | wrap | compact | length) > 0 {
          let short-part = ($parent | each { |part|
            if ($part | str starts-with ".") {
              $"($part | str substring [0 2])/"
            } {
              $"($part | str substring [0 1])/"
            }
          })
          $"~/($short-part | str collect)($path | last)"
        } {
          $"~/($path | last)"
        }
      } {
        "~"
      }
    } {
      let parent = (echo $path | drop | str substring [0 1] | each { echo $it "/" } | str collect)
      $"/($parent)($path | last)"
    }
  } {
    pwd
  }
}

# source lisp-mode functions. mainly for math operations
source ~/sys/script/lisp-mode.nu
source ~/sys/script/app.nu
source ~/sys/script/sys.nu
source ~/sys/script/xbps.nu
source ~/sys/script/misc.nu

# -- ALIASES
alias speed = speedometer -r enp7s0
alias speedb = speedometer -r enp7s0 -t enp7s0
alias gpg = gpg2
alias pong = ping -c 20 gnu.org
alias cop = xclip -selection clipboard
alias no32 = grep -v 32bit
alias ll = ls -al
alias la = ls -A
alias l = ls -C
alias www = w3m duckduckgo.com
alias curl = curl -LJO
alias simple = rg -v '32bit|devel|dbg'
alias em = emacsclient -s server --tty -a 'emacs'
alias anime = ani-cli -q 720
