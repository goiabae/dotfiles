use ./gnu-coreutils-completions.nu *

export def 'user list' [] {
  open /etc/passwd
  | lines
  | parse '{name}:{password}:{uid}:{gid}:{gecos}:{home}:{login}'
  | into int uid gid
}

export def 'user uid' [] {
	ps -l | where pid == ($nu.pid) | first | get user_id
}

export def user [] {
  user list | where uid == (user uid)
}

export def tty [] {
  ^tty | str trim -r
}
