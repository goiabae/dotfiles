# author: goiabae <goiabae@protonmail.com>
# written against GNU coreutils 9.2

# TODO
export extern "arch" []
export extern "b2sum" []
export extern "base32" []
export extern "base64" []
export extern "basename" []
export extern "basenc" []

# concatenate files and print on the standard output
export extern "cat" [
  --show-all (-A) # equivalent to -vET
  --number-nonblank (-b) # number nonempty output lines, overrides -n
  -e # equivalent to -vE
  --show-ends (-E) # display $ at end of each line
  --number (-n) # number all output lines
  --squeeze-blank (-s) # suppress repeated empty output lines
  -t # equivalent to -vT
  --show-tabs (-T) # display TAB characters as ^I
  -u # (ignored)
  --show-nonprinting (-v) # use ^ and M- notation, except for LFD and TAB
  --help # display this help and exit
  --version # output version information and exit
  ...files
]

export extern "chcon" []
export extern "chgrp" []

# change file mode bits
export extern "chmod" [
  --changes (-c) # like verbose but report only when a change is made
  --silent (-f) # suppress most error messages
  --quiet # suppress most error messages
  --verbose (-v) # output a diagnostic for every file processed
  --no-preserve-root # fail to operate recursively on '/'
  --reference: string # use reference file mode instead of specifying MODE values
  --recursive (-R) # change files and directories recursively
  --help # dipsplay this help and exit
  --version # output version information and exit
  mode # of the form '[ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=][0-7]+'
  ...files
]

# change file owner and group
export extern "chown" [
  --changes (-c) # like verbose but report only when a change is made
  --silent (-f) # suppress most error messages
  --quiet # suppress most error messages
  --verbose (-v) # output a diagnostic for every file processed
  --dereference # affect the referent of each symbolic link, rather than the symbolic link itself (default)
  --no-dereference (-h) # affect symbolic links instead of any referenced file
  --from: string # change the owner and/or group of each file only if its current owner and/or group match those specified here
  --no-preserve-root # do not treat '/' specially (the default)
  --preserve-root # fail to operate recursively on '/'
  --recursive (-R) # operate on files and directories recursively
  -H # if a command line argument is a symbolic link to a directory, traverse it
  -L # traverse every symbolic link to a directory encountered
  -P # do not traverse any symbolic links (default)
  --help # display this help and exit
  --version # output version information and exit
  --reference: string # use reference file owner and group instead of specifying MODE values
  user_group # USER:GROUP pair
  ...files
]

# run command or interactive shell with special root directory
export extern "chroot" [
  --groups: string # comma-separated list of supplementary groups
  --userspec: string # USER:GROUP specify user and group to user
  --skip-chdir # do not change working directory to '/'
  --help # display this help and exit
  --version # output version information and exit
  root: path
  command
  ...args
]

export extern "cksum" []
export extern "comm" []

# built-in on nushell
# export extern "cp" []

export extern "csplit" []

export extern "cut" [

]

export extern "date" [

]

export extern "dd" [

]

def "nu-complete df output" [] {
  ["source" "fstype" "itotal" "iused" "iavail" "ipcent" "size" "used" "avail" "pcent" "file" "target"]
}

export extern "df" [
  --all (-a) # include pseudo, duplicate, inaccessible file systems
  --block-size (-B): string # scale sizes by SIZE before printing them
  --human-readable (-h) # print sizes in powers of 1024
  --si (-H) # print sizes in powers of 1000
  --inodes (-i) # list inode information instead of block usage
  -k # like --block-size=1K
  --local (-l) # limit listing to local file systems
  --no-sync # do not invokw sync before getting usage info (default)
  --output: string@"nu-complete df output" # use the output format defined by FIELD_LIST, or print all fields if FIELD_LIST is omitted
  --portability (-P) # use the POSIX output format
  --sync # invoke sync before getting usage info
  --total # elide all entries insignificant to available space, and produce a grand total
  --type (-t): string # limit listing to file systems of type TYPE
  --print-type (-T) # print file system type
  --exclude-type (-x): string # limi tlisting to file systems not of type TYPE
  -v # ignored
  --help # display this help and exit
  --version # output version information and exit
  ...devices # absolute path to device files containing mounted filesystems
]

export extern "dir" []
export extern "dircolors" []
export extern "dirname" []

# built-in on nushell
# export extern "du" []

# built-in on nushell
# export extern "echo" []

export extern "env" [

]

export extern "expand" []
export extern "expr" []
export extern "factor" []
export extern "false" []
export extern "fmt" []
export extern "fold" []
export extern "groups" []

export extern "head" [

]

export extern "hostid" []
export extern "hostname-coreutils" []

export extern "id" [

]

export extern "install" []
export extern "join" []
export extern "link" []
export extern "ln" []
export extern "logname" []

# built-in on nushell
# export extern "ls" []

export extern "md5sum" []

export extern "mkdir" [

]

export extern "mkfifo" []
export extern "mknod" []
export extern "mktemp" []

# built-in on nushell
# export extern "mv" []

export extern "nice" []
export extern "nl" []
export extern "nohup" []
export extern "nproc" []
export extern "numfmt" []
export extern "od" []
export extern "paste" []
export extern "pathchk" []
export extern "pinky" []
export extern "pr" []
export extern "printenv" []

export extern "printf" [

]

export extern "ptx" []

export extern "pwd" [

]

export extern "readlink" [
  --canonicalize (-f) # all but the last component must exist
  --canonicalize-existing (-e) # all components must exist
  --canonicalize-missing (-m) # no requirements on components existence
  --no-newline (-n) # do not output the trailing delimiter
  --quiet (-q)
  --silent (-s) # suppress most error messages (default)
  --verbose (-v) # report error messages
  --zero (-z) # end each output line with NUL, not newline
  --help # display this help and exit
  --version # output version information and exit
  ...files
]

export extern "realpath" []

# built-in on nushell
# export extern "rm" []

export extern "rmdir" [

]

export extern "runcon" []
export extern "seq" []
export extern "sha1sum" []
export extern "sha224sum" []
export extern "sha256sum" []
export extern "sha384sum" []
export extern "sha512sum" []
export extern "shred" []
export extern "shuf" []

export extern "sleep" [

]

export extern "sort" [

]

export extern "split" []
export extern "stat" []
export extern "stdbuf" []
export extern "stty" []
export extern "sum" []
export extern "sync" []

export extern "tac" [

]

export extern "tail" [

]

export extern "tee" [

]

# aka '['
export extern "test" [

]

export extern "timeout" []

export extern "touch" [

]

export extern "tr" [

]

export extern "true" []
export extern "truncate" []
export extern "tsort" []

export extern "tty" [
  --silent (-s) # print nothing, only return an exit status
  --quiet # print nothing, only return an exit status
  --help # display this help and exit
  --version # output version information and exit
]

export extern "uname" [

]

export extern "unexpand" []
export extern "uniq" []
export extern "unlink" []

# print the user names of users currently logged in to the current host
export extern "users" [
  --help # display this help and exit
  --version # output version information and exit
  file
]

export extern "vdir" []

export extern "wc" [

]

export extern "who" []

export extern "whoami" [

]

export extern "yes" []
