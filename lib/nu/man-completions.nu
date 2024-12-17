# author: goiabae <goiabae@protonmail.com>
# developed against mandoc 1.14.6 https://mandoc.bsd.lv/

def encodings [] {
  [us-ascii iso-8859-1 utf-8]
}

def sections [] {
  [1 2 3 "3p" 4 5 6 7 8 9]
}

def formats [] {
  [ascii, html, man, markdown, pdf, "ps", tree, utf-8, lint]
}

def warning-levels [] {
  [base style warning error unsupp openbsd netbsd all stop]
}

export extern "man" [
  -a # Display all matching manual pages
  -C: path # Use the specified file instead of the default configuration file
  -c # Copy the manual page to the standard output instead of using less to paginate it
  -f # A synonym for whatis
  -h # Display only the SYNOPSIS lines of the requested manual pages
  -k # A synonym for apropos
  -l # A synonym for mandoc
  -M: path # Override the list of directories to search for manual pages
  -m: path # Augment the list of directories to search for manual pages
  -S: string # Only show pages for the specified machine architecture
  -s: string@sections # Only select manuals from the specified section
  -w # List the pathnames of all matching manual pages instead of displaying any of them
  -I: string # Override the default operating system name for the mdoc(7) Os and for the man(7) TH macro
  -K: string@encodings # Specify the input encoding
  -O: string # Comma-separated output options
  -T: string@formats # Select the output format
  -W: string@warning-levels # Specify the minimum message level to be reported
  ...name
]
