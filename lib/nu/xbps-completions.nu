export extern xbps-query [
  --config (-C): path      # Path to confdir (xbps.d)
  --cachedir (-c): path    # Path to cachedir
  --debug (-d)             # Debug mode shown to stderr
  --help (-h)              # Print help usage
  --ignore-conf-repos (-i) # Ignore repositories defined in xbps.d
  --memory-sync (-M)       # Remote repository data is fetched and stored
                           # in memory, ignoring on-disk repodata archives.
  --property (-p): string  # Show properties for PKGNAME
  --repository (-R)        # Enable repository mode. This mode explicitly
                           # looks for packages in repositories.
                           # to the top of the list. This option can be
                           # specified multiple times.
  --regex                  # Use Extended Regular Expressions to match
  --fulldeptree            # Full dependency tree for -x/--deps
  --rootdir (-r): path     # Full path to rootdir
  --version (-V)           # Show XBPS version
  --verbose (-v)           # Verbose messages

# MODE
  --list-pkgs (-l)         # List installed packages
  --list-repos (-L)        # List registered repositories
  --list-hold-pkgs (-H)    # List packages on hold state
  --list-repolock-pkgs     # List repolocked packages
  --list-manual-pkgs (-m)  # List packages installed explicitly
  --list-orphans (-O)      # List orphan packages
  --ownedby (-o): path     # Search for package files by matching STRING or REGEX
  --show (-S): string      # Show information for PKG [default mode]
  --search (-s): string    # Search for packages by matching PKG, STRING or REGEX
  --cat: path              # Print FILE from PKG binpkg to stdout
  --files (-f): string     # Show package files for PKG
  --deps (-x): string      # Show dependencies for PKG
  --revdeps (-X): string   # Show reverse dependencies for PKG
  pkg?: string             # used by --cat as a second parameter
]

export extern xbps-install [
  --automatic(-A)             # Set automatic installation mode
  --config(-C): path          # Path to confdir (xbps.d)
  --cachedir(-c): path        # Path to cachedir
  --debug(-d)                 # Debug mode shown to stderr
  --download-only(-D)         # Download packages and check integrity, nothing else
  --force(-f)                 # Force package re-installation
                              # If specified twice, all files will be overwritten.
  --help(-h)                  # Print help usage
  --ignore-conf-repos(-i)     # Ignore repositories defined in xbps.d
  --ignore-file-conflicts(-I) # Ignore detected file conflicts
  --unpack-only(-U)           # Unpack packages in transaction, do not configure them
  --memory-sync(-M)           # Remote repository data is fetched and stored
                              # in memory, ignoring on-disk repodata archives
  --dry-run(-n)               # Dry-run mode
  -R
  --repository(-R): string    # Add repository to the top of the list
                              # This option can be specified multiple times
  --rootdir(-r): path         # Full path to rootdir
  --reproducible              # Enable reproducible mode in pkgdb
  --sync(-S)                  # Sync remote repository index
  --update(-u)                # Update target package(s)
  --verbose(-v)               # Verbose messages
  --yes(-y)                   # Assume yes to all questions
  --version(-V)               # Show XBPS version
  ...pkgs                     # Package expressions
]
