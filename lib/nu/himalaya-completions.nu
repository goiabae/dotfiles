# author: goiabae <goiabae@protonmail.com>
# developed against himalaya 0.7.3

def formats [] {
  [plain json]
}

def colors-when [] {
  [never auto always "ansi"]
}

def accounts [] {
  himalaya accounts list -o json | from json | get name
}

# CLI to manage yours emails
export extern "himalaya" [
  --config (-c): path # Set a custom configuration file path
  --account (-a): string@accounts # Set the account
  --disable-cache # Disable any sort of cache
  --output (-o): string@formats # Set the output format
  --color (-C): string@colors-when # Controls when to use colors
  --folder (-f): string # Set the source folder
  --help (-h) # Print help
  --version (-V) # Print version
]

# Manage accounts
export extern "himalaya accounts" [
  --config (-c): path # Set a custom configuration file path
  --account (-a): string@accounts # Set the account
  --disable-cache # Disable any sort of cache
  --output (-o): string@formats # Set the output format
  --color (-C): string@colors-when # Controls when to use colors
  --folder (-f): string # Set the source folder
  --help (-h) # Print help
  --version (-V) # Print version
]

# List all accounts from the config file
export extern "himalaya accounts list" [
  --config (-c): path # Set a custom configuration file path
  --max-width (-w): int # Defines a maximum width for the table
  --account (-a): string@accounts # Set the account
  --disable-cache # Disable any sort of cache
  --output (-o): string@formats # Set the output format
  --color (-C): string@colors-when # Controls when to use colors
  --folder (-f): string # Set the source folder
  --help (-h) # Print help
  --version (-V) # Print version
]

# Synchronize the given account locally
export extern "himalaya accounts sync" [
  --all-folders (-A) # Synchronize all folders
  --config (-c): path # Set a custom configuration file path
  --account (-a): string@accounts # Set the account
  --include-folder (-F): string # Synchronize only the given folders
  --disable-cache # Disable any sort of cache
  --exclude-folder (-x): string # Synchronize all folders except the given ones
  --dry-run (-d) # Do not apply changes of the synchronization
  --output (-o): string@formats # Set the output format
  --color (-C): string@colors-when # Controls when to use colors
  --folder (-f): string # Set the source folder
  --help (-h) # Print help
  --version (-V) # Print version
]

# List folders
export extern "himalaya folders list" [
  --config (-c): path # Set a custom configuration file path
  --account (-a): string@accounts # Set the account
  --max-width (-w): int # Defines a maximum width for the table
  --disable-cache # Disable any sort of cache
  --output (-o): string@formats # Set the output format
  --color (-C): string@colors-when # Controls when to use colors
  --folder (-f): string # Set the source folder
  --help (-h) # Print help
  --version (-V) # Print version
]

# TODO

# Generates the completion script for the given shell
export extern "himalaya completion" []

# Generate all man pages to the given directory
export extern "himalaya man" []

# Manage folders
export extern "himalaya folders" []

# Handles email flags
export extern "himalaya flags" []

# Handles email templates
export extern "himalaya template" []

# Downloads all emails attachments
export extern "himalaya attachments" []

# List envelopes
export extern "himalaya list" []

# Filter envelopes matching the given query
export extern "himalaya search" []

# Sort envelopes by the given criteria and matching the given query
export extern "himalaya sort" []

# Write a new email
export extern "himalaya write" []

# Send a raw email
export extern "himalaya send" []

# Save a raw email
export extern "himalaya save" []

# Read text bodies of emails
export extern "himalaya read" []

# Answer to an email
export extern "himalaya reply" []

# Forward an email
export extern "himalaya forward" []

# Copy emails to the given folder
export extern "himalaya copy" []

# Move emails to the given folder
export extern "himalaya move" []

# Delete emails
export extern "himalaya delete" []

# Notifies when new messages arrive in the given folder
export extern "himalaya notify" []

# Watches IMAP server changes
export extern "himalaya watch" [
  --config (-c): path # Set a custom configuration file path
  --keepalive (-k): int # Specifies the keepalive duration in seconds. (default 500)
  --account (-a): string@accounts # Set the account
  --disable-cache # Disable any sort of cache
  --output (-o): string@formats # Set the output format
  --color (-C): string@colors-when # Controls when to use colors
  --folder (-f): string # Set the source folder
  --help (-h) # Print help
  --version (-V) # Print version
]

export extern "himalaya help" []
