# export use /store/source/nu_scripts/custom-completions/make/make-completions.nu *
# export use /store/source/nu_scripts/custom-completions/ani-cli/ani-cli-completions.nu *
# export use /store/source/nu_scripts/custom-completions/nix/nix-completions.nu *
# export use /store/source/nu_scripts/custom-completions/git/git-completions.nu *
# export use /store/source/nu_scripts/custom-completions/cargo/cargo-completions.nu *
# export use /store/source/nu_scripts/custom-completions/nnn/nnn-completions.nu *

module coreutils {
  export use ./gnu-coreutils-completions.nu *
}

use coreutils

export use ./ytfzf-completions.nu *
export use ./xbps-completions.nu * # completions + wrappers
export use ./himalaya-completions.nu *
export use ./man-completions.nu *
export use ./animdl-completions.nu *
export use ./firefox-completions.nu *
export use ./chezmoi-completions.nu *

# run command and output diff of environment variables
export extern diff-env [command, ...args]
