def 'flatpak ps' [] {
  ^flatpak ps --columns=all
  | detect columns --no-headers
  | rename instance wrapper-pid child-pid id arch branch commit runtime runtime-branch runtime-commit active background
}
