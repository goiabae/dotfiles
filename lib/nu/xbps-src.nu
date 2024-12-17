def env? [v] { $v in (env).name }

let default = {
  compression: "zstd"
}

def "xbps create" [
  --compression(-c): string
  pkg
  destdir
] {
  let compression = (if $compression != null {
    $compression
  } else $default.compression)

  (xbps-create
   --architecture $pkg.arch
   --build-options ($pkg.options | str join)
   --changelog $pkg.changelog
   --compression $compression
   --config-files $pkg.config-files
   --conflicts $pkg.conflicts
   --desc $pkg.description
   --dependencies $pkg.dependencies
   --homepage $pkg.homepage
   --license $pkg.license
   --maintainer $pkg.maintainer
   --mutable-files $pkg.mutable
   --provides $pkg.provides
   --pkgver $"($pkg.name)-($pkg.version)_($pkg.revision)"
   --preserve
   --quiet
   --replaces $pkg.replaces
   --reverts $pkg.reverts
   --source-revisions $pkg.revisions
   --shlib-provides $pkg.lib.provides
   --shlib-requires $pkg.lib.requires
   --alternatives $pkg.alternatives
   --tags ($tags | str join)
   $destdir
  )
}

def main [] {
  pkg info

  let config = (if env? "XBPS_SRC_CONFIG" {
    open $env.XBPS_SRC_CONFIG
  } else {
    { path: {
        template: $"($nu.home-path)/src/xbps-template"
      , libs: $"($nu.home-path)/lib/"
      , master: (if env? "XDG_DATA_HOME" {
        $"($env.XDG_DATA_HOME)/xbps-src/master"
      })
      , host: (if env? "XDG_CACHE_HOME" {
        $"($env.XDG_DATA_HOME)/xbps-src/host"
      })
      }
    , options: {
        cc: "clang"
      }
    }
  })

  echo $config
}
