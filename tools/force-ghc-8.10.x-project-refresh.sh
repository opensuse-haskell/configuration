#! /usr/bin/env bash

set -eu -o pipefail

osc meta prj devel:languages:haskell:ghc-8.10.x -m "disable Tumblweed so that we can force a project refresh later" -F - <<EOF
<project name="devel:languages:haskell:ghc-8.10.x">
  <title>Haskell Development Environment for GHC 8.10.x</title>
  <description>A Haskell development environment that contains ghc-8.10.x, cabal-install, and a few other useful tools.</description>
  <person userid="osukup" role="maintainer"/>
  <person userid="psimons" role="maintainer"/>
  <repository name="openSUSE_Tumbleweed">
    <path project="openSUSE:Factory" repository="snapshot"/>
    <arch>x86_64</arch>
  </repository>
  <build>
    <disable repository="openSUSE_Tumbleweed"/>
  </build>
</project>
EOF

sleep 60

osc meta prj devel:languages:haskell:ghc-8.10.x -m "enable Tumblweed to force a project refresh" -F - <<EOF
<project name="devel:languages:haskell:ghc-8.10.x">
  <title>Haskell Development Environment for GHC 8.10.x</title>
  <description>A Haskell development environment that contains ghc-8.10.x, cabal-install, and a few other useful tools.</description>
  <person userid="osukup" role="maintainer"/>
  <person userid="psimons" role="maintainer"/>
  <repository name="openSUSE_Tumbleweed">
    <path project="openSUSE:Factory" repository="snapshot"/>
    <arch>x86_64</arch>
  </repository>
</project>
EOF
