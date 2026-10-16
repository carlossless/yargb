#!/usr/bin/env bash
# Downloads the public test ROM suites the emulator is checked against and runs
# them.
#
#   scripts/test-roms.sh fetch   download the suites into target/test-roms
#   scripts/test-roms.sh run     run every test, including the ROM suites
#
# The ROM suites are ignored by a plain `cargo test`; `run` includes them. Set
# YARGB_TEST_ROMS to keep the ROMs somewhere else. Needs curl and unzip, which
# the Nix dev shell provides.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
roms=${YARGB_TEST_ROMS:-$root/target/test-roms}

fetch() {
  rm -rf "$roms"
  mkdir -p "$roms"/{acid,blargg,mooneye,mealybug/roms,mealybug/expected}
  local gh=https://raw.githubusercontent.com
  curl -sSfL -o "$roms/acid/dmg-acid2.gb" https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb
  curl -sSfL -o "$roms/acid/dmg-acid2.png" "$gh/mattcurrie/dmg-acid2/master/img/reference-dmg.png"
  curl -sSfL -o "$roms/acid/cgb-acid2.gbc" https://github.com/mattcurrie/cgb-acid2/releases/download/v1.1/cgb-acid2.gbc
  curl -sSfL -o "$roms/acid/cgb-acid2.png" "$gh/mattcurrie/cgb-acid2/master/img/reference.png"
  curl -sSfL https://github.com/retrio/gb-test-roms/archive/refs/heads/master.tar.gz \
    | tar xz -C "$roms/blargg" --strip-components=1
  curl -sSfL https://gekkio.fi/files/mooneye-test-suite/mts-20240926-1737-443f6e1/mts-20240926-1737-443f6e1.tar.xz \
    | tar xJ -C "$roms/mooneye" --strip-components=1
  curl -sSfL -o "$roms/mealybug/roms.zip" "$gh/mattcurrie/mealybug-tearoom-tests/master/mealybug-tearoom-tests.zip"
  unzip -q -o "$roms/mealybug/roms.zip" -d "$roms/mealybug/roms"
  local name
  for name in $(curl -sSfL https://api.github.com/repos/mattcurrie/mealybug-tearoom-tests/contents/expected/DMG-blob \
    | grep -o '"name": "[^"]*\.png"' | cut -d'"' -f4); do
    curl -sSfL -o "$roms/mealybug/expected/$name" "$gh/mattcurrie/mealybug-tearoom-tests/master/expected/DMG-blob/$name"
  done
}

case "${1:-}" in
  fetch) fetch ;;
  run) cd "$root" && YARGB_TEST_ROMS=$roms cargo test --release -- --include-ignored ;;
  *) sed -n '2,10p' "$0"; exit 1 ;;
esac
