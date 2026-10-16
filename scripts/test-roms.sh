#!/usr/bin/env bash
# Re-downloads the public test ROM suites that `cargo test` runs into
# tests/roms, keeping only the files the tests use and their licences.
#
#   scripts/test-roms.sh update
#
# Needs curl, tar and unzip, which the Nix dev shell provides.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
roms=$root/tests/roms
gh=https://raw.githubusercontent.com

update() {
  download=$(mktemp -d)
  trap 'rm -rf "$download"' EXIT
  mkdir -p "$download"/{blargg,mooneye,mealybug}
  curl -sSfL https://github.com/retrio/gb-test-roms/archive/refs/heads/master.tar.gz \
    | tar xz -C "$download/blargg" --strip-components=1
  curl -sSfL https://gekkio.fi/files/mooneye-test-suite/mts-20240926-1737-443f6e1/mts-20240926-1737-443f6e1.tar.xz \
    | tar xJ -C "$download/mooneye" --strip-components=1
  curl -sSfL -o "$download/mealybug.zip" "$gh/mattcurrie/mealybug-tearoom-tests/master/mealybug-tearoom-tests.zip"
  unzip -q -o "$download/mealybug.zip" -d "$download/mealybug"

  rm -rf "$roms"
  mkdir -p "$roms"/{acid,blargg,mooneye,mealybug/roms,mealybug/expected}

  curl -sSfL -o "$roms/acid/dmg-acid2.gb" https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb
  curl -sSfL -o "$roms/acid/dmg-acid2.png" "$gh/mattcurrie/dmg-acid2/master/img/reference-dmg.png"
  curl -sSfL -o "$roms/acid/dmg-acid2.LICENSE" "$gh/mattcurrie/dmg-acid2/master/LICENSE"
  curl -sSfL -o "$roms/acid/cgb-acid2.gbc" https://github.com/mattcurrie/cgb-acid2/releases/download/v1.1/cgb-acid2.gbc
  curl -sSfL -o "$roms/acid/cgb-acid2.png" "$gh/mattcurrie/cgb-acid2/master/img/reference.png"
  curl -sSfL -o "$roms/acid/cgb-acid2.LICENSE" "$gh/mattcurrie/cgb-acid2/master/LICENSE"

  local rom
  for rom in cpu_instrs/cpu_instrs.gb instr_timing/instr_timing.gb mem_timing/mem_timing.gb \
    mem_timing-2/mem_timing.gb halt_bug.gb dmg_sound/dmg_sound.gb oam_bug/oam_bug.gb \
    cgb_sound/cgb_sound.gb interrupt_time/interrupt_time.gb; do
    mkdir -p "$roms/blargg/$(dirname "$rom")"
    cp "$download/blargg/$rom" "$roms/blargg/$rom"
  done
  cp "$download/blargg/readme.txt" "$roms/blargg/README.txt"

  cp "$download/mooneye/LICENSE" "$roms/mooneye/LICENSE"
  (cd "$download/mooneye" && find acceptance misc -name '*.gb' -exec cp --parents {} "$roms/mooneye/" \;)

  curl -sSfL -o "$roms/mealybug/LICENSE" "$gh/mattcurrie/mealybug-tearoom-tests/master/LICENSE"
  local name
  for name in $(curl -sSfL https://api.github.com/repos/mattcurrie/mealybug-tearoom-tests/contents/expected/DMG-blob \
    | grep -o '"name": "[^"]*\.png"' | cut -d'"' -f4); do
    curl -sSfL -o "$roms/mealybug/expected/$name" "$gh/mattcurrie/mealybug-tearoom-tests/master/expected/DMG-blob/$name"
    cp "$download/mealybug/${name%.png}.gb" "$roms/mealybug/roms/"
  done
}

case "${1:-}" in
  update) update ;;
  *) sed -n '2,7p' "$0"; exit 1 ;;
esac
