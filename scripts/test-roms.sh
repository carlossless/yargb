#!/usr/bin/env bash
# Runs public Game Boy test ROM suites against a yargb build.
#
#   scripts/test-roms.sh fetch                 download the suites into target/test-roms
#   scripts/test-roms.sh SUITE [yargb flags]   run one suite: blargg, acid, mooneye, mealybug, all
#
# Set YARGB to choose the binary (default: target/release/yargb). Needs curl,
# unzip and ImageMagick, which the Nix dev shell provides.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
roms=$root/target/test-roms
out=$root/target/test-results
bin=${YARGB:-$root/target/release/yargb}

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

differing_pixels() {
  magick compare -metric AE "$1" "$2" null: 2>&1 | cut -d' ' -f1 || true
}

blargg() {
  local rom
  for rom in cpu_instrs/cpu_instrs.gb instr_timing/instr_timing.gb mem_timing/mem_timing.gb; do
    echo "blargg $rom: $("$bin" "$roms/blargg/$rom" --headless --frames 4000 --serial "$@" 2>&1 >/dev/null | tr -s '\n' ' ' | tail -c 60)"
  done
  for rom in mem_timing-2/mem_timing.gb halt_bug.gb dmg_sound/dmg_sound.gb oam_bug/oam_bug.gb; do
    local png=$out/blargg-${rom//\//-}.png
    "$bin" "$roms/blargg/$rom" --headless --frames 3000 --screenshot "$png" "$@"
    echo "blargg $rom: reports on screen, see $png"
  done
}

acid() {
  "$bin" "$roms/acid/dmg-acid2.gb" --headless --frames 120 --screenshot "$out/dmg-acid2.png" "$@"
  echo "dmg-acid2: $(differing_pixels "$out/dmg-acid2.png" "$roms/acid/dmg-acid2.png") differing pixels"
}

# Mooneye tests report success by sending the Fibonacci bytes 3 5 8 13 21 34
# over serial. Tests for other models than the DMG are skipped.
mooneye() {
  local dir=$roms/mooneye/acceptance pass=0 failed=() rom
  while IFS= read -r rom; do
    case "$(basename "$rom" .gb)" in
      *-dmg0|*-mgb|*-sgb|*-sgb2|*-S|*-C|*-A|*-cgb*|*-agb*|*-ags*) continue ;;
    esac
    local result
    result=$(timeout 20 "$bin" "$rom" --headless --frames 600 --serial "$@" 2>&1 >/dev/null \
      | head -c 64 | od -An -tx1 | tr -d ' \n' || true)
    if [[ "$result" == *0305080d1522* ]]; then pass=$((pass + 1)); else failed+=("${rom#"$dir"/}"); fi
  done < <(find "$dir" -name '*.gb' | sort)
  echo "mooneye acceptance: $pass passed, ${#failed[@]} failed"
  if ((${#failed[@]})); then printf '  %s\n' "${failed[@]}"; fi
}

mealybug() {
  local pass=0 failed=() expected
  for expected in "$roms"/mealybug/expected/*.png; do
    local name
    name=$(basename "$expected" .png)
    "$bin" "$roms/mealybug/roms/$name.gb" --headless --frames 60 --screenshot "$out/mealybug-$name.png" "$@"
    local diff
    diff=$(differing_pixels "$out/mealybug-$name.png" "$expected")
    if [[ "$diff" == 0 ]]; then pass=$((pass + 1)); else failed+=("$name ($diff px)"); fi
  done
  echo "mealybug: $pass passed, ${#failed[@]} failed"
  if ((${#failed[@]})); then printf '  %s\n' "${failed[@]}"; fi
}

command=${1:-}
shift || true
mkdir -p "$out"
case "$command" in
  fetch) fetch ;;
  blargg | acid | mooneye | mealybug) "$command" "$@" ;;
  all) blargg "$@"; acid "$@"; mooneye "$@"; mealybug "$@" ;;
  *) sed -n '2,8p' "$0"; exit 1 ;;
esac
