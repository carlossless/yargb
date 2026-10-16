# yargb

A Game Boy and Game Boy Color emulator.

## Run

Needs Rust and SDL2. With Nix, `nix develop` provides both.

```sh
cargo run --release -- game.gb
```

Battery saves are written next to the ROM as `game.sav`.

| Key | Button |
|---|---|
| Arrows | D-pad |
| Z | A |
| X | B |
| Enter, Space | Start |
| Backspace, Right Shift | Select |
| Tab (hold) | Fast forward |
| Esc | Quit |

Run `yargb` with no arguments for the other options.

## License

MIT
