# yargb

A Game Boy and Game Boy Color emulator.

## Run

Needs Rust and SDL2. With Nix, `nix develop` provides both.

```sh
cargo run --release -- game.gb
```

In-game saves are kept next to the ROM as `game.sav` and written when you quit.

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
