{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, utils, naersk, rust-overlay }:
    utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];

        pkgs = import nixpkgs {
          inherit system overlays;
        };

        toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

        naersk' = pkgs.callPackage naersk {
          cargo = toolchain;
          rustc = toolchain;
          clippy = toolchain;
        };

        buildInputs = with pkgs; [
          pkg-config
          binutils
          SDL2
        ] ++
        (lib.optionals (stdenv.hostPlatform.isLinux) [
          udev
        ]) ++
        (lib.optionals (stdenv.hostPlatform.isDarwin) [
          darwin.apple_sdk.frameworks.IOKit
          darwin.apple_sdk.frameworks.AppKit
          iconv
        ]);
      in
      {
        formatter = pkgs.nixpkgs-fmt;

        packages = {
          # For `nix build` `nix run`, & `nix profile install`:
          default = naersk'.buildPackage rec {
            pname = "sinowealth-kb-tool";
            version = "latest";

            src = ./.;
            doCheck = true; # run `cargo test` on build

            inherit buildInputs;

            meta = with pkgs.lib; {
              description = "Yet another gameboy emulator";
              homepage = "https://github.com/carlossless/yargb";
              license = licenses.mit;
              mainProgram = "yargb";
              maintainers = with maintainers; [ carlossless ];
            };
          };
        };

        devShells.default = pkgs.mkShell {
          inherit buildInputs;
          nativeBuildInputs = with pkgs; [ rustup toolchain ];
        };
      }
    );
}
