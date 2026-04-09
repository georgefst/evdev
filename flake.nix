{
  description = "Basic Haskell flake";
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2511";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = inputs@{ self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskell-nix.overlay
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc912";
                evalSystem = "x86_64-linux";
                crossPlatforms = p: [ p.aarch64-multiplatform ];
                shell.tools.cabal = "latest";
                shell.tools.haskell-language-server = "latest";
                shell.withHoogle = false;
                modules = [{
                  packages.libclang-bindings.components.library = {
                    build-tools = [ pkgs.llvmPackages.llvm ];
                    libs = [ pkgs.llvmPackages.libclang ];
                  };
                  packages.evdev.components.library = {
                    build-tools = [ hsBindgenHook ];
                  };
                }];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
        # hs-bindgen's libclang is separate from Cabal's C compilation pipeline,
        # so it needs explicit include paths. This hook (modelled on hs-bindgen's
        # own hsBindgenHook) sets BINDGEN_EXTRA_CLANG_ARGS so that libclang can
        # find system and library headers in the Nix store.
        # See: https://github.com/well-typed/hs-bindgen/tree/main/nix/
        hsBindgenHook = pkgs.makeSetupHook {
          name = "hs-bindgen-hook";
          substitutions = {
            clang = pkgs.llvmPackages.clang;
          };
        } (pkgs.writeText "hs-bindgen-hook.sh" ''
          populateHsBindgenEnv() {
              BINDGEN_EXTRA_CLANG_ARGS="$(<@clang@/nix-support/cc-cflags) $(<@clang@/nix-support/libc-cflags) $NIX_CFLAGS_COMPILE"
              export BINDGEN_EXTRA_CLANG_ARGS
              BINDGEN_BUILTIN_INCLUDE_DIR=disable
              export BINDGEN_BUILTIN_INCLUDE_DIR
          }
          postHook="''${postHook:-}"$'\n'"populateHsBindgenEnv"$'\n'
        '');
        flake = pkgs.myHaskellProject.flake { };
      in
      flake // {
        packages = flake.packages // {
          ci = pkgs.linkFarm "ci" (
            pkgs.lib.mapAttrsToList (name: drv: { inherit name; path = drv; })
              flake.ciJobs.packages
          );
        };
      }
    );
}
