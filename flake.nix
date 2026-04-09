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
              let
                addIncludeDir =
                  ''
                    export C_INCLUDE_PATH="${final.stdenv.cc.libc.dev}/include''${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
                  '';
              in
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc912";
                evalSystem = "x86_64-linux";
                crossPlatforms = p: [ p.aarch64-multiplatform ];
                shell.tools.cabal = "latest";
                shell.tools.haskell-language-server = "latest";
                shell.withHoogle = false;
                shell.shellHook = addIncludeDir;
                modules = [{
                  packages.libclang-bindings.components.library = {
                    build-tools = [ pkgs.llvmPackages.llvm ];
                    libs = [ pkgs.llvmPackages.libclang ];
                  };
                  packages.evdev.components.library.preBuild = addIncludeDir;
                }];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
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
