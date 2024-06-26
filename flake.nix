{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    generic-type-functions.url   = "github:raehik/generic-type-functions";
    generic-type-functions.flake = false;
    type-level-bytestrings.url   = "github:raehik/type-level-bytestrings";
    type-level-bytestrings.flake = false;
  };
  outputs = inputs:
  let
    # simple devshell for non-dev compilers: really just want `cabal repl`
    nondevDevShell = compiler: {
      mkShellArgs.name = "${compiler}-bytezap";
      hoogle = false;
      tools = _: {
        hlint = null;
        haskell-language-server = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc96-bytezap;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc98 = {
          # shouldn't work, pkgs aren't up to date and mine aren't 9.8 ready
          basePackages = pkgs.haskell.packages.ghc98;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell.mkShellArgs.name = "ghc96-bytezap";
          devShell.tools = _: {
            haskell-language-server = null; # 2024-03-06: broken
          };
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc94";
        };
        haskellProjects.ghc92 = {
          basePackages = pkgs.haskell.packages.ghc92;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc92";
        };
      };
    };
}
