{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs:
  let
    defDevShell = compiler: {
      mkShellArgs.name = "${compiler}";
      hoogle = false;
      tools = _: {
        haskell-language-server = null;
        hlint = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc98-bytezap;
        devShells.default = self'.devShells.ghc98;
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          devShell = defDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          devShell = defDevShell "ghc96";
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          devShell = defDevShell "ghc94";
        };
        haskellProjects.ghc92 = {
          basePackages = pkgs.haskell.packages.ghc92;
          devShell = defDevShell "ghc92";
        };
      };
    };
}
