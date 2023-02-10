{
  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    #flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc944;
          packages = {
            store2.root = ./.;
          };
          # buildTools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          buildTools = hp: { haskell-language-server = null; ghcid = null; hlint = null; };
          # overrides = self: super: { };
          # hlintCheck.enable = true;
          hlintCheck.enable = false;
          # hlsCheck.enable = true;
          hlsCheck.enable = false;
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        #packages.default = self'.packages.my-package;
      };
    };
}
