{
  description = "A library for reading Java class-files";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachSystem ["x86_64-darwin"] (system: let
      pkgs = import nixpkgs {inherit system;};
      haskellPackages = pkgs.haskellPackages;
      project = returnShellEnv:
        haskellPackages.developPackage {
          inherit returnShellEnv;
          root = self;
          name = "jvm-binary";
          source-overrides = {};
          overrides = hsuper: hself: {};
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
            (with haskellPackages; [cabal-install ghcid haskell-language-server hpack fourmolu]);
        };
    in {
      defaultPackage = project false;
      devShell = project true;
    });
}
