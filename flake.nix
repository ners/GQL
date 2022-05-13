{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs: with inputs; flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      lib = pkgs.lib;
      haskell = pkgs.haskellPackages;
      haskellDeps = drv: builtins.foldl'
        (acc: type: acc ++ drv.getCabalDeps."${type}HaskellDepends")
        [ ]
        [ "executable" "library" "test" ];
      gql = haskell.callCabal2nix "GQL" ./. { };
    in
    {
      packages = { inherit gql; };
      defaultPackage = gql;

      devShell = pkgs.mkShell {
        nativeBuildInputs = [
          (haskell.ghcWithPackages (_: haskellDeps gql))
          haskell.cabal-install
          haskell.fourmolu
          haskell.haskell-language-server
          pkgs.cabal2nix
        ];
      };
    });
}

