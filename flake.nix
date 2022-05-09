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
      gql = haskell.callCabal2nix "GQL" ./. { };
      haskellDeps = drv:
        builtins.foldl'
          (acc: type: acc ++ drv.getCabalDeps."${type}HaskellDepends")
          [ ]
          [ "executable" "library" "test" ];
    in
    {
      packages = { inherit gql; };
      defaultPackage = gql;

      devShell = pkgs.mkShell {
        nativeBuildInputs =
          let
            haskell-deps = with builtins; with lib; pipe self.packages.${system} [
              attrValues
              (concatMap (drv: drv.getCabalDeps))
            ];
          in
          [
            (haskell.ghcWithPackages (_: haskellDeps gql))
            haskell.cabal-install
            haskell.fourmolu
            haskell.haskell-language-server
            pkgs.cabal2nix
          ];
      };
    });
}

