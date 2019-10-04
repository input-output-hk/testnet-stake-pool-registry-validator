let
  nixpkgs          = import ./default.nix;
  default-compiler = "ghc865";
  overrides        = import ./overrides.nix;
in
{ compiler       ? default-compiler
, system         ? builtins.currentSystem
, crossSystem    ? null
, config         ? {}
}:
let
  pkgs     = (nixpkgs {}).pkgs;
  ghcOrig  = pkgs.haskell.packages."${compiler}";
  ghc      = ghcOrig.override { overrides = overrides pkgs; };

  iohkLib = import ./iohk-common.nix { inherit system crossSystem config; };
  extras   = [
    (import ./jormungandr.nix { inherit iohkLib pkgs; }).jormungandr-cli
    ghc.ghcid
    ghc.cabal-install
  ];
  localPackages  = with ghc; with pkgs.lib; with builtins; {};
  final-inferred = ghc.callCabal2nix "registry" ../. {};
in {
  inherit ghc;

  registry = final-inferred;

  shell     = ghc.shellFor {
    packages    = p: [final-inferred];
    withHoogle  = true;
    buildInputs = extras ++ [final-inferred];
  };
}
