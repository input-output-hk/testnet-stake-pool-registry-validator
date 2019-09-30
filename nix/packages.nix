let
  nixpkgs          = import ./default.nix;
  default-compiler = "ghc865";
  overrides        = import ./overrides.nix;
in
{ compiler       ? default-compiler
}:
let
  pkgs     = (nixpkgs {}).pkgs;
  ghcOrig  = pkgs.haskell.packages."${compiler}";
  ghc      = ghcOrig.override { overrides = overrides pkgs; };
  extras   = [
               ghc.ghcid
               ghc.cabal-install
             ];
  localPackages  = with ghc; with pkgs.lib; with builtins; {};
  final-inferred = ghc.callCabal2nix "validator" ../. {};
in {
  inherit ghc;

  validator = final-inferred;

  shell     = ghc.shellFor {
    packages    = p: [final-inferred];
    withHoogle  = true;
    buildInputs = extras;
  };
}
