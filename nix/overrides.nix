pkgs: new: old:
with new; with pkgs.haskell.lib;
let
  local
    = repo: path: cabalExtras:
      doJailbreak
      (old.callCabal2nixWithOptions repo path cabalExtras {});
  cabal2nix
    = owner: repo: rev: sha256: cabalExtras:
      doJailbreak (old.callCabal2nixWithOptions repo (pkgs.fetchFromGitHub {
        inherit owner repo rev sha256;
      }) cabalExtras {});
  iohk
    = repo: rev: s: subdir: cabal2nix "input-output-hk" repo rev s "--subpath ${subdir}";
in {
  bech32 = iohk "bech32" "6e6b5f2ff8a61265ff0fa12b5c01614f9c747d92" "0h40w417jgc3pn3a5f8bd8bd92rgy43g8ab6pdspkfv11il155ig" ".";
}
