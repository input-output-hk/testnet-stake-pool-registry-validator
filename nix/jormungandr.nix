############################################################################
# Packages for jormungandr and jcli.
#
# These use the functions from iohk-nix to build the version that we
# require for cardano-wallet.
#
# To change the version:
# 1. Adjust the "version" and/or "rev" variables below.
# 2. Then invert the first digit in *both* sha256 and
#    cargoSha256. That is, change 0 to 1 and 1 to 0. It's important
#    that you change them to something different than before,
#    otherwise you may get the previous version from your local cache.
# 3. Run "nix-build -A jormungandr.src" (or let CI do it for you).
#    It will say that the hash is wrong. Update sha256 to the value it got.
# 4. Run "nix-build -A jormungandr". After some time downloading
#    crates, it should say that the vendor hash is wrong.
#    Update cargoSha256 with the value it got.
# 5. Run "nix-build -A jormungandr". It should complete the build.
# 6. Test that "nix-build -A jormungandr-cli" also works.
# 7. If you now run "nix-shell" you should have updated versions of
#    jormungandr and jcli.
#
############################################################################

{ iohkLib ? import ./iohk-common.nix {}
, pkgs ? iohkLib.pkgs
}:

let
  release = rec {
    version = "0.6.0-pre";
    # Git revision of input-output-hk/jormungandr repo.
    rev = "a4692b762f4b8d92a335f70f5acdfc7afcad3fe8";
    # Hash of git repo and all of its submodules.
    sha256 = "03ijiqhsv17px2lnpzq3bvpq3dy8pmlqdrda100l3mkk1qb7ayjc";
    # Hash of all Cargo dependencies.
    cargoSha256 = "0ms8inl8cp5fla6xvrdgwjkya192mq0181n9yxfm0sk1ncnj8wgc";
  };

in {
  jormungandr = iohkLib.rust-packages.pkgs.makeJormungandr release;
  jormungandr-cli = iohkLib.rust-packages.pkgs.makeJcli release;
}
