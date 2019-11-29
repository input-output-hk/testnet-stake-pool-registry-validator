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
    version = "0.8.0-rc1+";
    # Git revision of input-output-hk/jormungandr repo.
    rev = "9a13696e07517498601c8f49767eb1bac9576cdb";
    # Hash of git repo and all of its submodules.
    sha256 = "1s2sc3fdijgkcxphlb657l9qw7ngqc6vdk6j5qxvh8w7010gfm5n";
    # Hash of all Cargo dependencies.
    cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
  };

in {
  jormungandr = iohkLib.rust-packages.pkgs.makeJormungandr release;
  jormungandr-cli = iohkLib.rust-packages.pkgs.makeJcli release;
}
