{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "simula-openvr";
  inherit ghc;
  buildInputs = with pkgs; [ 
                            (callPackage ./openvr.nix { })
                          ];
  LANG= "en_US.UTF-8";
  TMPDIR = "/tmp";
}
