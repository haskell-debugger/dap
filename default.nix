{ pkgs ? import <nixpkgs> {} }:
let
  dap = pkgs.haskell.packages.ghc927.callCabal2nix "dap" ./. {};
in
{
  inherit dap pkgs;
}
