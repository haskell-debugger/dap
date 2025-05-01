{ pkgs ? import <nixpkgs> {} }:
let
  dap = pkgs.haskell.packages.ghc966.callCabal2nix "dap" ./. {};
in
{
  inherit dap pkgs;
}
