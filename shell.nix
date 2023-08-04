with (import ./default.nix {});

dap.env.overrideAttrs (drv: {
  shellHook = ''
    export PATH=$PATH:${pkgs.cabal-install}/bin

    function ghcid () {
        ${pkgs.ghcid}/bin/ghcid --poll --allow-eval -c 'cabal repl library:dap'
     }
  '';
})
