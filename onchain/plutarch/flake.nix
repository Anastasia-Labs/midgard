{
  description = "Midgard";


  inputs = {
    # nixpkgs 24.11 still carries the ghc94x boot compilers haskell.nix needs to
    # bootstrap ghc966 (mirrors wsc-poc / sc-tools main's wiring).
    nixpkgs.follows = "haskell-nix/nixpkgs-2411";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      # Pinned to sc-tools main's haskell.nix (required for typed-protocols >= 1.2
      # public sublibraries; older haskell.nix fails with "Dependency on
      # unbuildable package cborg" when configuring typed-protocols:stateful-cborg).
      url = "github:input-output-hk/haskell.nix/4c085ca207389ae2f2bfdc811afeebfcb326a399";
      inputs.hackage.follows = "hackage";
    };

    systems.url = "github:nix-systems/default";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    import ./nix/outputs.nix { inherit inputs system; }
  );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.ml42.de"
      "https://sc-tools.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.ml42.de:RKmSRP9TOc87nh9FZCM/b/pMIE3kBLEeIe71ReCBwRM="
      "sc-tools.cachix.org-1:DY2+6v0HuMvoCt7wEqZTPqzZBcNk/Lexb72Vixz6n6I="
    ];
    allow-import-from-derivation = true;
  };

}
