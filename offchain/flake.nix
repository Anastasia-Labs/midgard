{
  description = "midgard-offchain";

  inputs = {

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    cardano-node = {
      url = "github:input-output-hk/cardano-node?ref=10.6.1";
    };

    cardano-cli = {
      url = "github:intersectmbo/cardano-cli?ref=cardano-cli-10.14.0.0";
    };

    systems.url = "github:nix-systems/default";
  };


  # NOTE: nix flake show --override-input systems github:nix-systems/x86_64-linux
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    import ./nix/outputs.nix { inherit inputs system; }
  );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://sc-tools.cachix.org"
      "https://cache.ml42.de"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "sc-tools.cachix.org-1:DY2+6v0HuMvoCt7wEqZTPqzZBcNk/Lexb72Vixz6n6I="
      "cache.ml42.de:RKmSRP9TOc87nh9FZCM/b/pMIE3kBLEeIe71ReCBwRM="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
