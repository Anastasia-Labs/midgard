{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
    "https://github.com/j-mueller/sc-tools"."e1f4359b6fb6a2b939bac0840e229193dd4f6b54" = "sha256-+dVtHGOQHvAjwOZZ0ZyHsQLF2roREUVIoMU31AD3YZs=";
    "https://github.com/colll78/plutarch-plutus"."548cb641378b4c00bc02abfdcd723821b08141f5" = "sha256-JdQ0wlktqMqH5kcIu3gUsR65Ulg7YmttL/1CM9UjHhs=";
  };

  modules = [{ }];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "midgard-contracts";
    compiler-nix-name = "ghc966";
    index-state = "2024-09-17T19:03:21Z";
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.CHaP;
    };
    shell.withHoogle = false;
  };

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in
project
