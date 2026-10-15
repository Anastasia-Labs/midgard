{ inputs, pkgs, lib, project, utils, ghc, system }:

let

  allTools = {
    "ghc966".cabal = project.projectVariants.ghc966.tool "cabal" "latest";
    "ghc966".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest";
    "ghc966".haskell-language-server = project.projectVariants.ghc966.tool "haskell-language-server" "latest";
    "ghc966".stylish-haskell = project.projectVariants.ghc966.tool "stylish-haskell" "latest";
    "ghc966".fourmolu = project.projectVariants.ghc966.tool "fourmolu" "latest";
    "ghc966".hlint = project.projectVariants.ghc966.tool "hlint" "latest";

    # Enable alongside the matching `flake.variants` entry in ./project.nix.
    #
    # "ghc9102".cabal                   = project.projectVariants.ghc9102.tool "cabal" "latest";
    # "ghc9102".cabal-fmt               = project.projectVariants.ghc966.tool  "cabal-fmt" "latest"; # cabal-fmt not buildable with ghc9102
    # "ghc9102".haskell-language-server = project.projectVariants.ghc9102.tool "haskell-language-server" "latest";
    # "ghc9102".stylish-haskell         = project.projectVariants.ghc9102.tool "stylish-haskell" "latest";
    # "ghc9102".fourmolu                = project.projectVariants.ghc9102.tool "fourmolu" "latest";
    # "ghc9102".hlint                   = project.projectVariants.ghc9102.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSource ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = true;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
      # `./format` is the formatting entry point for this package and passes a
      # bespoke fourmolu configuration. Enabling the hook without first running
      # `./format` over the tree rewrites every source file.
      fourmolu = {
        enable = false;
        package = tools.fourmolu;
        args = [ ];
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
    };
  };

  linuxPkgs = lib.optionals pkgs.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.hostPlatform.isDarwin [
  ];

  commonPkgs = [
    tools.haskell-language-server
    tools.stylish-haskell
    tools.fourmolu
    tools.cabal
    tools.hlint
    tools.cabal-fmt

    pkgs.ghcid
    pkgs.shellcheck
    pkgs.nixpkgs-fmt
    pkgs.github-cli
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.which
  ];

  shell = project.shellFor {
    name = "midgard-merkle-${project.args.compiler-nix-name}";

    buildInputs = lib.concatLists [
      commonPkgs
      darwinPkgs
      linuxPkgs
    ];

    withHoogle = true;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    '';
  };

in

shell
