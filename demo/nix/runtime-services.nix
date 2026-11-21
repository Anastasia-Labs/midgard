# The Cardano services a Midgard deployment runs beside its own processes,
# taken from each project's release binaries: the same releases the Docker
# route pulls as images (`toolchain.json` names the versions). The pins are the
# sha256 each GitHub release publishes for its assets. The Linux binaries are
# statically linked, so they run unpatched.
{ pkgs, system, versions }:
let
  inherit (pkgs) lib;
  release = { pname, version, url, sha256, unpack, description }:
    pkgs.stdenvNoCC.mkDerivation {
      inherit pname version;
      src = pkgs.fetchurl { inherit url sha256; };
      nativeBuildInputs = lib.optional (lib.hasSuffix ".zip" url) pkgs.unzip;
      dontConfigure = true;
      dontBuild = true;
      dontStrip = true;
      sourceRoot = ".";
      unpackPhase = if lib.hasSuffix ".zip" url then "unzip -q $src" else "tar xzf $src";
      installPhase = ''
        runHook preInstall
        mkdir -p $out
        ${unpack}
        runHook postInstall
      '';
      meta = { inherit description; platforms = [ system ]; };
    };

  cardanoNodeAssets = {
    x86_64-linux = { asset = "linux-amd64"; sha256 = "530fb99986fbdccc46f9ee0dd86cabb1393b015feac9173cb731d89fa748365f"; };
    aarch64-linux = { asset = "linux-arm64"; sha256 = "3b638c87448bf08d3680f9116ca92e876579b865c0540f8f33b5c586a02ebf8a"; };
    x86_64-darwin = { asset = "macos-amd64"; sha256 = "f8cc4181a16ffab896e3b981819fbd8865e9b0838e4eef09a48542090332403b"; };
    aarch64-darwin = { asset = "macos-arm64"; sha256 = "8c3162031d63838709a5d59117130ddf8059b3456a367ea374b054cf2161a960"; };
  };
  # Ogmios, Kupo and Mithril publish no Intel macOS build.
  ogmiosAssets = {
    x86_64-linux = { asset = "x86_64-linux"; sha256 = "a4d07112dbbacf9a302970b6f6f7d35aa06c9a9edf013dff5547f6a7afee295e"; };
    aarch64-linux = { asset = "aarch64-linux"; sha256 = "bbb553d5a7acecbf309dd3da301a03f7b8d0c9e4c62338551b5f1308171c3787"; };
    aarch64-darwin = { asset = "aarch64-macos"; sha256 = "b3e5bfc1bd04d923a91f05371378ce98c918ee6c3b0f0e74bdbcf81f8be2f510"; };
  };
  kupoAssets = {
    x86_64-linux = { asset = "x86_64-linux"; sha256 = "1646f8918ea5a0d5cecb9abd28c2e6934ed497527f19d2f33c1470093ab65df5"; };
    aarch64-linux = { asset = "aarch64-linux"; sha256 = "f24bc3552333da7e7e0b976e8664324099d80e6fc018c68b90f9698c852a51e0"; };
    aarch64-darwin = { asset = "aarch64-macos"; sha256 = "f0bb7369be7af974d75db8a2eb9189c201d0ba94394f40c1c0db921a2c0e97c0"; };
  };
  mithrilAssets = {
    x86_64-linux = { asset = "linux-x64"; sha256 = "6eb745fdf124fad3d93ce6450c45d4fb6f6b8baed8aec10c83c11031bec70ec8"; };
    aarch64-linux = { asset = "linux-arm64"; sha256 = "35ba871a0ca548c73d6dc4e6fc91e7b0a4883fc4cb3ffea803ebec694b1bac2d"; };
    aarch64-darwin = { asset = "macos-arm64"; sha256 = "ec7253d62b3778503ea291a4fb56651077a30818eea8e55cafc4d82d111c4a51"; };
  };

  # Kupo tags its releases by minor version (`v2.11`).
  kupoTag = lib.versions.majorMinor versions.kupo;

  available = assets: build: lib.optionalAttrs (assets ? ${system}) (build assets.${system});
in
available cardanoNodeAssets (pin: {
  # cardano-node, cardano-cli and the published network configurations
  # (`share/<network>/config.json` and its genesis files).
  cardano-node = release {
    pname = "cardano-node";
    version = versions.cardano-node;
    url = "https://github.com/IntersectMBO/cardano-node/releases/download/${versions.cardano-node}/cardano-node-${versions.cardano-node}-${pin.asset}.tar.gz";
    inherit (pin) sha256;
    unpack = ''
      mkdir -p $out/bin
      cp bin/cardano-node bin/cardano-cli $out/bin/
      cp -r share $out/share
    '';
    description = "cardano-node and cardano-cli ${versions.cardano-node}";
  };
})
// available ogmiosAssets (pin: {
  ogmios = release {
    pname = "ogmios";
    version = versions.ogmios;
    url = "https://github.com/CardanoSolutions/ogmios/releases/download/v${versions.ogmios}/ogmios-v${versions.ogmios}-${pin.asset}.zip";
    inherit (pin) sha256;
    unpack = ''
      install -Dm755 bin/ogmios $out/bin/ogmios
    '';
    description = "Ogmios ${versions.ogmios}";
  };
})
// available kupoAssets (pin: {
  kupo = release {
    pname = "kupo";
    version = versions.kupo;
    url = "https://github.com/CardanoSolutions/kupo/releases/download/v${kupoTag}/kupo-v${versions.kupo}-${pin.asset}.zip";
    inherit (pin) sha256;
    unpack = ''
      install -Dm755 bin/kupo $out/bin/kupo
    '';
    description = "Kupo ${versions.kupo}";
  };
})
// available mithrilAssets (pin: {
  mithril-client = release {
    pname = "mithril-client";
    version = versions.mithril-client;
    url = "https://github.com/input-output-hk/mithril/releases/download/${versions.mithril-client}/mithril-${versions.mithril-client}-${pin.asset}.tar.gz";
    inherit (pin) sha256;
    unpack = ''
      install -Dm755 mithril-client $out/bin/mithril-client
    '';
    description = "mithril-client ${versions.mithril-client}";
  };
})
