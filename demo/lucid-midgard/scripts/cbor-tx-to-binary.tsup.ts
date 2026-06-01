import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["scripts/cbor-tx-to-binary.ts"],
  format: ["esm"],
  outDir: ".tmp/cbor-bin",
  clean: true,
  dts: false,
  sourcemap: false,
  noExternal: [/^cborg/, /^@noble\/hashes/],
});
