import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["scripts/binary-tx-to-cbor.ts"],
  format: ["esm"],
  outDir: ".tmp/cbor-bin",
  clean: true,
  dts: false,
  sourcemap: false,
  noExternal: [/^cborg/, /^@noble\/hashes/],
});
