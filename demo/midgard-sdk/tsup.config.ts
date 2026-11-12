import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["src/index.ts", "src/blueprint-abi/index.ts"],
  format: ["esm", "cjs"],
  dts: true,
  clean: false,
  sourcemap: true,
});
