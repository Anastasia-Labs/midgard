import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["src/index.ts"],
  format: ["esm", "cjs"],
  dts: true,
  clean: false,
  sourcemap: true,
  esbuildOptions(options) {
    options.loader = {
      ...(options.loader ?? {}),
      ".sql": "text",
    };
    options.alias = {
      ...(options.alias ?? {}),
      "@al-ft/midgard-sdk": "./src/vendor/midgard-sdk.ts",
    };
  },
});
