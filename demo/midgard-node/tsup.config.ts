import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["src/index.ts"],
  format: ["esm", "cjs"],
  dts: false,
  clean: false,
  sourcemap: true,
  esbuildOptions(options) {
    options.loader = {
      ...(options.loader ?? {}),
      ".sql": "text",
    };
  },
});
