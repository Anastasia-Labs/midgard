import { defineConfig } from "tsup";

export default defineConfig({
  // cborg exposes only ESM: bundle it so the declared CommonJS exports can load.
  noExternal: ["cborg"],
});
