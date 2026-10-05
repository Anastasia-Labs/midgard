import { defineConfig } from "tsup";

export default defineConfig({
  // node:sqlite requires its builtin prefix on the declared Node 22 toolchain.
  removeNodeProtocol: false,
  // cborg exposes only ESM: bundle it so the declared CommonJS exports can load.
  noExternal: ["cborg"],
});
