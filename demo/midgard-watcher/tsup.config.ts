import { defineConfig } from "tsup";

export default defineConfig({
  // The independently guarded Go owner lives here. Rebuilding JavaScript
  // must clean its own outputs while preserving the native binary and stamp.
  clean: ["!native/**"],
});
