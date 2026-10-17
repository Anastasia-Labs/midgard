import { defineConfig } from "tsup";

export default defineConfig({
  // dist/native holds the sidecar binary, which has its own build receipt.
  clean: ["!native/**"],
});
