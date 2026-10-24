// Each binary has one owner, even when it shares a directory with JavaScript.
export const NATIVE_RECIPES = {
  "midgard-node": {
    recipe: "native:mpf-owner:build",
    output: "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
    tools: [
      ["cargo", "--version"],
      ["rustc", "--version"],
    ],
  },
  "@al-ft/l1-node-transport": {
    recipe: "native:build",
    output: "dist/native/midgard-l1-node-transport",
    toolsDirectory: "native",
    tools: [["go", "version"]],
  },
};
