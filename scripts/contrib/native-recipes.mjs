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
  "midgard-watcher": {
    recipe: "native:build",
    output: "dist/native/midgard-chain-sync",
    tools: [["go", "version"]],
  },
};
