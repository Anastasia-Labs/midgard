import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// Keep these cases in separate files so Vitest gives each group a fresh process.
registerTransitionTraceFinalCases(
  [{ assetCount: 1304, outputCount: 1 }],
  "many-assets",
);
