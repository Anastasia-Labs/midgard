import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// Keep these cases in separate files so Vitest gives each group a fresh process.
registerTransitionTraceFinalCases(
  [
    {
      assetCount: 1295,
      outputCount: 1,
      kind: "deposit",
      datumBytes: 256,
      depth: 64,
    },
  ],
  "deep-deposit",
);
