import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// Keep these cases in separate files so Vitest gives each group a fresh process.
registerTransitionTraceFinalCases(
  [
    {
      // Full 32-byte history NFT leaves room for 1,287 assets at exactly 5,000 bytes.
      assetCount: 1287,
      outputCount: 1,
      kind: "deposit",
      datumBytes: 256,
      depth: 64,
    },
  ],
  "deep-deposit",
);
