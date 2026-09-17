import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// Keep these cases in separate files so Vitest gives each group a fresh process.
registerTransitionTraceFinalCases([
  { assetCount: 0, outputCount: 1 },
  { assetCount: 0, outputCount: 1, kind: "deposit" },
  { assetCount: 0, outputCount: 1, kind: "deposit", honest: true },
  { assetCount: 0, outputCount: 1, kind: "claim" },
  { assetCount: 0, outputCount: 1, kind: "claim", honest: true },
  { assetCount: 0, outputCount: 1, datumBytes: 12000, outputBytes: 16384 },
  { assetCount: 0, outputCount: 1, datumBytes: 15841 },
  { assetCount: 0, outputCount: 1, datumBytes: 65, corruptDatum: true },
  { assetCount: 3, outputCount: 1, kind: "deposit", datumBytes: 256 },
  { assetCount: 3, outputCount: 1, kind: "deposit", corruptAssetIndex: true },
  {
    assetCount: 3,
    outputCount: 1,
    kind: "deposit",
    corruptSourceReference: true,
  },
  { assetCount: 0, outputCount: 1, kind: "deposit", datumBytes: 12000 },
  { assetCount: 0, outputCount: 1, cancelAt: 2 },
  { assetCount: 0, outputCount: 1, kind: "deposit", cancelAt: 2 },
  { assetCount: 0, outputCount: 16 },
  { assetCount: 0, outputCount: 1, outputBytes: 16384 },
  { assetCount: 0, outputCount: 2, outputBytes: 16384, fieldBytes: 32768 },
  { assetCount: 0, outputCount: 1, honest: true },
  {
    assetCount: 0,
    outputCount: 2,
    outputBytes: 16384,
    fieldBytes: 32768,
    depth: 64,
  },
]);
