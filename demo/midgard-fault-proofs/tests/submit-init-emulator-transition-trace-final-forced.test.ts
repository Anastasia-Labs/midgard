import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// Accepted forced transactions take the L2 replay route from their forced
// leaf. A wrong committed post-root is removed, the honest root is defended at
// the terminal step, and a leaf whose verdict is not ForcedTxValid is refused
// when the thread opens it. The last case is the deepest source proof with the
// largest output field.
registerTransitionTraceFinalCases(
  [
    { assetCount: 0, outputCount: 1, forced: "valid" },
    { assetCount: 0, outputCount: 1, forced: "valid", honest: true },
    { assetCount: 0, outputCount: 1, forced: "invalid" },
    {
      assetCount: 0,
      outputCount: 2,
      outputBytes: 16384,
      fieldBytes: 32768,
      depth: 64,
      forced: "valid",
    },
  ],
  "forced",
);
