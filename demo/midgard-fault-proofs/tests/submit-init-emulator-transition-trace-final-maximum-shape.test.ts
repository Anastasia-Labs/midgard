import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// Accepted L2 transactions at the outputs, inputs and ledger-depth extremes the
// direct replay must carry.
registerTransitionTraceFinalCases(
  [
    { assetCount: 0, outputCount: 18 },
    { assetCount: 0, outputCount: 1, spentCount: 20 },
    { assetCount: 0, outputCount: 8, ledgerFill: 65536 },
    { assetCount: 0, outputCount: 1, spentCount: 12, ledgerFill: 65536 },
  ],
  "maximum-shape",
);
