import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// A transaction that spends the ledger's only entry (the case's one output
// shape) and produces nothing. The honest block commits the drained ledger as
// the committed empty-ledger root and defends the L2 replay route; a block that
// commits the MPF null root is removed by it.
registerTransitionTraceFinalCases(
  [
    { assetCount: 0, outputCount: 1, drained: true },
    { assetCount: 0, outputCount: 1, drained: true, honest: true },
  ],
  "drained",
);
