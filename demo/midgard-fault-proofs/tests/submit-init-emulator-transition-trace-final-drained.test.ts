import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

// A transaction that spends the ledger's only entry (the case's one output
// shape) and produces nothing. The honest block commits the drained ledger as
// the committed empty-ledger root and defends both routes; a block that commits
// the MPF null root is removed by both the L2 replay route and an accepted
// validation claim.
registerTransitionTraceFinalCases(
  [
    { assetCount: 0, outputCount: 1, drained: true },
    { assetCount: 0, outputCount: 1, drained: true, honest: true },
    { assetCount: 0, outputCount: 1, kind: "claim", drained: true },
    {
      assetCount: 0,
      outputCount: 1,
      kind: "claim",
      drained: true,
      honest: true,
    },
  ],
  "drained",
);
