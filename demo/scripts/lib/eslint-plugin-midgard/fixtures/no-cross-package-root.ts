// fixture-path: midgard-watcher/tests/support/narrowed-imports.ts
// ruleid: midgard/no-cross-package-root
import { prepareMinFeeFromTransactions } from "@al-ft/midgard-fault-proofs";
// ruleid: midgard/no-cross-package-root
import "@al-ft/midgard-validation";
// ruleid: midgard/no-cross-package-root
export { resolveProverSigner } from "@al-ft/midgard-fault-proofs";
// ruleid: midgard/no-cross-package-root
import type { ValidationCandidate } from "@al-ft/midgard-validation";
// ok: midgard/no-cross-package-root
import { prepareMinFeeWorkflow } from "@al-ft/midgard-fault-proofs/prepare-min-fee";
// ok: midgard/no-cross-package-root
import { replayBlock } from "@al-ft/midgard-fault-proofs/block-replay";
// ok: midgard/no-cross-package-root
import { runPhaseAValidation } from "@al-ft/midgard-validation/phase-a";
// ok: midgard/no-cross-package-root
import * as SDK from "@al-ft/midgard-sdk";

export const loaded = async (candidate: ValidationCandidate) => {
  // ruleid: midgard/no-cross-package-root
  const root = await import("@al-ft/midgard-validation");
  return [
    candidate,
    root,
    prepareMinFeeFromTransactions,
    prepareMinFeeWorkflow,
    replayBlock,
    runPhaseAValidation,
    SDK,
  ];
};
