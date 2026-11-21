// fixture-path: midgard-fault-proofs/tests/support/narrowed-imports.ts
// ruleid: midgard/no-own-package-barrel
import { buildMinFeeStep01 } from "../../src/index.js";
// ruleid: midgard/no-own-package-barrel
import "../../src/index.js";
// ruleid: midgard/no-own-package-barrel
export { resolveProverSigner } from "../../src";
// ruleid: midgard/no-own-package-barrel
import * as FaultProofs from "@al-ft/midgard-fault-proofs";
// ok: midgard/no-own-package-barrel
import { buildMinFeeStep02 } from "../../src/min-fee-contracts.js";
// ok: midgard/no-own-package-barrel
import { replayBlock } from "@al-ft/midgard-fault-proofs/block-replay";
// ok: midgard/no-own-package-barrel
import { encodeMidgardTx } from "@al-ft/midgard-core";
// ok: midgard/no-own-package-barrel
import { familyIndex } from "../../src/workflow/index.js";

export const loaded = async () => {
  // ruleid: midgard/no-own-package-barrel
  const barrel = await import("../../src/index.js");
  return [
    barrel,
    buildMinFeeStep01,
    buildMinFeeStep02,
    FaultProofs,
    replayBlock,
    encodeMidgardTx,
    familyIndex,
  ];
};
