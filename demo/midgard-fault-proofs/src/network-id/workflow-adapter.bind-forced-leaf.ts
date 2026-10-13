import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  ForcedInclusionTxV1Schema,
  OutputReferenceSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type ForcedSourcePayload,
  proofSteps,
} from "./workflow-adapter.prepared-from-artifact.js";

/**
 * Replays the counted-root membership the forced door re-derives on chain, so a
 * journal artifact whose proof no longer opens its own leaf is refused here
 * rather than at submission.
 */
export const bindForcedLeafMembership = (source: ForcedSourcePayload): void => {
  const membership = source.membership;
  let replayed: Buffer | null;
  try {
    replayed = MpfProof.fromJSON(
      Buffer.from(
        Data.to(membership.key as never, OutputReferenceSchema as never),
        "hex",
      ),
      Buffer.from(
        Data.to(membership.value as never, ForcedInclusionTxV1Schema as never),
        "hex",
      ),
      proofSteps(membership.proof),
    ).verify(true);
  } catch {
    throw new Error(
      "network-id forced artifact membership proof cannot be replayed",
    );
  }
  if (replayed?.toString("hex") !== membership.phas_root) {
    throw new Error(
      "network-id forced artifact membership proof opens another root",
    );
  }
};
