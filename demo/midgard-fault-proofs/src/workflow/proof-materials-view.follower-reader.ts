/**
 * The proof-materials reader over the L1 follower: bounded reads of the
 * follower's facts with the node's mempool applied, each at the point of its
 * own snapshot (the ledger point and the mempool transactions applied), so
 * the view memoises per point.
 */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import type { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import type { Provider } from "@lucid-evolution/lucid";

import type { ProofMaterialsReader } from "./proof-materials-view.js";

/** What the reader needs of the follower's provider. */
export type ProofMaterialsFollower = Pick<
  L1FollowerProvider,
  "plainOutputsByDatum" | "plainOutputsExceptDatums" | "factsPoint"
>;

/** Whether `provider` answers the follower's bounded plain-output reads. */
export const isProofMaterialsFollower = (
  provider: Provider | ProofMaterialsFollower,
): provider is ProofMaterialsFollower => {
  const candidate = provider as Partial<ProofMaterialsFollower>;
  return (
    typeof candidate.plainOutputsByDatum === "function" &&
    typeof candidate.plainOutputsExceptDatums === "function" &&
    typeof candidate.factsPoint === "function"
  );
};

const outRefs = (labels: readonly string[]) =>
  labels.map((label) => {
    const { txHash, outputIndex } = parseOutRefLabel(label);
    return { txHash, outputIndex };
  });

/** The reader of `address` over `follower`. */
export const followerProofMaterialsReader = (
  follower: ProofMaterialsFollower,
  address: string,
): ProofMaterialsReader =>
  Object.freeze({
    byDatum: async (requests) =>
      await follower.plainOutputsByDatum({ address, requests }),
    exceptDatums: async (datums, excluded, limit) =>
      await follower.plainOutputsExceptDatums({
        address,
        datums,
        excluded: outRefs(excluded),
        limit,
      }),
    point: async () => await follower.factsPoint(),
  });
