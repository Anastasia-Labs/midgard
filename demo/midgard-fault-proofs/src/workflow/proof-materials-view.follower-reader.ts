/**
 * The proof-materials reader over the L1 follower: bounded reads of the
 * follower's facts, each at the point of its own snapshot, so the view
 * memoises per point and a sweep's standing comes from the follower too.
 */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import type { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import type { Provider } from "@lucid-evolution/lucid";

import type {
  ProofMaterialsReader,
  ProofMaterialsSweepState,
} from "./proof-materials-view.js";

/** What the reader needs of the follower's provider. */
export type ProofMaterialsFollower = Pick<
  L1FollowerProvider,
  | "plainOutputsByDatum"
  | "plainOutputsExceptDatums"
  | "factsPoint"
  | "getTransactionStatus"
>;

/** Whether `provider` answers the follower's bounded plain-output reads. */
export const isProofMaterialsFollower = (
  provider: Provider | ProofMaterialsFollower,
): provider is ProofMaterialsFollower => {
  const candidate = provider as Partial<ProofMaterialsFollower>;
  return (
    typeof candidate.plainOutputsByDatum === "function" &&
    typeof candidate.plainOutputsExceptDatums === "function" &&
    typeof candidate.factsPoint === "function" &&
    typeof candidate.getTransactionStatus === "function"
  );
};

const outRefs = (labels: readonly string[]) =>
  labels.map((label) => {
    const { txHash, outputIndex } = parseOutRefLabel(label);
    return { txHash, outputIndex };
  });

/**
 * The reader of `address` over `follower`. A sweep has landed once the
 * follower applied it (a phase-two failure spends no input, so it is over
 * too), is pending while the node's mempool holds it, and is absent
 * otherwise.
 */
export const followerProofMaterialsReader = (
  follower: ProofMaterialsFollower,
  address: string,
): ProofMaterialsReader =>
  Object.freeze({
    byDatum: async (requests, excluded) =>
      await follower.plainOutputsByDatum({
        address,
        requests,
        excluded: outRefs(excluded),
      }),
    exceptDatums: async (datums, excluded, limit) =>
      await follower.plainOutputsExceptDatums({
        address,
        datums,
        excluded: outRefs(excluded),
        limit,
      }),
    point: async () => await follower.factsPoint(),
    sweepState: async (txHash): Promise<ProofMaterialsSweepState> => {
      const { status } = await follower.getTransactionStatus(txHash);
      return status === "confirmed" || status === "failed"
        ? "landed"
        : status === "pending"
          ? "pending"
          : "absent";
    },
  });
