import type { FactStore, StoredBlock } from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import type {
  WatcherFundingInputFacts,
  WatcherFundingInputStanding,
} from "../funding/prover-funding-input-facts.js";
import {
  blockAtLeastDepth,
  chainMoved,
  currentView,
  withCheckpointRetries,
} from "./fault-proof-l1-source.chain.js";
import { isTrackedOutput } from "./raw-reads.js";
import { createdOutputOf, parseOutRefLabel } from "./reads.js";

type Standing = "spent" | "unspent" | Readonly<{ undetermined: string }>;

/**
 * One input's standing from its stored row. A wallet input's row is the
 * seed row or the row of the tracked tx that created it; the row keeps its
 * spend until the pruning removes it.
 */
const inputStanding = async (
  store: FactStore,
  final: StoredBlock,
  label: string,
): Promise<Standing> => {
  const outRef = parseOutRefLabel(label);
  const row = await store.output(outRef);
  if (row === null) {
    // A pruned row was spent at or below the pruned slot: known only while
    // its tracked creating tx is still stored.
    const cursor = await store.cursor();
    const creating = await store.txByHash(outRef.txHash);
    if (creating !== null && cursor !== null) {
      const body = CML.TransactionBody.from_cbor_bytes(creating.bodyCbor);
      try {
        const output = createdOutputOf(body, creating.isValid, outRef.index);
        if (
          output !== undefined &&
          isTrackedOutput(store.trackedSet(), output) &&
          cursor.prunedThroughSlot > cursor.origin.slot &&
          cursor.prunedThroughSlot <= final.slot
        )
          return "spent";
      } finally {
        body.free();
      }
    }
    return {
      undetermined: `${label} has no tracked row (the wallet is not seeded yet, it is not a tracked output, or it was spent and pruned with its creating tx)`,
    };
  }
  if (row.spent !== null)
    return row.spent.slot <= final.slot
      ? "spent"
      : { undetermined: `${label} is spent above the release-final point` };
  const since = row.created?.slot ?? row.seedSlot;
  if (since === null || since > final.slot)
    return {
      undetermined: `${label} was created or seeded above the release-final point`,
    };
  return "unspent";
};

const standingOnce = async (
  store: FactStore,
  recoveryDepth: number,
  outRefs: readonly string[],
): Promise<WatcherFundingInputStanding> => {
  const view = await currentView(store);
  const final = await blockAtLeastDepth(store, view, recoveryDepth);
  const spent: string[] = [];
  const unspent: string[] = [];
  const undetermined: string[] = [];
  for (const label of outRefs) {
    const standing = await inputStanding(store, final, label);
    if (standing === "spent") spent.push(label);
    else if (standing === "unspent") unspent.push(label);
    else undetermined.push(standing.undetermined);
  }
  if (!(await store.viewValid(view)))
    throw chainMoved("the follower rolled back during the funding read");
  return Object.freeze({
    spent: Object.freeze(spent),
    unspent: Object.freeze(unspent),
    undetermined: undetermined.length === 0 ? null : undetermined.join("; "),
  });
};

/**
 * The follower's answer for reservation inputs, from the stored rows of
 * its tracked wallets. The release-final point is the highest stored block
 * at least `recoveryDepth` deep (the depth signed recovery uses): an input
 * is spent when its spend is at or below it, and unspent when its row
 * existed there and is unspent at the view's tip. Anything else, and any
 * read the follower cannot answer, is undetermined; this never throws.
 */
export const createWatcherFundingInputFacts = (input: {
  readonly store: FactStore;
  /** The depth of the release-final boundary (`automaticRecoveryMaxDepth + 2`). */
  readonly recoveryDepth: number;
}): WatcherFundingInputFacts =>
  Object.freeze({
    standing: async (outRefs) => {
      try {
        return await withCheckpointRetries(() =>
          standingOnce(input.store, input.recoveryDepth, [...outRefs]),
        );
      } catch (error) {
        // Unavailable, moved or refused reads, and any store failure, keep
        // the reservation held; the next tick reads again.
        return Object.freeze({
          spent: Object.freeze([]),
          unspent: Object.freeze([]),
          undetermined: error instanceof Error ? error.message : String(error),
        });
      }
    },
  });
