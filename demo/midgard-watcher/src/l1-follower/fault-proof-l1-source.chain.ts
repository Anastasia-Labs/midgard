import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1RefusedError,
  FraudProofL1UnavailableError,
  type FraudProofRawL1Point,
} from "@al-ft/midgard-fault-proofs";
import type { FactStore, StoredBlock, View } from "@al-ft/midgard-l1-follower";

import {
  rawPointOf,
  type RawRead,
  type RawReadRefusal,
} from "./raw-reads.types.js";

/**
 * Chain positions and refusals shared by the watcher's fault-proof L1
 * source (`fault-proof-l1-source.ts`) and its signed-intent recovery.
 */

/**
 * A raw read the follower refused for a reason that is neither a moved chain
 * nor a missing cursor. The workflow hands it to the watcher, which holds the
 * objective by name instead of failing the process.
 */
export class WatcherFaultProofL1RefusedError extends FraudProofL1RefusedError {
  constructor(
    override readonly reason: RawReadRefusal,
    detail: string,
  ) {
    super(reason, detail);
    this.message = `the follower refused the read (${reason}): ${detail}`;
    this.name = "WatcherFaultProofL1RefusedError";
  }
}

/**
 * The named error for a refusal: a point that left the stored chain is a
 * moved checkpoint (retry), a missing cursor is unavailability (wait), and
 * every other refusal stays visible by its reason.
 */
export const refusalError = (reason: RawReadRefusal, detail: string): Error =>
  reason === "point_not_canonical" || reason === "not_at_point"
    ? new FraudProofL1CheckpointChangedError(`${reason}: ${detail}`)
    : reason === "not_initialized"
      ? new FraudProofL1UnavailableError(
          `the follower has no cursor: ${detail}`,
        )
      : new WatcherFaultProofL1RefusedError(reason, detail);

/** The read's value, or its refusal thrown as the named error. */
export const required = <T>(read: RawRead<T>): T => {
  if (read.kind === "ok") return read.value;
  throw refusalError(read.reason, read.detail);
};

export const chainMoved = (
  detail: string,
): FraudProofL1CheckpointChangedError =>
  new FraudProofL1CheckpointChangedError(detail);

/** The follower's view: the tip every depth is counted from. */
export const currentView = async (store: FactStore): Promise<View> => {
  const view = await store.currentView();
  if (view === null)
    throw new FraudProofL1UnavailableError("the follower has no cursor yet");
  return view;
};

export const tipPointOf = (view: View): FraudProofRawL1Point =>
  rawPointOf({
    slot: view.point.slot,
    hash: view.point.hash,
    height: view.height,
  });

const notDeepEnough = (minimum: number): FraudProofL1UnavailableError =>
  new FraudProofL1UnavailableError(
    `the follower's chain is not ${minimum.toString()} blocks deep yet`,
  );

/** Whether `height` lies below the follower's origin block. */
const belowOrigin = async (
  store: FactStore,
  height: number,
): Promise<boolean> => {
  const cursor = await store.cursor();
  if (cursor === null)
    throw new FraudProofL1UnavailableError("the follower has no cursor yet");
  const origin = await store.blockByHash(cursor.origin.hash);
  return origin === null || height < origin.height;
};

/**
 * The stored block exactly `minimum` deep under the view (depth 1 is the
 * tip). Unavailable while the chain above the origin is shorter; a block
 * the pruning removed is `point_beyond_retention`.
 */
export const blockAtDepth = async (
  store: FactStore,
  view: View,
  minimum: number,
): Promise<StoredBlock> => {
  const height = view.height - minimum + 1;
  const block = await store.blockAtHeight(height);
  if (block !== null) return block;
  if (await belowOrigin(store, height)) throw notDeepEnough(minimum);
  throw new WatcherFaultProofL1RefusedError(
    "point_beyond_retention",
    `the block ${minimum.toString()} deep (height ${height.toString()}) is below the pruned window`,
  );
};

/** Retained blocks a walk below the pruned window may pass before giving up. */
const DEEP_BLOCK_WALK_LIMIT = 64;

/**
 * The highest stored block at least `minimum` deep under the view. The
 * exact block when stored; when the pruning removed it, the first block
 * the pruning kept below it (a checkpoint, the origin, or a block a
 * retained fact references): deeper, so no less final.
 */
export const blockAtLeastDepth = async (
  store: FactStore,
  view: View,
  minimum: number,
): Promise<StoredBlock> => {
  const height = view.height - minimum + 1;
  const exact = await store.blockAtHeight(height);
  if (exact !== null) return exact;
  if (await belowOrigin(store, height)) throw notDeepEnough(minimum);
  const cursor = await store.cursor();
  let slot = cursor?.prunedThroughSlot ?? view.point.slot;
  for (let step = 0; step < DEEP_BLOCK_WALK_LIMIT; step += 1) {
    const block = await store.blockAtOrBeforeSlot(slot);
    if (block === null) break;
    if (block.height <= height) return block;
    slot = block.slot - 1;
  }
  throw new FraudProofL1UnavailableError(
    `no stored block is ${minimum.toString()} deep within ${DEEP_BLOCK_WALK_LIMIT.toString()} retained blocks`,
  );
};

/** Checkpoint changes a capture absorbs before it gives up (the old source's bound). */
const CHECKPOINT_RETRIES = 2;

/** Runs `attempt` afresh while the chain moves under it, at most twice more. */
export const withCheckpointRetries = async <T>(
  attempt: () => Promise<T>,
): Promise<T> => {
  for (let retries = 0; ; retries += 1) {
    try {
      return await attempt();
    } catch (error) {
      if (
        !(error instanceof FraudProofL1CheckpointChangedError) ||
        retries >= CHECKPOINT_RETRIES
      )
        throw error;
    }
  }
};
