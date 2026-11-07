import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "@al-ft/midgard-fault-proofs";
import type { OutRef, Point } from "@al-ft/midgard-l1-follower";

/** The shapes and refusals of the follower-backed raw reads (see raw-reads.ts). */

export type RawReadRefusal =
  /** The address is not in the tracked set, so the facts do not hold its outputs. */
  | "untracked_address"
  /** No projection records the history of this unit. */
  | "unit_not_projected"
  /** The point is not a block of the stored chain (or its height or id differ). */
  | "point_not_canonical"
  /** The point is below the pruned window. */
  | "point_beyond_retention"
  /** The store has no cursor yet. */
  | "not_initialized"
  /** A fact the read needs may have been pruned. */
  | "beyond_retention"
  /** The read needs the exact bytes of an output created before the origin. */
  | "l1_input_before_origin"
  /** An input's creating body is not stored and the ledger state could not answer. */
  | "l1_input_unresolved"
  /** The transaction is not stored, and nothing that could have held it was pruned. */
  | "not_stored"
  /** The transaction is stored at another point than the one expected. */
  | "not_at_point"
  /** The transaction failed phase 2: the raw reads admit valid transactions only. */
  | "phase2_invalid"
  /**
   * The stored facts contradict one another (two unit histories place one
   * transaction at different points). Raised by the snapshot capture, never
   * by a single read.
   */
  | "store_inconsistent";

export type RawRead<T> =
  | Readonly<{ kind: "ok"; value: T }>
  | Readonly<{ kind: "refused"; reason: RawReadRefusal; detail: string }>;

export const ok = <T>(value: T): RawRead<T> => ({ kind: "ok", value });
export const refused = <T>(
  reason: RawReadRefusal,
  detail: string,
): RawRead<T> => ({
  kind: "refused",
  reason,
  detail,
});

/** A stored block as the raw source's point. */
export const rawPointOf = (
  block: Readonly<{ slot: number; hash: Buffer; height: number }>,
): FraudProofRawL1Point => {
  const point = {
    slot: block.slot.toString(),
    blockHash: block.hash.toString("hex"),
    blockNo: block.height.toString(),
  };
  return { ...point, pointId: computeFraudProofRawL1PointId(point) };
};

export type FollowerVerifiedSpend = Readonly<{
  outRef: string;
  spendingTxHash: string;
  spendPoint: FraudProofRawL1Point;
}>;

export type FollowerOutRefsAtPoint = Readonly<{
  /** The requested outrefs unspent at the point. */
  outputs: readonly FraudProofRawL1Utxo[];
  /** The requested outrefs spent at or below the point. */
  spends: readonly FollowerVerifiedSpend[];
  /**
   * Requested outrefs no tracked output covers at the point, provably: not
   * created by then, untracked, nonexistent, or nothing was pruned yet.
   */
  unknown: readonly string[];
  /** Requested outrefs whose tracked row pruning may have removed. */
  beyondRetention: readonly string[];
}>;

export type FollowerUnitHistory = Readonly<{
  checkpoint: FraudProofRawL1Point;
  transactions: readonly Readonly<{
    txHash: string;
    inclusionPoint: FraudProofRawL1Point;
  }>[];
}>;

/** An input of a raw transaction the read could not resolve, and why. */
export type FollowerUnresolvedInput = Readonly<{
  outRef: string;
  reason: Extract<
    RawReadRefusal,
    "beyond_retention" | "l1_input_before_origin" | "l1_input_unresolved"
  >;
}>;

export type FollowerRawTransaction = Readonly<{
  transaction: FraudProofRawL1Transaction;
  /** Inputs left out of `resolvedInputs`, with the reason. */
  unresolvedInputs: readonly FollowerUnresolvedInput[];
  unresolvedReferenceInputs: readonly FollowerUnresolvedInput[];
}>;

/**
 * The exact outputs the node's ledger holds at `point` for `outRefs`, by
 * outref label; outrefs it does not hold are absent. Null when the node
 * cannot acquire the point (more than k blocks deep, or off its chain).
 */
export type LedgerOutputsAt = (
  point: Point,
  outRefs: readonly OutRef[],
) => Promise<ReadonlyMap<string, FraudProofRawL1Utxo> | null>;

export type FollowerRawReads = Readonly<{
  addressUtxosAtPoint: (
    address: string,
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<readonly FraudProofRawL1Utxo[]>>;
  utxosByOutRefAtPoint: (
    outRefs: readonly string[],
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<FollowerOutRefsAtPoint>>;
  unitHistoryAtPoint: (
    unit: string,
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<FollowerUnitHistory>>;
  /**
   * The point a stored tx is included at, or null when it is not included.
   * Null is claimed only when nothing that could have held the tx was
   * pruned: `landedNoEarlierThanSlot` (the earliest slot it could land at,
   * such as its validity start) above the pruned window, or no pruning yet.
   */
  transactionInclusion: (
    txHash: string,
    landedNoEarlierThanSlot?: number,
  ) => Promise<RawRead<FraudProofRawL1Point | null>>;
  /** The stored tx, with every input resolved that the order above can resolve. */
  rawTransaction: (
    txHash: string,
    expectedInclusionPoint: FraudProofRawL1Point,
  ) => Promise<RawRead<FollowerRawTransaction>>;
  predecessorPoint: (
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<FraudProofRawL1Point>>;
}>;

/** A raw transaction with an unresolved input becomes that input's refusal. */
export const requireResolvedInputs = (
  read: RawRead<FollowerRawTransaction>,
): RawRead<FraudProofRawL1Transaction> => {
  if (read.kind !== "ok") return read;
  const unresolved = [
    ...read.value.unresolvedInputs,
    ...read.value.unresolvedReferenceInputs,
  ][0];
  return unresolved === undefined
    ? ok(read.value.transaction)
    : refused(
        unresolved.reason,
        `input ${unresolved.outRef} of ${read.value.transaction.txHash} is unresolved`,
      );
};
