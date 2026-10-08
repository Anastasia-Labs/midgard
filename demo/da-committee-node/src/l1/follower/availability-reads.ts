// The availability responder's, promise admission's and retirement's L1
// reads, all from the committee follower's facts (plan §8.1–§8.4): the
// boundary is the follower's view, canonicity is the store's point status,
// spends and transactions are stored facts. Nothing here reads a chain index.
import { createHash } from "node:crypto";

import {
  canonicalJson,
  type CanonicalJsonValue,
  canonicalJsonValue,
  compareCanonicalJsonKeys,
} from "@al-ft/midgard-core/canonical-json";
import { isPlainRecord, isUnknownArray } from "@al-ft/midgard-core/narrowing";
import type {
  FactStore,
  OutRef,
  StoredBlock,
  StoredTx,
  View,
} from "@al-ft/midgard-l1-follower";
import type * as SDK from "@al-ft/midgard-sdk";
import type { ProtocolParameters } from "@lucid-evolution/lucid";

import { AvailabilityResponderAwaitingScanError } from "../../availability/responder.js";
import type { CommitteeL1Readiness } from "./l1-follower.js";

/** A block on the follower's chain: its slot, hash and height. */
export type FollowerPoint = Readonly<{
  slot: number;
  blockHash: string;
  blockNo: number;
}>;

/**
 * The canonical boundary every availability, promise and retirement read is
 * bracketed by: the follower's view (§8.1). Two boundaries are the same view
 * when their `pointId`, height and `generation` agree.
 */
export type FollowerBoundary = FollowerPoint &
  Readonly<{ pointId: string; generation: number; view: View }>;

/** A point proof read at `tip`, the boundary it was read under. */
export type FollowerPointProof = Readonly<{
  point: FollowerPoint;
  tip: FollowerPoint;
}>;

export const sameBoundary = (
  a: FollowerBoundary,
  b: FollowerBoundary,
): boolean =>
  a.pointId === b.pointId &&
  a.blockNo === b.blockNo &&
  a.generation === b.generation;

const boundaryOf = (view: View): FollowerBoundary => {
  const blockHash = view.point.hash.toString("hex");
  return {
    pointId: `${view.point.slot.toString()}:${blockHash}`,
    slot: view.point.slot,
    blockHash,
    blockNo: view.height,
    generation: view.generation,
    view,
  };
};

const tipOf = (boundary: FollowerPoint): FollowerPoint => ({
  slot: boundary.slot,
  blockHash: boundary.blockHash,
  blockNo: boundary.blockNo,
});

const outRef = (txHash: string, index: number): OutRef => ({
  txHash: Buffer.from(txHash, "hex"),
  index,
});

/**
 * The transaction's CBOR as it was submitted: `[body, witness set, is_valid,
 * auxiliary data]`, from the exact bytes the follower stored per block. The
 * body bytes are the block's own, so the transaction id is unchanged.
 */
export const storedTransactionCbor = (tx: StoredTx): string =>
  Buffer.concat([
    Buffer.from([0x84]),
    tx.bodyCbor,
    tx.witnessCbor,
    Buffer.from([tx.isValid ? 0xf5 : 0xf4]),
    tx.auxCbor ?? Buffer.from([0xf6]),
  ]).toString("hex");

/** The node's protocol parameters as canonical JSON: bigints read as decimal strings. */
const protocolJsonValue = (value: unknown): CanonicalJsonValue => {
  const subject = "native protocol parameters";
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "number") {
    if (
      !Number.isFinite(value) ||
      (Number.isInteger(value) && !Number.isSafeInteger(value))
    )
      throw new TypeError(
        `${subject} numbers must be finite and integers must be safe`,
      );
    return value;
  }
  if (isUnknownArray(value)) return value.map(protocolJsonValue);
  if (isPlainRecord(value))
    return Object.fromEntries(
      Object.entries(value)
        .filter(([, child]) => child !== undefined)
        .sort(([left], [right]) => compareCanonicalJsonKeys(left, right))
        .map(([key, child]) => [key, protocolJsonValue(child)]),
    );
  return canonicalJsonValue(value, subject);
};

/** sha256 of the node's current protocol parameters, read by LSQ. */
export const protocolParametersDigest = (
  parameters: ProtocolParameters,
): string =>
  createHash("sha256")
    .update(
      canonicalJson(
        protocolJsonValue(parameters),
        "native protocol parameters",
      ),
    )
    .digest("hex");

export type CommitteeAvailabilityReadsInput = Readonly<{
  store: Pick<
    FactStore,
    | "currentView"
    | "viewValid"
    | "pointStatus"
    | "spenderOf"
    | "txSpending"
    | "txByHash"
    | "blockAtOrBeforeSlot"
  >;
  /** The follower's readiness reasons: any one holds every read. */
  readiness: () => readonly CommitteeL1Readiness[];
}>;

/** The availability, promise and retirement reads over the follower's facts. */
export type CommitteeAvailabilityReads = Readonly<{
  /**
   * The follower's view as a boundary. Throws
   * {@link AvailabilityResponderAwaitingScanError} while the follower holds
   * the committee unready, naming its reasons.
   */
  readBoundary: () => Promise<FollowerBoundary>;
  /** Whether work built at `view` is still current (§8.1). */
  viewValid: (view: View) => Promise<boolean>;
  /**
   * The block `point` on the follower's chain, read under `boundary`: null
   * when it is not on the chain, a throw when the store cannot answer
   * (beyond its retained window, or not initialized) or the view moved.
   */
  canonicalPoint: (
    point: Readonly<{ slot: number; blockHash: string }>,
    boundary: FollowerPoint,
  ) => Promise<FollowerPointProof | null>;
  /**
   * The block a valid transaction `txHash` landed in, read under
   * `boundary`: null when the follower holds no valid such transaction.
   */
  submissionPoint: (
    txHash: string,
    boundary: FollowerPoint,
  ) => Promise<FollowerPointProof | null>;
  /** The SDK's verified foreign-spend readers over stored spends and txs. */
  foreignSpend: Omit<SDK.DaAvailabilityForeignSpendReaders, "readBoundary">;
}>;

export const committeeAvailabilityReads = (
  input: CommitteeAvailabilityReadsInput,
): CommitteeAvailabilityReads => {
  const { store } = input;
  const readBoundary = async (): Promise<FollowerBoundary> => {
    const blocked = input.readiness();
    if (blocked.length > 0)
      throw new AvailabilityResponderAwaitingScanError(
        blocked.map(({ reason, detail }) => `${reason}: ${detail}`).join("; "),
      );
    const view = await store.currentView();
    if (view === null)
      throw new AvailabilityResponderAwaitingScanError(
        "the follower store is not initialized",
      );
    return boundaryOf(view);
  };
  /** Throws unless the follower's view is still exactly `boundary`. */
  const assertAt = async (boundary: FollowerPoint): Promise<void> => {
    const view = await store.currentView();
    if (
      view === null ||
      view.point.slot !== boundary.slot ||
      view.point.hash.toString("hex") !== boundary.blockHash ||
      view.height !== boundary.blockNo
    )
      throw new Error("The follower's view moved during a canonical read");
  };
  /** The exact stored block at `slot`, or null. */
  const blockAt = async (slot: number): Promise<StoredBlock | null> => {
    const block = await store.blockAtOrBeforeSlot(slot);
    return block === null || block.slot !== slot ? null : block;
  };
  const pointOf = async (
    point: Readonly<{ slot: number; blockHash: string }>,
  ): Promise<FollowerPoint | null> => {
    const status = await store.pointStatus({
      slot: point.slot,
      hash: Buffer.from(point.blockHash, "hex"),
    });
    if (status.kind === "canonical")
      return {
        slot: point.slot,
        blockHash: point.blockHash,
        blockNo: status.height,
      };
    if (status.kind === "point_not_canonical") return null;
    throw new Error(`${status.kind}: ${status.detail}`);
  };
  return {
    readBoundary,
    viewValid: (view) => store.viewValid(view),
    canonicalPoint: async (point, boundary) => {
      await assertAt(boundary);
      const found = await pointOf(point);
      await assertAt(boundary);
      return found === null ? null : { point: found, tip: tipOf(boundary) };
    },
    submissionPoint: async (txHash, boundary) => {
      await assertAt(boundary);
      const tx = await store.txByHash(Buffer.from(txHash, "hex"));
      if (tx === null || !tx.isValid) {
        await assertAt(boundary);
        return null;
      }
      const block = await blockAt(tx.blockSlot);
      if (block === null)
        throw new Error(
          `The follower holds transaction ${txHash} without its block`,
        );
      const found = await pointOf({
        slot: block.slot,
        blockHash: block.hash.toString("hex"),
      });
      await assertAt(boundary);
      return found === null ? null : { point: found, tip: tipOf(boundary) };
    },
    foreignSpend: {
      fetchSpend: async (ref) => {
        const target = outRef(ref.txHash, ref.outputIndex);
        const spender = await store.spenderOf(target);
        const spend =
          spender.kind === "spent"
            ? { txHash: spender.txHash, slot: spender.slot }
            : spender.kind === "unknown"
              ? await store.txSpending(target)
              : null;
        if (spend === null) return undefined;
        const block = await blockAt(spend.slot);
        if (block === null) return undefined;
        return {
          transactionId: spend.txHash.toString("hex"),
          point: { slot: block.slot, blockHash: block.hash.toString("hex") },
        };
      },
      // The follower reads a transaction by its hash; the ancestor only
      // names the block before the spend, as the SDK's interface asks.
      fetchAncestor: async (slot) => {
        const block =
          slot > 0 ? await store.blockAtOrBeforeSlot(slot - 1) : null;
        if (block === null)
          throw new Error(`The follower holds no block before slot ${slot}`);
        return { slot: block.slot, blockHash: block.hash.toString("hex") };
      },
      readTransaction: async ({ point, txHash }) => {
        const tx = await store.txByHash(Buffer.from(txHash, "hex"));
        if (tx === null || tx.blockSlot !== point.slot) return undefined;
        const block = await blockAt(tx.blockSlot);
        if (block === null || block.hash.toString("hex") !== point.blockHash)
          return undefined;
        return {
          txHash,
          point: {
            slot: block.slot,
            blockHash: point.blockHash,
            blockNo: block.height,
          },
          cbor: storedTransactionCbor(tx),
        };
      },
    },
  };
};
