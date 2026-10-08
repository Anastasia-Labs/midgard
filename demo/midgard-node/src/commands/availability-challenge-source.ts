/**
 * The availability command's canonical source: the node's follower store,
 * read once the follower has reached the local node's ledger tip. The
 * boundary is the follower's cursor (its point and block height); inclusion
 * depth comes from the transaction status, or else from the stored
 * transaction's block; a foreign
 * spend is read from the stored spending transaction, verified by the SDK
 * from its own bytes; the Open's commitment preimage comes from the retained
 * DA attestation outputs.
 */
import {
  changedUtxosIn,
  type FactStore,
  type StoredTx,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { NodeL1Access } from "../services/l1-provider.js";

/** The follower store reads the availability source makes. */
export type AvailabilityStore = Pick<
  FactStore,
  | "dialect"
  | "cursor"
  | "pointStatus"
  | "txSpending"
  | "txByHash"
  | "blockAtOrBeforeSlot"
  | "transaction"
>;

export type AvailabilityL1Access = Readonly<{
  store: AvailabilityStore;
  /** The follower's cursor once it reached the node's ledger tip. */
  synchronizedViewPoint: NodeL1Access["synchronizedViewPoint"];
}>;

type CanonicalBoundary = Readonly<{
  pointId: string;
  slot: number;
  blockNo: number;
  blockHash: string;
}>;

/** The stored transaction's full CBOR: `[body, witness set, is_valid, auxiliary data]`. */
export const storedTransactionCbor = (stored: StoredTx): string =>
  Buffer.concat([
    Buffer.from([0x84]),
    stored.bodyCbor,
    stored.witnessCbor,
    Buffer.from([stored.isValid ? 0xf5 : 0xf4]),
    stored.auxCbor ?? Buffer.from([0xf6]),
  ]).toString("hex");

/**
 * The canonical boundary: the follower's cursor once the follower reached
 * the node's ledger tip, so follower lag is never read as chain state.
 */
export const availabilityStoreBoundary =
  (access: AvailabilityL1Access) => async (): Promise<CanonicalBoundary> => {
    await access.synchronizedViewPoint();
    const cursor = await access.store.cursor();
    if (cursor === null)
      throw new Error(
        "Availability command needs the node's L1 follower initialized",
      );
    const blockHash = cursor.point.hash.toString("hex");
    return {
      pointId: `${cursor.point.slot.toString()}:${blockHash}`,
      slot: cursor.point.slot,
      blockNo: cursor.height,
      blockHash,
    };
  };

const storedBlockAt = async (
  store: AvailabilityStore,
  slot: number,
): Promise<Readonly<{ slot: number; blockHash: string; height: number }>> => {
  const block = await store.blockAtOrBeforeSlot(slot);
  if (block === null || block.slot !== slot)
    throw new Error(
      `The follower store holds no canonical block at slot ${slot.toString()}`,
    );
  return {
    slot: block.slot,
    blockHash: block.hash.toString("hex"),
    height: block.height,
  };
};

/**
 * The node's readers for the SDK's verified foreign-spend check: the stored
 * valid transaction that spent the outref, its block, and its bytes. A spend
 * the follower did not store (no tracked transaction consumed the outref)
 * reads as none.
 */
export const availabilityForeignSpendResolver =
  (input: {
    readonly store: AvailabilityStore;
    readonly readBoundary: () => Promise<CanonicalBoundary>;
  }) =>
  (outRef: string): Promise<SDK.DaAvailabilityForeignSpend | undefined> =>
    SDK.resolveDaAvailabilityForeignSpend({
      outRef,
      readBoundary: input.readBoundary,
      fetchSpend: async (ref) => {
        const spend = await input.store.txSpending({
          txHash: Buffer.from(ref.txHash, "hex"),
          index: ref.outputIndex,
        });
        if (spend === null) return undefined;
        const block = await storedBlockAt(input.store, spend.slot);
        return {
          transactionId: spend.txHash.toString("hex"),
          point: { slot: block.slot, blockHash: block.blockHash },
        };
      },
      // The store reads a transaction by its hash: no chain-sync
      // intersection is needed, so the spend's own point serves.
      fetchAncestor: async (slot) => ({
        slot,
        blockHash: (await storedBlockAt(input.store, slot)).blockHash,
      }),
      readTransaction: async ({ point, txHash }) => {
        const stored = await input.store.txByHash(Buffer.from(txHash, "hex"));
        if (stored === null || stored.blockSlot !== point.slot)
          return undefined;
        const block = await storedBlockAt(input.store, stored.blockSlot);
        if (block.blockHash !== point.blockHash) return undefined;
        return {
          txHash: stored.hash.toString("hex"),
          point: {
            slot: block.slot,
            blockHash: block.blockHash,
            blockNo: block.height,
          },
          cbor: storedTransactionCbor(stored),
        };
      },
    });

/**
 * The inline datums of every retained output that held `policyId.assetName`,
 * live or spent: the history the Open's commitment is recovered from
 * (`recoverAvailabilityOpenCommitment`).
 */
export const availabilityStoreUnitHistory =
  (store: Pick<AvailabilityStore, "dialect" | "transaction">) =>
  async (
    input: Readonly<{ policyId: string; assetName: string }>,
  ): Promise<readonly (string | null)[]> => {
    const read = await store.transaction("read", (tx) =>
      changedUtxosIn(
        tx,
        store.dialect,
        {
          by: "unit",
          policyId: Buffer.from(input.policyId, "hex"),
          assetName: Buffer.from(input.assetName, "hex"),
        },
        null,
      ),
    );
    if (read.kind !== "ok")
      throw new Error(
        `The follower store refused the unit history read: ${read.kind}: ${read.detail}`,
      );
    return read.utxos.map((row) => row.output.datum?.toString("hex") ?? null);
  };

export const availabilityCommandCanonicalSource = (input: {
  readonly lucid: LucidEvolution;
  readonly access: AvailabilityL1Access;
}) => {
  const readBoundary = availabilityStoreBoundary(input.access);
  return {
    readBoundary,
    async assertCanonicalAncestor(anchor: {
      readonly slot: number;
      readonly blockHash: string;
    }): Promise<void> {
      await readBoundary();
      const status = await input.access.store.pointStatus({
        slot: anchor.slot,
        hash: Buffer.from(anchor.blockHash, "hex"),
      });
      if (status.kind !== "canonical") {
        throw new Error(
          `Availability command canonical generation changed; recover durable intents before new work (${status.kind}: ${status.detail})`,
        );
      }
    },
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      // Read only when the transaction status omits the block depth (the
      // follower provider's carries it): the stored transaction's block,
      // counted against the boundary.
      resolveInclusion: async (output) => {
        const before = await readBoundary();
        const stored = await input.access.store.txByHash(
          Buffer.from(output.txHash, "hex"),
        );
        if (stored === null)
          throw new Error(
            `The follower store holds no transaction ${output.txHash}`,
          );
        const block = await storedBlockAt(input.access.store, stored.blockSlot);
        const after = await readBoundary();
        if (before.pointId !== after.pointId || block.height > after.blockNo) {
          throw new Error(
            "Availability transaction inclusion changed during its canonical read",
          );
        }
        return {
          slot: block.slot,
          blockHash: block.blockHash,
          depth: after.blockNo - block.height,
        };
      },
      resolveForeignSpend: availabilityForeignSpendResolver({
        store: input.access.store,
        readBoundary,
      }),
    }),
  };
};
