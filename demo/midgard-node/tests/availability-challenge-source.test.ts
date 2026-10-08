import type { Cursor, StoredBlock, StoredTx } from "@al-ft/midgard-l1-follower";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Emulator, Lucid } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  availabilityCommandCanonicalSource,
  availabilityForeignSpendResolver,
  type AvailabilityStore,
  availabilityStoreBoundary,
  storedTransactionCbor,
} from "../src/commands/availability-challenge-source.js";

const hash = (byte: string) => Buffer.from(byte.repeat(32), "hex");

const cursorAt = (slot: number, blockHash: Buffer, height: number): Cursor => ({
  point: { slot, hash: blockHash },
  height,
  generation: 1,
  origin: { slot: 0, hash: hash("00") },
  prunedThroughSlot: 0,
});

/** A follower store holding the given canonical blocks and transactions. */
const fakeStore = (state: {
  cursor: Cursor | null;
  blocks: StoredBlock[];
  txs?: StoredTx[];
  spends?: Map<string, { txHash: Buffer; slot: number }>;
}): AvailabilityStore =>
  ({
    cursor: async () => state.cursor,
    pointStatus: async (point) => {
      const block = state.blocks.find(
        (b) => b.slot === point.slot && b.hash.equals(point.hash),
      );
      return block === undefined
        ? {
            kind: "point_not_canonical",
            detail: `${point.hash.toString("hex")} at slot ${point.slot.toString()} is not on the stored chain`,
          }
        : { kind: "canonical", height: block.height, depth: 1 };
    },
    blockAtOrBeforeSlot: async (slot) =>
      [...state.blocks].reverse().find((b) => b.slot <= slot) ?? null,
    txByHash: async (txHash) =>
      state.txs?.find((tx) => tx.hash.equals(txHash)) ?? null,
    txSpending: async (outRef) =>
      state.spends?.get(
        `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`,
      ) ?? null,
  }) as AvailabilityStore;

const block = (
  slot: number,
  blockHash: Buffer,
  height: number,
): StoredBlock => ({
  slot,
  hash: blockHash,
  height,
  parentHash: null,
  qualifyingTxCount: 1,
});

describe("availability command canonical source (follower store)", () => {
  it("reads the boundary only once the follower reached the node's ledger tip", async () => {
    const tip = hash("11");
    const store = fakeStore({
      cursor: cursorAt(100, tip, 10),
      blocks: [block(100, tip, 10)],
    });
    const behind = availabilityStoreBoundary({
      store,
      synchronizedViewPoint: async () => {
        throw new L1ProviderTransientError("follower", "behind_node_tip");
      },
    });
    await expect(behind()).rejects.toThrow(
      "L1 provider follower unavailable: behind_node_tip",
    );
    const synced = availabilityStoreBoundary({
      store,
      synchronizedViewPoint: async () => ({
        slot: 100,
        id: tip.toString("hex"),
      }),
    });
    await expect(synced()).resolves.toEqual({
      pointId: `100:${tip.toString("hex")}`,
      slot: 100,
      blockNo: 10,
      blockHash: tip.toString("hex"),
    });
  });

  it("revokes a captured generation when the anchor is no longer on the stored chain", async () => {
    const original = hash("11");
    const state = {
      cursor: cursorAt(100, original, 10),
      blocks: [block(100, original, 10)],
    };
    const lucid = await Lucid(new Emulator([]), "Custom");
    const source = availabilityCommandCanonicalSource({
      lucid,
      access: {
        store: fakeStore(state),
        synchronizedViewPoint: async () => ({
          slot: state.cursor.point.slot,
          id: state.cursor.point.hash.toString("hex"),
        }),
      },
    });
    const anchor = await source.readBoundary();
    await expect(
      source.assertCanonicalAncestor(anchor),
    ).resolves.toBeUndefined();
    const rival = hash("22");
    state.cursor = cursorAt(100, rival, 10);
    state.blocks = [block(100, rival, 10)];
    await expect(source.assertCanonicalAncestor(anchor)).rejects.toThrow(
      /canonical generation changed.*point_not_canonical/,
    );
  });

  it("verifies a foreign spend from the stored spending transaction's own bytes", async () => {
    const spentRef = `${"aa".repeat(32)}#0`;
    const input = CML.TransactionInput.new(
      CML.TransactionHash.from_hex("aa".repeat(32)),
      0n,
    );
    const inputs = CML.TransactionInputList.new();
    inputs.add(input);
    const body = CML.TransactionBody.new(
      inputs,
      CML.TransactionOutputList.new(),
      0n,
    );
    const bodyCbor = Buffer.from(body.to_cbor_bytes());
    const spendingHash = Buffer.from(CML.hash_transaction(body).to_raw_bytes());
    const witnessCbor = Buffer.from(
      CML.TransactionWitnessSet.new().to_cbor_bytes(),
    );
    const stored = {
      hash: spendingHash,
      blockSlot: 90,
      isValid: true,
      bodyCbor,
      witnessCbor,
      auxCbor: null,
    } as unknown as StoredTx;
    const spendBlock = hash("33");
    const tip = hash("44");
    const store = fakeStore({
      cursor: cursorAt(100, tip, 12),
      blocks: [block(90, spendBlock, 9), block(100, tip, 12)],
      txs: [stored],
      spends: new Map([[spentRef, { txHash: spendingHash, slot: 90 }]]),
    });
    const readBoundary = availabilityStoreBoundary({
      store,
      synchronizedViewPoint: async () => ({
        slot: 100,
        id: tip.toString("hex"),
      }),
    });
    const resolve = availabilityForeignSpendResolver({ store, readBoundary });
    const spend = await resolve(spentRef);
    expect(spend).toMatchObject({
      outRef: spentRef,
      spendingTxHash: spendingHash.toString("hex"),
      spendPoint: `90:${spendBlock.toString("hex")}`,
      confirmationDepth: 3,
    });
    expect(
      SDK.transactionConsumesOutRef({
        transactionCbor: storedTransactionCbor(stored),
        transactionId: spendingHash.toString("hex"),
        outRef: spentRef,
      }),
    ).toBe(true);
    // An outref no stored transaction spent reads as no spend.
    await expect(resolve(`${"bb".repeat(32)}#0`)).resolves.toBeUndefined();
  });
});
