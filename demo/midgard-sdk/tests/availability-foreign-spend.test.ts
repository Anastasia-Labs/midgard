import {
  loadL1Recording,
  recordedTransaction,
} from "@al-ft/midgard-test-support/l1-recordings";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  type DaAvailabilityForeignSpendReaders,
  resolveDaAvailabilityForeignSpend,
  transactionConsumesOutRef,
} from "../src/availability-challenge-operation.js";

/**
 * An availability intent whose every normal input is gone may expire only on
 * a spend proven by the consuming transaction's own bytes; Kupo's `spent_at`
 * names a candidate and nothing more. The consuming transaction here is a
 * recorded preprod state-queue timeout removal, read as Ogmios served it.
 */
const removal = loadL1Recording("preprod-state-queue-removal-a2a47d2e");
const removalBlock = removal.transaction!.block;
const removalTx = recordedTransaction(removal) as {
  id: string;
  cbor: string;
  inputs: { transaction: { id: string }; index: number }[];
};
const removalInput = `${removalTx.inputs[0]!.transaction.id}#${removalTx.inputs[0]!.index.toString()}`;
const SPEND_BLOCK_NO = 5_220_548;

const readers = (
  overrides: Partial<DaAvailabilityForeignSpendReaders> = {},
): DaAvailabilityForeignSpendReaders => ({
  readBoundary: async () => ({
    pointId: "tip",
    blockNo: SPEND_BLOCK_NO + 40,
  }),
  fetchSpend: async () => ({
    transactionId: removalTx.id,
    point: { slot: removalBlock.slot, blockHash: removalBlock.id },
  }),
  fetchAncestor: async (slot) => ({
    slot: slot - 1,
    blockHash: "00".repeat(32),
  }),
  readTransaction: async ({ point, txHash }) => ({
    txHash,
    point: { ...point, blockNo: SPEND_BLOCK_NO },
    cbor: removalTx.cbor,
  }),
  ...overrides,
});

const resolve = (
  overrides: Partial<DaAvailabilityForeignSpendReaders> = {},
  outRef = removalInput,
) => resolveDaAvailabilityForeignSpend({ ...readers(overrides), outRef });

describe("verified availability foreign spend", () => {
  it("reports a spend whose consuming transaction lists the input", async () => {
    expect(removalTx.id).toBe(removal.transaction!.id);
    await expect(resolve()).resolves.toStrictEqual({
      outRef: removalInput,
      spendingTxHash: removalTx.id,
      spendPoint: `${removalBlock.slot.toString()}:${removalBlock.id}`,
      confirmationDepth: 40,
      spendingTransactionCbor: removalTx.cbor,
    });
  });

  it("reads an input Kupo reports unspent as no spend, and reads no transaction", async () => {
    const readTransaction = vi.fn(readers().readTransaction);
    await expect(
      resolve({ fetchSpend: async () => undefined, readTransaction }),
    ).resolves.toBeUndefined();
    expect(readTransaction).not.toHaveBeenCalled();
  });

  it("refuses a Kupo spend the named transaction does not consume", async () => {
    await expect(resolve({}, `${"12".repeat(32)}#0`)).resolves.toBeUndefined();
  });

  it("refuses a consuming transaction that failed phase 2", async () => {
    const valid = CML.Transaction.from_cbor_hex(removalTx.cbor);
    const invalid = CML.Transaction.new(
      valid.body(),
      valid.witness_set(),
      false,
      valid.auxiliary_data(),
    ).to_cbor_hex();
    expect(
      transactionConsumesOutRef({
        transactionCbor: invalid,
        transactionId: removalTx.id,
        outRef: removalInput,
      }),
    ).toBe(false);
    await expect(
      resolve({
        readTransaction: async ({ point, txHash }) => ({
          txHash,
          point: { ...point, blockNo: SPEND_BLOCK_NO },
          cbor: invalid,
        }),
      }),
    ).resolves.toBeUndefined();
  });

  it("refuses bytes that do not hash to the transaction Kupo named", async () => {
    await expect(
      resolve({
        fetchSpend: async () => ({
          transactionId: "ee".repeat(32),
          point: { slot: removalBlock.slot, blockHash: removalBlock.id },
        }),
      }),
    ).resolves.toBeUndefined();
    await expect(
      resolve({
        readTransaction: async ({ point }) => ({
          txHash: "ee".repeat(32),
          point: { ...point, blockNo: SPEND_BLOCK_NO },
          cbor: removalTx.cbor,
        }),
      }),
    ).resolves.toBeUndefined();
    expect(
      transactionConsumesOutRef({
        transactionCbor: "00",
        transactionId: removalTx.id,
        outRef: removalInput,
      }),
    ).toBe(false);
  });

  it("refuses a spend whose transaction the named block does not carry", async () => {
    await expect(
      resolve({ readTransaction: async () => undefined }),
    ).resolves.toBeUndefined();
  });

  it("names the Ogmios flag when the spend's raw transaction is not served", async () => {
    await expect(
      resolve({
        readTransaction: async ({ point, txHash }) => ({
          txHash,
          point: { ...point, blockNo: SPEND_BLOCK_NO },
        }),
      }),
    ).rejects.toThrow(
      "Ogmios must run with --include-transaction-cbor to verify a rival spend",
    );
  });

  it("refuses to answer while the canonical tip moves under the read", async () => {
    let reads = 0;
    await expect(
      resolve({
        readBoundary: async () => ({
          pointId: (reads += 1).toString(),
          blockNo: SPEND_BLOCK_NO + 40,
        }),
      }),
    ).rejects.toThrow(/changed during its canonical read/);
  });

  it("refuses a spend above the canonical boundary", async () => {
    await expect(
      resolve({
        readBoundary: async () => ({
          pointId: "tip",
          blockNo: SPEND_BLOCK_NO - 1,
        }),
      }),
    ).rejects.toThrow(
      "Availability input spend lies above the canonical boundary",
    );
  });
});
