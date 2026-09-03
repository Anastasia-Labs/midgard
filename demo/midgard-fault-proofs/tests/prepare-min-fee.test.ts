import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { deriveMidgardNativeTxProofSourceFromCanonicalCbor } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type NodeTransactionPayload,
  prepareMinFeeFromTransactions,
} from "../src/index.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  h28,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const CATEGORY_ID = "00000013";
type LucidDataSchema = Parameters<typeof Data.to>[1];

const fixture = async (fee: bigint) => {
  const tx = buildFixtureTransaction({
    spendInputs: [outRefCbor(0x21, 0n)],
    fee,
  });
  // The header's normative transactions MPF commits
  // `Data(L2TransactionSourceV1)` per transaction id, which is the value
  // `prepareMinFeeFromTransactions` recounts, so the authenticated root this
  // fixture hands it is the payload-source one.
  const block = await buildCanonicalBlockFixture({
    transactions: [tx],
  });
  const transactions: readonly NodeTransactionPayload[] = [
    { nodeTxId: tx.txId, txCbor: tx.canonicalCbor.toString("hex") },
  ];
  return { tx, block, transactions };
};

describe("prepare-min-fee", () => {
  it("derives the exact canonical size, strict fee boundary, and all nine ordered fields", async () => {
    const { tx, block, transactions } = await fixture(1n);
    const output = await prepareMinFeeFromTransactions({
      headerHash: h28(0xaa),
      transactions,
      expectedTransactionsRoot: block.payloadSourceTransactionsRoot,
      minFeeA: 2n,
      minFeeB: 7n,
      categoryId: CATEGORY_ID,
    });
    const source = deriveMidgardNativeTxProofSourceFromCanonicalCbor(
      tx.canonicalCbor,
    );
    const boundary = SDK.minimumFeeFromProofSource({
      source,
      minFeeA: 2n,
      minFeeB: 7n,
    });

    expect(output.tx.nodeTxId).toBe(tx.txId);
    expect(output.tx.canonicalTxSize).toBe(boundary.canonicalTxSize);
    expect(output.tx.minimumFee).toBe(2n * output.tx.canonicalTxSize + 7n);
    expect(output.tx.shortfall).toBe(output.tx.minimumFee - 1n);
    expect(output.tx.fieldItemCbors).toHaveLength(9);
    expect(output.tx.fieldItemCbors.map((items) => items.length)).toEqual([
      1, 0, 0, 0, 0, 0, 0, 0, 0,
    ]);
    expect(output.threadTokenAssetName).toBe(`${CATEGORY_ID}${h28(0xaa)}`);
    expect(output.committedTransactionsRoot).toBe(
      block.payloadSourceTransactionsRoot,
    );
  });

  it("refuses a fee exactly at the canonical minimum and one above it", async () => {
    const seed = await fixture(0n);
    const source = deriveMidgardNativeTxProofSourceFromCanonicalCbor(
      seed.tx.canonicalCbor,
    );
    // CBOR integer width can change with the fee itself. Use a flat schedule
    // to pin the exact adjacent boundary without a fixed-point assumption.
    const boundary = 1_000n;
    for (const fee of [boundary, boundary + 1n]) {
      const { tx, block, transactions } = await fixture(fee);
      await expect(
        prepareMinFeeFromTransactions({
          headerHash: h28(0xaa),
          transactions,
          expectedTransactionsRoot: block.payloadSourceTransactionsRoot,
          minFeeA: 0n,
          minFeeB: boundary,
          txId: tx.txId,
        }),
      ).rejects.toThrow(/satisfying exact minimum 1000/u);
    }
    expect(
      SDK.minimumFeeFromProofSource({
        source,
        minFeeA: 0n,
        minFeeB: boundary,
      }).minimumFee,
    ).toBe(boundary);
  });

  it("fails closed on an unauthenticated root and negative fee parameters", async () => {
    const { transactions } = await fixture(1n);
    await expect(
      prepareMinFeeFromTransactions({
        headerHash: h28(0xaa),
        transactions,
        expectedTransactionsRoot: h32(0xff),
        minFeeA: 1n,
        minFeeB: 0n,
      }),
    ).rejects.toThrow(/not authenticated header root/u);
    await expect(
      prepareMinFeeFromTransactions({
        headerHash: h28(0xaa),
        transactions,
        expectedTransactionsRoot: h32(0xff),
        minFeeA: -1n,
        minFeeB: 0n,
      }),
    ).rejects.toThrow(/--min-fee-a must be non-negative/u);
    expect(() =>
      SDK.hasMinFeeViolation({
        fee: -1n,
        minFeeA: 1n,
        minFeeB: 0n,
        canonicalTxSize: 1n,
      }),
    ).toThrow(/fee must be non-negative/u);
  });

  it("pins exact step-02 Data order, nine carriages, and emitted artifacts", async () => {
    const { block, transactions } = await fixture(1n);
    const dir = await mkdtemp(join(tmpdir(), "midgard-min-fee-"));
    try {
      const output = await prepareMinFeeFromTransactions({
        headerHash: h28(0xaa),
        transactions,
        expectedTransactionsRoot: block.payloadSourceTransactionsRoot,
        minFeeA: 2n,
        minFeeB: 7n,
        categoryId: CATEGORY_ID,
        outputDir: dir,
      });
      const state: SDK.MinFeeStep02State = {
        bad_tx: output.tx.nativeTx,
        bad_tx_body_fee: output.tx.fee,
        bad_tx_id: output.tx.nodeTxId,
        min_fee_a: output.minFeeA,
        min_fee_b: output.minFeeB,
      };
      const manualStateSchema = Data.Object({
        bad_tx: SDK.NativeTxCompactSchema,
        bad_tx_body_fee: Data.Integer(),
        bad_tx_id: SDK.H32Schema,
        min_fee_a: Data.Integer(),
        min_fee_b: Data.Integer(),
      });
      expect(Data.to(state, SDK.MinFeeStep02State)).toBe(
        Data.to(state, manualStateSchema as unknown as LucidDataSchema),
      );

      const carriages = output.tx.fieldItemCbors.map((items) => ({
        Inline: {
          preimage: Data.to(
            [...items],
            Data.Array(Data.Bytes()) as unknown as LucidDataSchema,
          ),
        },
      })) as unknown as SDK.MinFeeStep02Args["field_carriages"];
      expect(carriages).toHaveLength(9);
      const args: SDK.MinFeeStep02Args = {
        input_index: 0n,
        output_index: 1n,
        fraud_proof_mint_redeemer_index: 2n,
        native_tx_compact_cbor: output.tx.nativeTxCompactCbor,
        witness_set: output.tx.witnessSet,
        field_carriages: carriages,
      };
      const manualArgsSchema = Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        fraud_proof_mint_redeemer_index: Data.Integer(),
        native_tx_compact_cbor: Data.Bytes(),
        witness_set: SDK.NativeTxWitnessSetCompactSchema,
        field_carriages: Data.Tuple([
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
          SDK.FieldCarriageSchema,
        ]),
      });
      expect(Data.to(args, SDK.MinFeeStep02Args)).toBe(
        Data.to(args, manualArgsSchema as unknown as LucidDataSchema),
      );
      expect(
        Data.from(Data.to(args, SDK.MinFeeStep02Args), SDK.MinFeeStep02Args),
      ).toStrictEqual(args);

      for (const path of Object.values(output.files!)) {
        await expect(readFile(path, "utf8")).resolves.toMatch(/\{/u);
      }
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });
});
