import { encodeCbor } from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { h28 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  midgardTxOutputFromCanonicalCbor,
  prepareInputNoIdxFromTransactions,
} from "../src/prepare-input-no-idx.js";
import {
  committedTransactionsRoot,
  producerTx,
  spenderTx,
  violatingBlock,
} from "./prepare-input-no-idx.make-native-tx.js";

describe("Q13 input-no-idx canonical evidence", () => {
  it("selects the input whose producing transaction is committed but has no such output", async () => {
    const block = await violatingBlock();
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28(0xaa),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });

    expect(output.schemaVersion).toBe("midgard-input-no-idx-evidence-v1");
    expect(output.violationId).toBe(SDK.INPUT_NO_IDX_VIOLATION_ID);
    expect(output.txCount).toBe(2);
    expect(output.evidence.isViolation).toBe(true);
    expect(output.evidence.badTxId).toBe(block.spender.nodeTxId);
    expect(output.evidence.producingTxId).toBe(block.producer.nodeTxId);
    expect(output.evidence.badInputsIndex).toBe(0);
    expect(output.evidence.badInput.output_index).toBe(7n);
    expect(output.evidence.producingTxOutputCount).toBe(1);
    expect(output.expectedTransactionsRoot).toEqual({
      value: block.expectedTransactionsRoot,
      matches: true,
    });
    expect(output.transactionsPhasRoot).not.toBe(
      output.committedTransactionsRoot,
    );
  });

  it("emits both inclusion arguments and every forwarded step state the validators derive", async () => {
    const block = await violatingBlock();
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28(0xaa),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });

    expect(output.badTxInclusion.nativeTxId).toBe(block.spender.nodeTxId);
    expect(output.producingTxInclusion.nativeTxId).toBe(
      block.producer.nodeTxId,
    );
    for (const inclusion of [
      output.badTxInclusion,
      output.producingTxInclusion,
    ]) {
      expect(inclusion.transactionsPhasRoot).toBe(output.transactionsPhasRoot);
      expect(inclusion.txMembershipProofCbor.length).toBeGreaterThan(0);
    }

    // step-01 -> step-02. #604: the §2.5 anchor, and no `Direct`/`Folding` sum.
    expect(output.step02State).toEqual({
      verified_tx_id: output.badTxInclusion.nativeTxId,
    });
    // step-02 -> step-03
    expect(output.step03State).toEqual({
      bad_input_tx_id: block.producer.nodeTxId,
      bad_input_output_index: 7n,
    });
    expect(output.step02.inputsPreimage).toEqual([
      { tx_id: block.producer.nodeTxId, output_index: 7n },
    ]);
    expect(output.step02.badInputsIndex).toBe(0);
    // step-03 -> step-04
    // step-03 -> step-04. #604: the producing transaction's anchor, not its
    // outputs commitment — step-04 opens its field 2 through the §8.8 door.
    expect(output.step04State).toEqual({
      producing_tx_id: output.producingTxInclusion.nativeTxId,
      bad_input_output_index: 7n,
    });
    expect(output.outputsPreimage).toHaveLength(1);
    expect(output.step04.outputsPreimageCbor).toHaveLength(1);
  });

  it("projects the producing transaction's canonical outputs into step-04 PlutusData", async () => {
    const block = await violatingBlock();
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28(0xaa),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });

    const [outputZero] = output.outputsPreimage;
    expect(outputZero).toBeDefined();
    expect(outputZero!.address.protected).toBe(false);
    expect(outputZero!.address.network_id).toBe(0n);
    expect(outputZero!.address.stake_credential).toBeNull();
    expect(outputZero!.address.payment_credential).toEqual({
      PubKeyCredential: ["40".repeat(28)],
    });
    expect(outputZero!.value.lovelace).toBe(5_000_000n);
    expect(outputZero!.datum_cbor).toBeNull();
    expect(outputZero!.script_ref).toBeNull();

    const outputsSchema = SDK.MidgardTxOutputList as unknown as Parameters<
      typeof Data.to
    >[1];
    const encoded = Data.to(
      output.outputsPreimage as unknown as Parameters<typeof Data.to>[0],
      outputsSchema,
    );
    expect(Data.from(encoded, outputsSchema)).toEqual(output.outputsPreimage);
  });

  it("inverts a canonical output carrying a stake credential, datum and script reference", () => {
    const address = Buffer.concat([
      Buffer.from([0x00]),
      Buffer.alloc(28, 0x21),
      Buffer.alloc(28, 0x22),
    ]);
    const datum = Buffer.from([0xd8, 0x79, 0x80]);
    const script = Buffer.from([0x01, 0x02, 0x03]);
    const bytes = Buffer.concat([
      Buffer.from([0xa4, 0x00, 0x58, 0x39]),
      address,
      Buffer.from([0x01, 0x82]),
      encodeCbor(2_000_000n),
      Buffer.from([0xa1, 0x58, 0x1c]),
      Buffer.alloc(28, 0x33),
      Buffer.from([0xa1, 0x43]),
      Buffer.from("abc", "ascii"),
      Buffer.from([0x05, 0x02, 0x43]),
      datum,
      Buffer.from([0x03, 0x82, 0x03, 0x43]),
      script,
    ]);

    const projected = midgardTxOutputFromCanonicalCbor(bytes);
    expect(projected.address.payment_credential).toEqual({
      PubKeyCredential: ["21".repeat(28)],
    });
    expect(projected.address.stake_credential).toEqual({
      PubKeyCredential: ["22".repeat(28)],
    });
    expect(projected.value.lovelace).toBe(2_000_000n);
    expect([...projected.value.assets.entries()]).toEqual([
      [`${"33".repeat(28)}616263`, 5n],
    ]);
    expect(projected.datum_cbor).toBe(datum.toString("hex"));
    expect(projected.script_ref).toEqual({
      language: "PlutusV3Script",
      script_bytes: script.toString("hex"),
    });
  });

  it("re-derives both bounded-collection commitments from the emitted preimages", async () => {
    // A three-output producer challenged past its end: the canonical encoders
    // must reproduce the transaction's own committed hashes, which is what the
    // two opening steps recompute on-chain.
    const producer = producerTx(3, 1n);
    const spender = spenderTx(producer.nodeTxId, 5n, 2n);
    const transactions = [producer, spender];
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28(0xaa),
      transactions,
      expectedTransactionsRoot: await committedTransactionsRoot(transactions),
    });

    expect(output.evidence.producingTxOutputCount).toBe(3);
    expect(output.outputsPreimage).toHaveLength(3);
    expect(SDK.inputNoIdxOutputsCommitment(output.outputsPreimage)).toBe(
      output.producingTxInclusion.nativeTx.body.outputs_hash,
    );
    expect(
      SDK.inputNoIdxSpendInputsCommitment(output.step02.inputsPreimage),
    ).toBe(output.badTxInclusion.nativeTx.body.spend_inputs_hash);
    // The artifact carries the canonical bytes the validator re-encodes.
    expect(
      output.step04.outputsPreimageCbor.map((item) =>
        SDK.encodeMidgardTxOutputCanonical(
          midgardTxOutputFromCanonicalCbor(Buffer.from(item, "hex")),
        ).toString("hex"),
      ),
    ).toEqual([...output.step04.outputsPreimageCbor]);
  });

  it("plans one §8 carriage tier for the whole field-0 preimage", async () => {
    // #604: the retired test here derived per-item counted fold openings
    // (`buildInputNoIdxSpendInputFoldOpeningsV1`). §4 gives a field one flat hash
    // and no per-item openings at all, so those functions were deleted rather
    // than re-pointed. What the artifact reports instead is the §5.1 preimage's
    // length and the §8.4 tier that length selects.
    const block = await violatingBlock(20);
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28(0xaa),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });
    const preimage = encodeMidgardFieldPreimage(
      output.step02.inputsPreimage.map(SDK.encodeMidgardTxInputCanonical),
    );
    expect(output.proofFit.step02SpendInputsPreimageBytes).toBe(
      preimage.length,
    );
    expect(output.proofFit.step02CarriageTier).toBe(
      selectMidgardFieldCarriageTier(preimage.length),
    );
    // §4: the preimage the artifact describes is the one the door will hash.
    expect(midgardFieldCommitment(preimage).toString("hex")).toBe(
      output.badTxInclusion.nativeTx.body.spend_inputs_hash,
    );
  });

  it.each([20, 296])(
    "keeps one step-02 route at the %i-input preimage",
    async (spendInputCount) => {
      const block = await violatingBlock(spendInputCount);
      const output = await prepareInputNoIdxFromTransactions({
        headerHash: h28(0xaa),
        transactions: block.transactions,
        expectedTransactionsRoot: block.expectedTransactionsRoot,
      });

      expect(output.step02.inputsPreimage).toHaveLength(spendInputCount);
      expect(output.step02.badInputsIndex).toBe(spendInputCount - 1);
      // #604: there is no direct/fold boundary any more. Both sizes are one
      // route; both fit tier 1, because §8.4's bound is 14,336 bytes and 296
      // spend inputs are 40 bytes apiece.
      const preimage = encodeMidgardFieldPreimage(
        output.step02.inputsPreimage.map(SDK.encodeMidgardTxInputCanonical),
      );
      expect(output.proofFit.step02SpendInputsPreimageBytes).toBe(
        preimage.length,
      );
      expect(output.proofFit.step02CarriageTier).toBe("Inline");
      expect(output.step03State).toEqual({
        bad_input_tx_id: block.producer.nodeTxId,
        bad_input_output_index: 7n,
      });
    },
  );

  it("reports the §8.4 tier at the retired 19-input release boundary", async () => {
    // The boundary itself is retired: `INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT`
    // bounded the direct redeemer arm against the folding one, and step-02 has
    // one arm now. The size is kept as a case because it is a real preimage the
    // family produced; what is asserted is the §8.4 tier, which is a function of
    // bytes rather than of item count.
    const block = await violatingBlock(
      SDK.INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT,
    );
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28(0xaa),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });

    expect(output.step02.inputsPreimage).toHaveLength(
      SDK.INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT,
    );
    expect(output.proofFit.step02CarriageTier).toBe("Inline");
  });

  it("round-trips a native-asset output through the canonical encoder", () => {
    const bytes = Buffer.concat([
      Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x71]),
      Buffer.alloc(28, 0x44),
      Buffer.from([0x01, 0x82]),
      encodeCbor(3_000_000n),
      Buffer.from([0xa1, 0x58, 0x1c]),
      Buffer.alloc(28, 0x55),
      Buffer.from([0xa2, 0x43]),
      Buffer.from("abc", "ascii"),
      Buffer.from([0x07, 0x44]),
      Buffer.from("defg", "ascii"),
      Buffer.from([0x09]),
    ]);
    const projected = midgardTxOutputFromCanonicalCbor(bytes);
    expect(projected.address.payment_credential).toEqual({
      ScriptCredential: ["44".repeat(28)],
    });
    expect(projected.address.network_id).toBe(1n);
    expect([...projected.value.assets.values()]).toEqual([7n, 9n]);
    expect(SDK.encodeMidgardTxOutputCanonical(projected)).toEqual(bytes);
  });

  it("measures the complete proof item carried by each step (§3.2 tier 1)", async () => {
    const block = await violatingBlock();
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28(0xaa),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });

    expect(output.proofFit.step02CarriageTier).toBe("Inline");
    expect(output.proofFit.step02InputsPreimageItemCount).toBe(1);
    expect(output.proofFit.step04OutputsPreimageItemCount).toBe(1);
    expect(output.proofFit.step02InputsPreimageDatumBytes).toBeGreaterThan(0);
    expect(output.proofFit.step04OutputsPreimageDatumBytes).toBeGreaterThan(0);
    expect(output.proofFit.badTxCompactCborBytes).toBeGreaterThan(0);
    expect(output.proofFit.producingTxCompactCborBytes).toBeGreaterThan(0);
  });
});
