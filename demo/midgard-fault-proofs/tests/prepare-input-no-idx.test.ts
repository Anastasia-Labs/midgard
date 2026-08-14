/**
 * `Q13` — `input-no-idx` (`nonExistentInputNoIndex`) off-chain surface.
 *
 * Acceptance (GOAL_SPEC.md §9.1 outputs 6-8): a canonical evidence definition,
 * evidence built from retained public data through the Q03 evidence-source
 * API, resumable prepare tooling, a valid-block negative, and a complete-item
 * proof-fit measurement.
 */
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimageV1,
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  authenticateTransactionsInclusionRootsV1,
  canonicalBlockEvidenceFromVerifiedPayloadV1,
  type CanonicalBlockEvidenceV1,
} from "../src/evidence/index.js";
import * as FaultProofs from "../src/index.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  nativeTrieItem,
  type NodeTransactionPayload,
} from "../src/prepare-double-spend.js";
import {
  InputNoIdxRejectionV1,
  midgardTxOutputFromCanonicalCborV1,
  prepareInputNoIdxFromCanonicalEvidenceV1,
  prepareInputNoIdxFromTransactions,
} from "../src/prepare-input-no-idx.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  type CanonicalBlockFixtureV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_NULL_ROOT = computeHash32(encodeCbor(null));

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

/**
 * One canonical native output: an enterprise (no stake credential) pubkey
 * address on network 0 holding only lovelace, exactly as
 * `encode_midgard_tx_output` frames it.
 */
const nativeOutputCbor = (paymentByte: number, lovelace: bigint): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, paymentByte),
    Buffer.from([0x01, 0x82]),
    encodeCbor(lovelace),
    Buffer.from([0xa0]),
  ]);

const makeNativeTx = ({
  spendInputCbors,
  outputCbors,
  fee,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly outputCbors: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputCbors]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor([...outputCbors]),
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const payloadFromTx = (tx: MidgardNativeTxFullV1): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxIdV1(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonicalV1(tx).toString("hex"),
});

/** A producer committing `outputCount` canonical outputs. */
const producerTx = (outputCount: number, fee: bigint): NodeTransactionPayload =>
  payloadFromTx(
    makeNativeTx({
      spendInputCbors: [inputCbor(h32("99"), fee)],
      outputCbors: Array.from({ length: outputCount }, (_, index) =>
        nativeOutputCbor(0x40 + index, 5_000_000n + BigInt(index)),
      ),
      fee,
    }),
  );

/** A transaction spending `(producerTxId, outputIndex)`. */
const spenderTx = (
  producerTxId: string,
  outputIndex: bigint,
  fee: bigint,
  spendInputCount = 1,
): NodeTransactionPayload =>
  payloadFromTx(
    makeNativeTx({
      spendInputCbors: Array.from({ length: spendInputCount }, (_, index) =>
        index === spendInputCount - 1
          ? inputCbor(producerTxId, outputIndex)
          : inputCbor(index.toString(16).padStart(64, "0"), BigInt(index)),
      ),
      outputCbors: [nativeOutputCbor(0x11, 1_000_000n)],
      fee,
    }),
  );

const committedTransactionsRoot = async (
  transactions: readonly NodeTransactionPayload[],
): Promise<string> => {
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const trie = await buildTrieView(decoded.map(nativeTrieItem));
  return await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain: SDK.ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
};

/** Producer with one output; spender challenging index 7 (out of range). */
const violatingBlock = async (
  spendInputCount = 1,
): Promise<{
  readonly transactions: readonly NodeTransactionPayload[];
  readonly producer: NodeTransactionPayload;
  readonly spender: NodeTransactionPayload;
  readonly expectedTransactionsRoot: string;
}> => {
  const producer = producerTx(1, 1n);
  const spender = spenderTx(producer.nodeTxId, 7n, 2n, spendInputCount);
  const transactions = [producer, spender];
  return {
    transactions,
    producer,
    spender,
    expectedTransactionsRoot: await committedTransactionsRoot(transactions),
  };
};

const rejectionCode = async (run: () => Promise<unknown>): Promise<string> => {
  try {
    await run();
  } catch (error) {
    if (
      error instanceof InputNoIdxRejectionV1 ||
      error instanceof SDK.CanonicalEvidenceRejectionV1
    ) {
      return error.code;
    }
    return `unexpected:${error instanceof Error ? error.message : String(error)}`;
  }
  return "no_rejection";
};

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-input-no-idx-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

describe("Q13 input-no-idx canonical evidence", () => {
  it("selects the input whose producing transaction is committed but has no such output", async () => {
    const block = await violatingBlock();
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28("aa"),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });

    expect(output.schemaVersion).toBe("midgard-input-no-idx-evidence-v1");
    expect(output.violationId).toBe(SDK.INPUT_NO_IDX_VIOLATION_ID_V1);
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
      headerHash: h28("aa"),
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
      headerHash: h28("aa"),
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

    const projected = midgardTxOutputFromCanonicalCborV1(bytes);
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
      headerHash: h28("aa"),
      transactions,
      expectedTransactionsRoot: await committedTransactionsRoot(transactions),
    });

    expect(output.evidence.producingTxOutputCount).toBe(3);
    expect(output.outputsPreimage).toHaveLength(3);
    expect(SDK.inputNoIdxOutputsCommitmentV1(output.outputsPreimage)).toBe(
      output.producingTxInclusion.nativeTx.body.outputs_hash,
    );
    expect(
      SDK.inputNoIdxSpendInputsCommitmentV1(output.step02.inputsPreimage),
    ).toBe(output.badTxInclusion.nativeTx.body.spend_inputs_hash);
    // The artifact carries the canonical bytes the validator re-encodes.
    expect(
      output.step04.outputsPreimageCbor.map((item) =>
        SDK.encodeMidgardTxOutputCanonicalV1(
          midgardTxOutputFromCanonicalCborV1(Buffer.from(item, "hex")),
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
      headerHash: h28("aa"),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });
    const preimage = encodeMidgardFieldPreimageV1(
      output.step02.inputsPreimage.map(SDK.encodeMidgardTxInputCanonicalV1),
    );
    expect(output.proofFit.step02SpendInputsPreimageBytes).toBe(
      preimage.length,
    );
    expect(output.proofFit.step02CarriageTier).toBe(
      selectMidgardFieldCarriageTierV1(preimage.length),
    );
    // §4: the preimage the artifact describes is the one the door will hash.
    expect(midgardFieldCommitmentV1(preimage).toString("hex")).toBe(
      output.badTxInclusion.nativeTx.body.spend_inputs_hash,
    );
  });

  it.each([20, 296])(
    "keeps one step-02 route at the %i-input preimage",
    async (spendInputCount) => {
      const block = await violatingBlock(spendInputCount);
      const output = await prepareInputNoIdxFromTransactions({
        headerHash: h28("aa"),
        transactions: block.transactions,
        expectedTransactionsRoot: block.expectedTransactionsRoot,
      });

      expect(output.step02.inputsPreimage).toHaveLength(spendInputCount);
      expect(output.step02.badInputsIndex).toBe(spendInputCount - 1);
      // #604: there is no direct/fold boundary any more. Both sizes are one
      // route; both fit tier 1, because §8.4's bound is 14,336 bytes and 296
      // spend inputs are 40 bytes apiece.
      const preimage = encodeMidgardFieldPreimageV1(
        output.step02.inputsPreimage.map(SDK.encodeMidgardTxInputCanonicalV1),
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
    // The boundary itself is retired: `INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1`
    // bounded the direct redeemer arm against the folding one, and step-02 has
    // one arm now. The size is kept as a case because it is a real preimage the
    // family produced; what is asserted is the §8.4 tier, which is a function of
    // bytes rather than of item count.
    const block = await violatingBlock(
      SDK.INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1,
    );
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28("aa"),
      transactions: block.transactions,
      expectedTransactionsRoot: block.expectedTransactionsRoot,
    });

    expect(output.step02.inputsPreimage).toHaveLength(
      SDK.INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1,
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
    const projected = midgardTxOutputFromCanonicalCborV1(bytes);
    expect(projected.address.payment_credential).toEqual({
      ScriptCredential: ["44".repeat(28)],
    });
    expect(projected.address.network_id).toBe(1n);
    expect([...projected.value.assets.values()]).toEqual([7n, 9n]);
    expect(SDK.encodeMidgardTxOutputCanonicalV1(projected)).toEqual(bytes);
  });

  it("measures the complete proof item carried by each step (§3.2 tier 1)", async () => {
    const block = await violatingBlock();
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28("aa"),
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

describe("Q13 input-no-idx valid-block negatives", () => {
  it("refuses to prove non-existence of an input the producing transaction really created", async () => {
    const producer = producerTx(3, 1n);
    const spender = spenderTx(producer.nodeTxId, 2n, 2n);
    const transactions = [producer, spender];
    const expectedTransactionsRoot =
      await committedTransactionsRoot(transactions);

    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28("aa"),
          transactions,
          expectedTransactionsRoot,
        }),
      ),
    ).toBe("input_exists_in_producing_tx");
  });

  it("refuses when the input's producing transaction is not committed in this block", async () => {
    const spender = spenderTx(h32("77"), 0n, 2n);
    const transactions = [spender];
    const expectedTransactionsRoot =
      await committedTransactionsRoot(transactions);

    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28("aa"),
          transactions,
          expectedTransactionsRoot,
        }),
      ),
    ).toBe("producing_tx_not_committed");
  });

  it("refuses a pinned transaction that is not committed in the block", async () => {
    const block = await violatingBlock();
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28("aa"),
          transactions: block.transactions,
          expectedTransactionsRoot: block.expectedTransactionsRoot,
          badTxId: h32("ee"),
        }),
      ),
    ).toBe("bad_tx_not_committed");
  });

  it("refuses a pinned input position the transaction does not have", async () => {
    const block = await violatingBlock();
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28("aa"),
          transactions: block.transactions,
          expectedTransactionsRoot: block.expectedTransactionsRoot,
          badTxId: block.spender.nodeTxId,
          badInputsIndex: 4,
        }),
      ),
    ).toBe("bad_input_index_out_of_range");
  });

  it("refuses a root the block header does not commit", async () => {
    const block = await violatingBlock();
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28("aa"),
          transactions: block.transactions,
          expectedTransactionsRoot: h32("ff"),
        }),
      ),
    ).toBe("transactions_root_mismatch");
  });

  it("refuses the raw PHAS root where the counted header root is required", async () => {
    const block = await violatingBlock();
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const trie = await buildTrieView(decoded.map(nativeTrieItem));
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28("aa"),
          transactions: block.transactions,
          expectedTransactionsRoot: trie.root,
        }),
      ),
    ).toBe("transactions_root_mismatch");
  });
});

describe("Q13 input-no-idx resumable artifacts", () => {
  it("writes the four submit-step artifacts and the plan", async () => {
    const block = await violatingBlock();
    await withTempDir(async (dir) => {
      const output = await prepareInputNoIdxFromTransactions({
        headerHash: h28("aa"),
        transactions: block.transactions,
        expectedTransactionsRoot: block.expectedTransactionsRoot,
        outputDir: dir,
      });

      expect(output.files).toEqual({
        badTxInclusionPath: join(dir, "bad-tx-inclusion.json"),
        producingTxInclusionPath: join(dir, "producing-tx-inclusion.json"),
        inputsPreimagePath: join(dir, "inputs-preimage.json"),
        outputsPreimagePath: join(dir, "outputs-preimage.json"),
        planPath: join(dir, "plan.json"),
      });

      const badTxInclusion = JSON.parse(
        await readFile(output.files!.badTxInclusionPath, "utf8"),
      ) as { readonly nativeTxId: string };
      expect(badTxInclusion.nativeTxId).toBe(block.spender.nodeTxId);

      const producingTxInclusion = JSON.parse(
        await readFile(output.files!.producingTxInclusionPath, "utf8"),
      ) as { readonly nativeTxId: string };
      expect(producingTxInclusion.nativeTxId).toBe(block.producer.nodeTxId);

      const inputsPreimage = JSON.parse(
        await readFile(output.files!.inputsPreimagePath, "utf8"),
      ) as { readonly badInputsIndex: number };
      expect(inputsPreimage.badInputsIndex).toBe(0);

      const outputsPreimage = JSON.parse(
        await readFile(output.files!.outputsPreimagePath, "utf8"),
      ) as { readonly badInputOutputIndex: string };
      expect(outputsPreimage.badInputOutputIndex).toBe("7");

      const plan = JSON.parse(
        await readFile(output.files!.planPath, "utf8"),
      ) as {
        readonly committedTransactionsRoot: string;
        readonly proofFit: { readonly step02CarriageTier: string };
      };
      expect(plan.committedTransactionsRoot).toBe(
        output.committedTransactionsRoot,
      );
      expect(plan.proofFit.step02CarriageTier).toBe("Inline");
    });
  });

  it("is exported from the package root for the resumable prepare/submit flow", () => {
    expect(FaultProofs.prepareInputNoIdxFromTransactions).toBeTypeOf(
      "function",
    );
    expect(FaultProofs.prepareInputNoIdxFromCanonicalEvidenceV1).toBeTypeOf(
      "function",
    );
    expect(FaultProofs.prepareInputNoIdxFromNode).toBeTypeOf("function");
    expect(FaultProofs.prepareInputNoIdxFromFile).toBeTypeOf("function");
    expect(SDK.INPUT_NO_IDX_VIOLATION_ID_V1).toBe("input-no-idx");
    expect(SDK.INPUT_NO_IDX_CATALOGUE_CATEGORY_V1).toBe(
      "nonExistentInputNoIndex",
    );
  });
});

describe("Q13 input-no-idx Q03 evidence gates", () => {
  const DA_PROVENANCE: SDK.EvidenceProvenanceV1 = {
    trustClass: "public_or_permissionless_da",
    sourceId: "libp2p/peer-a",
    grade: "security",
  };

  /**
   * A block whose producer commits no outputs at all, so its single spender
   * challenges index 0. Built through the shared Q03 payload fixture so the
   * evidence really is DA + L1 derived.
   */
  const canonicalViolatingFixture = async (
    transactionsRootMode: "payloadSource" | "nativeCompact",
  ): Promise<CanonicalBlockFixtureV1> => {
    const producer = buildFixtureTransactionV1({
      spendInputs: [inputCbor(h32("99"), 0n)],
      fee: 1n,
    });
    const spender = buildFixtureTransactionV1({
      spendInputs: [inputCbor(producer.txId, 0n)],
      fee: 2n,
    });
    return await buildCanonicalBlockFixtureV1({
      transactionsRootMode,
      transactions: [producer, spender],
    });
  };

  const evidenceFor = async (
    fixture: CanonicalBlockFixtureV1,
  ): Promise<CanonicalBlockEvidenceV1> => {
    const payloadFixture =
      fixture.transactionsRootMode === "nativeCompact"
        ? await buildCanonicalBlockFixtureV1({
            transactions: fixture.transactions,
            startTime: fixture.header.startTime,
            endTime: fixture.header.endTime,
          })
        : fixture;
    const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation: authenticatedHeaderObservationV1(payloadFixture),
      payloadEnvelopeCbor: payloadFixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    if (fixture.transactionsRootMode !== "nativeCompact") {
      return evidence;
    }
    return {
      ...evidence,
      observation: authenticatedHeaderObservationV1(fixture),
      headerHash: fixture.headerHash,
      header: fixture.header,
      inclusionRootAuthentication:
        await authenticateTransactionsInclusionRootsV1({
          header: fixture.header,
          reconstruction: evidence.reconstruction,
          transactions: evidence.transactions,
        }),
    };
  };

  it("builds the proof from retained public DA bound to an authenticated L1 header", async () => {
    const fixture = await canonicalViolatingFixture("nativeCompact");
    const output = await prepareInputNoIdxFromCanonicalEvidenceV1({
      evidence: await evidenceFor(fixture),
    });

    expect(output.txCount).toBe(2);
    expect(output.evidence.isViolation).toBe(true);
    expect(output.evidence.producingTxOutputCount).toBe(0);
    expect(output.evidence.badInput.output_index).toBe(0n);
    expect(output.headerHash).toBe(fixture.headerHash);
    expect(output.committedTransactionsRoot).toBe(
      fixture.header.transactionsRoot,
    );
  });

  it("refuses evidence whose native inclusion root cannot be authenticated on-chain", async () => {
    const evidence = await evidenceFor(
      await canonicalViolatingFixture("payloadSource"),
    );
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromCanonicalEvidenceV1({ evidence }),
      ),
    ).toBe("native_inclusion_root_unauthenticated");
  });

  it("refuses evidence whose DA record was downgraded to an operator diagnostic", async () => {
    const evidence = await evidenceFor(
      await canonicalViolatingFixture("nativeCompact"),
    );
    const downgraded: CanonicalBlockEvidenceV1 = {
      ...evidence,
      provenance: {
        ...evidence.provenance,
        da: {
          trustClass: "operator_admin_api",
          sourceId: "midgard-node-url",
          grade: "diagnostic",
          diagnosticLabel: "operator REST diagnostic",
        },
      },
    };
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromCanonicalEvidenceV1({ evidence: downgraded }),
      ),
    ).toBe("prohibited_trust_class");
  });
});
