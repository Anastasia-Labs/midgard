/**
 * `input-no-idx` emulator lifecycle (Goal task `Q13`, §9.1 output 9).
 *
 * Drives the real Aiken step validators through a Lucid emulator with the
 * production submitters: init -> step-01 -> step-02 -> step-03 -> step-04 ->
 * permanent fraud-proof token -> fraudulent block removal, plus the
 * valid-block negative on both reachable planes.
 *
 * The committed evidence is a two-transaction block: a **producing**
 * transaction and a **spender** that spends `(producer_tx_id, output_index)`
 * where `output_index` is at or past the end of the producer's canonical
 * outputs list. The producer's id preimage *is* in the block, which is exactly
 * what distinguishes this family from `non-existent-input`.
 *
 * Kept in its own file so the leaked wasm heap stays far below the ~4 GiB
 * wasm32 ceiling; see tests/support/uplc-heap-guard.ts.
 */

import { createHash } from "node:crypto";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { outRefLabel } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core/codec";
import {
  FraudProofTokenDatum,
  inputNoIdxOutputsCommitmentV1,
  inputNoIdxSpendInputsCommitmentV1,
  InputNoIdxStep02Datum,
  InputNoIdxStep03Datum,
  InputNoIdxStep04Datum,
  type MidgardTxInput,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type EmulatorAccount,
  getAddressDetails,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  midgardTxOutputFromCanonicalCborV1,
  submitInputNoIdxStep01,
  submitInputNoIdxStep02,
  submitInputNoIdxStep03,
  submitInputNoIdxStep04,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import type { SubmitStep01TxInclusion } from "../src/submit-step-01.js";
import {
  nativeTxFromCoreCompact,
  submitInit,
} from "./support/legacy-submit-emulator.js";
import {
  expectStateQueueHeaderOrder,
  setupFraudulentBlockV1 as setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  makeFaultProofEmulatorHarnessV1,
  makeNativeTx,
  network,
  publishRemovalReferenceScripts,
  trieRootHex,
} from "./support/submit-init-emulator-shared.js";

/** The out-of-range output index the fraudulent spender claims. */
const CHALLENGED_OUTPUT_INDEX = 7n;

// Public BIP39 vectors used only by this emulator test. They are not secrets
// and must never fund a real wallet.
const TEST_ONLY_FUNDER_SEED =
  "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
const TEST_ONLY_PROVER_SEED =
  "test test test test test test test test test test test junk";
const FIXED_EMULATOR_UNIX_MS = 1_735_689_600_000;

const fixedBaseEmulatorAccount = (
  seedPhrase: string,
  lovelace: bigint,
): EmulatorAccount => ({
  seedPhrase,
  privateKey: "",
  address: walletFromSeed(seedPhrase, {
    addressType: "Base",
    accountIndex: 0,
    network: "Custom",
  }).address,
  assets: { lovelace },
});

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

/** One canonical native output: enterprise pubkey address, lovelace only. */
const nativeOutputCbor = (paymentByte: number, lovelace: bigint): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, paymentByte),
    Buffer.from([0x01, 0x82]),
    encodeCbor(lovelace),
    Buffer.from([0xa0]),
  ]);

type InputNoIdxBlockFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly producingTxId: string;
  readonly producingTxOutputsHash: string;
  readonly producingOutputsCbor: readonly string[];
  readonly badTxId: string;
  readonly badInput: MidgardTxInput;
  readonly badInputs: readonly MidgardTxInput[];
  readonly badInputsIndex: number;
  readonly verifiedTxInputsHash: string;
  readonly badTxInclusion: SubmitStep01TxInclusion;
  readonly producingTxInclusion: SubmitStep01TxInclusion;
};

/**
 * Commits a producer with `producingOutputs` canonical outputs and a spender
 * of `(producerTxId, challengedOutputIndex)` as the two native-compact leaves
 * of one block's transactions MPF. The block carries the `input-no-idx`
 * violation exactly when `challengedOutputIndex >= producingOutputs.length`.
 */
const buildInputNoIdxBlockFixture = async ({
  producingOutputCount,
  challengedOutputIndex = CHALLENGED_OUTPUT_INDEX,
  badSpendInputCount = 1,
}: {
  readonly producingOutputCount: number;
  readonly challengedOutputIndex?: bigint;
  readonly badSpendInputCount?: number;
}): Promise<InputNoIdxBlockFixture> => {
  if (!Number.isSafeInteger(badSpendInputCount) || badSpendInputCount <= 0) {
    throw new Error("badSpendInputCount must be a positive safe integer");
  }
  const producingOutputs = Array.from(
    { length: producingOutputCount },
    (_, index) => nativeOutputCbor(0x40 + index, 5_000_000n + BigInt(index)),
  );
  const producingTx = makeNativeTx({
    spendInputCbors: [inputCbor("99".repeat(32), 0n)],
    fee: 7n,
    ...(producingOutputs[0] === undefined
      ? {}
      : { outputCbor: producingOutputs[0] }),
  });
  const producingTxId = computeMidgardNativeTxIdV1(producingTx).toString("hex");
  const challengedInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: challengedOutputIndex,
  };
  const badInputs = [
    challengedInput,
    ...Array.from({ length: badSpendInputCount - 1 }, (_, index) => ({
      tx_id: (index + 1).toString(16).padStart(64, "0"),
      output_index: 0n,
    })),
  ].sort((left, right) =>
    Buffer.compare(
      inputCbor(left.tx_id, left.output_index),
      inputCbor(right.tx_id, right.output_index),
    ),
  );
  const badInputsIndex = badInputs.findIndex(
    (input) =>
      input.tx_id === challengedInput.tx_id &&
      input.output_index === challengedInput.output_index,
  );
  if (badInputsIndex < 0) {
    throw new Error("Expected challenged input in canonical bad input list");
  }
  const badTx = makeNativeTx({
    spendInputCbors: badInputs.map((input) =>
      inputCbor(input.tx_id, input.output_index),
    ),
    fee: 9n,
  });
  const badTxId = computeMidgardNativeTxIdV1(badTx).toString("hex");

  const producingCompactCbor = encodeMidgardNativeTxCompactV1(
    producingTx.compact,
  );
  const badCompactCbor = encodeMidgardNativeTxCompactV1(badTx.compact);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(Buffer.from(producingTxId, "hex"), producingCompactCbor);
  await trie.insert(Buffer.from(badTxId, "hex"), badCompactCbor);
  const producingProof = await trie.prove(Buffer.from(producingTxId, "hex"));
  const badProof = await trie.prove(Buffer.from(badTxId, "hex"));
  const transactionsRoot = trieRootHex(trie);
  const producingCompact = nativeTxFromCoreCompact(producingTx.compact);
  const badCompact = nativeTxFromCoreCompact(badTx.compact);

  const inclusionFor = (
    nativeTxId: string,
    compactCbor: Buffer,
    proofCbor: string,
    compact: typeof producingTx.compact,
  ): SubmitStep01TxInclusion => ({
    nativeTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor: compactCbor.toString("hex"),
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(proofCbor, Proof),
    txMembershipProofCbor: proofCbor,
  });

  return {
    transactionsRoot,
    l2TransactionCount: 2n,
    producingTxId,
    producingTxOutputsHash: producingCompact.body.outputs_hash,
    producingOutputsCbor: producingOutputs.map((item) => item.toString("hex")),
    badTxId,
    badInput: challengedInput,
    badInputs,
    badInputsIndex,
    verifiedTxInputsHash: badCompact.body.spend_inputs_hash,
    badTxInclusion: inclusionFor(
      badTxId,
      badCompactCbor,
      badProof.toCBOR().toString("hex"),
      badTx.compact,
    ),
    producingTxInclusion: inclusionFor(
      producingTxId,
      producingCompactCbor,
      producingProof.toCBOR().toString("hex"),
      producingTx.compact,
    ),
  };
};

const makeEmulatorHarness = async () =>
  await makeFaultProofEmulatorHarnessV1({
    contractOptions: { realInputNoIdx: true, alwaysFraudProofCatalogue: true },
    accounts: {
      funder: fixedBaseEmulatorAccount(TEST_ONLY_FUNDER_SEED, 40_000_000_000n),
      prover: fixedBaseEmulatorAccount(TEST_ONLY_PROVER_SEED, 20_000_000_000n),
    },
    emulatorTimeMs: FIXED_EMULATOR_UNIX_MS,
  });

const STEP02_RELEASE_MEMORY_LIMIT = 13_200_000n;
const STEP02_RELEASE_CPU_LIMIT = 8_000_000_000n;
const HALF_CANONICAL_MATURITY_MS = 302_400_000;
/*
 * The byte-exact `CompletePublished` proof-fit pin that stood here is **gone,
 * not relaxed.** It measured a transaction shape that no longer exists: the
 * retired route referenced a bespoke `PublishedSpendInputsV1` datum, and #604
 * replaced it with §8.5 raw carriage under a different redeemer. Every measured
 * quantity — signed bytes, fee, execution units, the CBOR sha256 — moves with
 * that change, so re-pinning here would mean inventing numbers rather than
 * measuring them.
 *
 * Re-measurement is **#580**, which this ticket blocks by owner order precisely
 * so it runs against working builders. Until then the structural fit assertions
 * (`expectStep02ProofFit`, the release memory/cpu margins, the reference-input
 * shape) are what hold, and they are the ones that would catch a regression in
 * kind rather than in degree.
 */

const measureStep02ProofTransaction = ({
  transactionCbor,
  outputIndex,
  elapsedMs,
}: {
  readonly transactionCbor: string;
  readonly outputIndex: number;
  readonly elapsedMs?: number;
}) => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const output = body.outputs().get(outputIndex);
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionCpu = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    executionMemory += units.mem();
    executionCpu += units.steps();
  }
  const signedTxBytes = transactionCbor.length / 2;
  const outputValueBytes = output.amount().to_cbor_bytes().length;
  const localBuildSubmitConfirmWallMs =
    elapsedMs === undefined ? undefined : Number(elapsedMs.toFixed(3));
  return {
    signedTxBytes,
    signedTxSha256: createHash("sha256")
      .update(Buffer.from(transactionCbor, "hex"))
      .digest("hex"),
    txByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize - signedTxBytes,
    fee: body.fee(),
    executionMemory,
    executionCpu,
    releaseMemoryMargin: STEP02_RELEASE_MEMORY_LIMIT - executionMemory,
    releaseCpuMargin: STEP02_RELEASE_CPU_LIMIT - executionCpu,
    inputCount: body.inputs().len(),
    referenceInputCount: body.reference_inputs()?.len() ?? 0,
    outputCount: body.outputs().len(),
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
    redeemerCount: redeemers?.len() ?? 0,
    outputLovelace: output.amount().coin(),
    outputMinAda: CML.min_ada_required(
      output,
      BigInt(PROTOCOL_PARAMETERS_DEFAULT.coinsPerUtxoByte),
    ),
    outputValueBytes,
    valueByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxValSize - outputValueBytes,
    ...(localBuildSubmitConfirmWallMs === undefined
      ? {}
      : {
          localBuildSubmitConfirmWallMs,
          localHalfMaturityMarginMs:
            HALF_CANONICAL_MATURITY_MS - localBuildSubmitConfirmWallMs,
        }),
  };
};

const expectStep02ProofFit = (
  measurement: ReturnType<typeof measureStep02ProofTransaction>,
): void => {
  expect(measurement.txByteMargin).toBeGreaterThanOrEqual(0);
  expect(measurement.releaseMemoryMargin).toBeGreaterThanOrEqual(0n);
  expect(measurement.releaseCpuMargin).toBeGreaterThanOrEqual(0n);
  expect(measurement.outputLovelace).toBeGreaterThanOrEqual(
    measurement.outputMinAda,
  );
  expect(measurement.valueByteMargin).toBeGreaterThanOrEqual(0);
  expect(measurement.vkeyWitnessCount).toBe(1);
  expect(measurement.redeemerCount).toBe(1);
  if (measurement.localBuildSubmitConfirmWallMs !== undefined) {
    expect(measurement.localBuildSubmitConfirmWallMs).toBeGreaterThan(0);
    expect(measurement.localHalfMaturityMarginMs).toBeGreaterThan(0);
  }
};

const startInputNoIdxStep02Thread = async ({
  harness,
  fixture,
}: {
  readonly harness: Awaited<ReturnType<typeof makeEmulatorHarness>>;
  readonly fixture: InputNoIdxBlockFixture;
}) => {
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
  );
  const initResult = await submitInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "nonExistentInputNoIndex",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const step01Result = await submitInputNoIdxStep01({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.badTxInclusion,
    awaitConfirmation: true,
  });
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    step01Result.secondStepAddress,
    initResult.computationThreadUnit,
  );
  return {
    deploymentInfo,
    initResult,
    secondStepUtxo,
    setup,
    step01Result,
  };
};

describe("input-no-idx fault-proof emulator lifecycle", () => {
  it("proves and removes an out-of-range spend-input block end to end", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;

    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });

    // The producer commits no outputs at all, so index 7 cannot exist.
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 0,
    });
    expect(fixture.producingOutputsCbor).toHaveLength(0);
    expect(inputNoIdxOutputsCommitmentV1([])).toBe(
      fixture.producingTxOutputsHash,
    );
    expect(inputNoIdxSpendInputsCommitmentV1([fixture.badInput])).toBe(
      fixture.verifiedTxInputsHash,
    );

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );

    // ## init
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInputNoIndex",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("nonExistentInputNoIndex");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.nonExistentInputNoIndex.categoryId,
    );

    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // ## step-01: bind the bad transaction to the committed header
    const step01Result = await submitInputNoIdxStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      awaitConfirmation: true,
    });
    expect(step01Result.badTxId).toBe(fixture.badTxId);
    // #604: the thread carries the §2.5 anchor, not field 0's commitment.
    expect(step01Result.verifiedTxId).toBe(fixture.badTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(secondStepUtxo.datum!, InputNoIdxStep02Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: { verified_tx_id: fixture.badTxId },
    });

    // ## step-02: open the spend-inputs commitment and forward the input
    const directStartedAt = performance.now();
    const directCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        await submitInputNoIdxStep02({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          inputsPreimage: {
            inputsPreimage: [fixture.badInput],
            badInputsIndex: 0,
          },
          nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
          awaitConfirmation: true,
        }),
    );
    const directElapsedMs = performance.now() - directStartedAt;
    const step02Result = directCapture.result;
    const directProofMeasurement = measureStep02ProofTransaction({
      transactionCbor: directCapture.transactionCbors[0]!,
      outputIndex: step02Result.outputIndex,
      elapsedMs: directElapsedMs,
    });
    expectStep02ProofFit(directProofMeasurement);
    expect(directProofMeasurement.referenceInputCount).toBe(0);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { q13DirectConsumingProof: directProofMeasurement },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
    expect(step02Result.badInputTxId).toBe(fixture.producingTxId);
    expect(step02Result.badInputOutputIndex).toBe(
      Number(CHALLENGED_OUTPUT_INDEX),
    );

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(thirdStepUtxo.datum!, InputNoIdxStep03Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        bad_input_tx_id: fixture.producingTxId,
        bad_input_output_index: CHALLENGED_OUTPUT_INDEX,
      },
    });

    // ## step-03: bind the producing transaction from the same block
    const step03Result = await submitInputNoIdxStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      awaitConfirmation: true,
    });
    expect(step03Result.producingTxId).toBe(fixture.producingTxId);

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(fourthStepUtxo.datum!, InputNoIdxStep04Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        producing_tx_id: fixture.producingTxId,
        bad_input_output_index: CHALLENGED_OUTPUT_INDEX,
      },
    });

    // ## step-04: open the outputs commitment and mint the permanent token
    const step04Result = await submitInputNoIdxStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      outputsPreimage: { outputsPreimage: [] },
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      awaitConfirmation: true,
    });
    expect(step04Result.producingTxOutputCount).toBe(0);
    expect(step04Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step04Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step03Result.fourthStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    // ## removal: the proven block leaves the state queue, the token stays
    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInputNoIndex",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("nonExistentInputNoIndex");
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);
  }, 240_000);

  it("carries field 0 as §8.5 raw carriage and consumes it through the door", async () => {
    // The §8 replacement for the retired `CompletePublished` route. That route
    // referenced a bespoke `PublishedSpendInputsV1` datum bound to one
    // computation thread and one prover; this one references a
    // nothing-but-bytes §8.5 publication located by content, which is what makes
    // it healable by anyone (§8.7).
    const harness = await makeEmulatorHarness();
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 0,
    });
    const { deploymentInfo, secondStepUtxo } =
      await startInputNoIdxStep02Thread({ harness, fixture });

    const proofStartedAt = performance.now();
    const proofCapture = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitInputNoIdxStep02({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          inputsPreimage: {
            inputsPreimage: fixture.badInputs,
            badInputsIndex: fixture.badInputsIndex,
          },
          nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
          // The one tier choice §8 leaves open: spend a prior transaction's
          // bytes rather than this one's.
          publishCarriage: true,
          awaitConfirmation: true,
        }),
    );
    const proofElapsedMs = performance.now() - proofStartedAt;
    const proofResult = proofCapture.result;
    // Tier 2 is two transactions, in this order: §8.5 publication first, then the
    // step that references it. Reference inputs are resolved against the UTxO set
    // as it stands *before* a transaction, so they cannot share one — that is a
    // ledger rule, not a builder limitation.
    expect(proofCapture.transactionCbors).toHaveLength(2);
    const proofTransactionCbor = proofCapture.transactionCbors[1]!;
    const proofMeasurement = measureStep02ProofTransaction({
      transactionCbor: proofTransactionCbor,
      outputIndex: proofResult.outputIndex,
      elapsedMs: proofElapsedMs,
    });

    expect(proofResult.carriageTier).toBe("RawUtxo");
    expect(proofResult.carriageOutRefs).toHaveLength(1);
    // The consuming transaction reads exactly the carriage it published, and
    // reads it as a reference input rather than spending it.
    expect(proofMeasurement.referenceInputCount).toBe(1);
    const signedProofTransaction =
      CML.Transaction.from_cbor_hex(proofTransactionCbor);
    const proofReferenceInputs = signedProofTransaction
      .body()
      .reference_inputs();
    expect(proofReferenceInputs?.len()).toBe(1);
    const [carriageTxHash, carriageOutputIndex] =
      proofResult.carriageOutRefs[0]!.split("#");
    expect(proofReferenceInputs?.get(0).transaction_id().to_hex()).toBe(
      carriageTxHash,
    );
    expect(proofReferenceInputs?.get(0).index()).toBe(
      BigInt(carriageOutputIndex!),
    );
    expectStep02ProofFit(proofMeasurement);
    // The publication survives its consumer: §8.7 carriage is referenced, never
    // spent, so a second dispute over the same field reuses it.
    await expect(
      harness.proverLucid.utxosByOutRef([
        {
          txHash: carriageTxHash!,
          outputIndex: Number(carriageOutputIndex!),
        },
      ]),
    ).resolves.toHaveLength(1);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { q13Tier2Carriage: { consumingProof: proofMeasurement } },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
  }, 240_000);

  it("opens a 20-input field in one transaction, where the fold took twenty", async () => {
    // The §8 replacement for the retired ordered fold. Twenty inputs are 800
    // bytes of §5.1 preimage — far inside §8.4's 14,336-byte tier-1 bound — so
    // the whole field rides in the step's own redeemer and the thread advances
    // to step 03 in a single transaction. The fold existed only because the
    // collection had to be reproduced inside the step to re-hash it.
    const harness = await makeEmulatorHarness();
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 0,
      badSpendInputCount: 20,
    });
    expect(fixture.badInputs).toHaveLength(20);
    const { deploymentInfo, secondStepUtxo } =
      await startInputNoIdxStep02Thread({ harness, fixture });

    const startedAt = performance.now();
    const capture = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitInputNoIdxStep02({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          inputsPreimage: {
            inputsPreimage: fixture.badInputs,
            badInputsIndex: fixture.badInputsIndex,
          },
          nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
          awaitConfirmation: true,
        }),
    );
    const elapsedMs = performance.now() - startedAt;
    const result = capture.result;

    // One transaction, not twenty.
    expect(capture.transactionCbors).toHaveLength(1);
    expect(result.carriageTier).toBe("Inline");
    expect(result.carriageOutRefs).toHaveLength(0);
    expect(result.inputsPreimageItemCount).toBe(20);
    // And it lands at step 03 directly.
    const transaction = CML.Transaction.from_cbor_hex(
      capture.transactionCbors[0]!,
    );
    const threadOutput = coreToTxOutput(
      transaction.body().outputs().get(result.outputIndex),
    );
    expect(Data.from(threadOutput.datum!, InputNoIdxStep03Datum)).toEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        bad_input_tx_id: fixture.badInput.tx_id,
        bad_input_output_index: fixture.badInput.output_index,
      },
    });
    const measurement = measureStep02ProofTransaction({
      transactionCbor: capture.transactionCbors[0]!,
      outputIndex: result.outputIndex,
      elapsedMs,
    });
    expectStep02ProofFit(measurement);
    expect(
      HALF_CANONICAL_MATURITY_MS - measurement.localBuildSubmitConfirmWallMs!,
    ).toBeGreaterThan(0);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { q13Inline20: { measurement } },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
  }, 240_000);

  it("cannot finalize an input-no-idx thread against a valid block", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;

    // A valid block: the spender takes index 0 of a producer that really has
    // an output at index 0.
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 1,
      challengedOutputIndex: 0n,
    });
    const producingOutputs = fixture.producingOutputsCbor.map((item) =>
      midgardTxOutputFromCanonicalCborV1(Buffer.from(item, "hex")),
    );
    expect(inputNoIdxOutputsCommitmentV1(producingOutputs)).toBe(
      fixture.producingTxOutputsHash,
    );

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInputNoIndex",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // Steps 01-03 carry no verdict: a valid block advances just as far, which
    // is why the family's adjudication lives in step 04.
    const step01Result = await submitInputNoIdxStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );

    // Plane 1 — off-chain fail-closed at the preimage opening: a preimage that
    // does not open the committed spend-inputs hash is refused before any
    // transaction is built.
    await expect(
      submitInputNoIdxStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        inputsPreimage: {
          inputsPreimage: [{ tx_id: fixture.producingTxId, output_index: 7n }],
          badInputsIndex: 0,
        },
        nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/the disputed transaction commits at §2\.5 field 0/u);

    const step02Result = await submitInputNoIdxStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      inputsPreimage: {
        inputsPreimage: [fixture.badInput],
        badInputsIndex: 0,
      },
      nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
      awaitConfirmation: true,
    });
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await submitInputNoIdxStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      awaitConfirmation: true,
    });
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );

    // Plane 2 — the valid-block verdict: index 0 exists in the producing
    // transaction, so finalization is refused off-chain...
    await expect(
      submitInputNoIdxStep04({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(fourthStepUtxo),
        outputsPreimage: { outputsPreimage: producingOutputs },
        nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(
      /an existing transaction input cannot be proven non-existent/u,
    );

    // ...and on-chain, if a prover strips the producer's outputs to fake an
    // empty list, the outputs commitment no longer opens.
    await expect(
      submitInputNoIdxStep04({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(fourthStepUtxo),
        outputsPreimage: { outputsPreimage: [] },
        nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
        awaitConfirmation: true,
      }),
      // #604: the refusal now names the slot as well as the mismatch. Stripping
      // the producer's outputs to fake an empty list produces a §5.1 preimage
      // that commits to the empty-field constant, which is not what that
      // transaction commits *at field 2* — and under §4 that constant is the
      // same 32 bytes in all nine slots, so naming the slot is what makes the
      // refusal mean anything.
    ).rejects.toThrow(/the disputed transaction commits at §2\.5 field 2/u);

    // The thread is stuck at step 04 and the valid block is still queued.
    const stillFourthStep = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(stillFourthStep)).toBe(outRefLabel(fourthStepUtxo));
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
  }, 240_000);
});
