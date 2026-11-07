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
 * Lives in its own file for the reason its siblings do. The split was made
 * while `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on
 * every script evaluation and vitest isolates per FILE; that leak is fixed
 * upstream, and the split is kept so each file runs in its own fresh process.
 */

import "node:crypto";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/index.js";
import "./support/legacy-submit-emulator.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./submit-init-emulator-input-no-idx.build-input-no-idx-block-fixture.js";
import "./submit-init-emulator-input-no-idx.measure-step02-proof-transaction.js";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  fieldPreimagePublicationDatumCbor,
  FraudProofTokenDatum,
  inputNoIdxOutputsCommitment,
  inputNoIdxSpendInputsCommitment,
  InputNoIdxStep02Datum,
  InputNoIdxStep03Datum,
  InputNoIdxStep04Datum,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
  toUnit,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  midgardTxOutputFromCanonicalCbor,
  submitInputNoIdxStep01,
  submitInputNoIdxStep02,
  submitInputNoIdxStep03,
  submitInputNoIdxStep04,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  buildInputNoIdxBlockFixture,
  CHALLENGED_OUTPUT_INDEX,
  HALF_CANONICAL_MATURITY_MS,
  inputCbor,
  makeEmulatorHarness,
  TIER2_PRODUCING_OUTPUT_COUNT,
  TIER2_SPEND_INPUT_COUNT,
} from "./submit-init-emulator-input-no-idx.build-input-no-idx-block-fixture.js";
import {
  expectStep02ProofFit,
  measureStep02ProofTransaction,
  startInputNoIdxStep02Thread,
} from "./submit-init-emulator-input-no-idx.measure-step02-proof-transaction.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  expectStateQueueHeaderOrder,
  setupFraudulentBlock as setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

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
    expect(inputNoIdxOutputsCommitment([])).toBe(
      fixture.producingTxOutputsHash,
    );
    expect(inputNoIdxSpendInputsCommitment([fixture.badInput])).toBe(
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

    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      removalReferenceScripts: removalReferenceScriptPublications.published,
    });

    // ## init
    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
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
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofNonExistentInputNoIndex!
          .utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
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
          referenceScriptUtxo:
            harness.faultProofReferenceScripts
              .fraudProofNonExistentInputNoIndexStep02!.utxo,
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
    expect(directProofMeasurement.referenceInputCount).toBe(1);
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
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep03!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
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
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep04!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
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
    //
    // 365 spend inputs put the §5.1 field-0 preimage at 14,603 bytes — past
    // §8.4's 14,336-byte tier-1 redeemer bound, inside the single-publication
    // window — so the ladder itself routes to `RawUtxo`. Nothing forces the
    // tier; the committed data's size does.
    const harness = await makeEmulatorHarness();
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 0,
      badSpendInputCount: TIER2_SPEND_INPUT_COUNT,
    });
    const preimageBytes = encodeMidgardFieldPreimage(
      fixture.badInputs.map((input) =>
        inputCbor(input.tx_id, input.output_index),
      ),
    ).length;
    expect(preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(preimageBytes).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K);
    const { deploymentInfo, secondStepUtxo } =
      await startInputNoIdxStep02Thread({ harness, fixture });

    const proofStartedAt = performance.now();
    const proofCapture = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitInputNoIdxStep02({
          lucid: harness.proverLucid,
          referenceScriptUtxo:
            harness.faultProofReferenceScripts
              .fraudProofNonExistentInputNoIndexStep02!.utxo,
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
    // The consuming transaction reads the carriage it published plus its
    // mandatory step reference script, without spending either one.
    expect(proofMeasurement.referenceInputCount).toBe(2);
    const signedProofTransaction =
      CML.Transaction.from_cbor_hex(proofTransactionCbor);
    const proofReferenceInputs = signedProofTransaction
      .body()
      .reference_inputs();
    expect(proofReferenceInputs?.len()).toBe(2);
    const [carriageTxHash, carriageOutputIndex] =
      proofResult.carriageOutRefs[0]!.split("#");
    expect(
      Array.from({ length: proofReferenceInputs?.len() ?? 0 }, (_, index) =>
        proofReferenceInputs!.get(index),
      ).some(
        (input) =>
          input.transaction_id().to_hex() === carriageTxHash &&
          input.index() === BigInt(carriageOutputIndex!),
      ),
    ).toBe(true);
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

  it("routes an oversized field-2 outputs preimage through tier-2 published carriage to the conviction", async () => {
    // The spender claims an output index one past a producer that really has
    // 340 outputs — still out of range, so the violation stands, and the
    // outputs preimage step-04 must open is now genuinely large: past §8.4's
    // tier-1 redeemer bound, inside the single-publication window. The ladder
    // itself routes field 2 to `RawUtxo`; nothing forces the tier. Field 0
    // stays a one-item preimage, so the same journey pins tier-1 and tier-2
    // selection side by side, each decided by size alone.
    const harness = await makeEmulatorHarness();
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: TIER2_PRODUCING_OUTPUT_COUNT,
      challengedOutputIndex: BigInt(TIER2_PRODUCING_OUTPUT_COUNT),
    });
    const producingOutputItems = fixture.producingOutputsCbor.map((item) =>
      Buffer.from(item, "hex"),
    );
    const preimage = encodeMidgardFieldPreimage(producingOutputItems);
    expect(preimage.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(preimage.length).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K);
    const producingOutputs = producingOutputItems.map(
      midgardTxOutputFromCanonicalCbor,
    );
    expect(inputNoIdxOutputsCommitment(producingOutputs)).toBe(
      fixture.producingTxOutputsHash,
    );

    const { deploymentInfo, initResult, secondStepUtxo, setup } =
      await startInputNoIdxStep02Thread({ harness, fixture });
    const step02Result = await submitInputNoIdxStep02({
      lucid: harness.proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep02!.utxo,
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
    });
    expect(step02Result.carriageTier).toBe("Inline");
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await submitInputNoIdxStep03({
      lucid: harness.proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep03!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      awaitConfirmation: true,
    });
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    const step04Result = await submitInputNoIdxStep04({
      lucid: harness.proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep04!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      outputsPreimage: { outputsPreimage: producingOutputs },
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      awaitConfirmation: true,
    });
    expect(step04Result.carriageTier).toBe("RawUtxo");
    expect(step04Result.producingTxOutputCount).toBe(
      TIER2_PRODUCING_OUTPUT_COUNT,
    );

    // The tier-2 publication really exists and survives its consumer: the
    // whole §5.1 preimage sits at the prover's address as a bytes-only inline
    // datum (§8.5), referenced rather than spent (§8.7).
    const expectedDatum = fieldPreimagePublicationDatumCbor(preimage);
    const publications = (
      await harness.proverLucid.utxosAt(harness.proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });
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
          referenceScriptUtxo:
            harness.faultProofReferenceScripts
              .fraudProofNonExistentInputNoIndexStep02!.utxo,
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
      midgardTxOutputFromCanonicalCbor(Buffer.from(item, "hex")),
    );
    expect(inputNoIdxOutputsCommitment(producingOutputs)).toBe(
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
      witnessReferenceScripts: harness.witnessReferenceScripts,
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
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofNonExistentInputNoIndex!
          .utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
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
        referenceScriptUtxo:
          harness.faultProofReferenceScripts
            .fraudProofNonExistentInputNoIndexStep02!.utxo,
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
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep02!.utxo,
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
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep03!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
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
        referenceScriptUtxo:
          harness.faultProofReferenceScripts
            .fraudProofNonExistentInputNoIndexStep04!.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
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
        referenceScriptUtxo:
          harness.faultProofReferenceScripts
            .fraudProofNonExistentInputNoIndexStep04!.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
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
