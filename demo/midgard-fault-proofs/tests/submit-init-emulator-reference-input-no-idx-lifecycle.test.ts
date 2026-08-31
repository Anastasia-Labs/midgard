/**
 * `reference-input-no-idx` emulator lifecycle (Goal task `Q31`).
 *
 * Drives the real Aiken step validators through a Lucid emulator with the
 * production submitters, in both polarities:
 *
 * - the real fault convicts end to end — init → step-01 (bad tx membership) →
 *   step-02 (open §2.5 field 1, forward the challenged reference input) →
 *   step-03 (producing tx membership) → step-04 (open §2.5 field 2, require
 *   `output_index >= |outputs|`, finalize) → permanent fraud-proof token →
 *   fraudulent-block removal;
 * - an adversary attacking an HONEST commitment is refused at the exact
 *   on-chain check, with the submitters' local guards bypassed by the raw
 *   drivers in `tests/support/reference-input-no-idx-emulator-v1.ts`.
 *
 * The committed evidence is always a **two-transaction block**: step-01 proves
 * the bad transaction's membership and step-03 proves the *producing*
 * transaction's membership under the same counted `transactions_root`, so both
 * leaves have to live in the same trie.
 *
 * Every step reads its validator from a published reference script (the
 * standing deployment ruling), and every submitter call passes it, so §8.7's
 * positional carriage indices resolve against the transaction's COMPLETE
 * canonically-sorted reference-input set rather than the carriage alone.
 *
 * Kept in its own file so the leaked wasm heap stays far below the ~4 GiB
 * wasm32 ceiling; see tests/support/uplc-heap-guard.ts.
 */
import {
  encodeMidgardFieldPreimageV1,
  midgardFieldCarriageBoundsV1,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonicalV1,
  encodeMidgardTxOutputCanonicalV1,
  fieldPreimagePublicationDatumCborV1,
  FraudProofTokenDatum,
  MIDGARD_FIELD_INDEX_V1,
  referenceInputNoIdxOutputsCommitmentV1,
  referenceInputNoIdxReferenceInputsCommitmentV1,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep03Datum,
  ReferenceInputNoIdxStep04Datum,
} from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpeningV1 } from "../src/field-opening-v1.js";
import {
  submitReferenceInputNoIdxStep01,
  submitReferenceInputNoIdxStep02,
  submitReferenceInputNoIdxStep03,
  submitReferenceInputNoIdxStep04,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  buildReferenceInputNoIdxBlockFixtureV1,
  expectOnchainRefusalV1,
  makeReferenceInputNoIdxEmulatorHarnessV1,
  publishReferenceInputNoIdxReferenceScriptsV1,
  REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT_V1,
  REFERENCE_INPUT_NO_IDX_TIER2_REFERENCE_INPUT_COUNT_V1,
  submitRawReferenceInputNoIdxStep02V1,
  submitRawReferenceInputNoIdxStep04V1,
} from "./support/reference-input-no-idx-emulator-v1.js";
import {
  expectStateQueueHeaderOrder,
  setupFraudulentBlockV1 as setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

/** The out-of-range output index the fraudulent reader claims by default. */
const CHALLENGED_OUTPUT_INDEX = 7n;

describe("reference-input-no-idx fault-proof emulator lifecycle", () => {
  it("proves and removes an out-of-range reference-input block end to end", async () => {
    const harness = await makeReferenceInputNoIdxEmulatorHarnessV1();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;

    // Removal must source its seven validators from reference inputs to stay
    // inside the 16,384-byte L1 envelope; the family's own four steps are
    // reference scripts per the standing deployment ruling.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishReferenceInputNoIdxReferenceScriptsV1({
        lucid: proverLucid,
        contracts: contracts.fraudProofContracts.referenceInputNoIdx,
      });

    // The normal producer commits one output, so index 7 cannot exist.
    const fixture = await buildReferenceInputNoIdxBlockFixtureV1({
      producingOutputCount: 1,
    });
    expect(fixture.producingOutputsCbor).toHaveLength(1);
    expect(
      referenceInputNoIdxOutputsCommitmentV1(fixture.producingOutputs),
    ).toBe(fixture.producingTxOutputsHash);
    expect(
      referenceInputNoIdxReferenceInputsCommitmentV1(fixture.referenceInputs),
    ).toBe(fixture.badTxReferenceInputsHash);
    expect(fixture.challengedReferenceInput.tx_id).toBe(fixture.producingTxId);

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
      claimRegistrySpendReference:
        harness.witnessReferenceScripts.claimRegistrySpend,
    });

    // ## init
    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "referenceInputNoIdx",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("referenceInputNoIdx");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.referenceInputNoIdx.categoryId,
    );

    const proverPaymentKeyHash = proverSigner.paymentKeyHash;
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // ## step-01: bind the BAD transaction to the committed header
    const step01Result = await submitReferenceInputNoIdxStep01({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      referenceScriptUtxo: step01Ref,
      awaitConfirmation: true,
    });
    expect(step01Result.badTxId).toBe(fixture.badTxId);
    expect(step01Result.verifiedTxId).toBe(fixture.badTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(
      Data.from(secondStepUtxo.datum!, ReferenceInputNoIdxStep02Datum),
    ).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: { verified_tx_id: fixture.badTxId },
    });

    // ## step-02: open §2.5 field 1 and forward the challenged reference input
    const step02Capture = await captureEmulatorSubmission(
      emulator,
      async () =>
        await submitReferenceInputNoIdxStep02({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          referenceInputsPreimage: {
            referenceInputsPreimage: fixture.referenceInputs,
            badReferenceInputIndex: fixture.challengedReferenceInputIndex,
          },
          nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
          referenceScriptUtxo: step02Ref,
          awaitConfirmation: true,
        }),
    );
    const step02Result = step02Capture.result;
    // A one-item field-1 preimage is far inside §8.4's tier-1 bound, so the
    // whole field rides in the step's own redeemer, so no carriage publication
    // transaction is needed.
    expect(step02Capture.transactionCbors).toHaveLength(1);
    expect(step02Result.verifiedTxReferenceInputsHash).toBe(
      fixture.badTxReferenceInputsHash,
    );
    expect(step02Result.badReferenceInputTxId).toBe(fixture.producingTxId);
    expect(step02Result.badReferenceInputOutputIndex).toBe(
      Number(CHALLENGED_OUTPUT_INDEX),
    );

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    expect(
      Data.from(thirdStepUtxo.datum!, ReferenceInputNoIdxStep03Datum),
    ).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        bad_reference_input_tx_id: fixture.producingTxId,
        bad_reference_input_output_index: CHALLENGED_OUTPUT_INDEX,
      },
    });

    // ## step-03: bind the PRODUCING transaction from the same block
    const step03Result = await submitReferenceInputNoIdxStep03({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      referenceScriptUtxo: step03Ref,
      awaitConfirmation: true,
    });
    expect(step03Result.producingTxId).toBe(fixture.producingTxId);

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(
      Data.from(fourthStepUtxo.datum!, ReferenceInputNoIdxStep04Datum),
    ).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        producing_tx_id: fixture.producingTxId,
        bad_reference_input_output_index: CHALLENGED_OUTPUT_INDEX,
      },
    });

    // ## step-04: open §2.5 field 2 and mint the permanent token
    const step04Result = await submitReferenceInputNoIdxStep04({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      outputsPreimage: { outputsPreimage: fixture.producingOutputs },
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      referenceScriptUtxo: step04Ref,
      awaitConfirmation: true,
    });
    expect(step04Result.producingTxOutputCount).toBe(1);
    expect(step04Result.producingTxOutputsHash).toBe(
      fixture.producingTxOutputsHash,
    );
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
      fraudCategory: "referenceInputNoIdx",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removeResult.fraudCategory).toBe("referenceInputNoIdx");
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    // The state-queue node NFT is burned with the block it authenticated.
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    // The slashed operator's active-operator node is gone too.
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    // The fraud-proof token survives removal untouched at the same out-ref:
    // permanent evidence, not a burnable receipt.
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);

    // A second removal claim finds nothing left to remove.
    await expect(
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "referenceInputNoIdx",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);

  it("routes an oversized field-1 reference-input preimage through tier-2 published carriage to the conviction", async () => {
    // Size alone selects the tier. 359 canonical out-refs put §2.5 field 1's
    // §5.1 preimage at `3 + 40 * 359 = 14,363` bytes — the FIRST count past
    // §8.4's 14,336-byte tier-1 redeemer bound, and inside the
    // single-publication window — so the ladder routes to `RawUtxo`. Nothing
    // forces it; the committed data's size does. Field 2 stays tiny, so the
    // same journey pins tier-2 and tier-1 selection side by side.
    const harness = await makeReferenceInputNoIdxEmulatorHarnessV1();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishReferenceInputNoIdxReferenceScriptsV1({
        lucid: proverLucid,
        contracts: contracts.fraudProofContracts.referenceInputNoIdx,
      });

    const fixture = await buildReferenceInputNoIdxBlockFixtureV1({
      producingOutputCount: 1,
      referenceInputCount:
        REFERENCE_INPUT_NO_IDX_TIER2_REFERENCE_INPUT_COUNT_V1,
    });
    expect(fixture.referenceInputs).toHaveLength(
      REFERENCE_INPUT_NO_IDX_TIER2_REFERENCE_INPUT_COUNT_V1,
    );
    const referenceInputItems = fixture.referenceInputs.map(
      encodeMidgardTxInputCanonicalV1,
    );
    const preimage = encodeMidgardFieldPreimageV1(referenceInputItems);
    expect(preimage.length).toBeGreaterThan(
      midgardFieldCarriageBoundsV1.maxTier1RedeemerPreimageBytes,
    );
    expect(preimage.length).toBeLessThanOrEqual(
      midgardFieldCarriageBoundsV1.maxPublishableCarriageBytes,
    );
    // The plan the submitter itself will make, from exactly its own inputs.
    const plannedField01 = planFaultProofFieldOpeningV1({
      fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs,
      anchorTxId: fixture.badTxId,
      nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
      itemCbors: referenceInputItems,
      owner: proverSigner.paymentKeyHash,
      label: "field-1 tier probe",
    });
    expect(plannedField01.plan.tier).toBe("RawUtxo");

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      claimRegistrySpendReference:
        harness.witnessReferenceScripts.claimRegistrySpend,
    });
    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "referenceInputNoIdx",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Result = await submitReferenceInputNoIdxStep01({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      referenceScriptUtxo: step01Ref,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );

    const step02Capture = await captureEmulatorSubmission(
      emulator,
      async () =>
        await submitReferenceInputNoIdxStep02({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          referenceInputsPreimage: {
            referenceInputsPreimage: fixture.referenceInputs,
            badReferenceInputIndex: fixture.challengedReferenceInputIndex,
          },
          nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
          referenceScriptUtxo: step02Ref,
          awaitConfirmation: true,
        }),
    );
    const step02Result = step02Capture.result;
    // Tier 2 is two transactions, in this order: the §8.5 publication first,
    // then the step that references it. Reference inputs resolve against the
    // UTxO set as it stands *before* a transaction, so they cannot share one.
    expect(step02Capture.transactionCbors).toHaveLength(2);
    // The successful RawUtxo opening audits the COMPLETE reference-input set:
    // the call above supplies the published step script, while the submitter
    // adds the carriage and resolves its canonical index across both UTxOs.
    // Omitting either reference would make the real validator reject.
    expect(step02Result.referenceInputsPreimageItemCount).toBe(
      REFERENCE_INPUT_NO_IDX_TIER2_REFERENCE_INPUT_COUNT_V1,
    );
    expect(step02Result.badReferenceInputTxId).toBe(fixture.producingTxId);

    // The whole §5.1 preimage sits at the prover's address as a bytes-only
    // inline datum (§8.5), referenced rather than spent (§8.7).
    const expectedDatum = fieldPreimagePublicationDatumCborV1(preimage);
    const publications = (
      await proverLucid.utxosAt(proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await submitReferenceInputNoIdxStep03({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      referenceScriptUtxo: step03Ref,
      awaitConfirmation: true,
    });
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    const step04Result = await submitReferenceInputNoIdxStep04({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      outputsPreimage: { outputsPreimage: fixture.producingOutputs },
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      referenceScriptUtxo: step04Ref,
      awaitConfirmation: true,
    });
    expect(step04Result.producingTxOutputCount).toBe(1);
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
    });
  }, 600_000);

  it("routes an oversized field-2 outputs preimage through tier-2 published carriage to the conviction", async () => {
    // The reader claims the index one past a producer that really has 334
    // outputs — still out of range, so the violation stands, and the outputs
    // preimage step-04 must open is now `3 + 43 * 334 = 14,365` bytes: the
    // FIRST output count past §8.4's tier-1 redeemer bound. Field 1 stays a
    // one-item preimage, so tier-1 and tier-2 selection are pinned side by
    // side in one journey, each decided by size alone.
    const harness = await makeReferenceInputNoIdxEmulatorHarnessV1();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishReferenceInputNoIdxReferenceScriptsV1({
        lucid: proverLucid,
        contracts: contracts.fraudProofContracts.referenceInputNoIdx,
      });

    const fixture = await buildReferenceInputNoIdxBlockFixtureV1({
      producingOutputCount:
        REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT_V1,
      challengedOutputIndex: BigInt(
        REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT_V1,
      ),
    });
    const outputItems = fixture.producingOutputs.map(
      encodeMidgardTxOutputCanonicalV1,
    );
    const preimage = encodeMidgardFieldPreimageV1(outputItems);
    expect(preimage.length).toBeGreaterThan(
      midgardFieldCarriageBoundsV1.maxTier1RedeemerPreimageBytes,
    );
    expect(preimage.length).toBeLessThanOrEqual(
      midgardFieldCarriageBoundsV1.maxPublishableCarriageBytes,
    );
    expect(
      referenceInputNoIdxOutputsCommitmentV1(fixture.producingOutputs),
    ).toBe(fixture.producingTxOutputsHash);
    const plannedField02 = planFaultProofFieldOpeningV1({
      fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
      anchorTxId: fixture.producingTxId,
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      itemCbors: outputItems,
      owner: proverSigner.paymentKeyHash,
      label: "field-2 tier probe",
    });
    expect(plannedField02.plan.tier).toBe("RawUtxo");

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      claimRegistrySpendReference:
        harness.witnessReferenceScripts.claimRegistrySpend,
    });
    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "referenceInputNoIdx",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Result = await submitReferenceInputNoIdxStep01({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      referenceScriptUtxo: step01Ref,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Result = await submitReferenceInputNoIdxStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      referenceInputsPreimage: {
        referenceInputsPreimage: fixture.referenceInputs,
        badReferenceInputIndex: fixture.challengedReferenceInputIndex,
      },
      nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
      referenceScriptUtxo: step02Ref,
      awaitConfirmation: true,
    });
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await submitReferenceInputNoIdxStep03({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      referenceScriptUtxo: step03Ref,
      awaitConfirmation: true,
    });
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );

    const step04Capture = await captureEmulatorSubmission(
      emulator,
      async () =>
        await submitReferenceInputNoIdxStep04({
          lucid: proverLucid,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(fourthStepUtxo),
          outputsPreimage: { outputsPreimage: fixture.producingOutputs },
          nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
          referenceScriptUtxo: step04Ref,
          awaitConfirmation: true,
        }),
    );
    const step04Result = step04Capture.result;
    expect(step04Capture.transactionCbors).toHaveLength(2);
    // The successful RawUtxo opening is the wiring audit again: the submitter
    // resolves the carriage index across the carriage plus the step-04
    // reference script supplied above, and the real validator accepts it.
    expect(step04Result.producingTxOutputCount).toBe(
      REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT_V1,
    );
    expect(step04Result.badReferenceInputOutputIndex).toBe(
      REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT_V1,
    );

    const expectedDatum = fieldPreimagePublicationDatumCborV1(preimage);
    const publications = (
      await proverLucid.utxosAt(proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
    });
  }, 600_000);

  it("refuses every attack on an honest commitment at the validator's own check", async () => {
    // The adversarial polarity. The committed block is HONEST: the reader's
    // reference input names index 0 of a producer that really has an output at
    // index 0. Steps 01-03 carry no verdict — an honest block advances just as
    // far, which is why the family's adjudication lives in step 04.
    const harness = await makeReferenceInputNoIdxEmulatorHarnessV1();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishReferenceInputNoIdxReferenceScriptsV1({
        lucid: proverLucid,
        contracts: contracts.fraudProofContracts.referenceInputNoIdx,
      });

    const fixture = await buildReferenceInputNoIdxBlockFixtureV1({
      producingOutputCount: 1,
      challengedOutputIndex: 0n,
      referenceInputCount: 2,
    });
    expect(fixture.referenceInputs).toHaveLength(2);
    expect(
      referenceInputNoIdxOutputsCommitmentV1(fixture.producingOutputs),
    ).toBe(fixture.producingTxOutputsHash);

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      claimRegistrySpendReference:
        harness.witnessReferenceScripts.claimRegistrySpend,
    });
    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "referenceInputNoIdx",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Result = await submitReferenceInputNoIdxStep01({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      referenceScriptUtxo: step01Ref,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );

    // ## Attack 1 — a field-1 preimage the transaction never committed.
    // Refused off-chain by the door's own twin before a transaction is built:
    // `planFaultProofFieldOpeningV1`'s `field_commitment_at` check, which names
    // the SLOT because under §4 identical items commit identically in fields 0
    // and 1.
    await expect(
      submitReferenceInputNoIdxStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        referenceInputsPreimage: {
          referenceInputsPreimage: [
            { tx_id: fixture.producingTxId, output_index: 7n },
          ],
          badReferenceInputIndex: 0,
        },
        nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
        referenceScriptUtxo: step02Ref,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/the disputed transaction commits at §2\.5 field 1/u);

    // ## Attack 2 — §7.3 abort-never-clamp at step-02's own call site.
    // The opening is honest, so the door admits it; only the selection index is
    // out of range. The forwarded reference input is deliberately the LAST
    // committed item, so a validator that CLAMPED instead of aborting would
    // produce exactly this transaction and succeed. The refusal is therefore
    // attributable to `spend_input_at`'s range guard in step-02, and to nothing
    // else. The production submitter cannot express this — it bounds-checks the
    // index — so a raw driver builds it.
    const lastReferenceInput =
      fixture.referenceInputs[fixture.referenceInputs.length - 1]!;
    await expectOnchainRefusalV1(
      async () =>
        await submitRawReferenceInputNoIdxStep02V1({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          referenceInputsPreimage: fixture.referenceInputs,
          badReferenceInputIndex: fixture.referenceInputs.length + 3,
          forwardedReferenceInput: lastReferenceInput,
          nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
          referenceScriptUtxo: step02Ref,
        }),
    );

    // The honest steps 02 and 03 still advance: no verdict lives in them.
    const step02Result = await submitReferenceInputNoIdxStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      referenceInputsPreimage: {
        referenceInputsPreimage: fixture.referenceInputs,
        badReferenceInputIndex: fixture.challengedReferenceInputIndex,
      },
      nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
      referenceScriptUtxo: step02Ref,
      awaitConfirmation: true,
    });
    expect(step02Result.badReferenceInputOutputIndex).toBe(0);
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await submitReferenceInputNoIdxStep03({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      referenceScriptUtxo: step03Ref,
      awaitConfirmation: true,
    });
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );

    // ## Attack 3 — the honest-block verdict, off-chain plane. Index 0 exists
    // in the producing transaction, so `isReferenceInputNoIdxViolationV1`
    // refuses finalization before a transaction is built.
    await expect(
      submitReferenceInputNoIdxStep04({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(fourthStepUtxo),
        outputsPreimage: { outputsPreimage: fixture.producingOutputs },
        nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
        referenceScriptUtxo: step04Ref,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(
      /an existing transaction reference input cannot be proven non-existent/u,
    );

    // ## Attack 4 — the same claim with that local guard bypassed, so it
    // reaches step-04's OWN check:
    // `bad_reference_input_output_index >= field_item_count(outputs_view)`,
    // which is `0 >= 1` here. This is the primary adversarial leg: an adversary
    // who rewrites the builder still cannot convict an honest commitment.
    await expectOnchainRefusalV1(
      async () =>
        await submitRawReferenceInputNoIdxStep04V1({
          lucid: proverLucid,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          blueprint: realBlueprint,
          deploymentInfo,
          signer: proverSigner,
          threadOutRef: outRefLabel(fourthStepUtxo),
          outputsPreimage: fixture.producingOutputs,
          nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
          referenceScriptUtxo: step04Ref,
        }),
    );

    // ## Attack 5 — the obvious way to fake an out-of-range index: strip the
    // producer's outputs so the count reads zero. The §5.1 preimage then
    // commits to the empty-field constant, which is not what that transaction
    // commits AT FIELD 2, so the door refuses before the count is ever read.
    await expect(
      submitReferenceInputNoIdxStep04({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(fourthStepUtxo),
        outputsPreimage: { outputsPreimage: [] },
        nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
        referenceScriptUtxo: step04Ref,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/the disputed transaction commits at §2\.5 field 2/u);

    // The thread is stuck at step 04, no fraud-proof token exists, and the
    // honest block is still queued.
    const stillFourthStep = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(stillFourthStep)).toBe(outRefLabel(fourthStepUtxo));
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        toUnit(
          contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
  }, 600_000);
});
