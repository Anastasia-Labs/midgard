/**
 * The reference-input-no-idx count reader past the walked count limit: a
 * producer with 334 outputs, published under tier 2, and a reader of output
 * index 334. Step 04 counts field 2 through the field grammar certificate, so
 * its cost does not grow with the field's item count, and convicts.
 *
 * Lives beside the lifecycle file so each runs in its own fresh process.
 */
import {
  encodeMidgardFieldPreimage,
  midgardFieldCarriageBounds,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutputCanonical,
  FraudProofTokenDatum,
  MIDGARD_FIELD_INDEX,
  referenceInputNoIdxOutputsCommitment,
} from "@al-ft/midgard-sdk/fraud-proof";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk/state-queue";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  fieldGrammarOfCarriagePlan,
  planFieldGrammarBatches,
} from "../src/field-grammar-certificate.certify-fault-proof-field-grammar.js";
import { FIELD_GRAMMAR_WALKED_COUNT_LIMIT } from "../src/field-grammar-certificate.route-fault-proof-field-grammar.js";
import { planFaultProofFieldOpening } from "../src/field-opening.plan-fault-proof-field-opening.js";
import { buildInit } from "../src/submit-init.submit-init.js";
import { buildReferenceInputNoIdxStep01 } from "../src/submit-reference-input-no-idx-step-01.js";
import { buildReferenceInputNoIdxStep02 } from "../src/submit-reference-input-no-idx-step-02.submit-reference-input-no-idx-step02.js";
import { buildReferenceInputNoIdxStep03 } from "../src/submit-reference-input-no-idx-step-03.js";
import { buildReferenceInputNoIdxStep04 } from "../src/submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04.js";
import { network } from "./support/emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import { deployFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import {
  buildReferenceInputNoIdxBlockFixture,
  makeReferenceInputNoIdxEmulatorHarness,
  REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT,
} from "./support/reference-input-no-idx-emulator.build-reference-input-no-idx-block-fixture.js";
import { publishReferenceInputNoIdxReferenceScripts } from "./support/reference-input-no-idx-emulator.submit-raw-reference-input-no-idx-step02.js";
import { setupFraudulentBlock } from "./support/submit-init/removal.js";
import { buildAndSubmit, submitTurns } from "./support/submit-steps.js";

describe("reference-input-no-idx step 04 through the field grammar certificate", () => {
  it("routes an oversized field-2 outputs preimage through tier-2 published carriage to the conviction", async () => {
    // The reader claims the index one past a producer that really has 334
    // outputs — still out of range, so the violation stands, and the outputs
    // preimage step-04 must open is now `3 + 43 * 334 = 14,365` bytes: the
    // FIRST output count past §8.4's tier-1 redeemer bound. Field 1 stays a
    // one-item preimage, so tier-1 and tier-2 selection are pinned side by
    // side in one journey, each decided by size alone.
    const harness = await makeReferenceInputNoIdxEmulatorHarness();
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
      await publishReferenceInputNoIdxReferenceScripts({
        lucid: proverLucid,
        contracts: contracts.fraudProofContracts.referenceInputNoIdx,
      });

    const fixture = await buildReferenceInputNoIdxBlockFixture({
      producingOutputCount: REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT,
      challengedOutputIndex: BigInt(
        REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT,
      ),
    });
    const outputItems = fixture.producingOutputs.map(
      encodeMidgardTxOutputCanonical,
    );
    const preimage = encodeMidgardFieldPreimage(outputItems);
    expect(preimage.length).toBeGreaterThan(
      midgardFieldCarriageBounds.maxTier1RedeemerPreimageBytes,
    );
    expect(preimage.length).toBeLessThanOrEqual(
      midgardFieldCarriageBounds.maxPublishableCarriageBytes,
    );
    expect(referenceInputNoIdxOutputsCommitment(fixture.producingOutputs)).toBe(
      fixture.producingTxOutputsHash,
    );
    const plannedField02 = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: MIDGARD_FIELD_INDEX.outputs,
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
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {});
    const initResult = await buildAndSubmit(buildInit, {
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
    const step01Result = await buildAndSubmit(buildReferenceInputNoIdxStep01, {
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
    const step02Result = await submitTurns(buildReferenceInputNoIdxStep02, {
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
    const step03Result = await buildAndSubmit(buildReferenceInputNoIdxStep03, {
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

    // 334 outputs is past the walked count limit, so step 04 counts field 2
    // through the field grammar certificate, issued over the same tier-2
    // publication.
    expect(REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT).toBeGreaterThan(
      FIELD_GRAMMAR_WALKED_COUNT_LIMIT,
    );
    // The deployed certificate, resolved through the startup resolver the
    // production workflow runs.
    const { fieldGrammar } = await deployFieldGrammarCertificate({
      lucid: funderLucid,
      blueprint: realBlueprint,
      network,
      fieldPreimageCertificatePolicyId:
        contracts.fieldPreimageCertificate.policyId,
      label: "nonExistentReferenceInputNoIndex",
    });
    const step04Capture = await captureEmulatorSubmission(
      emulator,
      async () =>
        await submitTurns(buildReferenceInputNoIdxStep04, {
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
          witnessSetCompactCbor: fixture.producingWitnessSetCompactCbor,
          fieldGrammar,
          awaitConfirmation: true,
        }),
    );
    const step04Result = step04Capture.result;
    // The publication, one certificate transaction per grammar batch, then
    // step 04.
    expect(step04Capture.transactionCbors).toHaveLength(
      2 +
        planFieldGrammarBatches(fieldGrammarOfCarriagePlan(plannedField02.plan))
          .length,
    );
    // The successful RawUtxo opening is the wiring audit again: the submitter
    // resolves the carriage index across the carriage plus the step-04
    // reference script supplied above, and the real validator accepts it.
    expect(step04Result.producingTxOutputCount).toBe(
      REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT,
    );
    expect(step04Result.badReferenceInputOutputIndex).toBe(
      REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT,
    );

    const expectedDatum = fieldPreimagePublicationDatumCbor(preimage);
    const publications = (
      await proverLucid.utxosAt(proverSigner.proofMaterialsAddress)
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
});
