/**
 * The count-reader end to end. Past the walked count limit, input-no-idx
 * step 04 counts the producer's outputs field through the field grammar
 * certificate: over a tier-2 publication, and over a tier-3 chunked field,
 * since the certified count also serves a chunked field.
 */
import {
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import {
  fieldGrammarCertificateUnit,
  inputNoIdxOutputsCommitment,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk/fraud-proof";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk/state-queue";
import { describe, expect, it } from "vitest";

import { fieldGrammarCertificateAddress } from "../src/field-grammar-certificate.certify-fault-proof-field-grammar.js";
import { FIELD_GRAMMAR_WALKED_COUNT_LIMIT } from "../src/field-grammar-certificate.route-fault-proof-field-grammar.js";
import { planFaultProofFieldOpening } from "../src/field-opening.plan-fault-proof-field-opening.js";
import { midgardTxOutputFromCanonicalCbor } from "../src/prepare-input-no-idx.midgard-tx-output-from-canonical-cbor.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { buildInputNoIdxStep04 } from "../src/submit-input-no-idx-step-04.submit-input-no-idx-step04.js";
import {
  buildInputNoIdxBlockFixture,
  makeEmulatorHarness,
  TIER2_PRODUCING_OUTPUT_COUNT,
} from "./submit-init-emulator-input-no-idx.build-input-no-idx-block-fixture.js";
import { network } from "./support/emulator/blueprints.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";
import { deployHarnessFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import {
  advanceInputNoIdxToStep04,
  expectInputNoIdxConviction,
} from "./support/input-no-idx-step04-thread.js";
import {
  certifyFieldCarriage,
  publishFieldCarriage,
  submitTurns,
} from "./support/submit-steps.js";

/** 761 producing outputs of 43 preimage bytes fill field 2 to the maximum. */
const TIER3_PRODUCING_OUTPUT_COUNT = 761;
const RESERVED_MEMORY = (VAN_ROSSEM_MAX_MEMORY_UNITS * 4n) / 5n;
const RESERVED_CPU = (VAN_ROSSEM_MAX_CPU_UNITS * 4n) / 5n;
const LABEL = "Input-no-idx step 04 outputs";

/**
 * A tier-3 thread at step 04: 761 producing outputs, published in chunks
 * under their §8.6 certificate, and the field grammar certificate deployed.
 */
const buildTier3Scenario = async () => {
  const harness = await makeEmulatorHarness();
  const fixture = await buildInputNoIdxBlockFixture({
    producingOutputCount: TIER3_PRODUCING_OUTPUT_COUNT,
    challengedOutputIndex: BigInt(TIER3_PRODUCING_OUTPUT_COUNT),
  });
  const itemCbors = fixture.producingOutputsCbor.map((item) =>
    Buffer.from(item, "hex"),
  );
  expect(encodeMidgardFieldPreimage(itemCbors).length).toBeGreaterThan(
    MIDGARD_CHUNK_BYTES_K,
  );
  const { deploymentInfo, fourthStepOutRef } = await advanceInputNoIdxToStep04(
    harness,
    fixture,
  );

  // Tier 3: the chunks and their §8.6 certificate exist before step 04.
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: fixture.producingTxId,
    nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
    itemCbors,
    owner: harness.proverSigner.paymentKeyHash,
    label: LABEL,
  });
  expect(planned.plan.tier).toBe("Certified");
  const chunkUtxos = await publishFieldCarriage({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.proofMaterialsAddress,
    label: LABEL,
  });
  const preimageCertificate = harness.contracts.fieldPreimageCertificate;
  const { utxo: preimageCertificateReference } =
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: preimageCertificate.mintingScript,
      label: "field preimage certificate",
    });
  const { certificateUtxo } = await certifyFieldCarriage({
    lucid: harness.proverLucid,
    network,
    signer: harness.proverSigner,
    planned,
    certificatePolicyId: preimageCertificate.policyId,
    certificateMintingScript: preimageCertificate.mintingScript,
    certificateReferenceScriptUtxo: preimageCertificateReference,
    chunkUtxos,
    compactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
    witnessSetCompactCbor: fixture.producingWitnessSetCompactCbor,
  });
  // The deployed certificate, resolved through the startup resolver the
  // production input-no-idx workflow runs.
  const grammar = await deployHarnessFieldGrammarCertificate(
    harness,
    "nonExistentInputNoIndex",
  );

  const step04 = (fieldGrammar: typeof grammar.fieldGrammar | undefined) =>
    submitTurns(buildInputNoIdxStep04, {
      lucid: harness.proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep04!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: fourthStepOutRef,
      outputsPreimage: {
        outputsPreimage: itemCbors.map(midgardTxOutputFromCanonicalCbor),
      },
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: fixture.producingWitnessSetCompactCbor,
      publishedCarriageUtxos: chunkUtxos,
      certificateUtxo,
      awaitConfirmation: true,
      ...(fieldGrammar === undefined ? {} : { fieldGrammar }),
    });
  return { harness, grammar, step04 };
};

let tier3Scenario: ReturnType<typeof buildTier3Scenario> | undefined;
const tier3ScenarioOf = () => (tier3Scenario ??= buildTier3Scenario());

describe("input-no-idx step 04 through the field grammar certificate", () => {
  it("routes an oversized field-2 outputs preimage through tier-2 published carriage to the conviction", async () => {
    // The spender claims an output index one past a producer that really has
    // 340 outputs — still out of range, so the violation stands, and the
    // outputs preimage step-04 must open is now genuinely large: past §8.4's
    // tier-1 redeemer bound, inside the single-publication window. The ladder
    // itself routes field 2 to `RawUtxo`; nothing forces the tier. Field 0
    // stays a one-item preimage, so the same journey pins tier-1 and tier-2
    // selection side by side, each decided by size alone. 340 outputs is past
    // the walked count limit, so step 04 counts field 2 through the field
    // grammar certificate, issued over the same tier-2 publication.
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
    expect(TIER2_PRODUCING_OUTPUT_COUNT).toBeGreaterThan(
      FIELD_GRAMMAR_WALKED_COUNT_LIMIT,
    );
    const producingOutputs = producingOutputItems.map(
      midgardTxOutputFromCanonicalCbor,
    );
    expect(inputNoIdxOutputsCommitment(producingOutputs)).toBe(
      fixture.producingTxOutputsHash,
    );

    const { deploymentInfo, fourthStepOutRef } =
      await advanceInputNoIdxToStep04(harness, fixture);
    const step04Result = await submitTurns(buildInputNoIdxStep04, {
      lucid: harness.proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts
          .fraudProofNonExistentInputNoIndexStep04!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: fourthStepOutRef,
      outputsPreimage: { outputsPreimage: producingOutputs },
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: fixture.producingWitnessSetCompactCbor,
      fieldGrammar: (
        await deployHarnessFieldGrammarCertificate(
          harness,
          "nonExistentInputNoIndex",
        )
      ).fieldGrammar,
      awaitConfirmation: true,
    });
    expect(step04Result.carriageTier).toBe("RawUtxo");
    expect(step04Result.producingTxOutputCount).toBe(
      TIER2_PRODUCING_OUTPUT_COUNT,
    );

    // The tier-2 publication really exists and survives its consumer: the whole
    // §5.1 preimage sits at the prover's proof-materials address as a
    // bytes-only inline datum (§8.5), referenced rather than spent (§8.7).
    const expectedDatum = fieldPreimagePublicationDatumCbor(preimage);
    const publications = (
      await harness.proverLucid.utxosAt(
        harness.proverSigner.proofMaterialsAddress,
      )
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);
    await expectInputNoIdxConviction(harness, step04Result);
  }, 240_000);

  it("fails on chain when step 04 walks a 761-output chunked field without the certificate", async () => {
    // The submitter's walked route, reached only by a direct call that passes
    // no certificate deployment: a walk does not count a chunked field, so
    // the step aborts on chain.
    const { step04 } = await tier3ScenarioOf();
    await expect(step04(undefined)).rejects.toThrow(
      /Spend\[\d+\] the validator crashed/u,
    );
  }, 1_800_000);

  it("counts a 761-output chunked field through the deployed certificate and convicts", async () => {
    const { harness, grammar, step04 } = await tier3ScenarioOf();
    const convicted = await captureEmulatorSubmission(harness.emulator, () =>
      step04(grammar.fieldGrammar),
    );
    expect(convicted.measurement.executionMemory).toBeLessThanOrEqual(
      RESERVED_MEMORY,
    );
    expect(convicted.measurement.executionSteps).toBeLessThanOrEqual(
      RESERVED_CPU,
    );
    expect(convicted.result.carriageTier).toBe("Certified");
    expect(convicted.result.producingTxOutputCount).toBe(
      TIER3_PRODUCING_OUTPUT_COUNT,
    );

    const grammarCertificates = (
      await harness.proverLucid.utxosAt(
        fieldGrammarCertificateAddress({
          network,
          certificatePolicyId: grammar.policyId,
        }),
      )
    ).filter(
      (utxo) =>
        (utxo.assets[fieldGrammarCertificateUnit(grammar.policyId)] ?? 0n) ===
        1n,
    );
    expect(grammarCertificates).toHaveLength(1);
    await expectInputNoIdxConviction(harness, convicted.result);
  }, 1_800_000);
});
