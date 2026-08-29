import {
  encodeCbor,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core";
import {
  committedFieldShapeEvidenceFromCommittedFieldV1,
  committedFieldShapeStep02StateFromEvidenceV1,
  isCommittedFieldShapeViolationV1,
  MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE_V1,
  MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND_V1,
  MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE_V1,
  MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE_V1,
  midgardCommittedFieldShapeVerdictV1,
  sizedMidgardFieldEnvelopeV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyCommittedFieldShapeFieldsV1,
  committedFieldShapeInlineClaimDetailsV1,
  prepareCommittedFieldShapeFromCanonicalTxV1,
  submitCommittedFieldShapeCancel,
  submitCommittedFieldShapeInit,
  submitCommittedFieldShapeStep01,
  submitCommittedFieldShapeStep02,
} from "../src/committed-field-shape/index.js";
import {
  type CommittedFieldShapeEmulatorHarnessV1,
  committedFieldShapeInlineClaimV1,
  type CommittedFieldShapeScenarioV1,
  expectCommittedFieldShapeOnchainRefusalV1,
  fundCommittedFieldShapeOutsiderV1,
  makeCommittedFieldShapeEmulatorHarnessV1,
  publishCommittedFieldShapeReferenceScriptsV1,
  setupCommittedFieldShapeScenarioV1,
  submitRawCommittedFieldShapeCancelV1,
  submitRawCommittedFieldShapeStep01V1,
  submitRawCommittedFieldShapeStep02V1,
} from "./support/committed-field-shape-emulator-v1.js";
import {
  expectSingleUtxoWithUnit,
  makeNativeTx,
  network,
} from "./support/submit-init-emulator-shared.js";

const initThread = async (
  harness: CommittedFieldShapeEmulatorHarnessV1,
  scenario: CommittedFieldShapeScenarioV1,
) =>
  await submitCommittedFieldShapeInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.committedFieldShape,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

describe("committed-field-shape constraint space", () => {
  it("classifies all nine slots, both shape rules, and the disjointness frontier", () => {
    const base = makeNativeTx({ spendInputCbors: [], fee: 0n });
    const wrong = encodeCbor([Buffer.from([0x01])]);
    const canonical: MidgardNativeTxCanonicalV1 = {
      ...base,
      body: {
        ...base.body,
        spendInputsPreimageCbor: wrong,
        referenceInputsPreimageCbor: wrong,
        requiredObserversPreimageCbor: wrong,
        requiredSignersPreimageCbor: wrong,
      },
      witnessSet: {
        ...base.witnessSet,
        addrTxWitsPreimageCbor: wrong,
      },
    };
    const fields = classifyCommittedFieldShapeFieldsV1(canonical);
    expect(fields).toHaveLength(9);
    expect(
      fields
        .filter(({ evidence }) => evidence.isViolation)
        .map(({ fieldIndex }) => fieldIndex),
    ).toStrictEqual([0, 1, 3, 4, 7]);
    for (const fieldIndex of [0, 1, 3, 4, 7]) {
      expect(fields[fieldIndex]!.evidence.verdict).toBe(
        MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE_V1,
      );
    }
    for (const fieldIndex of [2, 5, 6, 8]) {
      expect(fields[fieldIndex]!.evidence.verdict).toBe(
        MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE_V1,
      );
    }
    const witness = prepareCommittedFieldShapeFromCanonicalTxV1({
      tx: canonical,
      fieldIndex: 7,
    });
    expect("WitnessFieldClaim" in witness.claim).toBe(true);
    expect(witness.step02State).toMatchObject({
      field_index: 7n,
      verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE_V1),
    });

    const oversize = sizedMidgardFieldEnvelopeV1(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1 + 1,
      0x5a,
    );
    const oversizeVariable: MidgardNativeTxCanonicalV1 = {
      ...base,
      body: { ...base.body, outputsPreimageCbor: oversize },
    };
    expect(
      classifyCommittedFieldShapeFieldsV1(oversizeVariable)[2]!.evidence
        .verdict,
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND_V1);
    const oversizeFixed: MidgardNativeTxCanonicalV1 = {
      ...base,
      body: { ...base.body, spendInputsPreimageCbor: oversize },
    };
    // Byte-bound precedes stride at their overlap.
    expect(
      classifyCommittedFieldShapeFieldsV1(oversizeFixed)[0]!.evidence.verdict,
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND_V1);

    expect(
      midgardCommittedFieldShapeVerdictV1(2, Buffer.from("8041", "hex")),
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE_V1);
    expect(
      isCommittedFieldShapeViolationV1({
        fieldIndex: 2,
        verdict: MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE_V1,
      }),
    ).toBe(false);
    expect(
      isCommittedFieldShapeViolationV1({ fieldIndex: 9, verdict: 3 }),
    ).toBe(false);
    expect(
      isCommittedFieldShapeViolationV1({ fieldIndex: 0, verdict: 4 }),
    ).toBe(false);

    const tier1Frontier = sizedMidgardFieldEnvelopeV1(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      0x6a,
    );
    expect(
      committedFieldShapeInlineClaimDetailsV1(
        committedFieldShapeInlineClaimV1({
          fieldIndex: 0,
          preimage: tier1Frontier,
        }),
      ).preimage,
    ).toHaveLength(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1);
    expect(() =>
      committedFieldShapeInlineClaimDetailsV1(
        committedFieldShapeInlineClaimV1({
          fieldIndex: 0,
          preimage: sizedMidgardFieldEnvelopeV1(
            MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 + 1,
            0x6b,
          ),
        }),
      ),
    ).toThrow(/above the tier-1 frontier/u);
  });

  it("refuses honest pinned slots and picks the first violation deterministically", () => {
    const honest = makeNativeTx({
      spendInputCbors: [Buffer.alloc(38, 0xa5)],
      fee: 0n,
    });
    expect(() =>
      prepareCommittedFieldShapeFromCanonicalTxV1({
        tx: honest,
        fieldIndex: 0,
      }),
    ).toThrow(/nonConvictingField/u);
    expect(() =>
      prepareCommittedFieldShapeFromCanonicalTxV1({ tx: honest }),
    ).toThrow(/noViolation/u);

    const wrongAtOne = materializeMidgardNativeTxFromCanonicalV1({
      ...honest,
      body: {
        ...honest.body,
        referenceInputsPreimageCbor: encodeCbor([Buffer.from([0x01])]),
      },
    });
    expect(
      prepareCommittedFieldShapeFromCanonicalTxV1({ tx: wrongAtOne }).evidence
        .fieldIndex,
    ).toBe(1);
    expect(() =>
      prepareCommittedFieldShapeFromCanonicalTxV1({
        tx: wrongAtOne,
        fieldIndex: 9,
      }),
    ).toThrow(/fieldIndexOutOfRange/u);
  });
});

describe("committed-field-shape adversarial prover and recovery", () => {
  it("refuses fabricated verdict and uncommitted bytes against an honest commitment at step-01", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarnessV1();
    const scenario = await setupCommittedFieldShapeScenarioV1({
      harness,
      kind: "honest",
    });
    const refs = await publishCommittedFieldShapeReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.committedFieldShape,
    });
    const init = await initThread(harness, scenario);
    const honestEvidence = committedFieldShapeEvidenceFromCommittedFieldV1({
      badTxId: scenario.nativeTxId,
      fieldIndex: 0,
      committedPreimage: scenario.committedPreimage,
    });
    const honestPrepared = {
      evidence: honestEvidence,
      claim: committedFieldShapeInlineClaimV1({
        fieldIndex: 0,
        preimage: scenario.committedPreimage,
      }),
      step02State: committedFieldShapeStep02StateFromEvidenceV1(honestEvidence),
    };
    await expect(
      submitCommittedFieldShapeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        prepared: honestPrepared,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/non-convicting verdict admissible/u);

    const fabricated = {
      bad_tx_id: scenario.nativeTxId,
      field_index: 0n,
      verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE_V1),
    };
    const fabricatedMessage = await expectCommittedFieldShapeOnchainRefusalV1(
      () =>
        submitRawCommittedFieldShapeStep01V1({
          harness,
          threadOutRef: init.nextThreadOutRef,
          scenario,
          claim: honestPrepared.claim,
          forwardedState: fabricated,
          referenceScriptUtxo: refs[0],
        }),
    );
    expect(fabricatedMessage.length).toBeGreaterThan(0);

    const wrongPreimage = Buffer.from("8144deadbeef", "hex");
    const uncommittedMessage = await expectCommittedFieldShapeOnchainRefusalV1(
      () =>
        submitRawCommittedFieldShapeStep01V1({
          harness,
          threadOutRef: init.nextThreadOutRef,
          scenario,
          claim: committedFieldShapeInlineClaimV1({
            fieldIndex: 0,
            preimage: wrongPreimage,
          }),
          forwardedState: fabricated,
          referenceScriptUtxo: refs[0],
        }),
    );
    expect(uncommittedMessage.length).toBeGreaterThan(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.committedFieldShape.steps[0].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 600_000);

  it("binds a committed non-envelope but refuses it at the exact step-02 predicate", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarnessV1();
    const scenario = await setupCommittedFieldShapeScenarioV1({
      harness,
      kind: "non-envelope",
    });
    const refs = await publishCommittedFieldShapeReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.committedFieldShape,
    });
    const init = await initThread(harness, scenario);
    const state = {
      bad_tx_id: scenario.nativeTxId,
      field_index: 2n,
      verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE_V1),
    };
    const step01 = await submitRawCommittedFieldShapeStep01V1({
      harness,
      threadOutRef: init.nextThreadOutRef,
      scenario,
      claim: committedFieldShapeInlineClaimV1({
        fieldIndex: 2,
        preimage: scenario.committedPreimage,
      }),
      forwardedState: state,
      referenceScriptUtxo: refs[0],
    });
    const atStep02 = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.committedFieldShape.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    expect(`${atStep02.txHash}#${atStep02.outputIndex.toString()}`).toBe(
      step01.nextThreadOutRef,
    );

    await expect(
      submitCommittedFieldShapeStep02({
        lucid: harness.proverLucid,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/is not a committed-field-shape violation/u);
    const refusal = await expectCommittedFieldShapeOnchainRefusalV1(() =>
      submitRawCommittedFieldShapeStep02V1({
        harness,
        threadOutRef: step01.nextThreadOutRef,
        referenceScriptUtxo: refs[1],
      }),
    );
    expect(refusal.length).toBeGreaterThan(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.committedFieldShape.steps[1].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 600_000);

  it("cancels explicitly at both steps, refuses outsiders on both planes, and resumes only from live state", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarnessV1();
    const scenario = await setupCommittedFieldShapeScenarioV1({
      harness,
      kind: "wrong-stride",
    });
    if (scenario.canonicalTx === null) {
      throw new Error("wrong-stride scenario must be canonical");
    }
    const prepared = prepareCommittedFieldShapeFromCanonicalTxV1({
      tx: scenario.canonicalTx,
      fieldIndex: 0,
    });
    const refs = await publishCommittedFieldShapeReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.committedFieldShape,
    });

    const first = await initThread(harness, scenario);
    const cancelledAt01 = await submitCommittedFieldShapeCancel({
      lucid: harness.proverLucid,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: first.nextThreadOutRef,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelledAt01.cancelledStepIndex).toBe(0);
    await expect(
      submitCommittedFieldShapeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: first.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        prepared,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow();

    const second = await initThread(harness, scenario);
    const step01 = await submitCommittedFieldShapeStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: second.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.inclusion,
      prepared,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const cancelledAt02 = await submitCommittedFieldShapeCancel({
      lucid: harness.proverLucid,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelledAt02.cancelledStepIndex).toBe(1);

    const outsiderTarget = await initThread(harness, scenario);
    await fundCommittedFieldShapeOutsiderV1(harness);
    await expect(
      submitCommittedFieldShapeCancel({
        lucid: harness.outsiderLucid,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        signer: harness.outsiderSigner,
        threadOutRef: outsiderTarget.nextThreadOutRef,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/only the prover can cancel/u);
    const { threadUtxo, threadToken } = await import(
      "../src/committed-field-shape/submit-common-v1.js"
    ).then(({ requireCommittedFieldShapeThreadUtxoV1 }) =>
      requireCommittedFieldShapeThreadUtxoV1({
        lucid: harness.proverLucid,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        stepIndex: 0,
        threadOutRef: outsiderTarget.nextThreadOutRef,
      }),
    );
    const outsiderRefusal = await expectCommittedFieldShapeOnchainRefusalV1(
      () =>
        submitRawCommittedFieldShapeCancelV1({
          lucid: harness.outsiderLucid,
          contracts: harness.committedFieldShape,
          signer: harness.outsiderSigner,
          stepIndex: 0,
          threadUtxo,
          threadUnit: threadToken.unit,
          threadAssetName: threadToken.assetName,
          referenceScriptUtxo: refs[0],
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(outsiderRefusal.length).toBeGreaterThan(0);
    await submitCommittedFieldShapeCancel({
      lucid: harness.proverLucid,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outsiderTarget.nextThreadOutRef,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  }, 600_000);
});
