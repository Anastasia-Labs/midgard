import {
  encodeCbor,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import {
  committedFieldShapeEvidenceFromCommittedField,
  committedFieldShapeStep02StateFromEvidence,
  isCommittedFieldShapeViolation,
  MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
  MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
  MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
  MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
  midgardCommittedFieldShapeVerdict,
  sizedMidgardFieldEnvelope,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyCommittedFieldShapeFields,
  committedFieldShapeInlineClaimDetails,
  prepareCommittedFieldShapeFromCanonicalTx,
  submitCommittedFieldShapeCancel,
  submitCommittedFieldShapeInit,
  submitCommittedFieldShapeStep01,
  submitCommittedFieldShapeStep02,
} from "../src/committed-field-shape/index.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  committedFieldShapeInlineClaim,
  type CommittedFieldShapeScenario,
  expectCommittedFieldShapeOnchainRefusal,
  fundCommittedFieldShapeOutsider,
  makeCommittedFieldShapeEmulatorHarness,
  publishCommittedFieldShapeReferenceScripts,
  setupCommittedFieldShapeScenario,
  submitRawCommittedFieldShapeCancel,
  submitRawCommittedFieldShapeStep01,
  submitRawCommittedFieldShapeStep02,
} from "./support/committed-field-shape-emulator.js";
import {
  expectSingleUtxoWithUnit,
  makeNativeTx,
  network,
} from "./support/submit-init-emulator-shared.js";

const initThread = async (
  harness: CommittedFieldShapeEmulatorHarness,
  scenario: CommittedFieldShapeScenario,
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
    const canonical: MidgardNativeTxCanonical = {
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
    const fields = classifyCommittedFieldShapeFields(canonical);
    expect(fields).toHaveLength(9);
    expect(
      fields
        .filter(({ evidence }) => evidence.isViolation)
        .map(({ fieldIndex }) => fieldIndex),
    ).toStrictEqual([0, 1, 3, 4, 7]);
    for (const fieldIndex of [0, 1, 3, 4, 7]) {
      expect(fields[fieldIndex]!.evidence.verdict).toBe(
        MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
      );
    }
    for (const fieldIndex of [2, 5, 6, 8]) {
      expect(fields[fieldIndex]!.evidence.verdict).toBe(
        MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
      );
    }
    const witness = prepareCommittedFieldShapeFromCanonicalTx({
      tx: canonical,
      fieldIndex: 7,
    });
    expect("WitnessFieldClaim" in witness.claim).toBe(true);
    expect(witness.step02State).toMatchObject({
      field_index: 7n,
      verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE),
    });

    const oversize = sizedMidgardFieldEnvelope(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
      0x5a,
    );
    const oversizeVariable: MidgardNativeTxCanonical = {
      ...base,
      body: { ...base.body, outputsPreimageCbor: oversize },
    };
    expect(
      classifyCommittedFieldShapeFields(oversizeVariable)[2]!.evidence.verdict,
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND);
    const oversizeFixed: MidgardNativeTxCanonical = {
      ...base,
      body: { ...base.body, spendInputsPreimageCbor: oversize },
    };
    // Byte-bound precedes stride at their overlap.
    expect(
      classifyCommittedFieldShapeFields(oversizeFixed)[0]!.evidence.verdict,
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND);

    expect(
      midgardCommittedFieldShapeVerdict(2, Buffer.from("8041", "hex")),
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE);
    expect(
      isCommittedFieldShapeViolation({
        fieldIndex: 2,
        verdict: MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
      }),
    ).toBe(false);
    expect(isCommittedFieldShapeViolation({ fieldIndex: 9, verdict: 3 })).toBe(
      false,
    );
    expect(isCommittedFieldShapeViolation({ fieldIndex: 0, verdict: 4 })).toBe(
      false,
    );

    const tier1Frontier = sizedMidgardFieldEnvelope(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
      0x6a,
    );
    expect(
      committedFieldShapeInlineClaimDetails(
        committedFieldShapeInlineClaim({
          fieldIndex: 0,
          preimage: tier1Frontier,
        }),
      ).preimage,
    ).toHaveLength(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES);
    expect(() =>
      committedFieldShapeInlineClaimDetails(
        committedFieldShapeInlineClaim({
          fieldIndex: 0,
          preimage: sizedMidgardFieldEnvelope(
            MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES + 1,
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
      prepareCommittedFieldShapeFromCanonicalTx({
        tx: honest,
        fieldIndex: 0,
      }),
    ).toThrow(/nonConvictingField/u);
    expect(() =>
      prepareCommittedFieldShapeFromCanonicalTx({ tx: honest }),
    ).toThrow(/noViolation/u);

    const wrongAtOne = materializeMidgardNativeTxFromCanonical({
      ...honest,
      body: {
        ...honest.body,
        referenceInputsPreimageCbor: encodeCbor([Buffer.from([0x01])]),
      },
    });
    expect(
      prepareCommittedFieldShapeFromCanonicalTx({ tx: wrongAtOne }).evidence
        .fieldIndex,
    ).toBe(1);
    expect(() =>
      prepareCommittedFieldShapeFromCanonicalTx({
        tx: wrongAtOne,
        fieldIndex: 9,
      }),
    ).toThrow(/fieldIndexOutOfRange/u);
  });
});

describe("committed-field-shape adversarial prover and recovery", () => {
  it("refuses fabricated verdict and uncommitted bytes against an honest commitment at step-01", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const scenario = await setupCommittedFieldShapeScenario({
      harness,
      kind: "honest",
    });
    const refs = await publishCommittedFieldShapeReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.committedFieldShape,
    });
    const init = await initThread(harness, scenario);
    const honestEvidence = committedFieldShapeEvidenceFromCommittedField({
      badTxId: scenario.nativeTxId,
      fieldIndex: 0,
      committedPreimage: scenario.committedPreimage,
    });
    const honestPrepared = {
      evidence: honestEvidence,
      claim: committedFieldShapeInlineClaim({
        fieldIndex: 0,
        preimage: scenario.committedPreimage,
      }),
      step02State: committedFieldShapeStep02StateFromEvidence(honestEvidence),
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
      verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE),
    };
    const fabricatedMessage = await expectCommittedFieldShapeOnchainRefusal(
      () =>
        submitRawCommittedFieldShapeStep01({
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
    const uncommittedMessage = await expectCommittedFieldShapeOnchainRefusal(
      () =>
        submitRawCommittedFieldShapeStep01({
          harness,
          threadOutRef: init.nextThreadOutRef,
          scenario,
          claim: committedFieldShapeInlineClaim({
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
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const scenario = await setupCommittedFieldShapeScenario({
      harness,
      kind: "non-envelope",
    });
    const refs = await publishCommittedFieldShapeReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.committedFieldShape,
    });
    const init = await initThread(harness, scenario);
    const state = {
      bad_tx_id: scenario.nativeTxId,
      field_index: 2n,
      verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE),
    };
    const step01 = await submitRawCommittedFieldShapeStep01({
      harness,
      threadOutRef: init.nextThreadOutRef,
      scenario,
      claim: committedFieldShapeInlineClaim({
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
    const refusal = await expectCommittedFieldShapeOnchainRefusal(() =>
      submitRawCommittedFieldShapeStep02({
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
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const scenario = await setupCommittedFieldShapeScenario({
      harness,
      kind: "wrong-stride",
    });
    if (scenario.canonicalTx === null) {
      throw new Error("wrong-stride scenario must be canonical");
    }
    const prepared = prepareCommittedFieldShapeFromCanonicalTx({
      tx: scenario.canonicalTx,
      fieldIndex: 0,
    });
    const refs = await publishCommittedFieldShapeReferenceScripts({
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
    await fundCommittedFieldShapeOutsider(harness);
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
      "../src/committed-field-shape/submit-common.js"
    ).then(({ requireCommittedFieldShapeThreadUtxo }) =>
      requireCommittedFieldShapeThreadUtxo({
        lucid: harness.proverLucid,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        stepIndex: 0,
        threadOutRef: outsiderTarget.nextThreadOutRef,
      }),
    );
    const outsiderRefusal = await expectCommittedFieldShapeOnchainRefusal(() =>
      submitRawCommittedFieldShapeCancel({
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
