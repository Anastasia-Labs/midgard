/**
 * `input-set-uniqueness` adversarial polarity: honest commitments survive.
 *
 * Two soundness fronts, each against a commitment no honest prover could
 * convict:
 *
 * 1. An operator-accepted transaction whose input sets ARE unique and
 *    disjoint. The scan finds nothing; init and step-01 still land (any
 *    accepted committed leaf binds); the honest step-02 submitter refuses
 *    every fabricated claim locally; and raw finalizes that skip the local
 *    twins are refused by the VALIDATOR at the exact decisive check —
 *    item byte-equality for a fabricated duplicate/overlap, `i < j` for a
 *    reflexive pair, and the §8.8 door's `index < count` range gate.
 * 2. A committed leaf the operator honestly recorded as REJECTED
 *    (`validity_code != 0`). However degenerate its input sets, the family
 *    must never convict a no-op: the honest step-01 submitter refuses
 *    locally, and a raw bind that skips the guard dies on the validator's
 *    own `validity_code == 0` expect.
 *
 * Lives in its own file for the reason its siblings do. The split was made
 * while `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on
 * every script evaluation and vitest isolates per FILE; that leak is fixed
 * upstream, and the split is kept so each file runs in its own fresh process.
 */
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  faultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../src/field-opening.js";
import {
  requireInputSetUniquenessClaim,
  scanInputSetUniqueness,
  submitInputSetUniquenessInit,
  submitInputSetUniquenessStep01,
  submitInputSetUniquenessStep02,
} from "../src/input-set-uniqueness/index.js";
import type { FaultProofWitnessReferenceScripts } from "../src/witness-reference-scripts.js";
import {
  buildInputSetUniquenessFixture,
  expectOnchainRefusal,
  type InputSetUniquenessFixture,
  type InputSetUniquenessHarness,
  isuOutRef,
  makeInputSetUniquenessEmulatorHarness,
  publishInputSetUniquenessReferenceScripts,
  setupInputSetUniquenessScenario,
  submitRawInputSetUniquenessBind,
  submitRawInputSetUniquenessFinalize,
} from "./support/input-set-uniqueness-emulator.js";
import { network } from "./support/submit-init-emulator-shared.js";

/** Init + honest step-01 against the harness's committed header. */
const bindHonestly = async (
  harness: InputSetUniquenessHarness,
  fixture: InputSetUniquenessFixture,
  setup: {
    readonly fraudulentBlockOutRef: string;
    readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  },
  step01Ref: UTxO,
) => {
  const initResult = await submitInputSetUniquenessInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.family,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    witnessReferenceScripts: setup.witnessReferenceScripts,
  });
  const step01 = await submitInputSetUniquenessStep01({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    network,
    signer: harness.proverSigner,
    threadOutRef: initResult.nextThreadOutRef,
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.txInclusion,
    referenceScriptUtxo: step01Ref,
    witnessReferenceScripts: setup.witnessReferenceScripts,
  });
  return { initResult, step01 };
};

describe("input-set-uniqueness emulator adversarial polarity", () => {
  it("refuses every fabricated claim against an honest all-unique commitment, at the exact on-chain check", async () => {
    const harness = await makeInputSetUniquenessEmulatorHarness();
    const { proverLucid, proverSigner, family } = harness;

    // Unique spends, unique refs, fully disjoint: nothing to prove.
    const fixture = await buildInputSetUniquenessFixture({
      spendInputs: [isuOutRef("11", 0), isuOutRef("22", 1)],
      referenceInputs: [isuOutRef("33", 2)],
    });
    expect(
      scanInputSetUniqueness({
        spendInputItemCbors: fixture.spendInputItemCbors,
        referenceInputItemCbors: fixture.referenceInputItemCbors,
      }),
    ).toStrictEqual([]);
    expect(() =>
      requireInputSetUniquenessClaim({
        spendInputItemCbors: fixture.spendInputItemCbors,
        referenceInputItemCbors: fixture.referenceInputItemCbors,
      }),
    ).toThrow(/unique and disjoint/u);

    // The commitment is honest, but binding is not conviction: any accepted
    // committed leaf reaches step-02 armed with nothing. (Reference scripts
    // publish only after setup — the harness's one-shot nonce is the
    // funder's first UTxO.)
    const { setup } = await setupInputSetUniquenessScenario({
      harness,
      fixture,
    });
    const [step01Ref, step02Ref] =
      await publishInputSetUniquenessReferenceScripts({
        lucid: harness.funderLucid,
        contracts: family,
      });
    const { step01 } = await bindHonestly(harness, fixture, setup, step01Ref);
    expect(step01.stepState.bad_tx_id).toBe(fixture.nativeTxId);
    const threadOutRef = step01.nextThreadOutRef;

    // The honest submitter's local twins refuse before anything is paid for.
    const commonStep02 = {
      lucid: proverLucid,
      contracts: family,
      categoryId: harness.category.categoryId,
      signer: proverSigner,
      threadOutRef,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    } as const;
    await expect(
      submitInputSetUniquenessStep02({
        ...commonStep02,
        claim: {
          kind: "duplicateSpendInputs",
          firstIndex: 0n,
          secondIndex: 1n,
        },
      }),
    ).rejects.toThrow(/name different out-refs/u);
    await expect(
      submitInputSetUniquenessStep02({
        ...commonStep02,
        claim: {
          kind: "spendReferenceOverlap",
          spendIndex: 0n,
          referenceIndex: 0n,
        },
      }),
    ).rejects.toThrow(/name different out-refs/u);

    // The raw finalizes skip the twins; the VALIDATOR refuses each one. The
    // openings themselves are genuine — honest bytes, honest anchor — so the
    // refusal can only come from the decisive comparison itself.
    const planField = (fieldIndex: number, items: readonly string[]) =>
      planFaultProofFieldOpening({
        fieldIndex,
        anchorTxId: fixture.nativeTxId,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        itemCbors: items.map((item) => Buffer.from(item, "hex")),
        owner: proverSigner.paymentKeyHash,
        label: "adversarial input-set-uniqueness",
      });
    const plannedSpend = planField(
      MIDGARD_FIELD_INDEX.spendInputs,
      fixture.spendInputItemCbors,
    );
    const plannedReference = planField(
      MIDGARD_FIELD_INDEX.referenceInputs,
      fixture.referenceInputItemCbors,
    );
    const finalizationReferenceInputs = [
      step02Ref,
      ...[
        setup.witnessReferenceScripts.computationThreadMint,
        setup.witnessReferenceScripts.fraudProofMint,
      ].filter((utxo): utxo is UTxO => utxo !== undefined),
    ];
    const spendOpening = faultProofFieldOpening({
      planned: plannedSpend,
      referenceInputs: finalizationReferenceInputs,
      label: "adversarial spend-inputs opening",
    });

    // Fabricated duplicate over distinct items: dies at the byte-equality
    // conviction check.
    await expectOnchainRefusal(() =>
      submitRawInputSetUniquenessFinalize({
        harness,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
        buildArgs: (layout) => ({
          DuplicateSpendInputs: {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            first_index: 0n,
            second_index: 1n,
            spend_inputs_opening: spendOpening,
          },
        }),
      }),
    );

    // A reflexive pair (i == j) is trivially "equal" — the `first < second`
    // gate refuses it before any comparison.
    await expectOnchainRefusal(() =>
      submitRawInputSetUniquenessFinalize({
        harness,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
        buildArgs: (layout) => ({
          DuplicateSpendInputs: {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            first_index: 1n,
            second_index: 1n,
            spend_inputs_opening: spendOpening,
          },
        }),
      }),
    );

    // second_index == count: the §8.8 door's arithmetic item addressing
    // refuses anything outside `0 <= index < count`.
    await expectOnchainRefusal(() =>
      submitRawInputSetUniquenessFinalize({
        harness,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
        buildArgs: (layout) => ({
          DuplicateSpendInputs: {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            first_index: 0n,
            second_index: BigInt(fixture.spendInputItemCbors.length),
            spend_inputs_opening: spendOpening,
          },
        }),
      }),
    );

    // Fabricated overlap on disjoint sets: dies at the cross-list
    // byte-equality check.
    await expectOnchainRefusal(() =>
      submitRawInputSetUniquenessFinalize({
        harness,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
        buildArgs: (layout) => ({
          SpendReferenceOverlap: {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            spend_index: 0n,
            reference_index: 0n,
            native_tx_compact_cbor: plannedSpend.nativeTxCompactCbor,
            spend_inputs_carriage: faultProofFieldCarriage({
              planned: plannedSpend,
              referenceInputs: finalizationReferenceInputs,
              label: "adversarial spend-inputs carriage",
            }),
            reference_inputs_carriage: faultProofFieldCarriage({
              planned: plannedReference,
              referenceInputs: finalizationReferenceInputs,
              label: "adversarial reference-inputs carriage",
            }),
          },
        }),
      }),
    );

    // Every refusal left the thread exactly where it was: parked at step-02,
    // convicting nothing.
    await expect(
      proverLucid.utxosAtWithUnit(
        family.steps[1].spendingScriptAddress,
        step01.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 600_000);

  it("never binds a committed leaf the operator honestly recorded as rejected", async () => {
    const harness = await makeInputSetUniquenessEmulatorHarness();

    // Degenerate input sets — but the operator marked the tx invalid, so the
    // committed leaf is a no-op, not an acceptance.
    const duplicated = isuOutRef("bb", 5);
    const fixture = await buildInputSetUniquenessFixture({
      spendInputs: [duplicated, duplicated],
      referenceInputs: [],
      validity: "TxIsInvalid",
    });
    const { setup } = await setupInputSetUniquenessScenario({
      harness,
      fixture,
    });
    const [step01Ref] = await publishInputSetUniquenessReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const initResult = await submitInputSetUniquenessInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });

    // The honest submitter refuses locally,
    await expect(
      submitInputSetUniquenessStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: initResult.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/validity code/u);

    // ... and a raw bind that skips the guard dies on the validator's own
    // `validity_code == 0` expect.
    await expectOnchainRefusal(() =>
      submitRawInputSetUniquenessBind({
        harness,
        threadOutRef: initResult.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      }),
    );

    // The thread never advanced past step-01.
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.family.steps[0].spendingScriptAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.family.steps[1].spendingScriptAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);
});
