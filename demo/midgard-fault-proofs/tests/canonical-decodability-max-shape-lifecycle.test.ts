/**
 * canonicalDecodability at the consensus field bound: 32,768-byte committed
 * fields carried by §8.6 certified chunks and walked in batches. A field
 * declaring one item more than it carries is convicted from Init through
 * removal; the honest field of the same size walks to verdict 0 and no
 * forwarded verdict convicts it; the costliest single batch fits one step.
 */
import { MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES } from "@al-ft/midgard-core";
import {
  advanceCanonicalDecodabilityWalk,
  CanonicalDecodabilityStep01Datum,
  CanonicalDecodabilityStep02Datum,
  type CanonicalDecodabilityWalkState,
  type CommittedFieldClaim,
  FraudProofTokenDatum,
  MIDGARD_ENVELOPE_BATCH_ITEMS,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
  midgardEnvelopeVerdict,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { faultProofRawFieldCarriage } from "../src/field-opening.js";
import {
  submitCanonicalDecodabilityInit,
  submitCanonicalDecodabilityStep01,
  submitCanonicalDecodabilityStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  buildCanonicalDecodabilityBodyFixture,
  type CanonicalDecodabilityCommittedFieldFixture,
  makeCanonicalDecodabilityEmulatorHarness,
  network,
  publishCanonicalDecodabilityReferenceScripts,
  setupCanonicalDecodabilityScenario,
  submitCanonicalDecodabilityStep01Raw,
  submitCanonicalDecodabilityStep02Raw,
} from "./support/canonical-decodability-emulator.js";
import {
  MAX_ITEMS,
  maxField,
  worstBatchField,
} from "./support/canonical-decodability-emulator.max-shape-fields.js";
import {
  type CertifiedCommittedField,
  expectStageFit,
  publishCertifiedCommittedField,
} from "./support/certified-committed-field.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const LABEL = "canonical-decodability-max";
const B = MIDGARD_ENVELOPE_BATCH_ITEMS;

type Harness = Awaited<
  ReturnType<typeof makeCanonicalDecodabilityEmulatorHarness>
>;

const openThread = async (
  harness: Harness,
  committedPreimage: Buffer,
  grammatical: boolean,
) => {
  const fixture = await buildCanonicalDecodabilityBodyFixture({
    committedPreimage,
    grammatical,
  });
  const setup = await setupCanonicalDecodabilityScenario({ harness, fixture });
  const [step01Ref, step02Ref] =
    await publishCanonicalDecodabilityReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.canonicalDecodability,
    });
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    submitCanonicalDecodabilityInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.canonicalDecodability,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  expectStageFit(LABEL, "init", init.measurement);
  const certified = await certify(harness, fixture);
  return { fixture, setup, step01Ref, step02Ref, init: init.result, certified };
};

const certify = async (
  harness: Harness,
  fixture: CanonicalDecodabilityCommittedFieldFixture,
): Promise<CertifiedCommittedField> => {
  const certified = await publishCertifiedCommittedField({
    emulator: harness.emulator,
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    certificate: harness.contracts.fieldPreimageCertificate,
    nativeTxId: fixture.badTxId,
    fieldIndex: fixture.fieldIndex,
    preimage: fixture.committedPreimage,
    compactCbor: fixture.nativeTxCompactCbor,
    witnessSetCompactCbor: fixture.witnessSetCompactCbor,
    label: LABEL,
  });
  expect(certified.plan.tier).toBe("Certified");
  expect(certified.chunks.length).toBeGreaterThan(1);
  return certified;
};

const threadsAt = async (
  harness: Harness,
  stepIndex: 0 | 1,
  unit: string,
): Promise<number> =>
  (
    await harness.proverLucid.utxosAtWithUnit(
      harness.canonicalDecodability.steps[stepIndex].spendingScriptAddress,
      unit,
    )
  ).length;

describe("canonical-decodability at the consensus field bound", () => {
  it("convicts a 32,768-byte field declaring one item more than it carries, mints proof and removes the block", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarness();
    const committedPreimage = maxField(MAX_ITEMS + 1);
    expect(committedPreimage).toHaveLength(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    expect(midgardEnvelopeVerdict(committedPreimage)).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
    );
    const { fixture, setup, step01Ref, step02Ref, init, certified } =
      await openThread(harness, committedPreimage, false);
    expect(fixture.prepared?.evidence.verdict).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
    );

    let threadOutRef = init.nextThreadOutRef;
    const verdicts: (number | null)[] = [];
    for (;;) {
      const step = await captureEmulatorSubmission(harness.emulator, () =>
        submitCanonicalDecodabilityStep01({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          contracts: harness.canonicalDecodability,
          categoryId: harness.category.categoryId,
          network,
          signer: harness.proverSigner,
          threadOutRef,
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion: fixture.txInclusion,
          fieldIndex: fixture.fieldIndex,
          committedPreimage,
          publishedFieldCarriageUtxos: certified.chunks,
          fieldCertificateUtxo: certified.certificateUtxo,
          fieldCertificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          referenceScriptUtxo: step01Ref,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      expectStageFit(
        LABEL,
        `step-01-${(verdicts.length + 1).toString()}`,
        step.measurement,
      );
      verdicts.push(step.result.verdict);
      threadOutRef = step.result.nextThreadOutRef;
      if (step.result.complete) break;
    }
    expect(verdicts).toHaveLength(Math.ceil(MAX_ITEMS / B));
    expect(verdicts.slice(0, -1).every((verdict) => verdict === null)).toBe(
      true,
    );
    expect(verdicts.at(-1)).toBe(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER);
    const verdictThread = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.canonicalDecodability.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    expect(
      Data.from(verdictThread.datum!, CanonicalDecodabilityStep02Datum),
    ).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        bad_tx_id: fixture.badTxId,
        field_index: BigInt(fixture.fieldIndex),
        verdict: BigInt(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER),
      },
    });

    const step02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitCanonicalDecodabilityStep02({
        lucid: harness.proverLucid,
        contracts: harness.canonicalDecodability,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expectStageFit(LABEL, "step-02-mint", step02.measurement);
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02.result.fraudProofAddress,
      step02.result.fraudProofUnit,
    );
    expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const removeNow = BigInt(harness.emulator.now());
    const removal = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalReferences.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "canonicalDecodability",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    removal.measurements.forEach((measurement, index) =>
      expectStageFit(LABEL, `remove-${(index + 1).toString()}`, measurement),
    );
    expect(removal.result.fraudCategoryId).toBe(harness.category.categoryId);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 1_800_000);

  it("walks the honest 32,768-byte field to verdict 0 and refuses every convicting verdict for it", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarness();
    const committedPreimage = maxField(MAX_ITEMS);
    expect(committedPreimage).toHaveLength(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    expect(midgardEnvelopeVerdict(committedPreimage)).toBe(
      MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
    );
    const { fixture, setup, step01Ref, step02Ref, init, certified } =
      await openThread(harness, committedPreimage, true);
    expect(fixture.prepared).toBeNull();
    const unit = init.computationThreadUnit;

    // The prover's submitter will not walk a field that convicts nothing.
    await expect(
      submitCanonicalDecodabilityStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.canonicalDecodability,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        fieldIndex: fixture.fieldIndex,
        committedPreimage,
        publishedFieldCarriageUtxos: certified.chunks,
        fieldCertificateUtxo: certified.certificateUtxo,
        fieldCertificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/verdict 0.*valid block cannot be challenged/u);

    const certifiedClaim = (
      referenceInputs: readonly UTxO[],
    ): CommittedFieldClaim => ({
      BodyFieldClaim: {
        field_index: BigInt(fixture.fieldIndex),
        carriage: faultProofRawFieldCarriage({
          plan: certified.plan,
          referenceInputs,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          label: LABEL,
        }),
      },
    });
    const raw = (
      threadOutRef: string,
      successor: Parameters<
        typeof submitCanonicalDecodabilityStep01Raw
      >[0]["successor"],
    ) =>
      captureEmulatorSubmission(harness.emulator, () =>
        submitCanonicalDecodabilityStep01Raw({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          contracts: harness.canonicalDecodability,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion: fixture.txInclusion,
          claim: certifiedClaim,
          successor,
          carriageReferenceInputs: certified.referenceInputs,
          referenceScriptUtxo: step01Ref,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );

    let threadOutRef = init.nextThreadOutRef;
    let previous: CanonicalDecodabilityWalkState | null = null;
    let steps = 0;
    for (;;) {
      const transition = advanceCanonicalDecodabilityWalk({
        badTxId: fixture.badTxId,
        fieldIndex: fixture.fieldIndex,
        committedPreimage,
        previous,
      });
      steps += 1;
      if (transition.kind === "verdict") {
        expect(transition.state.verdict).toBe(
          BigInt(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL),
        );
        // Every convicting code, forwarded from the last batch, is refused.
        for (const forged of [
          MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
          10,
        ]) {
          await expectOnchainRefusal(() =>
            raw(threadOutRef, {
              step: 2,
              state: { ...transition.state, verdict: BigInt(forged) },
            }),
          );
        }
        expect(await threadsAt(harness, 0, unit)).toBe(1);
        expect(await threadsAt(harness, 1, unit)).toBe(0);
        // The identical transaction forwarding the truthful verdict lands.
        const truthful = await raw(threadOutRef, {
          step: 2,
          state: transition.state,
        });
        expectStageFit(
          LABEL,
          `honest-step-01-${steps.toString()}`,
          truthful.measurement,
        );
        threadOutRef = truthful.result.nextThreadOutRef;
        break;
      }
      if (previous === null) {
        // Before any batch is read, a convicting verdict is refused too.
        await expectOnchainRefusal(() =>
          raw(threadOutRef, {
            step: 2,
            state: {
              bad_tx_id: fixture.badTxId,
              field_index: BigInt(fixture.fieldIndex),
              verdict: BigInt(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER),
            },
          }),
        );
      }
      const step = await raw(threadOutRef, {
        step: 1,
        state: transition.state,
      });
      expectStageFit(
        LABEL,
        `honest-step-01-${steps.toString()}`,
        step.measurement,
      );
      const thread = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        harness.canonicalDecodability.steps[0].spendingScriptAddress,
        unit,
      );
      expect(
        Data.from(thread.datum!, CanonicalDecodabilityStep01Datum).data,
      ).toStrictEqual(transition.state);
      threadOutRef = step.result.nextThreadOutRef;
      previous = transition.state;
    }
    expect(steps).toBe(Math.ceil(MAX_ITEMS / B));
    expect(await threadsAt(harness, 0, unit)).toBe(0);
    expect(await threadsAt(harness, 1, unit)).toBe(1);

    // Verdict 0 finalizes nothing.
    await expectOnchainRefusal(() =>
      submitCanonicalDecodabilityStep02Raw({
        lucid: harness.proverLucid,
        contracts: harness.canonicalDecodability,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(await threadsAt(harness, 1, unit)).toBe(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 1_800_000);

  it("decides the costliest single batch in one step under the transaction limits", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarness();
    const committedPreimage = worstBatchField();
    expect(committedPreimage.length).toBeLessThanOrEqual(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    expect(midgardEnvelopeVerdict(committedPreimage)).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
    );
    const { fixture, setup, step01Ref, init, certified } = await openThread(
      harness,
      committedPreimage,
      false,
    );
    const step = await captureEmulatorSubmission(harness.emulator, () =>
      submitCanonicalDecodabilityStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.canonicalDecodability,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        fieldIndex: fixture.fieldIndex,
        committedPreimage,
        publishedFieldCarriageUtxos: certified.chunks,
        fieldCertificateUtxo: certified.certificateUtxo,
        fieldCertificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expectStageFit(LABEL, "worst-batch-step-01", step.measurement);
    expect(step.result.complete).toBe(true);
    expect(step.result.verdict).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
    );
  }, 900_000);
});
