import {
  decodeMidgardForcedTxCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import {
  detectRedeemerCanonicityFromCanonicalBlock,
  prepareRedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
  RedeemerCanonicityStep03DatumSchema,
  submitRedeemerCanonicityStep01Forced,
  submitRedeemerCanonicityStep02,
  submitRedeemerCanonicityStep03,
} from "../src/redeemer-canonicity/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  emitFit,
  familyContracts,
  finalizeWithoutGuards,
  initThread,
  publishFamilyReferences,
  readRedeemerData,
} from "./redeemer-canonicity-lifecycle.init-thread.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  createRecordingLeaseCoordinator,
} from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

/**
 * Lands a block that commits one forced transaction ForcedTxValid with the
 * given Spend redeemer data, opens a redeemer-canonicity thread against it,
 * and returns what the production scan detects in that block.
 */
const landForcedAcceptance = async (redeemerDataHex: string) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realRedeemerCanonicity: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = familyContracts(harness);
  const category = harness.catalogue.categories.redeemerCanonicity;
  if (category === undefined) throw new Error("redeemer category absent");
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("missing funder key");
  const forced = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    acceptedRedeemerDataHex: redeemerDataHex,
  });
  expect(forced.forcedTransaction.verdict).toBe("ForcedTxValid");
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: forced.header,
  });
  const references = await publishFamilyReferences(harness);
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  // The scan reads only the reconstruction's forced leaves for this block;
  // it has no accepted L2 transactions.
  const detections = detectRedeemerCanonicityFromCanonicalBlock({
    headerHash: forced.headerHash,
    transactions: [],
    reconstruction: forced.reconstruction,
  } as unknown as CanonicalBlockEvidence);
  const scanned = detections[0]?.evidence;
  const init = await initThread({
    harness,
    contracts,
    category,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
  });
  return {
    harness,
    contracts,
    category,
    forced,
    setup,
    references,
    membership,
    detections,
    scanned,
    threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
  };
};

/** Binds the forced event as a wrongful acceptance (direction 0), opens the
 * committed item and reads its data to the verdict: steps 01, 02 and the
 * data step. */
const bindAndOpen = async (
  run: Awaited<ReturnType<typeof landForcedAcceptance>>,
  evidence: NonNullable<
    Awaited<ReturnType<typeof landForcedAcceptance>>["scanned"]
  >,
) => {
  const { harness, contracts, category, forced, references, membership } = run;
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    submitRedeemerCanonicityStep01Forced({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: run.threadOutRef,
      finding: evidence,
      forcedSource: { header: forced.header, membership, direction: 0n },
      witnessSetHash: Buffer.from(
        decodeMidgardForcedTxCompact(
          Buffer.from(
            forced.forcedTransaction.submitted_source.compact_cbor,
            "hex",
          ),
        ).transactionWitnessSetHash,
      ).toString("hex"),
      referenceScriptUtxo: references[0],
    }),
  );
  const step02 = await captureEmulatorSubmission(harness.emulator, () =>
    submitRedeemerCanonicityStep02({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.result.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor:
        forced.forcedTransaction.submitted_source.compact_cbor,
      witnessSetCompactCbor:
        forced.forcedTransaction.submitted_source.witness_set_compact_cbor,
      referenceScriptUtxo: references[1],
    }),
  );
  expect(step02.result.nextStage).toBe("step02Data");
  const read = await readRedeemerData({
    harness,
    contracts,
    categoryId: category.categoryId,
    threadOutRef: step02.result.nextThreadOutRef,
    evidence,
    referenceScriptUtxo: references[2],
  });
  return { step01, step02, verdictThreadOutRef: read.threadOutRef };
};

describe("redeemer-canonicity forced wrongful acceptance", () => {
  it("proves an accepted forced transaction with out-of-image redeemer data and removes its block", async () => {
    const run = await landForcedAcceptance("d8798101");
    expect(run.detections).toHaveLength(1);
    const evidence = run.scanned!;
    expect(evidence.subject.direction).toBe(0n);
    expect(evidence.subject.rejection_reason).toBeNull();
    expect(evidence.canonical).toBe(false);
    expect(redeemerCanonicityEvidenceCloses(evidence)).toBe(true);
    const { step01, step02, verdictThreadOutRef } = await bindAndOpen(
      run,
      evidence,
    );
    emitFit("forced-accepted-step01", step01.measurement);
    emitFit("forced-accepted-step02", step02.measurement);
    const { harness, contracts, category, references } = run;
    const step03 = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: verdictThreadOutRef,
        evidence,
        referenceScriptUtxo: references[3],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    emitFit("forced-accepted-step03-permanent-mint", step03.measurement);
    expect(step03.result.fraudProofUnit).toBeTruthy();
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const leaseEvents: Parameters<typeof createRecordingLeaseCoordinator>[0] =
      [];
    const now = BigInt(harness.emulator.now());
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
        fraudCategory: "redeemerCanonicity",
        fraudulentHeaderHash: run.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(leaseEvents),
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    emitFit("forced-accepted-removal", removal.measurement);
    expect(removal.result.fraudCategoryId).toBe("00000028");
    expect(removal.result.transactions.map(({ kind }) => kind)).toContain(
      "remove-target",
    );
    for (const capture of [step01, step02, step03, removal]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 300_000);

  it("refuses to finalize a wrongful-acceptance thread against an honest canonical redeemer", async () => {
    const run = await landForcedAcceptance("d87980");
    // The honest block yields no detection; the adversary builds the same
    // direction-0 subject by hand and walks it as far as the chain allows.
    expect(run.detections).toEqual([]);
    const { harness, contracts, category, references } = run;
    const field =
      run.forced.forcedNativeTx.witnessSet.redeemerTxWitsPreimageCbor;
    const evidence = prepareRedeemerCanonicityEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId: run.forced.forcedTransaction.tx_id,
          sourceKey: run.membership.key,
          rejectionReason: null,
        }),
        redeemerIndex: 0,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
    expect(evidence.canonical).toBe(true);
    expect(redeemerCanonicityEvidenceCloses(evidence)).toBe(false);
    const { verdictThreadOutRef: threadOutRef } = await bindAndOpen(
      run,
      evidence,
    );
    const { threadUtxo } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: "redeemer-canonicity",
      stepIndex: 3,
      threadOutRef,
    });
    // The data step recorded the chain's own verdict on the item: canonical.
    expect(
      requireLinearFaultStepState<{ canonical: boolean }>({
        threadUtxo,
        signer: harness.proverSigner,
        schema: RedeemerCanonicityStep03DatumSchema as never,
        family: "redeemer-canonicity",
        stepIndex: 3,
      }).canonical,
    ).toBe(true);
    await expect(
      submitRedeemerCanonicityStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[3],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow();
    // Finalize differs from the positive case only in the item the chain
    // judged canonical, so the refusal is step-03's terminal contradiction.
    await expectOnchainRefusal(
      () =>
        finalizeWithoutGuards({
          harness,
          contracts,
          categoryId: category.categoryId,
          threadOutRef,
          referenceScriptUtxo: references[3],
        }),
      {
        refusedBy: "fraud_proofs/redeemer_canonicity/step_03",
        check: /^Validator returned false$/u,
      },
    );
  }, 300_000);
});
