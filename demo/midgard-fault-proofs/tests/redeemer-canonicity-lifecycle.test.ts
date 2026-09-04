import {
  decodeMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  EMPTY_MERKLE_TREE_ROOT,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import type { RedeemerCanonicityContracts } from "../src/redeemer-canonicity/contracts.js";
import {
  prepareRedeemerCanonicityEvidence,
  submitRedeemerCanonicityCancel,
  submitRedeemerCanonicityStep01Accepted,
  submitRedeemerCanonicityStep01Forced,
  submitRedeemerCanonicityStep02,
  submitRedeemerCanonicityStep03,
} from "../src/redeemer-canonicity/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  setupCommittedFieldShapeScenario,
} from "./support/committed-field-shape-emulator.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  createRecordingLeaseCoordinator,
  emulatorSuccessorHeaderStart,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const emitFit = (
  stage: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  if (process.env.MIDGARD_PRINT_FIT !== "1") return;
  console.info(
    `[redeemer-canonicity-fit] ${JSON.stringify({
      stage,
      signedBytes: measurement.completeSignedBytes,
      byteMargin: measurement.l1ByteMargin,
      memory: measurement.executionMemory.toString(),
      memoryMargin: (16_500_000n - measurement.executionMemory).toString(),
      cpu: measurement.executionSteps.toString(),
      cpuMargin: (10_000_000_000n - measurement.executionSteps).toString(),
    })}`,
  );
};

const familyContracts = (
  harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>,
): RedeemerCanonicityContracts => {
  const chain = harness.contracts.fraudProofContracts.redeemerCanonicity;
  return {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: [
        "fraud_proofs/redeemer_canonicity/step_01.main.spend",
        "fraud_proofs/redeemer_canonicity/step_02.main.spend",
        "fraud_proofs/redeemer_canonicity/step_03.main.spend",
      ][index]!,
      referenceOutRef: `${"0".repeat(64)}#0`,
    })) as unknown as RedeemerCanonicityContracts["steps"],
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
};

const publishFamilyReferences = async (
  harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>,
): Promise<readonly [UTxO, UTxO, UTxO]> => {
  const result: UTxO[] = [];
  for (const [
    index,
    step,
  ] of harness.contracts.fraudProofContracts.redeemerCanonicity.steps.entries())
    result.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `redeemer-canonicity-${index.toString()}`,
        })
      ).utxo,
    );
  return result as unknown as readonly [UTxO, UTxO, UTxO];
};

const initThread = async ({
  harness,
  contracts,
  category,
  fraudulentBlockOutRef,
}: {
  readonly harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
  readonly contracts: RedeemerCanonicityContracts;
  readonly category: NonNullable<
    Awaited<
      ReturnType<typeof makeFaultProofEmulatorHarness>
    >["catalogue"]["categories"]["redeemerCanonicity"]
  >;
  readonly fraudulentBlockOutRef: string;
}) =>
  await submitCommittedFieldShapeInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: contracts as never,
    category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

describe("redeemer-canonicity accepted lifecycle", () => {
  it("runs maximum retained field 8 through permanent mint and leased queue removal", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realRedeemerCanonicity: true,
        alwaysFraudProofCatalogue: true,
      },
      lucidOptions: { evaluator: createScalusEvaluator() },
    });
    const contracts = familyContracts(harness);
    const category = harness.catalogue.categories.redeemerCanonicity;
    if (category === undefined) throw new Error("redeemer category absent");
    const scenario = await setupCommittedFieldShapeScenario({
      harness: harness as unknown as CommittedFieldShapeEmulatorHarness,
      kind: "redeemer-canonicity",
    });
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("missing funder key");
    const successorStart = emulatorSuccessorHeaderStart({
      predecessorEndTime: scenario.setup.header.endTime,
      emulator: harness.emulator,
    });
    const successorBase = makeHeader(
      credential.hash,
      successorStart,
      EMPTY_MERKLE_TREE_ROOT,
    );
    const successor = await submitSuccessorBlockTx({
      lucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      anchorBlockUnit: scenario.setup.stateQueueBlockUnit,
      header: {
        ...successorBase,
        endTime: successorBase.startTime + 60_000n,
        prevHeaderHash: scenario.setup.headerHash,
      },
      hubOracle: scenario.setup.hubOracle,
      scheduler: scenario.setup.scheduler,
      activeOperatorNode: scenario.setup.activeOperatorNode,
      activeOperatorNodeUnit: scenario.setup.activeOperatorNodeUnit,
    });
    const references = await publishFamilyReferences(harness);
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "redeemer-canonicity-certificate",
      })
    ).utxo;
    const evidence = prepareRedeemerCanonicityEvidence({
      finding: {
        subject: acceptedVerdictSubject(scenario.nativeTxId),
        redeemerIndex: 0,
      },
      fieldPreimage: scenario.committedPreimage,
      committedFieldHashHex: midgardFieldCommitment(
        scenario.committedPreimage,
      ).toString("hex"),
    });
    expect(evidence.canonical).toBe(false);
    expect(evidence.carriage).toBe("Certified");
    const init = await captureEmulatorSubmission(harness.emulator, () =>
      initThread({
        harness,
        contracts,
        category,
        fraudulentBlockOutRef: successor.continuedAnchorOutRef,
      }),
    );
    emitFit("accepted-init", init.measurement);
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: init.result.txHash,
        outputIndex: init.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    const step01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo,
        threadToken: {
          unit: init.result.computationThreadUnit,
          fraudulentHeaderHash: init.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: successor.continuedAnchorOutRef,
        txInclusion: scenario.inclusion,
        referenceScriptUtxo: references[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    emitFit("accepted-step01", step01.measurement);
    const step02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: scenario.compactCbor,
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
          deriveMidgardNativeTxWitnessSetCompact(scenario.fullTx!.witnessSet),
        ).toString("hex"),
        referenceScriptUtxo: references[1],
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    emitFit("accepted-step02-certified-maximum", step02.measurement);
    expect(step02.result.carriageTier).toBe("Certified");
    const step03 = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[2],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    emitFit("accepted-step03-permanent-mint", step03.measurement);
    expect(step03.result.fraudProofUnit).toBeTruthy();
    for (const capture of [init, step01, step02, step03]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }
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
        fraudulentHeaderHash: scenario.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(leaseEvents),
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    emitFit("leased-state-queue-removal", removal.measurement);
    expect(removal.result.fraudCategoryId).toBe("00000028");
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(leaseEvents.map(({ kind }) => kind)).toContain("lease.acquire");
    expect(leaseEvents.map(({ kind }) => kind)).toContain("lease.release");
    expect(removal.measurement.l1ByteMargin).toBeGreaterThan(0);
  }, 300_000);

  it.each([0, 1, 2] as const)(
    "cancels the real thread at physical step %s",
    async (targetStep) => {
      const harness = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realRedeemerCanonicity: true,
          alwaysFraudProofCatalogue: true,
        },
      });
      const contracts = familyContracts(harness);
      const category = harness.catalogue.categories.redeemerCanonicity;
      if (category === undefined) throw new Error("redeemer category absent");
      const scenario = await setupCommittedFieldShapeScenario({
        harness: harness as unknown as CommittedFieldShapeEmulatorHarness,
        kind: "redeemer-canonicity",
      });
      const references = await publishFamilyReferences(harness);
      const certificateReference = (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: `redeemer-cancel-certificate-${targetStep.toString()}`,
        })
      ).utxo;
      const evidence = prepareRedeemerCanonicityEvidence({
        finding: {
          subject: acceptedVerdictSubject(scenario.nativeTxId),
          redeemerIndex: 0,
        },
        fieldPreimage: scenario.committedPreimage,
        committedFieldHashHex: midgardFieldCommitment(
          scenario.committedPreimage,
        ).toString("hex"),
      });
      const init = await initThread({
        harness,
        contracts,
        category,
        fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      });
      let threadOutRef = `${init.txHash}#${init.firstStepOutputIndex.toString()}`;
      if (targetStep > 0) {
        const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
          { txHash: init.txHash, outputIndex: init.firstStepOutputIndex },
        ]);
        if (threadUtxo === undefined) throw new Error("init thread absent");
        const step01 = await submitRedeemerCanonicityStep01Accepted({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          signer: harness.proverSigner,
          finding: evidence,
          threadUtxo,
          threadToken: {
            unit: init.computationThreadUnit,
            fraudulentHeaderHash: init.fraudulentHeaderHash,
          },
          stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
          txInclusion: scenario.inclusion,
          referenceScriptUtxo: references[0],
          witnessReferenceScripts: harness.witnessReferenceScripts,
        });
        threadOutRef = step01.nextThreadOutRef;
      }
      if (targetStep > 1) {
        const step02 = await submitRedeemerCanonicityStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          nativeTxCompactCbor: scenario.compactCbor,
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
            deriveMidgardNativeTxWitnessSetCompact(scenario.fullTx!.witnessSet),
          ).toString("hex"),
          referenceScriptUtxo: references[1],
          certificateReferenceScriptUtxo: certificateReference,
        });
        threadOutRef = step02.nextThreadOutRef;
      }
      const cancel = await captureEmulatorSubmission(harness.emulator, () =>
        submitRedeemerCanonicityCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo: references[targetStep],
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      emitFit(`cancel-step0${(targetStep + 1).toString()}`, cancel.measurement);
      expect(cancel.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(cancel.measurement.executionMemory).toBeGreaterThan(0n);
    },
    300_000,
  );
});

describe("redeemer-canonicity forced lifecycle", () => {
  it("authenticates a canonical redeemer against the exact malformed reason", async () => {
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
      redeemerMalformedIndex: 0,
    });
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
    const rejectionReason = {
      RedeemerMalformed: { redeemer_index: 0n },
    } as const;
    const evidence = prepareRedeemerCanonicityEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId: forced.forcedTransaction.tx_id,
          sourceKey: membership.key,
          rejectionReason,
        }),
        redeemerIndex: 0,
      },
      fieldPreimage:
        forced.forcedNativeTx.witnessSet.redeemerTxWitsPreimageCbor,
      committedFieldHashHex: midgardFieldCommitment(
        forced.forcedNativeTx.witnessSet.redeemerTxWitsPreimageCbor,
      ).toString("hex"),
    });
    expect(evidence.canonical).toBe(true);
    const init = await initThread({
      harness,
      contracts,
      category,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    const step01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
        finding: evidence,
        forcedSource: { header: forced.header, membership, direction: 1n },
        witnessSetHash: Buffer.from(
          decodeMidgardNativeTxCompact(
            Buffer.from(forced.forcedTransaction.source.compact_cbor, "hex"),
          ).transactionWitnessSetHash,
        ).toString("hex"),
        referenceScriptUtxo: references[0],
      }),
    );
    emitFit("forced-step01", step01.measurement);
    const step02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: forced.forcedTransaction.source.compact_cbor,
        witnessSetCompactCbor:
          forced.forcedTransaction.source.witness_set_compact_cbor,
        referenceScriptUtxo: references[1],
      }),
    );
    emitFit("forced-step02", step02.measurement);
    const step03 = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[2],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    emitFit("forced-step03-permanent-mint", step03.measurement);
    expect(step03.result.fraudProofUnit).toBeTruthy();
    for (const capture of [step01, step02, step03]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
    }
  }, 300_000);
});
