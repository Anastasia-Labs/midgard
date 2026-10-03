import { Data, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import {
  MISSING_REDEEMER_SCAN_BATCH,
  type MissingRedeemerPurposeKind,
} from "../src/missing-redeemer/family.js";
import { type MissingRedeemerMaterial } from "../src/missing-redeemer/replay.js";
import {
  type MissingRedeemerDecisionSchema,
  MissingRedeemerStep05DatumSchema,
  MissingRedeemerStep05RedeemerSchema,
} from "../src/missing-redeemer/schemas.js";
import {
  type MissingRedeemerStagedPlan,
  planMissingRedeemerStagedWalk,
} from "../src/missing-redeemer/staged-plan.js";
import {
  submitMissingRedeemerStep02,
  submitMissingRedeemerStep02a,
  submitMissingRedeemerStep02b,
} from "../src/missing-redeemer/submit-authentication.js";
import { submitMissingRedeemerCancel } from "../src/missing-redeemer/submit-cancel.js";
import {
  type MissingRedeemerStep03Action,
  planMissingRedeemerFieldOpening,
  submitMissingRedeemerStep03,
  submitMissingRedeemerStep04,
} from "../src/missing-redeemer/submit-field-scan.js";
import { submitMissingRedeemerInit } from "../src/missing-redeemer/submit-init.js";
import {
  submitMissingRedeemerStep01Accepted,
  submitMissingRedeemerStep01Forced,
} from "../src/missing-redeemer/submit-step-01.js";
import { submitMissingRedeemerStep05 } from "../src/missing-redeemer/submit-step-05.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  FAMILY,
  type Harness,
  measuredFit,
  outRefUtxo,
  PHYSICAL_STEPS,
  type Row,
} from "./missing-redeemer-lifecycle.make-harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { type MissingRedeemerFixture } from "./support/missing-redeemer-emulator.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  emulatorSuccessorHeaderStart,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/** Commits the fixture's block plus one successor and publishes its carriage. */
export let measuredScenario = 0;

export const makeStage = async (
  bundle: Harness,
  fixture: MissingRedeemerFixture,
) => {
  const measuredCase = `${measuredScenario++}-${fixture.shape.direction}-kind${fixture.shape.purposeKind}-${fixture.shape.sourceLocation}-field${fixture.fieldBytes}`;
  const { harness, contracts, catalogue, category, references, steps } = bundle;
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot: "00".repeat(32),
    subject:
      fixture.shape.direction === "accepted"
        ? { kind: "normal", nativeTx: fixture.transaction.tx }
        : {
            kind: "forced",
            nativeTx: fixture.transaction.tx,
            orderKey: fixture.orderKey,
            verdict: { ForcedTxInvalid: { reason: fixture.rejectionReason } },
          },
  });
  expect(block.nativeTxId).toBe(fixture.subject.transaction_id);
  const header = {
    ...block.header,
    endTime: block.header.endTime + 60_000n,
    validationTracesRoot: fixture.validationTracesRoot,
    validationTraceCount: fixture.validationTraceCount,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const successorStart = emulatorSuccessorHeaderStart({
    predecessorEndTime: header.endTime,
    emulator: harness.emulator,
  });
  const successor = await submitSuccessorBlockTx({
    lucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    anchorBlockUnit: setup.stateQueueBlockUnit,
    header: {
      ...header,
      startTime: BigInt(successorStart),
      endTime: BigInt(successorStart + 60_000),
      prevHeaderHash: setup.headerHash,
    },
    hubOracle: setup.hubOracle,
    scheduler: setup.scheduler,
    activeOperatorNode: setup.activeOperatorNode,
    activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
  });
  const rows: Row[] = [];
  const measured = async <T>(
    label: string,
    operation: () => Promise<T>,
  ): Promise<T> => {
    try {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      // Carriage publication submits one transaction per chunk; every one
      // of them is a lifecycle transaction the ledger must fit.
      if (captured.measurements.length === 1)
        rows.push({ label, measurement: captured.measurement });
      else
        captured.measurements.forEach((measurement, index) =>
          rows.push({ label: `${label}-${index.toString()}`, measurement }),
        );
      captured.measurements.forEach((measurement, index) =>
        measuredFit.record(
          `${measuredCase}/${rows.length - captured.measurements.length + index}-${label}`,
          measurement,
          measurement.executionMemory === 0n ? "publication" : "lifecycle",
        ),
      );
      return captured.result;
    } catch (error) {
      console.error(`[missing-redeemer-lifecycle] ${label} failed`);
      throw error;
    }
  };
  const material = fixture.material;
  const staged = planMissingRedeemerStagedWalk({
    transactionId: material.evidence.subject.transaction_id,
    fieldPreimageCbor: material.evidence.fieldPreimageHex,
  });
  const opening = planMissingRedeemerFieldOpening({
    ...material,
    staged,
    owner: harness.proverSigner.paymentKeyHash,
  });
  // Tier-2/3 carriage is published once per stage; every thread on this
  // block resolves the same content-addressed chunks and certificate.
  if (opening.plan.tier !== "Inline") {
    const chunks = await measured("publish-field-8-carriage", () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned: opening,
        publisherAddress: harness.proverSigner.address,
        label: "missing-redeemer field 8 carriage",
      }),
    );
    if (opening.plan.tier === "Certified") {
      const certificateReference = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "missing-redeemer field certificate mint",
      });
      await measured("certify-field-8", () =>
        certifyFaultProofFieldCarriage({
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          planned: opening,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            harness.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: certificateReference.utxo,
          chunkUtxos: chunks,
          compactCbor: material.nativeTxCompactCbor,
          witnessSetCompactCbor: material.witnessSetCompactCbor,
        }),
      );
    }
  }
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  } as const;
  const initialize = async (label = "init") =>
    (
      await measured(label, () =>
        submitMissingRedeemerInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: successor.continuedAnchorOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      )
    ).nextThreadOutRef;
  const bind = async (
    threadOutRef: string,
    options: {
      purposeKind?: MissingRedeemerPurposeKind;
      purposeIndex?: number;
      referenceScriptUtxo?: UTxO;
      label?: string;
    } = {},
  ) => {
    const purposeKind = options.purposeKind ?? fixture.shape.purposeKind;
    const purposeIndex = options.purposeIndex ?? fixture.shape.purposeIndex;
    const referenceScriptUtxo = options.referenceScriptUtxo ?? references[0];
    const result = await measured(options.label ?? "step01", async () =>
      fixture.shape.direction === "accepted"
        ? submitMissingRedeemerStep01Accepted({
            ...common,
            blueprint: harness.realBlueprint,
            network,
            threadOutRef,
            stateQueueBlockOutRef: successor.continuedAnchorOutRef,
            txInclusion: block.txInclusion!,
            header,
            purposeKind,
            purposeIndex,
            referenceScriptUtxo,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          })
        : submitMissingRedeemerStep01Forced({
            ...common,
            threadOutRef,
            header,
            membership: (await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey: fixture.eventKey,
            }))!,
            purposeKind,
            purposeIndex,
            referenceScriptUtxo,
          }),
    );
    return result.nextThreadOutRef;
  };
  const authenticate = async (
    threadOutRef: string,
    step: 1 | 2 | 3,
    authentication: MissingRedeemerMaterial["authentication"] = material.authentication,
    label?: string,
  ) => {
    const submit = [
      submitMissingRedeemerStep02,
      submitMissingRedeemerStep02a,
      submitMissingRedeemerStep02b,
    ][step - 1]!;
    const result = await measured(label ?? PHYSICAL_STEPS[step], () =>
      submit({
        ...common,
        threadOutRef,
        authentication,
        referenceScriptUtxo: references[step]!,
      }),
    );
    return result.nextThreadOutRef;
  };
  const fieldCommon = (plan: MissingRedeemerStagedPlan = staged) => ({
    ...common,
    evidence: material.evidence,
    nativeTxCompactCbor: material.nativeTxCompactCbor,
    witnessSetCompactCbor: material.witnessSetCompactCbor,
    staged: plan,
  });
  const open = async (
    threadOutRef: string,
    action: MissingRedeemerStep03Action,
    label?: string,
    plan?: MissingRedeemerStagedPlan,
  ) => {
    const result = await measured(label ?? `step03-${action.kind}`, () =>
      submitMissingRedeemerStep03({
        ...fieldCommon(plan),
        threadOutRef,
        action,
        referenceScriptUtxo: references[4]!,
      }),
    );
    return result.nextThreadOutRef;
  };
  /** Every step-03 transaction the carriage tier needs, in order. */
  const openField = async (threadOutRef: string) => {
    if (opening.plan.tier !== "Certified")
      return await open(threadOutRef, { kind: "direct" });
    let current = await open(threadOutRef, { kind: "grammar_start" });
    for (let ordinal = 1; ordinal < staged.grammar.length; ordinal += 1)
      current = await open(
        current,
        { kind: "grammar_resume", ordinal },
        `step03-grammar_resume-${ordinal.toString()}`,
      );
    return await open(current, { kind: "grammar_finish" });
  };
  const scanBatch = async (
    threadOutRef: string,
    label?: string,
    plan?: MissingRedeemerStagedPlan,
  ) => {
    const result = await measured(label ?? "step04", () =>
      submitMissingRedeemerStep04({
        ...fieldCommon(plan),
        threadOutRef,
        referenceScriptUtxo: references[5]!,
      }),
    );
    return result.nextThreadOutRef;
  };
  const batches = Math.max(
    1,
    Math.ceil(material.evidence.itemCount / MISSING_REDEEMER_SCAN_BATCH),
  );
  /** Runs the walk to its decision, one real batch per transaction. */
  const scan = async (threadOutRef: string) => {
    let current = threadOutRef;
    for (let batch = 0; batch < batches; batch += 1) {
      current = await scanBatch(current, `step04-batch-${batch.toString()}`);
      if (material.evidence.checkpoints[batch]?.found === true) break;
      if (batch + 1 < batches) {
        // Still at step 04 with a live checkpoint until the field is exhausted.
        const utxo = await outRefUtxo(harness.proverLucid, current);
        expect(utxo?.address).toBe(steps[5].spendingScriptAddress);
      }
    }
    return current;
  };
  const finalize = (threadOutRef: string, label = "step05") =>
    measured(label, () =>
      submitMissingRedeemerStep05({
        ...common,
        threadOutRef,
        evidence: material.evidence,
        referenceScriptUtxo: references[6]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const cancel = (threadOutRef: string, stepIndex: number, label: string) =>
    measured(label, () =>
      submitMissingRedeemerCancel({
        ...common,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const names = [
      "fraudProofMissingRedeemer",
      "fraudProofMissingRedeemerStep02",
      "fraudProofMissingRedeemerStep02a",
      "fraudProofMissingRedeemerStep02b",
      "fraudProofMissingRedeemerStep03",
      "fraudProofMissingRedeemerStep04",
      "fraudProofMissingRedeemerStep05",
    ] as const;
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        ...Object.fromEntries(
          steps.map((step, index) => [
            names[index]!,
            {
              scriptHash: step.spendingScriptHash,
              contract: {
                type: step.spendingScript.type,
                cborHex: step.spendingScript.script,
              },
            },
          ]),
        ),
      },
    };
    let leaseReleased = false;
    const now = BigInt(harness.emulator.now());
    const removal = await measured("remove-leased", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "missingRedeemer",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "missing-redeemer-lifecycle",
            source: "emulator",
            renew: async () => undefined,
            release: async () => {
              leaseReleased = true;
            },
            fail: async () => undefined,
          }),
        },
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.fraudulentHeaderHash).toBe(setup.headerHash);
    expect(leaseReleased).toBe(true);
  };
  /** A fresh process recovers the thread from the chain alone. */
  const restart = async (threadOutRef: string, stepIndex: number) => {
    const found = (
      await harness.proverLucid.utxosAt(steps[stepIndex]!.spendingScriptAddress)
    ).find(
      (utxo) =>
        `${utxo.txHash}#${utxo.outputIndex.toString()}` === threadOutRef,
    );
    expect(found, `thread at ${PHYSICAL_STEPS[stepIndex]!}`).toBeDefined();
    return threadOutRef;
  };
  const assertFit = () => {
    for (const { label, measurement } of rows) {
      expect(measurement.l1ByteMargin, label).toBeGreaterThan(0);
      // Raw carriage publication runs no script; every other row does.
      if (label.startsWith("publish-")) continue;
      expect(measurement.executionMemory, label).toBeGreaterThan(0n);
      expect(measurement.executionSteps, label).toBeGreaterThan(0n);
    }
  };
  /** Runs Init through the exhausted or matched walk, restarting at each step. */
  const runToDecision = async () => {
    let thread = await initialize();
    thread = await restart(await bind(thread), 1);
    thread = await restart(await authenticate(thread, 1), 2);
    thread = await restart(await authenticate(thread, 2), 3);
    thread = await restart(await authenticate(thread, 3), 4);
    thread = await restart(await openField(thread), 5);
    return await restart(await scan(thread), 6);
  };
  /** Spends the decision straight into the terminal validator, bypassing the off-chain guard. */
  const finalizeDirectly = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: FAMILY,
      stepIndex: 6,
      threadOutRef,
    });
    requireLinearFaultStepState<
      Data.Static<typeof MissingRedeemerDecisionSchema>
    >({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingRedeemerStep05DatumSchema as never,
      family: FAMILY,
      stepIndex: 6,
    });
    return await submitLinearFaultFinalize({
      lucid: harness.proverLucid,
      family: FAMILY,
      stepIndex: 6,
      step: contracts.steps[6],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: MissingRedeemerStep05RedeemerSchema,
      buildFamilyArgs: (layout) => ({
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: references[6]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  return {
    block,
    header,
    setup,
    opening,
    staged,
    rows,
    initialize,
    bind,
    authenticate,
    open,
    openField,
    scanBatch,
    scan,
    finalize,
    finalizeDirectly,
    cancel,
    remove,
    restart,
    assertFit,
    runToDecision,
  };
};
