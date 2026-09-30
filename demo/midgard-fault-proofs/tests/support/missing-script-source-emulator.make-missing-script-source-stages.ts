import { type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { type MissingScriptSourceContracts } from "../../src/missing-script-source/contracts.js";
import type { MissingScriptSourceEvidence } from "../../src/missing-script-source/family.js";
import { submitMissingScriptSourceCancel } from "../../src/missing-script-source/submit-cancel.js";
import { submitMissingScriptSourceInit } from "../../src/missing-script-source/submit-init.js";
import {
  submitMissingScriptSourceStep01Accepted,
  submitMissingScriptSourceStep01Forced,
} from "../../src/missing-script-source/submit-step-01.js";
import {
  type ExecutionSourceAuthenticationData,
  submitMissingScriptSourceStep02,
} from "../../src/missing-script-source/submit-step-02.js";
import { submitMissingScriptSourceStep03 } from "../../src/missing-script-source/submit-step-03.js";
import { submitMissingScriptSourceStep04 } from "../../src/missing-script-source/submit-step-04.js";
import { submitMissingScriptSourceStep05 } from "../../src/missing-script-source/submit-step-05.js";
import { submitMissingScriptSourceStep06 } from "../../src/missing-script-source/submit-step-06.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { expectProofFit } from "./emulator/proof-fit.js";
import { type MissingScriptSourcePurposeKind } from "./missing-script-source-emulator.build-missing-script-source-fixture.js";
import {
  type MissingScriptSourceBlock,
  type MissingScriptSourceHarness,
  type MissingScriptSourceStageRow,
} from "./missing-script-source-emulator.commit-missing-script-source-block.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishRemovalReferenceScripts,
} from "./submit-init-emulator-shared.js";

/** Measured drivers for every physical step over one harness and block. */
export const makeMissingScriptSourceStages = (
  {
    harness,
    contracts,
    catalogue,
    category,
    references,
  }: MissingScriptSourceHarness,
  block: MissingScriptSourceBlock,
) => {
  const measurements: MissingScriptSourceStageRow[] = [];
  const record = <T>(
    stage: string,
    captured: Awaited<ReturnType<typeof captureEmulatorSubmission<T>>>,
  ) => {
    measurements.push({ stage, measurement: captured.measurement });
    return captured.result;
  };
  const measured = async <T>(stage: string, operation: () => Promise<T>) => {
    console.info(`[missing-script-source-stage] ${stage}`);
    return record(
      stage,
      await captureEmulatorSubmission(harness.emulator, operation),
    );
  };
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  } as const;
  const init = async () =>
    (
      await measured("init", () =>
        submitMissingScriptSourceInit({
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
          fraudulentBlockOutRef: block.disputedBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      )
    ).nextThreadOutRef;
  const step01 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    options: {
      readonly purposeKind?: MissingScriptSourcePurposeKind;
      readonly purposeIndex?: bigint;
      readonly referenceScriptUtxo?: UTxO;
    } = {},
  ) => {
    const purposeKind = options.purposeKind ?? evidence.finding.purposeKind;
    const purposeIndex =
      options.purposeIndex ?? BigInt(evidence.finding.purposeIndex);
    const shared = {
      ...common,
      threadOutRef,
      header: block.header,
      executionIndex: BigInt(evidence.finding.executionIndex),
      purposeKind,
      purposeIndex,
      referenceScriptUtxo: options.referenceScriptUtxo ?? references[0]!,
    };
    return (
      await measured(
        `step01-${evidence.finding.subject.direction === 0n ? "accepted" : "forced"}`,
        () =>
          evidence.finding.subject.direction === 0n
            ? submitMissingScriptSourceStep01Accepted({
                ...shared,
                blueprint: harness.realBlueprint,
                network,
                stateQueueBlockOutRef: block.disputedBlockOutRef,
                txInclusion: block.block.txInclusion!,
                witnessReferenceScripts: harness.witnessReferenceScripts,
              })
            : submitMissingScriptSourceStep01Forced({
                ...shared,
                membership: block.forcedMembership!,
              }),
      )
    ).nextThreadOutRef;
  };
  const step02 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    authentication: ExecutionSourceAuthenticationData,
    referenceScriptUtxo: UTxO = references[1]!,
  ) =>
    (
      await measured("step02-trace", () =>
        submitMissingScriptSourceStep02({
          ...common,
          threadOutRef,
          evidence,
          authentication,
          referenceScriptUtxo,
        }),
      )
    ).nextThreadOutRef;
  const step03 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    authentication: ExecutionSourceAuthenticationData,
  ) =>
    (
      await measured("step03-frontiers", () =>
        submitMissingScriptSourceStep03({
          ...common,
          threadOutRef,
          evidence,
          authentication,
          referenceScriptUtxo: references[2]!,
        }),
      )
    ).nextThreadOutRef;
  const step04 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
  ) =>
    (
      await measured("step04-open-scan", () =>
        submitMissingScriptSourceStep04({
          ...common,
          threadOutRef,
          evidence,
          referenceScriptUtxo: references[3]!,
        }),
      )
    ).nextThreadOutRef;
  const step05 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    options: { readonly itemBudget?: number; readonly label?: string } = {},
  ) =>
    await measured(options.label ?? "step05-scan", () =>
      submitMissingScriptSourceStep05({
        ...common,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        ...(options.itemBudget === undefined
          ? {}
          : { itemBudget: options.itemBudget }),
      }),
    );
  /** Every scan batch in order; returns the step-06 thread and batch count. */
  const scan = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
  ) => {
    let cursor = threadOutRef;
    let batches = 0;
    for (;;) {
      const result = await step05(cursor, evidence, {
        label: `step05-scan-${batches.toString()}`,
      });
      batches += 1;
      cursor = result.nextThreadOutRef;
      if (result.closed) break;
      if (batches > 1_000)
        throw new Error("missing-script-source scan did not close");
    }
    return { threadOutRef: cursor, batches };
  };
  const step06 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
  ) =>
    await measured("step06-mint", () =>
      submitMissingScriptSourceStep06({
        ...common,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const cancel = async (threadOutRef: string, stepIndex: number) => {
    const result = await measured(
      `cancel-step-0${(stepIndex + 1).toString()}`,
      () =>
        submitMissingScriptSourceCancel({
          ...common,
          threadOutRef,
          referenceScriptUtxo: references[stepIndex]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(result.cancelledStepIndex).toBe(stepIndex);
    return result;
  };
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
    const stepEntry = (
      step: MissingScriptSourceContracts["steps"][number],
    ) => ({
      scriptHash: step.spendingScriptHash,
      contract: {
        type: step.spendingScript.type,
        cborHex: step.spendingScript.script,
      },
    });
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        fraudProofMissingScriptSource: stepEntry(contracts.steps[0]),
        ...Object.fromEntries(
          contracts.steps
            .slice(1)
            .map((step, index) => [
              `fraudProofMissingScriptSourceStep0${(index + 2).toString()}`,
              stepEntry(step),
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
        fraudCategory: "missingScriptSource",
        fraudulentHeaderHash: block.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "missing-script-source-lifecycle",
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
    expect(removal.fraudulentHeaderHash).toBe(block.setup.headerHash);
    expect(leaseReleased).toBe(true);
    return removal;
  };
  const assertFit = (label: string) => {
    for (const { stage, measurement } of measurements)
      expectProofFit({
        stage: `${label}:${stage}`,
        measurement,
        maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
        maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
      });
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[missing-script-source-fit:${label}] ${JSON.stringify(
          measurements.map(({ stage, measurement }) => ({
            stage,
            bytes: measurement.completeSignedBytes,
            margin: measurement.l1ByteMargin,
            memory: measurement.executionMemory.toString(),
            cpu: measurement.executionSteps.toString(),
          })),
        )}`,
      );
  };
  return {
    init,
    step01,
    step02,
    step03,
    step04,
    step05,
    scan,
    step06,
    cancel,
    remove,
    assertFit,
    measurements,
  };
};
