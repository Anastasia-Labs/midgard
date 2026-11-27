import { type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { type FaultProofFieldOpeningPlan } from "../src/field-opening.js";
import type { PreparedNetworkIdAcceptedForced } from "../src/network-id/accepted-forced.js";
import type {
  NetworkIdForcedScanPlan,
  NetworkIdForcedScanStep,
} from "../src/network-id/forced-scan-plan.advance-grammar-by.js";
import { planNetworkIdForcedScan } from "../src/network-id/forced-scan-plan.plan-network-id-forced-scan.js";
import { planNetworkIdOutputsOpening } from "../src/network-id/prepare.js";
import { buildNetworkIdAcceptedForcedBind } from "../src/network-id/submit-network-id-accepted-forced-bind.js";
import { buildNetworkIdCancel } from "../src/network-id/submit-network-id-cancel.js";
import { buildNetworkIdForcedBind } from "../src/network-id/submit-network-id-forced-bind.js";
import { buildNetworkIdForcedScanAction } from "../src/network-id/submit-network-id-forced-scan.submit-network-id-forced-scan-action.js";
import { buildNetworkIdForcedStep01 } from "../src/network-id/submit-network-id-forced-step-01.js";
import { buildNetworkIdInit } from "../src/network-id/submit-network-id-init.js";
import { buildNetworkIdStep02 } from "../src/network-id/submit-network-id-step-02.submit-network-id-step02.js";
import type { PreparedNetworkIdWrongfulRejection } from "../src/network-id/wrongful-rejection.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import {
  commitForcedBlock,
  type Harness,
  network,
} from "./network-id-wrongful-rejection-lifecycle.commit-forced-block.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { expectProofFit } from "./support/emulator/proof-fit.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import {
  makeNetworkIdEmulatorHarness,
  publishNetworkIdReferenceScriptsMeasured,
} from "./support/network-id-emulator.js";
import { buildAndSubmit, submitTurns } from "./support/submit-steps.js";

/** Stage builders over one harness; every submission is measured. */
export const makeStages = (
  harness: Harness,
  refs: readonly [UTxO, UTxO, UTxO, UTxO],
  setup: Awaited<ReturnType<typeof commitForcedBlock>>["setup"],
) => {
  const [step01Ref, step02Ref, forcedRef, scanRef] = refs;
  const measurements: {
    stage: string;
    measurement: CompleteSignedTransactionMeasurement;
  }[] = [];
  const record = <T>(
    stage: string,
    captured: Awaited<ReturnType<typeof captureEmulatorSubmission<T>>>,
  ) => {
    measurements.push({ stage, measurement: captured.measurement });
    return captured.result;
  };
  const initialize = async () => {
    console.info("[network-id-forced-stage] init");
    const result = record(
      "init",
      await captureEmulatorSubmission(harness.emulator, () =>
        buildAndSubmit(buildNetworkIdInit, {
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts: harness.networkId,
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
      ),
    );
    return `${result.txHash}#${result.firstStepOutputIndex.toString()}`;
  };
  const dispatch = async (
    threadOutRef: string,
    prepared:
      | PreparedNetworkIdWrongfulRejection
      | PreparedNetworkIdAcceptedForced,
    referenceScriptUtxo: UTxO = step01Ref,
  ) => {
    console.info("[network-id-forced-stage] dispatch");
    return record(
      "dispatch",
      await captureEmulatorSubmission(harness.emulator, () =>
        buildAndSubmit(buildNetworkIdForcedStep01, {
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          prepared,
          referenceScriptUtxo,
        }),
      ),
    ).nextThreadOutRef;
  };
  const bind = async (
    threadOutRef: string,
    prepared: PreparedNetworkIdWrongfulRejection,
    referenceScriptUtxo: UTxO = forcedRef,
  ) => {
    console.info("[network-id-forced-stage] bind");
    return record(
      "bind",
      await captureEmulatorSubmission(harness.emulator, () =>
        buildAndSubmit(buildNetworkIdForcedBind, {
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          prepared,
          referenceScriptUtxo,
        }),
      ),
    );
  };
  /** The forced door over an accepted leaf, handing step 02 its claim. */
  const acceptedBind = async (
    threadOutRef: string,
    prepared: PreparedNetworkIdAcceptedForced,
    unsafeSkipLocalViolationCheckForTest = false,
  ) => {
    console.info("[network-id-forced-stage] accepted-bind");
    return record(
      "accepted-bind",
      await captureEmulatorSubmission(harness.emulator, () =>
        buildAndSubmit(buildNetworkIdAcceptedForcedBind, {
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          prepared,
          referenceScriptUtxo: forcedRef,
          unsafeSkipLocalViolationCheckForTest,
        }),
      ),
    );
  };
  /**
   * One planned (or deliberately malformed) scan batch. Every batch re-supplies
   * the same authenticated opening, so the carriage and certificate a caller
   * publishes once are threaded through unchanged.
   */
  const scanStep = async (
    threadOutRef: string,
    prepared: PreparedNetworkIdWrongfulRejection,
    scan: NetworkIdForcedScanPlan,
    step: NetworkIdForcedScanStep,
    options: {
      readonly publish?: boolean;
      readonly certificateUtxos?: readonly UTxO[];
      readonly referenceScriptUtxo?: UTxO;
    } = {},
  ) => {
    const label = `scan-${step.kind}${
      "ordinal" in step ? `-${step.ordinal.toString()}` : ""
    }`;
    console.info(`[network-id-forced-stage] ${label}`);
    return record(
      label,
      await captureEmulatorSubmission(harness.emulator, () =>
        buildAndSubmit(buildNetworkIdForcedScanAction, {
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          network,
          signer: harness.proverSigner,
          threadOutRef,
          prepared,
          outputsOpeningPlan: planNetworkIdOutputsOpening({
            prepared,
            owner: harness.proverSigner.paymentKeyHash,
            publish: options.publish ?? false,
          }),
          scan,
          step,
          referenceScriptUtxo: options.referenceScriptUtxo ?? scanRef,
          ...(options.certificateUtxos === undefined
            ? {}
            : { certificateUtxos: options.certificateUtxos }),
        }),
      ),
    );
  };
  /** The whole planned scan, in order, returning the step-02 thread out-ref. */
  const scan = async (
    threadOutRef: string,
    prepared: PreparedNetworkIdWrongfulRejection,
    plan: NetworkIdForcedScanPlan,
    options: {
      readonly publish?: boolean;
      readonly certificateUtxos?: readonly UTxO[];
    } = {},
  ) => {
    let cursor = threadOutRef;
    for (const step of plan.steps) {
      cursor = (await scanStep(cursor, prepared, plan, step, options))
        .nextThreadOutRef;
    }
    return cursor;
  };
  /** The planned scan schedule for a prepared artifact's outputs field. */
  const scanPlanFor = (
    prepared: PreparedNetworkIdWrongfulRejection,
    publish = false,
  ) => {
    const opening = planNetworkIdOutputsOpening({
      prepared,
      owner: harness.proverSigner.paymentKeyHash,
      publish,
    });
    return {
      opening,
      plan: planNetworkIdForcedScan({
        outputsCarriagePlan: opening,
        outputCount: opening.itemCount,
      }),
    };
  };
  const finalize = async (
    threadOutRef: string,
    prepared:
      | PreparedNetworkIdWrongfulRejection
      | PreparedNetworkIdAcceptedForced,
    outputsOpeningPlan?: FaultProofFieldOpeningPlan,
    unsafeSkipLocalViolationCheckForTest = false,
  ) => {
    console.info("[network-id-forced-stage] finalize");
    return record(
      "finalize",
      await captureEmulatorSubmission(harness.emulator, () =>
        submitTurns(buildNetworkIdStep02, {
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          prepared,
          ...(outputsOpeningPlan === undefined ? {} : { outputsOpeningPlan }),
          referenceScriptUtxo: step02Ref,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          unsafeSkipLocalViolationCheckForTest,
        }),
      ),
    );
  };
  const cancel = async (
    threadOutRef: string,
    expected: "step01" | "forcedStep" | "forcedScan" | "step02",
  ) => {
    console.info(`[network-id-forced-stage] cancel-${expected}`);
    const result = record(
      `cancel-${expected}`,
      await captureEmulatorSubmission(harness.emulator, () =>
        buildAndSubmit(buildNetworkIdCancel, {
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo:
            expected === "step01"
              ? step01Ref
              : expected === "forcedStep"
                ? forcedRef
                : expected === "forcedScan"
                  ? scanRef
                  : step02Ref,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      ),
    );
    expect(result.cancelledStep).toBe(expected);
  };
  const remove = async () => {
    console.info("[network-id-forced-stage] remove");
    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    // The shared harness registers a scaffold for this family; removal must
    // see the applied chain the thread actually ran through.
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        fraudProofNetworkId: {
          scriptHash: harness.networkId.steps[0].spendingScriptHash,
        },
        fraudProofNetworkIdStep02: {
          scriptHash: harness.networkId.steps[1].spendingScriptHash,
        },
        fraudProofNetworkIdForcedStep: {
          scriptHash: harness.networkId.forcedStep!.spendingScriptHash,
        },
        fraudProofNetworkIdForcedScan: {
          scriptHash: harness.networkId.forcedScan!.spendingScriptHash,
        },
      },
    };
    const now = BigInt(harness.emulator.now());
    return record(
      "remove",
      await captureEmulatorSubmission(harness.emulator, () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          fraudCategory: "networkId",
          fraudulentHeaderHash: setup.headerHash,
          requireReferenceScripts: true,
          validFrom: now > 120_000n ? now - 120_000n : 0n,
          validTo: now + 300_000n,
        }),
      ),
    );
  };
  const assertFit = (label: string) => {
    for (const { stage, measurement } of measurements) {
      expectProofFit({
        stage: `${label}:${stage}`,
        measurement,
        maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
        maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
      });
    }
    console.info(
      `[network-id-forced-lifecycle:${label}] ${JSON.stringify(
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
    initialize,
    dispatch,
    bind,
    acceptedBind,
    scanPlanFor,
    scanStep,
    scan,
    finalize,
    cancel,
    remove,
    assertFit,
    measurements,
  };
};

export const makeHarness = async () => {
  const harness = await makeNetworkIdEmulatorHarness();
  const published = await publishNetworkIdReferenceScriptsMeasured({
    lucid: harness.proverLucid,
    contracts: harness.networkId,
  });
  for (const { name, scriptHash, measurement } of published.measurements) {
    expectProofFit({
      stage: `publication:${name}`,
      measurement,
      maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
      maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
    });
    console.info(
      `[network-id-forced-publication] ${JSON.stringify({
        name,
        scriptHash,
        bytes: measurement.completeSignedBytes,
        margin: measurement.l1ByteMargin,
      })}`,
    );
  }
  return { harness, refs: published.utxos };
};
