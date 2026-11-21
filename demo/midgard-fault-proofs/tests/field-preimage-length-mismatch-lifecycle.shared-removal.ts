import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { afterEach, expect, vi } from "vitest";

import { createFieldPreimageLengthRecoveryPorts } from "../src/field-preimage-length-mismatch/recovery.js";
import { FIELD_PREIMAGE_LENGTH_CURSOR_SPEC } from "../src/field-preimage-length-mismatch/workflow-spec.js";
import type { StateQueueMutationLeaseCoordinator } from "../src/remove-fraudulent-block.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.create-cursor-family-workflow-adapter.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../src/workflow/family-l1-observation.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
} from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  workflowPreflightTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "../src/workflow/transaction-boundary.js";
import type { setup } from "./field-preimage-length-mismatch-lifecycle.setup.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { network } from "./support/emulator/blueprints.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import {
  emulatorSuccessorHeaderStart,
  submitSuccessorBlockTx,
} from "./support/submit-init/successor.js";
import { countedTransactionsRoot } from "./support/submit-init/transaction-material.js";

const recorders = new Set<ReturnType<typeof recordCrossBlockRawEmulator>>();
afterEach(() => {
  for (const recorder of recorders) recorder.restore();
  recorders.clear();
});
export const createFieldRecoveryRecorder = () => {
  const recorder = recordCrossBlockRawEmulator();
  recorders.add(recorder);
  return recorder;
};

export const appendFieldRecoverySuccessor = async (
  fixture: Awaited<ReturnType<typeof setup>>,
) => {
  let predecessorHeader = fixture.fraudulentHeader;
  let predecessorHash = fixture.fraudulent.headerHash;
  let anchorBlockUnit = fixture.fraudulent.stateQueueBlockUnit;
  let activeOperatorNode = fixture.fraudulent.activeOperatorNode;
  let targetOutRef = fixture.fraudulent.fraudulentBlockOutRef;
  for (let index = 0; index < 2; index += 1) {
    const successorValidFrom = Number(predecessorHeader.endTime - 60_000n);
    const millisecondsToAdvance =
      successorValidFrom - fixture.harness.emulator.now() + 1_000;
    if (millisecondsToAdvance > 0)
      fixture.harness.emulator.awaitSlot(
        Math.ceil(millisecondsToAdvance / 1_000),
      );
    const header = {
      ...makeHeader(
        predecessorHeader.operatorVkey,
        emulatorSuccessorHeaderStart({
          predecessorEndTime: predecessorHeader.endTime,
          emulator: fixture.harness.emulator,
        }),
        await countedTransactionsRoot(fixture.transactionsRoot, 1n),
        1n,
      ),
      prevHeaderHash: predecessorHash,
      prevUtxosRoot: predecessorHeader.utxosRoot,
      utxosRoot: predecessorHeader.utxosRoot,
    };
    const successor = await submitSuccessorBlockTx({
      lucid: fixture.harness.funderLucid,
      emulator: fixture.harness.emulator,
      contracts: fixture.harness.contracts,
      anchorBlockUnit,
      header,
      hubOracle: fixture.fraudulent.hubOracle,
      scheduler: fixture.fraudulent.scheduler,
      activeOperatorNode,
      activeOperatorNodeUnit: fixture.fraudulent.activeOperatorNodeUnit,
    });
    if (index === 0) targetOutRef = successor.continuedAnchorOutRef;
    predecessorHeader = successor.header;
    predecessorHash = successor.successorHeaderHash;
    anchorBlockUnit = successor.successorBlockUnit;
    activeOperatorNode = successor.activeOperatorNode;
  }
  return targetOutRef;
};

/** Real removal bodies through the shared family port, reopened after every peel. */
export const removeThroughSharedFieldRecovery = async (
  fixture: Awaited<ReturnType<typeof setup>>,
  recorder: ReturnType<typeof recordCrossBlockRawEmulator>,
  expectedBondInputs: readonly boolean[],
) => {
  const { harness: h, config } = fixture;
  const published = await publishRemovalReferenceScripts({
    lucid: h.proverLucid,
    contracts: h.contracts,
  });
  const parameters = SDK.getProtocolParameters(network);
  const deploymentFingerprint = "aa".repeat(32);
  const blueprintHash = "bb".repeat(32);
  const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
  const releaseFinality = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  };
  const economicsPolicy = {
    profile: "bounded-acceptance-v1" as const,
    requiredBondLovelace: parameters.required_bond.toString(),
    slashingPenaltyLovelace: parameters.slashing_penalty.toString(),
    fraudProverRewardLovelace: parameters.fraud_prover_reward.toString(),
    inactivitySlashingPenaltyLovelace:
      parameters.inactivity_slashing_penalty.toString(),
    proverCollateralFloorLovelace: "5000000",
  };
  const releaseEconomics = {
    schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash,
    policyDigest:
      computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
    policy: economicsPolicy,
  };
  const chain = config.contracts.fieldPreimageLengthMismatch;
  const l1 = createFraudProofFamilyRawL1ObservationPort({
    authority: recorder.authority,
    releaseFinality,
    releaseEconomics,
    definition: {
      category: "fieldPreimageLengthMismatch",
      categoryId: "00000020",
      headerHash: fixture.fraudulent.headerHash,
      proverCredential: h.proverSigner.paymentKeyHash,
      stateQueue: {
        policyId: h.contracts.stateQueue.policyId,
        address: h.contracts.stateQueue.spendingScriptAddress,
      },
      computationThread: {
        policyId: h.contracts.computationThread.policyId,
        steps: chain.steps.map((step, index) => ({
          role: (
            [
              "computation_thread_step_01",
              "computation_thread_step_02",
              "computation_thread_step_03",
              "computation_thread_step_04",
            ] as const
          )[index]!,
          address: step.spendingScriptAddress,
          datumSchema: [
            SDKFraudProof.FieldPreimageLengthStep01DatumSchema,
            SDKFraudProof.FieldPreimageLengthStep02DatumSchema,
            SDKFraudProof.FieldPreimageLengthStep02DatumSchema,
            SDKFraudProof.FieldPreimageLengthStep03DatumSchema,
          ][index]!,
        })),
      },
      proofToken: {
        policyId: h.contracts.fraudProof.policyId,
        address: h.contracts.fraudProof.spendingScriptAddress,
      },
      operatorDirectory: {
        activePolicyId: h.contracts.activeOperators.policyId,
        activeAddress: h.contracts.activeOperators.spendingScriptAddress,
        retiredPolicyId: h.contracts.retiredOperators.policyId,
        retiredAddress: h.contracts.retiredOperators.spendingScriptAddress,
      },
      schedulerAddress: h.contracts.scheduler.spendingScriptAddress,
    },
  });
  const lease = {
    token: "field-recovery-descendant-removal",
    source: "emulator-shared-cursor",
    renew: async () => undefined,
    release: async () => undefined,
    fail: async (reason: string) => {
      throw new Error(reason);
    },
  };
  const stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator =
    { acquire: async () => lease };
  const binding = {
    ...config.binding,
    deploymentFingerprint,
    releaseFinality,
    releaseEconomics,
    deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
      removalReferenceScripts: published.published,
    }),
  };
  const adapter = () => {
    // A fresh port has no in-memory proof material. Removal is authenticated
    // exclusively by the exact L1 proof token and current queue topology.
    const ports = createFieldPreimageLengthRecoveryPorts({
      config: { ...config, binding },
      binding,
      l1,
      stateQueueMutationLeaseCoordinator,
    });
    return createCursorFamilyWorkflowAdapter({
      spec: FIELD_PREIMAGE_LENGTH_CURSOR_SPEC,
      l1,
      transactions: ports.transactions,
    });
  };
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
    deploymentFingerprint,
    category: "fieldPreimageLengthMismatch",
    target: {
      kind: "state_queue_header",
      headerHash: fixture.fraudulent.headerHash,
    },
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => h.emulator.now());
  try {
    const transactions: { kind: "remove-successor" | "remove-target" }[] = [];
    let measurement:
      | Awaited<ReturnType<typeof captureEmulatorSubmission>>["measurement"]
      | undefined;
    const actionIds = new Set<string>();
    const txHashes = new Set<string>();
    for (const [index, expectsBond] of expectedBondInputs.entries()) {
      const context = {
        identity,
        workflowId,
        artifact: {},
      };
      const current = adapter();
      const observed = await current.observe(context);
      if (observed.kind !== "action_required")
        throw new Error("missing shared removal action");
      const action = observed.action;
      expect(action.input.stage).toBe("remove");
      expect(actionIds.has(action.actionId)).toBe(false);
      actionIds.add(action.actionId);
      const preflight = await current.preflight({ ...context, action });
      expect(txHashes.has(preflight.txHash)).toBe(false);
      txHashes.add(preflight.txHash);
      const signed = workflowPreflightTransaction(preflight)!;
      expect(workflowTransactionInputOutRefs(signed)).toContain(
        action.input.nextRemovalOutRef,
      );
      expect(workflowTransactionReferenceInputOutRefs(signed)).toContain(
        action.input.fraudProofOutRef,
      );
      const liveBonds = await h.proverLucid.utxosAtWithUnit(
        h.contracts.activeOperators.spendingScriptAddress,
        fixture.fraudulent.activeOperatorNodeUnit,
      );
      expect(liveBonds).toHaveLength(expectsBond ? 1 : 0);
      if (expectsBond) {
        const bond = liveBonds[0]!;
        expect(workflowTransactionInputOutRefs(signed)).toContain(
          `${bond.txHash}#${bond.outputIndex}`,
        );
      }
      const outputs = signed.toTransaction().body().outputs();
      const rewards = Array.from({ length: outputs.len() }, (_, n) =>
        outputs.get(n),
      ).filter(
        (output) => output.amount().coin() === parameters.fraud_prover_reward,
      );
      expect(rewards).toHaveLength(expectsBond ? 1 : 0);
      if (expectsBond)
        expect(signed.toTransaction().body().fee()).toBe(
          parameters.slashing_penalty,
        );
      const capture = await captureEmulatorSubmission(h.emulator, () =>
        current.submit({ ...context, action, preflight }),
      );
      expect(capture.result).toEqual({
        kind: "submitted",
        txHash: preflight.txHash,
      });
      measurement = capture.measurement;
      h.emulator.awaitBlock();
      await expect(
        l1.transactionConfirmed({
          headerHash: fixture.fraudulent.headerHash,
          txHash: preflight.txHash,
          removal: {
            targetOutRef: String(action.input.stateQueueBlockOutRef),
            inputOutRef: "ee".repeat(32) + "#0",
            proofOutRef: String(action.input.fraudProofOutRef),
          },
        }),
      ).resolves.toBe(false);
      await expect(
        l1.transactionConfirmed({
          headerHash: fixture.fraudulent.headerHash,
          txHash: preflight.txHash,
          removal: {
            targetOutRef: String(action.input.stateQueueBlockOutRef),
            inputOutRef: String(action.input.nextRemovalOutRef),
            proofOutRef: "ee".repeat(32) + "#0",
          },
        }),
      ).resolves.toBe(false);
      const after = await l1.observe({
        headerHash: fixture.fraudulent.headerHash,
      });
      const receipt = {
        inputOutRef: String(action.input.nextRemovalOutRef),
        targetOutRef: String(action.input.stateQueueBlockOutRef),
        proofOutRef: String(action.input.fraudProofOutRef),
        ...(after.stage.kind === "proof_token"
          ? {
              continuation: {
                targetOutRef: after.stage.stateQueueBlockOutRef,
                nextRemovalOutRef: after.stage.nextRemovalOutRef,
              },
            }
          : {}),
      };
      for (const removal of [
        { ...receipt, targetOutRef: "ee".repeat(32) + "#0" },
        ...(receipt.continuation === undefined
          ? []
          : [
              {
                ...receipt,
                continuation: {
                  ...receipt.continuation,
                  nextRemovalOutRef: "ee".repeat(32) + "#0",
                },
              },
              {
                ...receipt,
                continuation: {
                  ...receipt.continuation,
                  targetOutRef: receipt.targetOutRef,
                },
              },
            ]),
      ]) {
        await expect(
          l1.transactionConfirmed({
            headerHash: fixture.fraudulent.headerHash,
            txHash: preflight.txHash,
            removal,
          }),
        ).resolves.toBe(false);
      }
      transactions.push({
        kind:
          index + 1 === expectedBondInputs.length
            ? "remove-target"
            : "remove-successor",
      });
    }
    expect(
      (await l1.observe({ headerHash: fixture.fraudulent.headerHash })).stage
        .kind,
    ).toBe("removed");
    if (measurement === undefined)
      throw new Error("missing removal measurement");
    return {
      result: { transactions, fraudCategoryId: "00000020" },
      measurement,
    };
  } finally {
    clock.mockRestore();
  }
};
