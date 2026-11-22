/**
 * A whole computation thread walked through the mempool: every action is
 * derived from the ledger with our node's mempool applied, and spends the
 * outputs the previous one created there, with no block between our own
 * submissions. Nothing completes until a block includes the removal. A
 * transaction the mempool drops is nowhere: its action is required again
 * and built anew.
 */
import { inspect } from "node:util";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  createWithdrawalMistagTransactionPort,
  withdrawalMistagEvidenceRequirement,
} from "../src/withdrawal-mistag/workflow.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import { WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.create-cursor-family-workflow-adapter.js";
import { WITHDRAWAL_MISTAG_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyRawL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
} from "../src/workflow/identity.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  withRawDatumPreimagePrerequisite,
} from "../src/workflow/raw-datum-preimage-prerequisite.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import {
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/emulator/setup-tx.js";
import { emulatorPending } from "./support/emulator-pending.js";
import { makeWithdrawalMistagEmulatorHarness } from "./support/withdrawal-mistag-emulator.build-withdrawal-mistag-evidence-material.js";
import { publishWithdrawalMistagScripts } from "./support/withdrawal-mistag-emulator.drive-withdrawal-mistag-to-fraud.js";
import { withdrawalMistagRetainedFixture } from "./support/withdrawal-mistag-retained.js";

const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;
const roles = [
  "computation_thread_step_01",
  "computation_thread_step_02",
  "computation_thread_step_03",
  "computation_thread_step_04",
  "computation_thread_step_05",
] as const;
const datumSchemas = [
  SDKFraudProof.FraudProofComputationThreadStepDatum,
  SDK.WithdrawalMistagStep02Datum,
  SDK.WithdrawalMistagStep03Datum,
  SDK.WithdrawalMistagStep04Datum,
  SDK.WithdrawalMistagStep05Datum,
];

const walkScene = async () => {
  const h = await makeWithdrawalMistagEmulatorHarness();
  // Every capture carries the emulator's mempool; the mempool layer sits
  // over the recorder's on the same instance.
  const recorder = recordCrossBlockRawEmulator({
    emulator: h.emulator,
    pending: () => mempool.pending(),
  });
  const mempool = emulatorPending(h.emulator);
  const authority = recorder.authority;
  let clock: ReturnType<typeof vi.spyOn> | undefined;
  const restore = () => {
    clock?.mockRestore();
    mempool.restore();
    recorder.restore();
  };
  try {
    const operator = getAddressDetails(
      await h.funderLucid.wallet().address(),
    ).paymentCredential!;
    const fixture = await withdrawalMistagRetainedFixture({
      direction: "valid-marked-invalid",
      operatorVkey: operator.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + 120_000,
        ) - 1,
    });
    await submitSetupTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      nonceUtxo: h.nonceUtxo,
      catalogue: h.catalogue,
      header: fixture.predecessor.header,
    });
    const committed = await submitSecondHeaderTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      header: fixture.evidence.header,
    });
    const steps = (await publishWithdrawalMistagScripts({ harness: h })).refs;
    const removal = await publishRemovalReferenceScripts({
      lucid: h.proverLucid,
      contracts: h.contracts,
    });
    clock = vi.spyOn(Date, "now").mockImplementation(() => h.emulator.now());
    // Setup is on the ledger at release depth before the walk begins.
    h.emulator.awaitBlock(DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth);
    expect(mempool.pending()).toEqual([]);
    const binding = {
      definition: { headerHash: committed.headerHash },
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
        removalReferenceScripts: removal.published,
      }),
      network: "Custom",
      resolvedContracts: { category: h.category },
      catalogue: {
        policyId: h.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          h.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: h.catalogue.root,
      },
      releaseFinality: {
        schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
        hubOraclePolicyId: "aa".repeat(28),
        blueprintHash: "bb".repeat(32),
        policyDigest:
          computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
        policy: finalityPolicy,
      },
      releaseEconomics: {
        schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
        blueprintHash: "bb".repeat(32),
        policyDigest:
          computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
        policy: economicsPolicy,
      },
    };
    const l1 = createFraudProofFamilyRawL1ObservationPort({
      authority,
      releaseFinality: binding.releaseFinality,
      releaseEconomics: binding.releaseEconomics,
      definition: {
        category: "withdrawalMistag",
        categoryId: h.category.categoryId,
        headerHash: committed.headerHash,
        proverCredential: h.proverSigner.paymentKeyHash,
        stateQueue: {
          policyId: h.contracts.stateQueue.policyId,
          address: h.contracts.stateQueue.spendingScriptAddress,
        },
        computationThread: {
          policyId: h.withdrawalMistag.computationThread.policyId,
          steps: h.withdrawalMistag.steps.map((step, index) => ({
            role: roles[index]!,
            address: step.spendingScriptAddress,
            datumSchema: datumSchemas[index]!,
          })),
        },
        proofToken: {
          policyId: h.withdrawalMistag.fraudProof.policyId,
          address: h.withdrawalMistag.fraudProof.spendingScriptAddress,
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
    const prerequisite = () =>
      createAuthenticatedRawDatumPreimagePrerequisitePort({
        category: "withdrawalMistag",
        lucid: h.proverLucid,
        network: "Custom",
        signer: h.proverSigner,
        publications: l1.publications,
        requirementForAction: withdrawalMistagEvidenceRequirement,
      });
    const port = createWithdrawalMistagTransactionPort({
      evidencePrerequisite: prerequisite(),
      binding: binding as never,
      lucid: h.proverLucid,
      signer: h.proverSigner,
      contracts: h.withdrawalMistag,
      references: {
        steps,
        witnesses: h.witnessReferenceScripts as Required<
          typeof h.witnessReferenceScripts
        >,
      },
      replayContext: fixture.context,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "decoding-lease",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    const decision = await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
      fixture.evidence,
      fixture.context,
    );
    const classification = await classifyCanonicalBlockViolations({
      evidence: fixture.evidence,
      detections: decision.detections,
      minimumConfirmationDepth: 1,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "withdrawalMistag"
    )
      throw new Error("real fixture did not detect");
    const artifact = await port.prepare({
      evidence: fixture.evidence,
      classification: { ...classification, category: "withdrawalMistag" },
    });
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
      hubOraclePolicyId: "aa".repeat(28),
      category: "withdrawalMistag",
      target: { kind: "state_queue_header", headerHash: committed.headerHash },
    };
    const context = () => ({
      identity,
      workflowId: computeFraudProofWorkflowId(identity),
      artifact: JSON.parse(JSON.stringify(artifact)),
    });
    // A fresh adapter for every action: nothing is remembered between them.
    const adapter = () =>
      withRawDatumPreimagePrerequisite({
        category: "withdrawalMistag",
        prerequisite: prerequisite(),
        base: createCursorFamilyWorkflowAdapter({
          spec: WITHDRAWAL_MISTAG_CURSOR_SPEC,
          l1,
          transactions: port,
        }),
      });
    /** Observes the next action with a fresh adapter, then builds and
     * submits it. */
    const act = async () => {
      const current = adapter();
      const observation = await current.observe(context());
      if (observation.kind !== "action_required")
        throw new Error(`walk stopped at ${inspect(observation)}`);
      const action = observation.action;
      const preflight = await current
        .preflight({ ...context(), action })
        .catch((error: unknown) => {
          throw new Error(
            `${String(action.input.stage)}: ${inspect(error, { depth: 8 })}`,
          );
        });
      await current.submit({ ...context(), action, preflight });
      return { stage: String(action.input.stage), txHash: preflight.txHash };
    };
    return {
      h,
      mempool,
      l1,
      committed,
      binding,
      context,
      adapter,
      act,
      restore,
    };
  } catch (cause) {
    restore();
    throw cause;
  }
};

it("walks Init, every step and the removal through the mempool, and completes only after a block", async () => {
  const { h, mempool, l1, committed, binding, context, adapter, act, restore } =
    await walkScene();
  try {
    const blockHeight = h.emulator.blockHeight;
    const walked: string[] = [];
    const submitted: string[] = [];
    for (let n = 0; n < 30 && walked.at(-1) !== "remove"; n++) {
      const { stage, txHash } = await act();
      // In the mempool, not on the ledger: the next action is derived from
      // it, and spends what it creates.
      expect((await h.emulator.getTransactionStatus(txHash)).status).toBe(
        "pending",
      );
      submitted.push(txHash);
      walked.push(stage);
    }
    expect(h.emulator.blockHeight).toBe(blockHeight);
    expect(walked[0]).toBe("init");
    expect(walked.at(-1)).toBe("remove");
    expect(walked.filter((stage) => stage === "remove")).toHaveLength(1);
    expect(new Set(walked).size).toBeGreaterThanOrEqual(
      WITHDRAWAL_MISTAG_CURSOR_SPEC.stepCount + 1,
    );
    expect(mempool.pending().map((tx) => tx.txHash)).toEqual(
      expect.arrayContaining(submitted),
    );

    // The removal is in the mempool: nothing is required, nothing completes,
    // and the terminal verifier refuses until the ledger shows it.
    const waiting = await adapter().observe(context());
    expect(waiting.kind).toBe("pending");
    expect(
      (await l1.observe({ headerHash: committed.headerHash })).stage,
    ).toEqual({
      kind: "pending",
      reason: "withdrawalMistag header removal awaits inclusion",
    });
    const verifier = createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1);
    await expect(
      verifier.verifyIncluded!({
        ...context(),
        releaseFinality: binding.releaseFinality,
        candidate: undefined as never,
      }),
    ).rejects.toThrow(
      "still reports an unfinished withdrawalMistag correction",
    );

    h.emulator.awaitBlock();
    expect(mempool.pending()).toEqual([]);
    for (const txHash of submitted)
      expect((await h.emulator.getTransactionStatus(txHash)).status).toBe(
        "confirmed",
      );
    const completed = await adapter().observe(context());
    if (completed.kind !== "completed")
      throw new Error(`removal did not complete: ${inspect(completed)}`);
    await expect(
      verifier.verifyIncluded!({
        ...context(),
        releaseFinality: binding.releaseFinality,
        candidate: completed.terminal,
      }),
    ).resolves.toEqual(completed.terminal);
  } finally {
    restore();
  }
}, 300_000);

it("builds an action again once the mempool drops its transaction", async () => {
  const { h, mempool, act, restore } = await walkScene();
  const status = async (txHash: string) =>
    (await h.emulator.getTransactionStatus(txHash)).status;
  try {
    const sent = await act();
    expect(sent.stage).toBe("init");
    expect(await status(sent.txHash)).toBe("pending");
    // The node discards its mempool without a block: the Init is nowhere,
    // and nothing remembers it was sent.
    mempool.drop();
    expect(mempool.pending()).toEqual([]);
    const again = await act();
    expect(again.stage).toBe("init");
    expect(await status(again.txHash)).toBe("pending");
    h.emulator.awaitBlock();
    expect(await status(again.txHash)).toBe("confirmed");
    // The walk goes on from the Init the ledger took.
    expect((await act()).stage).not.toBe("init");
  } finally {
    restore();
  }
}, 300_000);
