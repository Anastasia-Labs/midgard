/** The withdrawal-mistag own-thread reclaim scene: a header another prover
 * convicts and removes while a computation thread of ours stands on it
 * (`withdrawal-mistag-own-thread-reclaim.test.ts`). */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import {
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import {
  type ResolvedProverSigner,
  resolveProverSigner,
} from "../../src/runtime.js";
import {
  createWithdrawalMistagTransactionPort,
  withdrawalMistagEvidenceRequirement,
} from "../../src/withdrawal-mistag/workflow.js";
import { WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { createCursorFamilyWorkflowAdapter } from "../../src/workflow/cursor-family-adapter.js";
import { WITHDRAWAL_MISTAG_CURSOR_SPEC } from "../../src/workflow/cursor-family-spec.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../../src/workflow/family-l1-observation.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
} from "../../src/workflow/orchestrator.js";
import { runFraudProofWorkflow } from "../../src/workflow/orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import { createLinearCancelReclaimPort } from "../../src/workflow/own-thread-reclaim.linear-cancel-port.js";
import { withOwnThreadReclaim } from "../../src/workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  withRawDatumPreimagePrerequisite,
} from "../../src/workflow/raw-datum-preimage-prerequisite.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import {
  DEPLOYMENT_FINGERPRINT,
  releaseFinalityAuthority,
  terminalVerifier,
} from "../workflow.make-adapter.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./emulator/emulator-context.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import { submitSecondHeaderTx, submitSetupTx } from "./emulator/setup-tx.js";
import {
  network,
  publishRemovalReferenceScripts,
} from "./submit-init-emulator-shared.js";
import { publishWithdrawalMistagScripts } from "./withdrawal-mistag-emulator.drive-withdrawal-mistag-to-fraud.js";
import { makeWithdrawalMistagEmulatorHarness } from "./withdrawal-mistag-emulator.js";
import { withdrawalMistagRetainedFixture } from "./withdrawal-mistag-retained.js";
import { createTestWorkflowProcess } from "./workflow-process.js";

const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
export const FINAL_DEPTH = finalityPolicy.automaticRecoveryMaxDepth + 2;
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;
const ROLES = [
  "computation_thread_step_01",
  "computation_thread_step_02",
  "computation_thread_step_03",
  "computation_thread_step_04",
  "computation_thread_step_05",
] as const;
const DATUM_SCHEMAS = [
  SDKFraudProof.FraudProofComputationThreadStepDatum,
  SDK.WithdrawalMistagStep02Datum,
  SDK.WithdrawalMistagStep03Datum,
  SDK.WithdrawalMistagStep04Datum,
  SDK.WithdrawalMistagStep05Datum,
];

/**
 * A withdrawal-mistag header on the emulator, its scripts published, and a
 * workflow runner per prover over the recorded raw L1. `reclaiming` puts
 * the reclaim of our own threads in front of that prover's family adapter,
 * as a deployment wires it.
 */
export const scene = async () => {
  const recorder = recordCrossBlockRawEmulator();
  const h = await makeWithdrawalMistagEmulatorHarness();
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
  const headerHash = committed.headerHash;
  const steps = (await publishWithdrawalMistagScripts({ harness: h })).refs;
  const removal = await publishRemovalReferenceScripts({
    lucid: h.proverLucid,
    contracts: h.contracts,
  });
  // Another prover with its own wallet and key, funded from ours.
  const theirSigner = resolveProverSigner({
    network,
    walletSeedPhrase: generateEmulatorAccount({ lovelace: 0n }).seedPhrase,
  });
  expect(theirSigner.paymentKeyHash).not.toBe(h.proverSigner.paymentKeyHash);
  const funding = await h.proverLucid
    .newTx()
    .pay.ToAddress(theirSigner.address, { lovelace: 4_000_000_000n })
    .pay.ToAddress(theirSigner.address, { lovelace: 1_000_000_000n })
    .complete({ localUPLCEval: true });
  await h.proverLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const theirLucid = await Lucid(h.emulator, "Custom");
  theirSigner.selectWallet(theirLucid);
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => h.emulator.now());
  h.emulator.awaitBlock(DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth);
  const binding = {
    definition: { headerHash },
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
      deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
      policy: finalityPolicy,
    },
    releaseEconomics: {
      schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
      policy: economicsPolicy,
    },
  };
  const leaseCoordinator = {
    acquire: async () => ({
      token: "withdrawal-mistag-lease",
      source: "emulator",
      renew: async () => {},
      release: async () => {},
      fail: async () => {},
    }),
  };
  const decision = await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
    fixture.evidence,
    fixture.context,
  );
  const l1For = (signer: ResolvedProverSigner) =>
    createFraudProofFamilyRawL1ObservationPort({
      authority: recorder.authority,
      releaseFinality: binding.releaseFinality,
      releaseEconomics: binding.releaseEconomics,
      definition: {
        category: "withdrawalMistag",
        categoryId: h.category.categoryId,
        headerHash,
        proverCredential: signer.paymentKeyHash,
        stateQueue: {
          policyId: h.contracts.stateQueue.policyId,
          address: h.contracts.stateQueue.spendingScriptAddress,
        },
        computationThread: {
          policyId: h.withdrawalMistag.computationThread.policyId,
          steps: h.withdrawalMistag.steps.map((step, index) => ({
            role: ROLES[index]!,
            address: step.spendingScriptAddress,
            datumSchema: DATUM_SCHEMAS[index]!,
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
  /** One prover's process: its adapter and in-flight set, kept across its
   * runs. A restart is a new runner: it starts with nothing in flight. */
  const runner = async ({
    lucid,
    signer,
    reclaiming,
  }: {
    readonly lucid: LucidEvolution;
    readonly signer: ResolvedProverSigner;
    readonly reclaiming: boolean;
  }) => {
    const l1 = l1For(signer);
    const process = createTestWorkflowProcess();
    const adapter = (() => {
      const prerequisite = () =>
        createAuthenticatedRawDatumPreimagePrerequisitePort({
          category: "withdrawalMistag",
          lucid,
          network: "Custom",
          signer,
          publications: l1.publications,
          requirementForAction: withdrawalMistagEvidenceRequirement,
        });
      const base = withRawDatumPreimagePrerequisite({
        category: "withdrawalMistag",
        prerequisite: prerequisite(),
        base: createCursorFamilyWorkflowAdapter({
          spec: WITHDRAWAL_MISTAG_CURSOR_SPEC,
          l1,
          transactions: createWithdrawalMistagTransactionPort({
            evidencePrerequisite: prerequisite(),
            binding: binding as never,
            lucid,
            signer,
            contracts: h.withdrawalMistag,
            references: {
              steps,
              witnesses: h.witnessReferenceScripts as Required<
                typeof h.witnessReferenceScripts
              >,
            },
            replayContext: fixture.context,
            stateQueueMutationLeaseCoordinator: leaseCoordinator,
          }),
        }),
      });
      return reclaiming
        ? withOwnThreadReclaim({
            category: "withdrawalMistag",
            base,
            releaseFinality: binding.releaseFinality,
            reclaim: createLinearCancelReclaimPort({
              family: "withdrawalMistag",
              lucid,
              signer,
              categoryId: h.category.categoryId,
              contracts: () => h.withdrawalMistag,
              stepReferences: steps,
              witnessReferenceScripts: h.witnessReferenceScripts,
              l1: {
                observeOwnThreads: async (input) =>
                  await l1.observeOwnThreads!(input),
              },
            }),
          })
        : base;
    })();
    const run = async (): Promise<FraudProofWorkflowRunResult> =>
      await runFraudProofWorkflow({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        evidence: fixture.evidence,
        detections: decision.detections,
        replayContext: fixture.context,
        registry: createFraudProofWorkflowRegistry({
          adapters: [adapter],
          launchScope: ["withdrawalMistag"],
        }),
        run: process.run({ category: "withdrawalMistag", headerHash }),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
        maxActions: 1,
      });
    return { l1, run, process };
  };
  return {
    h,
    headerHash,
    theirLucid,
    theirSigner,
    runner,
    restore: () => {
      clock.mockRestore();
      recorder.restore();
    },
  };
};

/** Our run, until a thread of ours stands on the header. */
export const openOurThread = async (
  s: Awaited<ReturnType<typeof scene>>,
  ours: Awaited<ReturnType<Awaited<ReturnType<typeof scene>>["runner"]>>,
) => {
  for (let pass = 0; pass < 12; pass += 1) {
    const threads = await ours.l1.observeOwnThreads!({
      headerHash: s.headerHash,
    });
    if (threads.standing !== undefined) return threads.standing;
    expect((await ours.run()).kind).toBe("pending");
    s.h.emulator.awaitBlock();
  }
  throw new Error("our run never opened a thread");
};

/** The other prover's run, until its record removed the header. */
export const theyConvictAndRemove = async (
  s: Awaited<ReturnType<typeof scene>>,
  theirs: Awaited<ReturnType<Awaited<ReturnType<typeof scene>>["runner"]>>,
) => {
  for (let pass = 0; pass < 40; pass += 1) {
    if (
      (await theirs.l1.observe({ headerHash: s.headerHash })).stage.kind ===
      "removed"
    )
      return;
    expect((await theirs.run()).kind).toBe("pending");
    s.h.emulator.awaitBlock();
  }
  throw new Error("the other prover never removed the header");
};
