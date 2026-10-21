/** The convicted block's removal for the runner on the Open scene: the
 * family's authenticated raw-L1 observation over the recorded emulator, the
 * fixture state-queue mutation lease, and the removal's published reference
 * scripts. The L1 view can be held at one capture, as an observer that has
 * not yet seen the transactions after it. */
import {
  watcherDeploymentReleaseEconomicsAuthority,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";
import { makeWatcherDeploymentAuthorityFixture } from "midgard-watcher/tests/support/deployment-authority-fixture";

import {
  validationTraceComputationSteps,
  type ValidationTraceRemovalConfig,
} from "../../src/validation-trace-dispute/removal.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyRawL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../../src/workflow/family-l1-observation.js";
import type { FraudProofWorkflowTerminalVerifier } from "../../src/workflow/orchestrator.js";
import {
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "../../src/workflow/raw-l1-snapshot.js";
import type { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { fixtureLeaseCoordinator } from "./script-decode-installed-host.js";
import { publishRemovalReferenceScripts } from "./submit-init-emulator-shared.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

type Snapshot = Awaited<
  ReturnType<FraudProofRawL1SnapshotAuthority["capture"]>
>;

export type RecordedRemoval = Readonly<{
  removal: ValidationTraceRemovalConfig;
  l1For: (
    headerHash: string,
  ) => FraudProofFamilyL1ObservationPort<"validationTraceDispute">;
  terminalVerifier: (headerHash: string) => FraudProofWorkflowTerminalVerifier;
  /** Keeps the L1 view at its next capture until `releaseView`. */
  holdView: () => void;
  releaseView: () => void;
}>;

export const recordedRemoval = async (
  scene: Scene,
  recorder: ReturnType<typeof recordCrossBlockRawEmulator>,
): Promise<RecordedRemoval> => {
  const { h, chain } = scene;
  const fixture = makeWatcherDeploymentAuthorityFixture();
  const identity = fixture.result;
  const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
    identity,
  ).verifyForWorkflow({ deploymentFingerprint: identity.manifestId });
  const releaseEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    identity,
  ).verifyForWorkflow({ deploymentFingerprint: identity.manifestId });
  const published = await publishRemovalReferenceScripts({
    lucid: h.proverLucid,
    contracts: h.contracts,
  });
  const signed = emulatorSignedTransactionRecovery(h.emulator);
  let held: { snapshot?: Snapshot } | undefined;
  const authority: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) => {
      if (held === undefined) return await recorder.authority.capture(request);
      held.snapshot ??= await recorder.authority.capture(request);
      return held.snapshot;
    },
  };
  const category = h.catalogue.categories.validationTraceDispute;
  if (category === undefined)
    throw new Error("catalogue has no validation-trace category");
  const ports = new Map<
    string,
    FraudProofFamilyL1ObservationPort<"validationTraceDispute">
  >();
  const l1For = (headerHash: string) => {
    const existing = ports.get(headerHash);
    if (existing !== undefined) return existing;
    const port = createFraudProofFamilyRawL1ObservationPort({
      authority,
      releaseFinality,
      releaseEconomics,
      definition: {
        category: "validationTraceDispute",
        categoryId: category.categoryId,
        headerHash,
        proverCredential: h.proverSigner.paymentKeyHash,
        stateQueue: {
          policyId: h.contracts.stateQueue.policyId,
          address: h.contracts.stateQueue.spendingScriptAddress,
        },
        computationThread: {
          policyId: h.contracts.computationThread.policyId,
          steps: validationTraceComputationSteps(chain),
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
    ports.set(headerHash, port);
    return port;
  };
  return {
    removal: {
      l1For,
      stateQueueMutationLeaseCoordinator: fixtureLeaseCoordinator(),
      // The removal resolves 00000006 from the deployment as production
      // does: its chain re-derived under the manifest's dispute bond and the
      // case configuration's one-shot, which the scene applied.
      deploymentInfo: {
        ...buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
          removalReferenceScripts: published.published,
        }),
        availabilityChallenge: (
          fixture.signedIdentity.manifest as {
            readonly availabilityChallenge: unknown;
          }
        ).availabilityChallenge,
        hubOracleOneShot: {
          txHash: h.nonceUtxo.txHash,
          outputIndex: h.nonceUtxo.outputIndex,
        },
      },
      fraudProverRewardLovelace: BigInt(
        releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    },
    l1For,
    terminalVerifier: (headerHash) =>
      createFraudProofFamilyAuthenticatedL1TerminalVerifier({
        ...l1For(headerHash),
        observeSignedTransaction: signed.observeSignedTransaction,
      }),
    holdView: () => {
      held = {};
    },
    releaseView: () => {
      held = undefined;
    },
  };
};
