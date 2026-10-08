import {
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import type {
  RemoveFraudulentBlockExplicitCategory,
  RemoveFraudulentBlockFraudCategory,
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type {
  FraudProofWorkflowTerminal,
  JournalJsonObject,
} from "../workflow/journal.js";
import {
  type FraudProofL1Source,
  fraudProofSignedTransactionRecovery,
  withFraudProofL1Recovery,
} from "../workflow/l1-source.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import {
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1,
  deriveFraudProofRawL1FamilyStage,
  deriveRetainedStateQueueHeaderObservationFromRawL1,
  type FraudProofRawL1FamilyDefinition,
  fraudProofRawL1SnapshotRequestForFamily,
} from "../workflow/raw-l1-family-derivation.js";
import {
  createFraudProofAuthenticatedPublicationObserver,
  type FraudProofAuthenticatedPublicationObserver,
} from "../workflow/raw-l1-publication-observation.js";
import {
  admitFraudProofRawL1Snapshot,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "../workflow/raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseEconomicsPolicy } from "../workflow/release-economics-policy.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";
import type { NetworkIdContracts } from "./contracts.js";
import type { NetworkIdCatalogueCategory } from "./submit-common.js";
import {
  type NetworkIdRawL1ObservationPort,
  type NetworkIdWorkflowTerminalFacts,
  parseMutationLeaseRecovery,
} from "./workflow-adapter.admit-network-id-forced-artifact.js";

export const recoverMutationLease = async ({
  config,
  txHash,
  durableRecovery,
  mutationLeaseByTxHash,
}: {
  readonly config: NetworkIdWorkflowAdapterConfig;
  readonly txHash: string;
  readonly durableRecovery: JournalJsonObject | undefined;
  readonly mutationLeaseByTxHash: Map<string, StateQueueMutationLease>;
}): Promise<
  | { readonly kind: "ok"; readonly lease: StateQueueMutationLease | undefined }
  | { readonly kind: "conflict"; readonly reason: string }
> => {
  let identity: { readonly token: string; readonly source: string } | undefined;
  try {
    identity = parseMutationLeaseRecovery(durableRecovery);
  } catch (cause) {
    return {
      kind: "conflict",
      reason: cause instanceof Error ? cause.message : String(cause),
    };
  }
  if (identity === undefined) return { kind: "ok", lease: undefined };
  const cached = mutationLeaseByTxHash.get(txHash);
  if (cached !== undefined) {
    if (cached.token !== identity.token || cached.source !== identity.source) {
      return {
        kind: "conflict",
        reason: "network-id cached mutation lease changed its fencing identity",
      };
    }
    return { kind: "ok", lease: cached };
  }
  const resume = config.removal.stateQueueMutationLeaseCoordinator?.resume;
  if (resume === undefined) {
    return {
      kind: "conflict",
      reason:
        "network-id mutation-lease coordinator cannot resume the journaled fencing token",
    };
  }
  try {
    const lease = await resume(identity);
    mutationLeaseByTxHash.set(txHash, lease);
    return { kind: "ok", lease };
  } catch (cause) {
    return {
      kind: "conflict",
      reason: `journaled network-id mutation lease cannot be resumed: ${String(cause)}`,
    };
  }
};

export const createNetworkIdRawL1ObservationPort = ({
  authority,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: "networkId";
  };
}): NetworkIdRawL1ObservationPort & {
  readonly publications: FraudProofAuthenticatedPublicationObserver;
} => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY ||
    definition.computationThread.steps.length !== 2
  ) {
    throw new Error("network-id raw L1 observation authority is incomplete");
  }
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition,
    releaseFinality,
  });
  const capture = async (headerHash: string) => {
    if (headerHash !== definition.headerHash) {
      throw new Error("network-id raw L1 observation changed the header");
    }
    return admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
      observationDepth: "inclusion",
    });
  };
  return {
    publications: createFraudProofAuthenticatedPublicationObserver({
      authority,
      releaseFinality,
    }),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      (await capture(headerHash)).transactions.some(
        (transaction) => transaction.txHash === txHash,
      ),
    observeHeader: async ({ headerHash }) =>
      await deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: await capture(headerHash),
        definition,
      }),
    observeRetainedHeader: async ({ headerHash }) =>
      await deriveRetainedStateQueueHeaderObservationFromRawL1({
        snapshot: await capture(headerHash),
        definition,
      }),
    observe: async ({ headerHash }) => {
      const snapshot = await capture(headerHash);
      return await deriveFraudProofRawL1FamilyStage({
        snapshot,
        definition,
        releaseEconomics,
      });
    },
  };
};

/** Production construction over the fault-proof L1 source. */
export const createNetworkIdL1ObservationPort = ({
  l1,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly l1: FraudProofL1Source;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: "networkId";
  };
}): NetworkIdRawL1ObservationPort =>
  withFraudProofL1Recovery(
    createNetworkIdRawL1ObservationPort({
      authority: l1.snapshotAuthority({
        releaseFinality,
        observationDepth: "inclusion",
      }),
      releaseFinality,
      releaseEconomics,
      definition,
    }),
    fraudProofSignedTransactionRecovery(l1, releaseFinality),
  );

/** Independent second raw-L1 observation for terminal admission. */
export const createNetworkIdAuthenticatedL1TerminalVerifier = (
  l1: NetworkIdRawL1ObservationPort,
): FraudProofWorkflowTerminalVerifier => {
  const verify = async (
    {
      identity,
      candidate,
      releaseFinality,
    }: Parameters<FraudProofWorkflowTerminalVerifier["verify"]>[0],
    inclusionOnly: boolean,
  ): Promise<FraudProofWorkflowTerminal> => {
    const minimumDepth = inclusionOnly
      ? 1
      : releaseFinality.policy.confirmationDepth;
    if (identity.target.kind !== "state_queue_header") {
      throw new Error(
        "network-id terminal requires a state-queue header target",
      );
    }
    const stage = await l1.observe({
      headerHash: identity.target.headerHash,
    });
    if (stage.kind !== "removed") {
      throw new Error(
        "authenticated L1 still reports unfinished network-id correction",
      );
    }
    const facts = (terminal: FraudProofWorkflowTerminal) => ({
      ...terminal,
      observedAt: { ...terminal.observedAt, confirmationDepth: 0 },
    });
    if (
      JSON.stringify(facts(stage.terminal)) !== JSON.stringify(facts(candidate))
    ) {
      throw new Error(
        "network-id terminal candidate differs from independent L1 observation",
      );
    }
    if (
      stage.terminal.observedAt.confirmationDepth < minimumDepth ||
      candidate.observedAt.confirmationDepth < minimumDepth ||
      stage.terminal.observedAt.confirmationDepth <
        candidate.observedAt.confirmationDepth
    ) {
      throw new Error(
        `authenticated network-id terminal depth is below release finality: required=${minimumDepth.toString()} actual=${stage.terminal.observedAt.confirmationDepth.toString()}`,
      );
    }
    return candidate;
  };
  return {
    verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
    verify: (input) => verify(input, false),
    verifyIncluded: (input) => verify(input, true),
  };
};

export type NetworkIdWorkflowAdapterConfig = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: NetworkIdContracts;
  readonly stateQueueAddress: string;
  readonly category: NetworkIdCatalogueCategory;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  /**
   * Published `fraudProofNetworkIdForcedStep` reference script. Optional in the
   * deployment shape and mandatory the moment a forced (§5.2) artifact is
   * admitted; the forced door is reference-script-only like every other step.
   */
  readonly forcedStepReferenceScript?: UTxO;
  /**
   * Published `fraudProofNetworkIdForcedScan` reference script: the resumable
   * outputs scan the forced door hands the thread to. Optional and mandatory
   * on the same terms as the forced step.
   */
  readonly forcedScanReferenceScript?: UTxO;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly removal: {
    readonly deploymentInfo: unknown;
    readonly category:
      | RemoveFraudulentBlockFraudCategory
      | RemoveFraudulentBlockExplicitCategory;
    readonly requireReferenceScripts?: boolean;
    readonly validFrom?: bigint;
    readonly validTo?: bigint;
    /** Legacy normalized-provider route only. Production derives topology raw. */
    readonly isCurrentHead?: (headerHash: string) => Promise<boolean>;
    /** Required by the production raw-L1 route for descendant fencing. */
    readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  };
  /** Strict production route; omit only in emulator/diagnostic construction. */
  readonly rawL1?: NetworkIdRawL1ObservationPort;
  /** Candidate chain facts; the shared independent verifier reauthenticates them. */
  readonly terminalFacts?: (input: {
    readonly headerHash: string;
    readonly removalTxHash: string;
    readonly proofTokenOutRef: string;
  }) => Promise<NetworkIdWorkflowTerminalFacts>;
};

export type NetworkIdRemovalConfig = NetworkIdWorkflowAdapterConfig["removal"];
