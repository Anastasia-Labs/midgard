import type { AvailabilityResponseAdmissionDecision } from "@al-ft/midgard-core";
import {
  computeDaSha256Hash,
  DaGossipTopic,
  decodeDaConflictEvidenceCbor,
  decodeDaConflictingSignatureHeaderEvidenceCbor,
  encodeDaConflictEvidenceCbor,
  encodeDaConflictingSignatureHeaderEvidenceCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type {
  CommitteePromiseAdmission,
  CommitteePromiseAdmissionPolicyStatus,
} from "./availability/promise-admission.js";
import { type CommitteeConfig } from "./config.js";
import type { AttestationCoordinator } from "./coordinator/coordinator.js";
import type { SubmitterReconciler } from "./coordinator/submitter-reconciler.js";
import type {
  DaGossipMessageHandler,
  DaGossipMessageHandlerContext,
} from "./da/libp2p/DaGossip.js";
import type { DaLibp2pNode } from "./da/libp2p/DaLibp2pNode.js";
import type { DaPeerRegistry } from "./da/libp2p/DaPeerRegistry.js";
import type { DaPayloadFetchFailure, DaPayloadSource } from "./da/source.js";
import type {
  DaSignatureRecord,
  DaStoredConflictEvidenceRecord,
} from "./domain.js";
import type { DaAttestationChainReader } from "./l1/da-attestation-reader.js";
import type { CommitteeL1Source } from "./l1/follower/l1-follower.js";
import {
  type DaSigner,
  type DaSignerValidation,
  verifyDaSignatureWitness,
} from "./signer.js";
import {
  type CommitteeStore,
  type CommitteeStoreReadinessCounts,
} from "./store.js";
import type { RetentionDeadlineReport } from "./store/retention.js";

export type CommitteeServiceDeps = {
  readonly config: CommitteeConfig;
  readonly store: CommitteeStore;
  /** Every L1 read the committee's decisions make: its follower. */
  readonly l1: CommitteeL1Source;
  readonly payloadSource: DaPayloadSource;
  readonly signer?: DaSigner;
  readonly signerValidation?: DaSignerValidation;
  readonly promiseAdmission?: CommitteePromiseAdmission;
  readonly coordinator?: AttestationCoordinator;
  readonly submitterReconciler?: Pick<SubmitterReconciler, "reconcileHeader">;
  readonly daChainReader?: DaAttestationChainReader;
  readonly daLibp2pNode?: Pick<
    DaLibp2pNode,
    "setGossipHandler" | "publishGossip"
  >;
  readonly daPeerRegistry?: DaPeerRegistry;
  readonly now?: () => Date;
  /** Writes one structured JSON log line; defaults to stderr. */
  readonly writeEvent?: (line: string) => void;
};

export type CommitteeTickResult = {
  readonly scannedHeaders: number;
  readonly signedHeaders: number;
  /** Headers in good standing (attested, final or reconciled), not posts. */
  readonly reconciledHeaders: number;
  readonly skippedHeaders: number;
  readonly payloadFetches: readonly CommitteePayloadFetchObservation[];
  readonly errors: readonly string[];
  /**
   * Present when the tick made no decision: why the L1 source held it. Not
   * an error; readiness reports the same reasons.
   */
  readonly held?: readonly string[];
};

export type CommitteePayloadFetchObservation = {
  readonly headerHash: string;
  readonly status: "missing_da" | "available" | "fetch_failed";
  readonly sourcePeerIds: readonly string[];
  readonly detail?: string;
};

export type CommitteeL1SubmitterPreflightSnapshot = {
  readonly status: "ready" | "funded" | "failed" | "not_required" | "not_run";
  readonly detail?: unknown;
  readonly error?: string;
};

export type CommitteeRetentionReadinessSnapshot = {
  /**
   * `skipped`: no pass has run since the view was last stale, and the last
   * pass was skipped on a view younger than the staleness bound. Only
   * `not_checked`, `failed` and `l1_view_stale` make the committee not ready.
   */
  readonly status:
    | "not_checked"
    | "ok"
    | "failed"
    | "l1_view_stale"
    | "skipped";
  readonly checkedAt?: string;
  /** Age of the last fresh authenticated L1 view, when it is stale. */
  readonly l1ViewAgeMs?: number;
  /**
   * The last pass skipped for want of a view from its own tick while the
   * last accepted view was younger than the staleness bound. Detail only.
   */
  readonly skippedPass?: {
    readonly reason: "l1_view_unavailable" | "tick_in_flight";
    readonly checkedAt: string;
    readonly l1ViewAgeMs: number;
  };
  readonly scanned: number;
  readonly retained: number;
  readonly prunable: number;
  /**
   * Payloads the opt-in deadline alert flagged in the last cycle. Informational
   * only: it never makes the committee not ready.
   */
  readonly alerting: number;
  readonly error?: string;
  /**
   * Retirements held at a header, each naming it and its cause
   * (`committee_retirement_held`): a degraded detail shown on `/readyz` and
   * in status that never makes the committee not ready. Retention keeps the
   * header's record meanwhile.
   */
  readonly holds?: readonly string[];
  /**
   * Retention pins the follower could not keep or write
   * (`committee_retention_pin_pruned`, `committee_retention_pin_failed`):
   * each makes the committee not ready, as a lost pin loses history a record
   * needs and a failed pin write holds the follower's prune.
   */
  readonly pinFailures?: readonly string[];
};

/**
 * The readiness view of one completed retention cycle: always `ok`, whatever
 * the opt-in deadline alert (`DA_RETENTION_ALERT_THRESHOLD_MS`) reported.
 * Every merged payload ages to its deadline on its normal way to pruning, so
 * with any threshold some payload is inside it whenever blocks merge more often
 * than the threshold; a deadline alert that drove readiness would keep the
 * committee not ready in steady state. The count is carried for visibility.
 */
export const retentionReadinessFromDeadlines = (
  deadlines: RetentionDeadlineReport,
): CommitteeRetentionReadinessSnapshot => ({
  status: (deadlines.recoveryProofUnavailable ?? 0) > 0 ? "failed" : "ok",
  ...((deadlines.recoveryProofUnavailable ?? 0) > 0
    ? {
        error:
          "terminal_recovery_proof_unavailable: bytes retained; the next authenticated scan retries",
      }
    : {}),
  checkedAt: new Date(deadlines.nowMs).toISOString(),
  scanned: deadlines.scanned,
  retained: deadlines.retained,
  prunable: deadlines.prunable,
  alerting: deadlines.alerting,
});

export type CommitteeReadinessPeerSnapshot = {
  readonly localPeerId?: string;
  readonly signerIndex?: number;
  readonly producerPeerIds: readonly string[];
  readonly configuredPeerCount: number;
  readonly producerTargetCount: number;
  readonly localPeerIsProducer: boolean;
  readonly l1SubmissionEnabled: boolean;
  readonly l1SubmitterId?: string;
  readonly l1SubmitterIds: readonly string[];
  readonly l1SubmitterSignerIndexes: readonly number[];
  readonly l1SubmitterPreflight: CommitteeL1SubmitterPreflightSnapshot;
};

export type CommitteeReadinessSnapshot = {
  readonly ready: boolean;
  readonly promiseAdmissionPolicy?: CommitteePromiseAdmissionPolicyStatus;
  readonly promiseAdmission?: AvailabilityResponseAdmissionDecision;
  readonly l1Source?: {
    readonly sourceMode: "local_node";
    /**
     * `intervention`: the follower holds the committee on a reason no wait
     * clears (`rollback_beyond_k`, a stuck point, no configuration); the
     * process stays up and `intervention` names it.
     */
    readonly status: "uninitialized" | "healthy" | "intervention";
    readonly intervention?: string;
    readonly observedAt?: string;
  };
  readonly deployment: {
    readonly configuredFingerprint: string;
    readonly storeFingerprint?: string;
    readonly storeMatchesConfigured: boolean;
    readonly manifestSha256: string;
    readonly storeManifestSha256?: string;
    readonly contractDeploymentInfoSha256: string;
    readonly storeContractDeploymentInfoSha256?: string;
  };
  readonly contracts: {
    readonly stateQueuePolicyId: string;
    readonly stateQueueAddress: string;
    readonly daAttestationPolicyId: string;
    readonly daAttestationAddress: string;
    readonly daParamsGovernorPolicyId: string;
    readonly daParamsGovernorAddress: string;
    readonly committeeSignersHash: string;
    readonly threshold: number;
  };
  readonly peer: CommitteeReadinessPeerSnapshot;
  readonly scanner: {
    readonly status: "not_started" | "ok" | "degraded" | "failed";
    readonly lastStartedAt?: string;
    readonly lastFinishedAt?: string;
    readonly scannedHeaders: number;
    readonly signedHeaders: number;
    /** The last tick's headers in good standing, not posts. */
    readonly reconciledHeaders: number;
    readonly skippedHeaders: number;
    readonly errors: readonly string[];
  };
  readonly retention?: CommitteeRetentionReadinessSnapshot;
  readonly counts: CommitteeStoreReadinessCounts;
  readonly reasons: readonly string[];
};

export type SignedHeaderResult = {
  readonly signature: DaSignatureRecord;
};

export type CoordinatorPublishResult = {
  readonly broadcastStatus: DaSignatureRecord["broadcastStatus"];
  readonly error?: string;
};

/**
 * Retention exemption sets of the last tick that read a view on which the
 * committee decided, stamped with the time it was accepted: the confirmed
 * head and every header in the landed queue or in the queue at the latest
 * final block, with that block's time as the release clock.
 */
export type CommitteeL1View = {
  readonly observedAtMs: number;
  readonly confirmedHeadHash: string;
  readonly liveQueueHeaderHashes: ReadonlySet<string>;
  readonly finalBlockTimeMs: number | null;
};

export const createDaConflictEvidenceGossipHandler = (args: {
  readonly deploymentFingerprint: string;
  readonly registry: DaPeerRegistry;
  readonly store: Pick<CommitteeStore, "saveDaConflictEvidence">;
  readonly now?: () => Date;
}): DaGossipMessageHandler => {
  const now = args.now ?? (() => new Date());
  return async (context) => {
    await ingestDaConflictEvidence({
      ...args,
      context,
      receivedAt: now(),
    });
  };
};

export const payloadFetchObservation = (
  headerHash: string,
  attempts: DaPayloadFetchFailure["attempts"],
): CommitteePayloadFetchObservation => {
  const status = attempts.every((attempt) => attempt.status === "not_found")
    ? "missing_da"
    : "fetch_failed";
  const detail = attempts
    .map((attempt) => `${attempt.sourcePeerId}:${attempt.status}`)
    .join(",");
  return {
    headerHash,
    status,
    sourcePeerIds: attempts.map((attempt) => attempt.sourcePeerId),
    ...(detail.length === 0 ? {} : { detail }),
  };
};

export const ingestDaConflictEvidence = async (args: {
  readonly deploymentFingerprint: string;
  readonly registry: DaPeerRegistry;
  readonly store: Pick<CommitteeStore, "saveDaConflictEvidence">;
  readonly context: DaGossipMessageHandlerContext;
  readonly receivedAt: Date;
}): Promise<boolean> => {
  if (args.context.topicName !== DaGossipTopic.conflicts) {
    throw new Error("DA conflict evidence arrived on the wrong gossip topic");
  }
  args.registry.requireKnownPeer(args.context.remotePeerId);
  const conflict = decodeDaConflictEvidenceCbor(args.context.data);
  if (!encodeDaConflictEvidenceCbor(conflict).equals(args.context.data)) {
    throw new Error("DA conflict evidence must use canonical CBOR");
  }
  const deploymentFingerprint = conflict.deploymentFingerprint.toString("hex");
  if (deploymentFingerprint !== args.deploymentFingerprint) {
    throw new Error(
      "DA conflict evidence deployment does not match configured deployment",
    );
  }
  if (
    conflict.evidenceKind !== "equivocation" ||
    conflict.compactEvidence === null
  ) {
    throw new Error(
      "DA conflict evidence must contain canonical signature/header equivocation evidence",
    );
  }
  if (
    !computeDaSha256Hash(conflict.compactEvidence).equals(conflict.evidenceHash)
  ) {
    throw new Error(
      "DA conflict evidence hash does not match compact evidence",
    );
  }
  const equivocation = decodeDaConflictingSignatureHeaderEvidenceCbor(
    conflict.compactEvidence,
  );
  if (
    !encodeDaConflictingSignatureHeaderEvidenceCbor(equivocation).equals(
      conflict.compactEvidence,
    )
  ) {
    throw new Error(
      "DA conflicting signature/header evidence must use canonical CBOR",
    );
  }
  if (!conflict.headerHash.equals(equivocation.lowerHeaderHash)) {
    throw new Error(
      "DA conflict evidence header does not match the lower conflicting header",
    );
  }
  const signerPeer = args.registry.getBySignerIndex(equivocation.signerIndex);
  if (
    signerPeer?.daVkey === undefined ||
    signerPeer.daVkey !== equivocation.daVkey.toString("hex")
  ) {
    throw new Error(
      "DA conflict evidence signer identity does not match the configured committee",
    );
  }
  const lowerHeaderHash = equivocation.lowerHeaderHash.toString("hex");
  const upperHeaderHash = equivocation.upperHeaderHash.toString("hex");
  const lowerCommitment = SDK.parseDaAvailabilityCommitmentCbor(
    equivocation.lowerCommitmentCbor.toString("hex"),
  );
  const upperCommitment = SDK.parseDaAvailabilityCommitmentCbor(
    equivocation.upperCommitmentCbor.toString("hex"),
  );
  if (
    lowerCommitment.header_hash !== lowerHeaderHash ||
    upperCommitment.header_hash !== upperHeaderHash
  ) {
    throw new Error(
      "DA conflict evidence commitment identity does not match its header",
    );
  }
  if (
    !verifyDaSignatureWitness({
      publicKeyHex: signerPeer.daVkey,
      availabilityCommitment: lowerCommitment,
      witnessHex: equivocation.lowerHeaderWitness.toString("hex"),
    }) ||
    !verifyDaSignatureWitness({
      publicKeyHex: signerPeer.daVkey,
      availabilityCommitment: upperCommitment,
      witnessHex: equivocation.upperHeaderWitness.toString("hex"),
    })
  ) {
    throw new Error(
      "DA conflict evidence contains an invalid attestation signature",
    );
  }
  const record: DaStoredConflictEvidenceRecord = {
    conflictSchemaVersion: 1,
    deploymentFingerprint,
    headerHash: lowerHeaderHash,
    commitmentDigest: computeDaSha256Hash(
      equivocation.lowerCommitmentCbor,
    ).toString("hex"),
    conflictingHeaderHash: upperHeaderHash,
    conflictingCommitmentDigest: computeDaSha256Hash(
      equivocation.upperCommitmentCbor,
    ).toString("hex"),
    signerIndex: equivocation.signerIndex,
    evidenceKind: "equivocation",
    evidenceHash: conflict.evidenceHash.toString("hex"),
    compactEvidenceCborHex: conflict.compactEvidence.toString("hex"),
    reporterPeerId: args.context.remotePeerId,
    receivedAt: args.receivedAt.toISOString(),
  };
  return args.store.saveDaConflictEvidence(record);
};
