import { createHash } from "node:crypto";

import {
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { type DeploymentManifestAvailabilityChallenge } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type L1Origin } from "@al-ft/midgard-core/l1-origin";

import type { CommitteePromiseAdoptionConfig } from "./config.promise-admission.js";
import type { DaCommitteeMember } from "./domain.js";
import { type MidgardNodeDeployment } from "./l1/deployment.js";

export type DaParamsConfig = {
  readonly committeeHex: string;
  readonly committeeSignersHash: string;
  readonly threshold: number;
};

export type LocalStateConfig = {
  readonly kind: "database";
  readonly url: string;
};

/** The Cardano network the committee's node runs, by its magic. */
export type CardanoL1SourceConfig = {
  readonly networkMagic: number;
};

/**
 * The identity of the committee's L1 source: its network and its node's
 * authority id. The deployment's L1 origin is deliberately out, so correcting
 * `L1_ORIGIN` (or moving it into the manifest later) never invalidates a
 * stored retirement floor. The committee store is bound to it; a change is
 * reported, never fatal.
 */
export const l1SourceAuthorityDigest = (
  config: Pick<CommitteeConfig, "network" | "nativeLedger">,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify({
        network: config.network,
        authorityNodeId: config.nativeLedger?.authorityNodeId ?? null,
      }),
    )
    .digest("hex");

/**
 * The committee's local node: its socket, configuration and the native
 * transport binary the L1 follower reads and submits through.
 */
export type NativeLedgerConfig = {
  readonly authorityNodeId: string;
  readonly socketPath: string;
  readonly nodeConfigPath: string;
  readonly binaryPath: string;
};

export type CommitteeConfig = {
  readonly network: string;
  readonly deploymentManifestPath: string;
  readonly contractDeploymentInfoPath: string;
  readonly deploymentFingerprint: string;
  readonly deploymentManifestSha256: string;
  readonly contractDeploymentInfoSha256: string;
  readonly deploymentManifestRaw: string;
  readonly deploymentManifest: Record<string, unknown>;
  readonly contractDeploymentInfo: Record<string, unknown>;
  readonly availabilityChallenge: DeploymentManifestAvailabilityChallenge;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly midgardNodeDeployment: MidgardNodeDeployment;
  /** Absent when no local node ledger is configured; reward-account reads then fail closed. */
  readonly nativeLedger?: NativeLedgerConfig;
  /** The operator-configured L1 origin point (`L1_ORIGIN`); absent when unset. */
  readonly l1Origin?: L1Origin;
  readonly finalityDepth: number;
  /** Signed recovery horizon, distinct from confirmation admission. */
  readonly automaticRecoveryMaxDepth: number;
  readonly daTransport: Libp2pDaTransportConfig;
  readonly libp2pPrivateKeySource?: string;
  readonly signerIndex?: number;
  readonly signerKeySource?: string;
  readonly l1SubmitterKeySource?: string;
  readonly l1SubmissionEnabled: boolean;
  readonly availabilityPromiseAdoption?: CommitteePromiseAdoptionConfig;
  readonly availabilityJournalPath?: string;
  readonly availabilitySubmitterKeySource?: string;
  readonly l1SubmitterPreflight: L1SubmitterPreflightConfig;
  readonly l1SubmitterId?: string;
  readonly l1SubmitterIds: readonly string[];
  readonly l1LeaderFailoverMs: number;
  readonly localState: LocalStateConfig;
  readonly daParams: DaParamsConfig;
  readonly daCommitteeMembers: readonly DaCommitteeMember[];
  readonly l1SubmitterSignerIndexes: readonly number[];
  readonly daAttestationPolicyId: string;
  readonly daAttestationAddress: string;
  readonly daParamsGovernorPolicyId: string;
  readonly daParamsGovernorAddress: string;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly peerRequestTimeoutMs: number;
  readonly peerReplayWindowMs: number;
  readonly peerMaxBodyBytes: number;
  readonly peerRetryInitialDelayMs: number;
  readonly peerRetryMaxDelayMs: number;
  readonly peerRetryMaxAttempts: number;
  readonly peerRateLimitWindowMs: number;
  readonly peerRateLimitMaxRequests: number;
  readonly apiHost: string;
  readonly apiPort: number;
  readonly pollIntervalMs: number;
  /**
   * Longest time the committee may run without a fresh authenticated L1 view
   * before it exits with code 70.
   */
  readonly l1ViewFatalMs: number;
  /**
   * Opt-in retention deadline alert (`DA_RETENTION_ALERT_THRESHOLD_MS`): a
   * still-challengeable payload with at most this many milliseconds left to
   * its challengeability deadline is logged as `da_retention_deadline_alert`.
   * Unset, no deadline alert is raised. Informational only: every merged
   * payload passes through the threshold on its way to pruning, so the alert
   * never affects `/readyz`, and pruning is the same either way. Must be below
   * the merged-payload window (horizon minus block maturity), or it would
   * alert on every merged payload from the moment it merges.
   */
  readonly retentionAlertThresholdMs?: number;
};

/**
 * The configuration a committee L1 client factory reads: the committee
 * configuration plus the configured network magic.
 */
export type CommitteeL1ClientConfig = CommitteeConfig & {
  readonly cardanoL1Source: Pick<CardanoL1SourceConfig, "networkMagic">;
};

export type LoadedCommitteeConfig = CommitteeConfig & {
  readonly cardanoL1Source: CardanoL1SourceConfig;
};

export type Libp2pDaRole =
  | "committee"
  | "producer"
  | "watcher"
  | "challenger"
  | "coordinator"
  | "retrieval";

export type Libp2pDaTransportLimits = {
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
  readonly maxStreamsPerPeer: number;
  readonly requestTimeoutMs: number;
};

export type Libp2pDaGossipConfig = {
  readonly strictSign: true;
  readonly emitSelf: false;
  readonly allowedTopicsOnly: true;
  readonly maxGossipMessageBytes: number;
};

export type Libp2pDaPeerConfig = {
  readonly signerIndex: number;
  readonly daVkey: string;
  readonly peerId: string;
  readonly multiaddrs: readonly string[];
  readonly roles: readonly Libp2pDaRole[];
};

export type Libp2pDaTransportConfig = {
  readonly kind: "libp2p";
  readonly deploymentFingerprint: string;
  readonly noHttpDaTransport: true;
  readonly threshold: number;
  readonly listenMultiaddrs: readonly string[];
  readonly announceMultiaddrs: readonly string[];
  readonly bootstrapMultiaddrs: readonly string[];
  readonly gossip: Libp2pDaGossipConfig;
  readonly limits: Libp2pDaTransportLimits;
  readonly retentionDays: number;
  readonly peers: readonly Libp2pDaPeerConfig[];
};

export type PublicRetainedDaConfig = {
  readonly peerId: string;
  readonly privateKeySource: string;
  readonly listenMultiaddrs: readonly string[];
  readonly announceMultiaddrs: readonly string[];
  readonly protocols: readonly string[];
  readonly limits: {
    readonly maxStreamsPerPeer: number;
    readonly maxInflightRequests: number;
    readonly maxInflightRequestsPerPeer: number;
    readonly maxInflightProofRequests: number;
    readonly requestTimeoutMs: number;
  };
};

/** Minimal authority set for the separate public retained-DA executable. */
export type PublicRetainedDaRuntimeConfig = {
  readonly deploymentFingerprint: string;
  readonly publicRetainedDa: PublicRetainedDaConfig;
  readonly dataLimits: Libp2pDaTransportLimits;
  readonly databaseUrl: string;
  readonly databaseRole: string;
};

export type L1SubmitterPreflightConfig = {
  readonly enabled: boolean;
  readonly minPlainAdaLovelace: bigint;
  readonly minCollateralLovelace: bigint;
  readonly minSpendableUtxoCount: number;
  readonly autoFundKeySource?: string;
  readonly autoFundBufferLovelace: bigint;
  readonly retryCount: number;
  readonly retryDelayMs: number;
};

export type Env = Record<string, string | undefined>;

/**
 * What one DA attestation round spends in fees: the init, add-signatures and
 * apply fees, the separate collateral UTxO and the change output's minimum.
 */
export const DA_L1_SUBMITTER_FEE_HEADROOM_LOVELACE = 50_000_000n;

/**
 * The most lovelace one DA attestation output locks between init and apply:
 * the min-UTxO of the widest canonical attestation (a 64-tranche commitment,
 * a base-address rescue beneficiary and a signer count at its widest CBOR
 * form) at the ledger's `coinsPerUtxoByte`, rounded up to a whole ADA. Apply
 * and rescue return it to the rescue beneficiary, which is the submitter
 * wallet. `tests/tx-builders.test.ts` measures the widest attestation against
 * it.
 */
export const DA_ATTESTATION_OUTPUT_LOVELACE_ALLOWANCE = 30_000_000n;

/**
 * The least plain ADA that funds the next attestation round: the round's fees
 * plus the min-ADA the init locks until apply refunds it. The committee's
 * bond is the pooled DA bond, not the submitter wallet's, so no bond is part
 * of it. The startup preflight refuses a wallet below it, and the check
 * around each init reports one to readiness.
 */
export const DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE =
  DA_L1_SUBMITTER_FEE_HEADROOM_LOVELACE +
  DA_ATTESTATION_OUTPUT_LOVELACE_ALLOWANCE;

export const DEFAULT_L1_SUBMITTER_PREFLIGHT = {
  minCollateralLovelace: 5_000_000n,
  minSpendableUtxoCount: 2,
  autoFundBufferLovelace: 10_000_000n,
  retryCount: 3,
  retryDelayMs: 5_000,
} as const;

export const LIBP2P_DA_TRANSPORT_LIMITS = {
  maxPayloadBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
  maxInlineResponseBytes: 1_048_576,
  maxChunkBytes: 1_048_576,
  maxStreamsPerPeer: 16,
  requestTimeoutMs: 15_000,
} as const satisfies Libp2pDaTransportLimits;

export const LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES = 65_536;

export const LIBP2P_DA_MIN_RETENTION_DAYS = 15;
