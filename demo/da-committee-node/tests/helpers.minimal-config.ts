import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  daBondManifestAmounts,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";

import {
  type CommitteeConfig,
  LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
  LIBP2P_DA_MIN_RETENTION_DAYS,
  LIBP2P_DA_TRANSPORT_LIMITS,
} from "../src/config.js";
import type { DaPayloadSource } from "../src/da/source.js";
import type {
  MidgardAuthenticatedDeployment,
  MidgardNodeDeployment,
} from "../src/l1/deployment.js";
import type {} from "./global-setup.js";

export const writeJson = async (
  dir: string,
  name: string,
  value: unknown,
): Promise<string> => {
  const path = join(dir, name);
  await writeFile(path, `${JSON.stringify(value, null, 2)}\n`);
  return path;
};

const minimalAuthenticatedDeployment = ({
  prefix,
  policyId,
  spendingScriptHash,
  spendingScriptAddress,
}: {
  readonly prefix: string;
  readonly policyId: string;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
}): MidgardAuthenticatedDeployment => ({
  mint: {
    key: `${prefix}Mint`,
    purpose: "mint",
    script: { type: "Native", script: "00" },
    scriptHash: policyId,
    refScriptOutRef: { txHash: "11".repeat(32), outputIndex: 0 },
  },
  spend: {
    key: `${prefix}Spend`,
    purpose: "spend",
    script: { type: "Native", script: "00" },
    scriptHash: spendingScriptHash,
    refScriptOutRef: { txHash: "22".repeat(32), outputIndex: 0 },
  },
  policyId,
  spendingScriptHash,
  spendingScriptAddress,
});

export const minimalStateQueueYields =
  (): MidgardNodeDeployment["stateQueueYields"] =>
    Object.fromEntries(
      (
        [
          ["commit", "stateQueueCommitWithdraw", "c1"],
          ["unattestedTimeout", "stateQueueUnattestedTimeoutWithdraw", "c2"],
          ["unavailableTimeout", "stateQueueUnavailableTimeoutWithdraw", "c3"],
          ["fraudRemoval", "stateQueueFraudRemovalWithdraw", "c4"],
          ["merge", "stateQueueMergeWithdraw", "c5"],
        ] as const
      ).map(([role, key, byte]) => [
        role,
        {
          key,
          purpose: "withdraw",
          script: { type: "Native", script: "00" },
          scriptHash: byte.repeat(28),
          refScriptOutRef: { txHash: byte.repeat(32), outputIndex: 0 },
        },
      ]),
    ) as MidgardNodeDeployment["stateQueueYields"];

export const minimalAvailabilityChallengeYields =
  (): MidgardNodeDeployment["availabilityChallengeYields"] =>
    Object.fromEntries(
      (
        [
          ["open", "availabilityChallengeOpenWithdraw", "c2"],
          ["settle", "availabilityChallengeSettleWithdraw", "c3"],
          ["close", "availabilityChallengeCloseWithdraw", "c4"],
          ["timeout", "availabilityChallengeTimeoutWithdraw", "c5"],
        ] as const
      ).map(([role, key, byte]) => [
        role,
        {
          key,
          purpose: "withdraw",
          script: { type: "Native", script: "00" },
          scriptHash: byte.repeat(28),
          refScriptOutRef: { txHash: byte.repeat(32), outputIndex: 0 },
        },
      ]),
    ) as MidgardNodeDeployment["availabilityChallengeYields"];

export const minimalConfig = ({
  manifestPath,
  deploymentInfoPath,
  signerSeed,
  signerPublicKey,
}: {
  readonly manifestPath: string;
  readonly deploymentInfoPath: string;
  readonly signerSeed: string;
  readonly signerPublicKey: string;
}): CommitteeConfig => ({
  network: "Preprod",
  deploymentManifestPath: manifestPath,
  contractDeploymentInfoPath: deploymentInfoPath,
  deploymentFingerprint: "f".repeat(64),
  deploymentManifestSha256: "a".repeat(64),
  contractDeploymentInfoSha256: "b".repeat(64),
  deploymentManifestRaw: "{}",
  deploymentManifest: {},
  contractDeploymentInfo: {},
  availabilityChallenge: {
    responseClasses: {
      smallPayloadMaxBytes: 65_536,
      smallResponseWindowMs:
        SELECTED_DEPLOYMENT_PROFILE.timing.da_small_response_window_ms,
      fullPayloadMaxBytes: 67_108_864,
      fullResponseWindowMs:
        SELECTED_DEPLOYMENT_PROFILE.timing.da_full_response_window_ms,
    },
    responseGeometry: {
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    },
    ...daBondManifestAmounts(),
    challengerBondLovelace: 10_000_000_000,
    maxOpenFeeLovelace: 500_000,
    maxPublicationFeeLovelace: 500_000,
    maxSettlementFeeLovelace: 500_000,
    maxCloseFeeLovelace: 1_000_000,
    maxTimeoutFeeLovelace: 1_200_000,
  },
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  midgardNodeDeployment: {
    referenceScriptAuthPolicyId: "f0".repeat(28),
    hubOraclePolicyId: "99".repeat(28),
    correctionLockAddress: "addr_test1correctionlock",
    hubOracle: minimalAuthenticatedDeployment({
      prefix: "hubOracle",
      policyId: "99".repeat(28),
      spendingScriptHash: "97".repeat(28),
      spendingScriptAddress: "addr_test1huboracle",
    }),
    availabilityChallenge: minimalAuthenticatedDeployment({
      prefix: "availabilityChallenge",
      policyId: "96".repeat(28),
      spendingScriptHash: "95".repeat(28),
      spendingScriptAddress: "addr_test1availability",
    }),
    fraudProof: minimalAuthenticatedDeployment({
      prefix: "fraudProof",
      policyId: "98".repeat(28),
      spendingScriptHash: "97".repeat(28),
      spendingScriptAddress: "addr_test1fraudproof",
    }),
    daAttestation: minimalAuthenticatedDeployment({
      prefix: "daAttestation",
      policyId: "33".repeat(28),
      spendingScriptHash: "66".repeat(28),
      spendingScriptAddress: "addr_test1daattestation",
    }),
    daBondPool: minimalAuthenticatedDeployment({
      prefix: "daBondPool",
      policyId: "5b".repeat(28),
      spendingScriptHash: "5c".repeat(28),
      spendingScriptAddress: "addr_test1dabondpool",
    }),
    daParamsGovernor: minimalAuthenticatedDeployment({
      prefix: "daParamsGovernor",
      policyId: "55".repeat(28),
      spendingScriptHash: "77".repeat(28),
      spendingScriptAddress: "addr_test1daparams",
    }),
    stateQueue: minimalAuthenticatedDeployment({
      prefix: "stateQueue",
      policyId: "44".repeat(28),
      spendingScriptHash: "88".repeat(28),
      spendingScriptAddress: "addr_test1statequeue",
    }),
    availabilityChallengeYields: minimalAvailabilityChallengeYields(),
    stateQueueYields: minimalStateQueueYields(),
  },
  l1Source: {
    sourceMode: "local_node",
    authorityNodeId: "fixture-node",
    chainSyncProviderUrl: "chain-sync:fixture:/tmp/state-queue.json",
    chainSyncCursorPath: "/tmp/state-queue.chain-sync-cursor.json",
    queryProviderUrls: ["fixture:/tmp/state-queue.json"],
  },
  cardanoProviderUrls: ["fixture:/tmp/state-queue.json"],
  finalityDepth: 2,
  automaticRecoveryMaxDepth: 2,
  daTransport: {
    kind: "libp2p",
    deploymentFingerprint: "f".repeat(64),
    noHttpDaTransport: true,
    threshold: 1,
    listenMultiaddrs: ["/ip4/127.0.0.1/tcp/0"],
    announceMultiaddrs: [`/ip4/127.0.0.1/tcp/0/p2p/${MINIMAL_LIBP2P_PEER_ID}`],
    bootstrapMultiaddrs: [`/ip4/127.0.0.1/tcp/0/p2p/${MINIMAL_LIBP2P_PEER_ID}`],
    gossip: {
      strictSign: true,
      emitSelf: false,
      allowedTopicsOnly: true,
      maxGossipMessageBytes: LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
    },
    limits: LIBP2P_DA_TRANSPORT_LIMITS,
    retentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
    peers: [
      {
        signerIndex: 0,
        daVkey: signerPublicKey,
        peerId: MINIMAL_LIBP2P_PEER_ID,
        multiaddrs: [`/ip4/127.0.0.1/tcp/0/p2p/${MINIMAL_LIBP2P_PEER_ID}`],
        roles: ["committee", "coordinator", "retrieval"],
      },
    ],
  },
  signerIndex: 0,
  signerKeySource: `hex:${signerSeed}`,
  l1SubmissionEnabled: false,
  l1SubmitterPreflight: {
    enabled: false,
    minPlainAdaLovelace: 50_000_000n,
    minCollateralLovelace: 5_000_000n,
    minSpendableUtxoCount: 2,
    autoFundBufferLovelace: 10_000_000n,
    retryCount: 3,
    retryDelayMs: 5_000,
  },
  l1SubmitterIds: [],
  l1LeaderFailoverMs: 0,
  // Tests open their own store; this URL is never connected to.
  localState: {
    kind: "database",
    url: "postgresql://unused.invalid/committee",
  },
  daParams: {
    committeeHex: signerPublicKey,
    committeeSignersHash: "",
    threshold: 1,
  },
  daCommitteeMembers: [
    {
      index: 0,
      vkey: signerPublicKey,
      canSubmitL1: true,
    },
  ],
  l1SubmitterSignerIndexes: [0],
  daAttestationPolicyId: "33".repeat(28),
  daAttestationAddress: "addr_test1daattestation",
  daParamsGovernorPolicyId: "55".repeat(28),
  daParamsGovernorAddress: "addr_test1daparams",
  stateQueuePolicyId: "44".repeat(28),
  stateQueueAddress: "addr_test1statequeue",
  hubOraclePolicyId: "99".repeat(28),
  correctionLockAddress: "addr_test1correctionlock",
  fraudProofPolicyId: "98".repeat(28),
  fraudProofAddress: "addr_test1fraudproof",
  peerRequestTimeoutMs: 1000,
  peerReplayWindowMs: 300_000,
  peerMaxBodyBytes: 1_048_576,
  peerRetryInitialDelayMs: 100,
  peerRetryMaxDelayMs: 1000,
  peerRetryMaxAttempts: 3,
  peerRateLimitWindowMs: 60_000,
  peerRateLimitMaxRequests: 120,
  apiHost: "127.0.0.1",
  apiPort: 0,
  pollIntervalMs: 1000,
  l1ViewFatalMs: 3_600_000,
});

const MINIMAL_LIBP2P_PEER_ID =
  "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";

export const payloadSourceFromBytes = (
  payloadCbor: Buffer,
  sourcePeerId = "fixture-peer",
): DaPayloadSource => ({
  fetchPayloadCandidates: async () => ({
    ok: true,
    candidates: [{ sourcePeerId, payloadCbor, payloadSchemaVersion: 1 }],
    attempts: [],
  }),
});
