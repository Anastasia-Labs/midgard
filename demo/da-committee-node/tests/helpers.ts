import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_PROTOCOL_V1_VERSION,
  MIDGARD_VALIDATION_MACHINE_V1_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { encodeMidgardValidationTraceDescriptorV1 } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import {
  LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
  LIBP2P_DA_MIN_RETENTION_DAYS,
  LIBP2P_DA_TRANSPORT_LIMITS,
  type WatcherConfig,
} from "../src/config.js";
import { computeDaPayloadV1Roots } from "../src/da/payload.js";
import type { DaPayloadSource } from "../src/da/source.js";
import type { HeaderV1, ObservedStateQueueNode } from "../src/domain.js";
import type { MidgardAuthenticatedDeployment } from "../src/l1/deployment.js";
import { hashBlockHeaderV1 } from "../src/l1/state-queue-scanner.js";

export const tempDir = (): Promise<string> =>
  mkdtemp(join(tmpdir(), "midgard-watcher-test-"));

export const fixtureHeaderBase = (): Omit<
  HeaderV1,
  | "utxosRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
  | "withdrawalsRoot"
> => ({
  prevUtxosRoot:
    "0000000000000000000000000000000000000000000000000000000000000000",
  ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: BigInt(MIDGARD_PROTOCOL_V1_VERSION),
});

const canonicalTransaction = (fee: bigint) => {
  const tx = materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  return {
    tx,
    txId: Buffer.from(computeMidgardNativeTxIdV1(tx)).toString("hex"),
    txCbor: Buffer.from(encodeMidgardNativeTxCanonicalV1(tx)).toString("hex"),
  };
};

const transactionSourceV1 = (
  transaction: ReturnType<typeof canonicalTransaction>,
): SDK.L2TransactionSourceV1 => {
  const source = deriveMidgardNativeTxProofSourceV1(transaction.tx);
  return {
    tx_id: transaction.txId,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
};

const acceptedTraceDescriptor = (seed: number): string =>
  encodeMidgardValidationTraceDescriptorV1({
    schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
    machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
    traceRoot: Buffer.alloc(32, seed),
    stepCount: 0,
    initialStateHash: Buffer.alloc(32, seed + 1),
    terminalStateHash: Buffer.alloc(32, seed + 1),
    verdict: "accepted",
    rejectionCodeHash: Buffer.alloc(32),
  }).toString("hex");

export const makePayloadFixture = async (
  transactionCount = 3,
): Promise<{
  readonly payload: SDK.DaPayloadV1;
  readonly innerPayloadCbor: Buffer;
  readonly payloadCbor: Buffer;
  readonly header: HeaderV1;
  readonly headerHash: string;
}> => {
  if (
    !Number.isSafeInteger(transactionCount) ||
    transactionCount < 1 ||
    transactionCount > MIDGARD_CONSENSUS_LIMITS_V1.maxL2TransactionCount
  ) {
    throw new Error(
      `fixture transaction count must be in [1, ${MIDGARD_CONSENSUS_LIMITS_V1.maxL2TransactionCount.toString()}]; got ${transactionCount.toString()}`,
    );
  }
  const transactions = Array.from({ length: transactionCount }, (_, index) =>
    canonicalTransaction(BigInt(index)),
  ).sort((left, right) => left.txId.localeCompare(right.txId));
  if (new Set(transactions.map(({ txId }) => txId)).size !== transactionCount) {
    throw new Error("fixture transaction identities must be distinct");
  }
  const sources = transactions.map(transactionSourceV1);
  const sourceEvents: readonly SDK.EventKey[] = transactions.map(
    ({ txId }) => ({ L2TransactionEventKey: { tx_id: txId } }),
  );
  const transitionEntries = transitionTraceEntries(sourceEvents);
  const eventToStepEntries = eventToStepEntriesFor(sourceEvents);
  const validationTraceEntries = sortedEntries(
    sourceEvents.map((eventKey, index) => [
      LucidData.to(eventKey as never, SDK.EventKeySchema as never),
      acceptedTraceDescriptor(index + 1),
    ]),
  );
  const counts: SDK.DaPayloadCountsV1 = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: BigInt(transactionCount),
    depositCount: 0n,
    totalEventCount: BigInt(transactionCount),
    transitionStepCount: BigInt(transactionCount),
    validationTraceCount: BigInt(transactionCount),
  };
  const placeholderHeader: HeaderV1 = {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const payloadWithoutHash: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header: placeholderHeader,
      utxos: [],
      transactions: sources.map((source) => [
        source.tx_id,
        LucidData.to(source as never, SDK.L2TransactionSourceV1Schema as never),
      ]),
      transaction_preimages: transactions.map(({ txId, txCbor }) => [
        txId,
        txCbor,
      ]),
      deposits: [],
      withdrawals: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      transition_trace: transitionEntries,
      event_to_step: eventToStepEntries,
      validation_traces: validationTraceEntries,
      counts,
    },
  };
  const roots = await computeDaPayloadV1Roots(payloadWithoutHash);
  const header: HeaderV1 = {
    ...fixtureHeaderBase(),
    utxosRoot: roots.utxosRoot,
    forcedTransactionsRoot: roots.forcedTransactionsRoot,
    transactionsRoot: roots.transactionsRoot,
    depositsRoot: roots.depositsRoot,
    withdrawalsRoot: roots.withdrawalsRoot,
    transitionTraceRoot: roots.transitionTraceRoot,
    eventToStepRoot: roots.eventToStepRoot,
    validationTracesRoot: roots.validationTracesRoot,
    withdrawalCount: counts.withdrawalCount,
    forcedTransactionCount: counts.forcedTransactionCount,
    l2TransactionCount: counts.l2TransactionCount,
    depositCount: counts.depositCount,
    totalEventCount: counts.totalEventCount,
    transitionStepCount: counts.transitionStepCount,
    validationTraceCount: counts.validationTraceCount,
  };
  const headerHash = hashBlockHeaderV1(header);
  const payload: SDK.DaPayloadV1 = {
    ...payloadWithoutHash,
    block_body: {
      ...payloadWithoutHash.block_body,
      header_hash: headerHash,
      header,
    },
  };
  const innerPayloadCbor = SDK.encodeDaPayloadV1(payload);
  return {
    payload,
    innerPayloadCbor,
    payloadCbor: await wrapDaPayloadV1(innerPayloadCbor, { mode: "identity" }),
    header,
    headerHash,
  };
};

const sortedEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const eventPhase = (eventKey: SDK.EventKey): SDK.TransitionPhase => {
  if ("WithdrawalEventKey" in eventKey) {
    return "Withdrawal";
  }
  if ("ForcedTransactionEventKey" in eventKey) {
    return "ForcedTransaction";
  }
  if ("L2TransactionEventKey" in eventKey) {
    return "L2Transaction";
  }
  return "Deposit";
};

const transitionTraceEntries = (
  sourceEvents: readonly SDK.EventKey[],
): SDK.DaPayloadEntry[] =>
  sourceEvents.map((eventKey, index) => {
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: BigInt(index),
      event_key: eventKey,
      phase: eventPhase(eventKey),
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    return [
      LucidData.to(step.step_index as never, LucidData.Integer() as never),
      LucidData.to(step as never, SDK.TransitionStepSchema as never),
    ];
  });

const eventToStepEntriesFor = (
  sourceEvents: readonly SDK.EventKey[],
): SDK.DaPayloadEntry[] =>
  sortedEntries(
    sourceEvents.map((eventKey, index) => [
      LucidData.to(eventKey as never, SDK.EventKeySchema as never),
      LucidData.to(
        {
          step_index: BigInt(index),
          phase: eventPhase(eventKey),
        } satisfies SDK.EventToStepValue as never,
        SDK.EventToStepValueSchema as never,
      ),
    ]),
  );

export const makeObservedNode = ({
  header,
  headerHash,
  daAttestation = SDK.NO_DA_ATTESTATION,
  depth = 10,
  outRef = "ab".repeat(32) + "#0",
  slot = 1,
  blockHash = "cd".repeat(32),
  assetName = `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
  linkedListKey = headerHash,
}: {
  readonly header: HeaderV1;
  readonly headerHash: string;
  readonly daAttestation?: SDK.DaAvailabilityStateQueueStatusV1;
  readonly depth?: number;
  readonly outRef?: string;
  readonly slot?: number;
  readonly blockHash?: string;
  readonly assetName?: string;
  readonly linkedListKey?: string | "Empty";
}): ObservedStateQueueNode => ({
  outRef,
  assetName,
  linkedListKey,
  header,
  daAttestation,
  chainPoint: {
    slot,
    blockHash,
    depth,
    providerSource: "fixture",
  },
});

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

export const minimalConfig = ({
  dir,
  manifestPath,
  deploymentInfoPath,
  signerSeed,
  signerPublicKey,
}: {
  readonly dir: string;
  readonly manifestPath: string;
  readonly deploymentInfoPath: string;
  readonly signerSeed: string;
  readonly signerPublicKey: string;
}): WatcherConfig => ({
  network: "Preview",
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
      smallResponseWindowMs: 3_600_000,
      fullPayloadMaxBytes: 67_108_864,
      fullResponseWindowMs: 172_800_000,
    },
    responseGeometry: {
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    },
    daBondLovelace: 10_000_000_000,
    challengerBondLovelace: 10_000_000_000,
    maxOpenFeeLovelace: 500_000,
    maxPublicationFeeLovelace: 500_000,
    maxSettlementFeeLovelace: 500_000,
    maxCloseFeeLovelace: 1_000_000,
    maxTimeoutFeeLovelace: 1_200_000,
    bondOwnerCredential: "76".repeat(28),
  },
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  midgardNodeDeployment: {
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
  localState: { kind: "file", path: join(dir, "store") },
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
