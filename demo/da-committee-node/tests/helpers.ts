import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import type { WatcherConfig } from "../src/config.js";
import { computeDaPayloadRoots } from "../src/da/payload.js";
import type { Header, ObservedStateQueueNode } from "../src/domain.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";

export const IDENTITY_TX_PROJECTOR = (bytes: Buffer): Buffer => bytes;

export const tempDir = (): Promise<string> =>
  mkdtemp(join(tmpdir(), "midgard-watcher-test-"));

export const fixtureHeaderBase = (): Omit<
  Header,
  | "utxosRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
  | "withdrawalsRoot"
> => ({
  prevUtxosRoot:
    "0000000000000000000000000000000000000000000000000000000000000000",
  ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
  startTime: 1n,
  endTime: 2n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
});

export const makePayloadFixture = async (): Promise<{
  readonly payload: SDK.DaPayloadV2;
  readonly payloadCbor: Buffer;
  readonly header: Header;
  readonly headerHash: string;
}> => {
  const txIdA = "10".repeat(32);
  const txIdB = "20".repeat(32);
  const withdrawalId = outputReferenceCbor("30", 0n);
  const forcedTransactionId = outputReferenceCbor("31", 0n);
  const depositId = outputReferenceCbor("40", 0n);
  const sourceEvents: readonly SDK.EventKey[] = [
    {
      WithdrawalEventKey: {
        withdrawal_id: outputReference("30", 0n),
      },
    },
    {
      ForcedTransactionEventKey: {
        tx_order_id: outputReference("31", 0n),
      },
    },
    { L2TransactionEventKey: { tx_id: txIdA } },
    { L2TransactionEventKey: { tx_id: txIdB } },
    { DepositEventKey: { deposit_id: outputReference("40", 0n) } },
  ];
  const transitionEntries = transitionTraceEntries(sourceEvents);
  const eventToStepEntries = eventToStepEntriesFor(sourceEvents);
  const counts: SDK.DaPayloadCountsV2 = {
    withdrawalCount: 1n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 2n,
    depositCount: 1n,
    totalEventCount: 5n,
    transitionStepCount: 5n,
  };
  const placeholderHeader: Header = {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const payloadWithoutHash: SDK.DaPayloadV2 = {
    version: SDK.DA_PAYLOAD_V2_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header: placeholderHeader,
      utxos: [
        ["01", "aa"],
        ["02", "bb"],
      ],
      transactions: [
        [txIdA, "ca"],
        [txIdB, "fe"],
      ],
      deposits: [[depositId, "dd"]],
      withdrawals: [[withdrawalId, "ee"]],
      forced_transactions: [[forcedTransactionId, "fa"]],
      transition_trace: transitionEntries,
      event_to_step: eventToStepEntries,
      counts,
    },
  };
  const roots = await computeDaPayloadRoots(
    payloadWithoutHash,
    IDENTITY_TX_PROJECTOR,
  );
  const header: Header = {
    ...fixtureHeaderBase(),
    utxosRoot: roots.utxosRoot,
    forcedTransactionsRoot: roots.forcedTransactionsRoot,
    transactionsRoot: roots.transactionsRoot,
    depositsRoot: roots.depositsRoot,
    withdrawalsRoot: roots.withdrawalsRoot,
    transitionTraceRoot: roots.transitionTraceRoot,
    eventToStepRoot: roots.eventToStepRoot,
    withdrawalCount: counts.withdrawalCount,
    forcedTransactionCount: counts.forcedTransactionCount,
    l2TransactionCount: counts.l2TransactionCount,
    depositCount: counts.depositCount,
    totalEventCount: counts.totalEventCount,
    transitionStepCount: counts.transitionStepCount,
  };
  const headerHash = hashBlockHeader(header);
  const payload: SDK.DaPayloadV2 = {
    ...payloadWithoutHash,
    block_body: {
      ...payloadWithoutHash.block_body,
      header_hash: headerHash,
      header,
    },
  };
  return {
    payload,
    payloadCbor: SDK.encodeDaPayloadV2(payload),
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

const outputReference = (
  txByte: string,
  outputIndex: bigint,
): SDK.OutputReference => ({
  transactionId: txByte.repeat(32),
  outputIndex,
});

const outputReferenceCbor = (txByte: string, outputIndex: bigint): string =>
  LucidData.to(
    outputReference(txByte, outputIndex) as never,
    SDK.OutputReference as never,
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
  assetName = `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
  linkedListKey = headerHash,
}: {
  readonly header: Header;
  readonly headerHash: string;
  readonly daAttestation?: string;
  readonly depth?: number;
  readonly assetName?: string;
  readonly linkedListKey?: string | "Empty";
}): ObservedStateQueueNode => ({
  outRef: "ab".repeat(32) + "#0",
  assetName,
  linkedListKey,
  header,
  daAttestation,
  chainPoint: {
    slot: 1,
    blockHash: "cd".repeat(32),
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
  deploymentManifestRaw: "{}",
  deploymentManifest: {},
  contractDeploymentInfo: {},
  cardanoProviderUrls: ["fixture:/tmp/state-queue.json"],
  finalityDepth: 2,
  daPayloadEndpoints: ["http://da.example"],
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
      baseUrls: [],
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
  peerEndpoints: [],
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
