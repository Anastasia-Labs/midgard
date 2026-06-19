import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";

import type { WatcherConfig } from "../src/config.js";
import type { Header, ObservedStateQueueNode } from "../src/domain.js";
import { computeDaPayloadRoots } from "../src/da/payload.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";

export const IDENTITY_TX_PROJECTOR = (bytes: Buffer): Buffer => bytes;

export const tempDir = (): Promise<string> =>
  mkdtemp(join(tmpdir(), "midgard-watcher-test-"));

export const fixtureHeaderBase = (): Omit<
  Header,
  "utxosRoot" | "transactionsRoot" | "depositsRoot" | "withdrawalsRoot"
> => ({
  prevUtxosRoot:
    "0000000000000000000000000000000000000000000000000000000000000000",
  startTime: 1n,
  endTime: 2n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
});

export const makePayloadFixture = async (): Promise<{
  readonly payload: SDK.DaPayloadV1;
  readonly payloadCbor: Buffer;
  readonly header: Header;
  readonly headerHash: string;
}> => {
  const payloadWithoutHash: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    header_hash: "00".repeat(28),
    block_body: {
      utxos: [
        ["01", "aa"],
        ["02", "bb"],
      ],
      transactions: [
        ["10", "ca"],
        ["20", "fe"],
      ],
      deposits: [["30", "dd"]],
      withdrawals: [["40", "ee"]],
    },
  };
  const roots = await computeDaPayloadRoots(
    payloadWithoutHash,
    IDENTITY_TX_PROJECTOR,
  );
  const header: Header = {
    ...fixtureHeaderBase(),
    utxosRoot: roots.utxosRoot,
    transactionsRoot: roots.transactionsRoot,
    depositsRoot: roots.depositsRoot,
    withdrawalsRoot: roots.withdrawalsRoot,
  };
  const headerHash = hashBlockHeader(header);
  const payload: SDK.DaPayloadV1 = {
    ...payloadWithoutHash,
    header_hash: headerHash,
  };
  return {
    payload,
    payloadCbor: SDK.encodeDaPayloadV1(payload),
    header,
    headerHash,
  };
};

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
