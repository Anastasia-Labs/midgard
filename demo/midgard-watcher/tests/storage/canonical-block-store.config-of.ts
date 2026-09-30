import { createHash } from "node:crypto";

import {
  DA_PAYLOAD_INNER_SCHEMA_VERSION,
  DaPayloadContentEncoding,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_PROTOCOL_VERSION,
  type DaCapabilitiesResponse,
  daDeploymentFingerprintFromHex,
  type DaPayloadByHeaderResponse,
  encodeDaCapabilitiesResponseCbor,
  encodeDaEventToStepByEventResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
  encodeDaProofBundleByHeaderResponseCbor,
  encodeDaTraceStepByIndexResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  DA_PAYLOAD_VERSION,
  type DaPayload,
  EMPTY_MERKLE_TREE_ROOT,
} from "@al-ft/midgard-sdk";

import {
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "../../src/runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  WatcherPublicDaClient,
  type WatcherPublicDaLibp2pTransportV1,
  type WatcherPublicDaRequest,
} from "../../src/storage/public-da-client.js";

// ---------------------------------------------------------------------------
// Fixtures (public DA client wiring mirrors tests/public-da-client.test.ts)
// ---------------------------------------------------------------------------

export const repeatHex = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

export const FINGERPRINT = repeatHex(0x1a, 32);

export const HEADER_HASH = repeatHex(0xab, 28);

export const OTHER_MANIFEST_ID = repeatHex(0x7e, 32);

export const PEER = "da-peer-a";

const MULTIADDR =
  "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz1234A";

export const EVENT_KEY = "0a1b2c3d";

export const MARKER = makeDeploymentMarker(FINGERPRINT);

export const sha256Hex = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const configOf = (): WatcherConfig =>
  ({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: {
      source: {
        sourceMode: "external_providers",
        providers: [
          {
            identity: "provider-a",
            operatorIdentitySha256: repeatHex(0x11, 32),
            endpoint: "https://cardano-a.example",
          },
          {
            identity: "provider-b",
            operatorIdentitySha256: repeatHex(0x22, 32),
            endpoint: "https://cardano-b.example",
          },
        ],
      },
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
      finality: {
        depth: 15,
        rollback: {
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: 15,
        },
      },
    },
    da: {
      peers: [{ identity: PEER, multiaddr: MULTIADDR }],
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
    },
    storage: {
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      },
    },
    proverWallet: {
      keySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      },
    },
    deadlines: {
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    },
  }) as unknown as WatcherConfig;

export const identityOf = (
  manifestId: string = FINGERPRINT,
): VerifiedWatcherDeploymentIdentity => ({
  manifestId,
  network: "Preprod",
  trustRootId: "trust-root-a",
  blueprintHash: repeatHex(0x33, 32),
  fundingProfileBundleDigest: "ab".repeat(32),
  ruleBundleCommitment: repeatHex(0x44, 32),
  programCommitments: {},
  durableMarker: makeDeploymentMarker(manifestId),
});

export const daPayload = (headerHash: string): DaPayload => {
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return {
    version: DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header: {
        prevUtxosRoot: repeatHex(0x01, 32),
        utxosRoot: repeatHex(0x02, 32),
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: repeatHex(0x03, 32),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: repeatHex(0x04, 32),
        eventToStepRoot: repeatHex(0x05, 32),
        validationTracesRoot: repeatHex(0x06, 32),
        ...counts,
        startTime: 1_000n,
        endTime: 1_999n,
        blockSlot: 42n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: repeatHex(0x07, 28),
        operatorVkey: repeatHex(0x08, 28),
        protocolVersion: 1n,
      },
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [[repeatHex(0x09, 32), repeatHex(0x0a, 40)]],
      transaction_preimages: [[repeatHex(0x09, 32), repeatHex(0x0b, 64)]],
      forced_transaction_preimages: [],
      cek_program_material: [],
      deposits: [],
      transition_trace: [[repeatHex(0x0c, 32), repeatHex(0x0d, 48)]],
      event_to_step: [[repeatHex(0x0e, 32), repeatHex(0x0f, 8)]],
      validation_traces: [[repeatHex(0x10, 32), repeatHex(0x11, 24)]],
      validation_trace_witnesses: [],
      counts,
    },
  };
};

type ProtocolHandler = (
  request: WatcherPublicDaRequest,
) => Promise<Uint8Array> | Uint8Array;

class ScriptedTransport implements WatcherPublicDaLibp2pTransportV1 {
  constructor(private readonly script: Record<string, ProtocolHandler>) {}

  async request(request: WatcherPublicDaRequest): Promise<Uint8Array> {
    const handler = this.script[request.protocol];
    if (handler === undefined) {
      throw new Error(`unscripted protocol ${request.protocol}`);
    }
    return handler(request);
  }
}

const capabilitiesBytes = (
  overrides: Partial<DaCapabilitiesResponse> = {},
): Buffer =>
  encodeDaCapabilitiesResponseCbor({
    deploymentFingerprint: daDeploymentFingerprintFromHex(FINGERPRINT),
    transportProtocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
    payloadSchemaVersions: [DA_PAYLOAD_INNER_SCHEMA_VERSION],
    envelopeContentEncodings: [
      DaPayloadContentEncoding.identity,
      DaPayloadContentEncoding.zstd,
    ],
    maxPayloadBytes: 1_000_000,
    maxInlineResponseBytes: 500_000,
    maxChunkBytes: 250_000,
    maxStreamsPerPeer: 8,
    requestTimeoutMs: 10_000,
    ...overrides,
  });

const payloadByHeaderBytes = (
  overrides: Partial<DaPayloadByHeaderResponse>,
): Buffer =>
  encodeDaPayloadByHeaderResponseCbor({
    status: "found_inline",
    headerHash: Buffer.from(HEADER_HASH, "hex"),
    payloadHash: null,
    payloadBytes: null,
    chunkManifest: null,
    reasonCode: null,
    ...overrides,
  });

export const PROOF_BUNDLE_BYTES = Buffer.alloc(96, 0x5a);

export const TRACE_STEP_BYTES = Buffer.alloc(64, 0x6b);

export const TRACE_PROOF_BYTES = Buffer.alloc(48, 0x7c);

export const EVENT_ENTRY_BYTES = Buffer.alloc(32, 0x8d);

export const EVENT_PROOF_BYTES = Buffer.alloc(24, 0x9e);

export const clientFor = (envelope: Buffer): WatcherPublicDaClient =>
  new WatcherPublicDaClient({
    config: configOf(),
    deploymentIdentity: identityOf(),
    transport: new ScriptedTransport({
      capabilities: () => capabilitiesBytes(),
      "payload-by-header": () =>
        payloadByHeaderBytes({
          payloadHash: computeDaSha256Hash(envelope),
          payloadBytes: envelope,
        }),
      "proof-bundle-by-header": () =>
        encodeDaProofBundleByHeaderResponseCbor({
          status: "found_inline",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          proofBundleHash: computeDaSha256Hash(PROOF_BUNDLE_BYTES),
          proofBundleBytes: PROOF_BUNDLE_BYTES,
          chunkManifest: null,
          reasonCode: null,
        }),
      "trace-step-by-index": () =>
        encodeDaTraceStepByIndexResponseCbor({
          status: "found",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          stepIndex: 3,
          transitionStepBytes: TRACE_STEP_BYTES,
          membershipProofBytes: TRACE_PROOF_BYTES,
        }),
      "event-to-step-by-event": () =>
        encodeDaEventToStepByEventResponseCbor({
          status: "found",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          eventKey: Buffer.from(EVENT_KEY, "hex"),
          eventToStepEntryBytes: EVENT_ENTRY_BYTES,
          membershipOrNonmembershipProofBytes: EVENT_PROOF_BYTES,
        }),
    }),
  });

export const nonmembershipClient = (): WatcherPublicDaClient =>
  new WatcherPublicDaClient({
    config: configOf(),
    deploymentIdentity: identityOf(),
    transport: new ScriptedTransport({
      capabilities: () => capabilitiesBytes(),
      "event-to-step-by-event": () =>
        encodeDaEventToStepByEventResponseCbor({
          status: "found",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          eventKey: Buffer.from(EVENT_KEY, "hex"),
          eventToStepEntryBytes: null,
          membershipOrNonmembershipProofBytes: EVENT_PROOF_BYTES,
        }),
    }),
  });
