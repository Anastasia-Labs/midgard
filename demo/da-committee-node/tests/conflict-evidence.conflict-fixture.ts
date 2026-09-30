import {
  computeDaSha256Hash,
  DaGossipTopic,
  daGossipTopic,
  encodeDaConflictEvidenceCbor,
  encodeDaConflictingSignatureHeaderEvidenceCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { vi } from "vitest";

import { createDaConflictEvidenceGossipHandler } from "../src/committee-service.js";
import type { Libp2pDaTransportConfig } from "../src/config.js";
import { DaGossip, type DaPubsubMessage } from "../src/da/libp2p/DaGossip.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import { createDaTopicAllowlist } from "../src/da/libp2p/DaTopics.js";
import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import { JsonFileCommitteeStore } from "../src/store.js";

export const DEPLOYMENT_FINGERPRINT = "ab".repeat(32);

export const REPORTER_PEER_ID =
  "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";

export const UNKNOWN_PEER_ID =
  "12D3KooWCQ8WRN84GxEkR7k8dV6gb4ca3bNqM5LmT3evQVfBPGwv";

export const LOWER_HEADER_HASH = "11".repeat(28);

export const UPPER_HEADER_HASH = LOWER_HEADER_HASH;

export const conflictFixture = async () => {
  const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
  const config = libp2pConfig(signer.publicKeyHex);
  const registry = DaPeerRegistry.fromConfig(config);
  const commitments = [
    availabilityCommitment(LOWER_HEADER_HASH, "99".repeat(28)),
    availabilityCommitment(UPPER_HEADER_HASH, "55".repeat(28)),
  ].sort((left, right) => left.digest.localeCompare(right.digest));
  const lower = commitments[0]!;
  const upper = commitments[1]!;
  const compactEvidence = encodeDaConflictingSignatureHeaderEvidenceCbor({
    signerIndex: 0,
    daVkey: Buffer.from(signer.publicKeyHex, "hex"),
    lowerHeaderHash: Buffer.from(lower.commitment.header_hash, "hex"),
    lowerCommitmentCbor: Buffer.from(lower.cbor, "hex"),
    lowerHeaderWitness: Buffer.from(
      signDaAttestation({
        signer,
        signerIndex: 0,
        availabilityCommitment: lower.commitment,
      }),
      "hex",
    ),
    upperHeaderHash: Buffer.from(upper.commitment.header_hash, "hex"),
    upperCommitmentCbor: Buffer.from(upper.cbor, "hex"),
    upperHeaderWitness: Buffer.from(
      signDaAttestation({
        signer,
        signerIndex: 0,
        availabilityCommitment: upper.commitment,
      }),
      "hex",
    ),
  });
  const evidenceHash = computeDaSha256Hash(compactEvidence);
  const encoded = encodeDaConflictEvidenceCbor({
    deploymentFingerprint: Buffer.from(DEPLOYMENT_FINGERPRINT, "hex"),
    headerHash: Buffer.from(lower.commitment.header_hash, "hex"),
    evidenceKind: "equivocation",
    evidenceHash,
    compactEvidence,
  });
  return {
    config,
    registry,
    encoded,
    record: {
      conflictSchemaVersion: 1,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      headerHash: lower.commitment.header_hash,
      commitmentDigest: lower.digest,
      conflictingHeaderHash: upper.commitment.header_hash,
      conflictingCommitmentDigest: upper.digest,
      signerIndex: 0,
      evidenceKind: "equivocation",
      evidenceHash: evidenceHash.toString("hex"),
      compactEvidenceCborHex: compactEvidence.toString("hex"),
      reporterPeerId: REPORTER_PEER_ID,
      receivedAt: "2026-07-27T00:00:00.000Z",
    } as const,
  };
};

// A commitment under `deploymentIdentity`: the authority's is "99" * 28, so
// any other identity is a conflicting commitment to the same payload.
export const availabilityCommitment = (
  headerHash: string,
  deploymentIdentity: string,
) => {
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity,
    headerHash,
    payload: Buffer.from("public retained DA"),
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: 4_096,
      trancheByteLength: 4 * 1_024 * 1_024,
      maxTrancheCount: 16,
    }),
  });
  const cbor = SDK.encodeDaAvailabilityCommitment(commitment);
  return {
    commitment,
    cbor,
    digest: computeDaSha256Hash(Buffer.from(cbor, "hex")).toString("hex"),
  };
};

export const signatureRecord = ({
  signer,
  commitment,
  payloadHash,
  committeeSignersHash,
}: {
  readonly signer: Awaited<ReturnType<typeof loadDaSigner>>;
  readonly commitment: ReturnType<typeof availabilityCommitment>;
  readonly payloadHash: string;
  readonly committeeSignersHash: string;
}) => ({
  deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
  headerHash: commitment.commitment.header_hash,
  signerIndex: 0,
  signatureWitness: signDaAttestation({
    signer,
    signerIndex: 0,
    availabilityCommitment: commitment.commitment,
  }),
  availabilityCommitmentCbor: commitment.cbor,
  availabilityCommitmentDigest: commitment.digest,
  payloadHash,
  committeeSignersHash,
  signedAt: "2026-07-27T00:00:00.000Z",
  broadcastStatus: "local" as const,
  source: "local" as const,
  l1ChainPoint: {},
  validation: {
    payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
    rootsMatch: true,
    stateQueueOutRef: "aa".repeat(32) + "#0",
    headerHash: commitment.commitment.header_hash,
    rootSummary: {
      utxosRoot: "01".repeat(32),
      withdrawalsRoot: "02".repeat(32),
      forcedTransactionsRoot: "03".repeat(32),
      transactionsRoot: "04".repeat(32),
      depositsRoot: "05".repeat(32),
      transitionTraceRoot: "06".repeat(32),
      eventToStepRoot: "07".repeat(32),
      validationTracesRoot: "08".repeat(32),
    },
    countSummary: {
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    },
    l1Header: {
      startTime: "1",
      endTime: "2",
      operatorVkey: "09".repeat(28),
      prevHeaderHash: "0a".repeat(28),
      protocolVersion: "1",
    },
  },
});

export const conflictGossip = (
  registry: DaPeerRegistry,
  store: JsonFileCommitteeStore,
): DaGossip => {
  const handler = createDaConflictEvidenceGossipHandler({
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    registry,
    store,
    now: () => new Date("2026-07-27T00:00:00.000Z"),
  });
  return new DaGossip({
    pubsub: {
      publish: vi.fn(),
      subscribe: vi.fn(),
    },
    topics: createDaTopicAllowlist(DEPLOYMENT_FINGERPRINT),
    config: libp2pConfig(registry.getBySignerIndex(0)!.daVkey!),
    messageHandlers: new Map([[conflictTopic(), handler]]),
  });
};

export const signedMessage = (
  data: Uint8Array,
  peerId = REPORTER_PEER_ID,
): DaPubsubMessage => ({
  type: "signed",
  from: { toString: () => peerId },
  topic: conflictTopic(),
  data,
});

export const conflictTopic = (): string =>
  daGossipTopic(DEPLOYMENT_FINGERPRINT, DaGossipTopic.conflicts);

const libp2pConfig = (daVkey: string): Libp2pDaTransportConfig => ({
  kind: "libp2p",
  deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
  noHttpDaTransport: true,
  threshold: 1,
  listenMultiaddrs: ["/ip4/0.0.0.0/tcp/0"],
  announceMultiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${REPORTER_PEER_ID}`],
  bootstrapMultiaddrs: [],
  gossip: {
    strictSign: true,
    emitSelf: false,
    allowedTopicsOnly: true,
    maxGossipMessageBytes: 65_536,
  },
  limits: {
    maxPayloadBytes: 67_108_864,
    maxInlineResponseBytes: 1_048_576,
    maxChunkBytes: 1_048_576,
    maxStreamsPerPeer: 16,
    requestTimeoutMs: 15_000,
  },
  retentionDays: 15,
  peers: [
    {
      signerIndex: 0,
      daVkey,
      peerId: REPORTER_PEER_ID,
      multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${REPORTER_PEER_ID}`],
      roles: ["committee", "retrieval"],
    },
  ],
});
