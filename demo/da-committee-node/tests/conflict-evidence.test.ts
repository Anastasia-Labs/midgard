import {
  computeDaSha256Hash,
  DaGossipTopic,
  daGossipTopic,
  decodeDaConflictEvidenceV1Cbor,
  decodeDaConflictingSignatureHeaderEvidenceV1Cbor,
  encodeDaConflictEvidenceV1Cbor,
  encodeDaConflictingSignatureHeaderEvidenceV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it, vi } from "vitest";

import type { Libp2pDaTransportConfig } from "../src/config.js";
import { DaGossip, type DaPubsubMessage } from "../src/da/libp2p/DaGossip.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import { createDaTopicAllowlist } from "../src/da/libp2p/DaTopics.js";
import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { createDaConflictEvidenceGossipHandler } from "../src/watcher.js";
import { tempDir } from "./helpers.js";

const DEPLOYMENT_FINGERPRINT = "ab".repeat(32);
const REPORTER_PEER_ID = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
const UNKNOWN_PEER_ID = "12D3KooWCQ8WRN84GxEkR7k8dV6gb4ca3bNqM5LmT3evQVfBPGwv";
const LOWER_HEADER_HASH = "11".repeat(28);
const UPPER_HEADER_HASH = "22".repeat(28);

describe("DA conflict evidence V1 lifecycle", () => {
  it("persists authenticated conflicting signatures once and survives restart", async () => {
    const fixture = await conflictFixture();
    const directory = await tempDir();
    const store = await JsonFileWatcherStore.open(directory);
    const gossip = conflictGossip(fixture.registry, store);

    await expect(
      gossip.handleInboundMessage(signedMessage(fixture.encoded)),
    ).resolves.toBe(true);
    await expect(
      gossip.handleInboundMessage(signedMessage(fixture.encoded)),
    ).resolves.toBe(true);
    await expect(store.listDaConflictEvidence()).resolves.toEqual([
      fixture.record,
    ]);

    const reopened = await JsonFileWatcherStore.open(directory);
    await expect(reopened.listDaConflictEvidence()).resolves.toEqual([
      fixture.record,
    ]);
  });

  it("rejects forged, malformed, and wrong-deployment evidence before persistence", async () => {
    const fixture = await conflictFixture();
    const store = await JsonFileWatcherStore.open(await tempDir());
    const gossip = conflictGossip(fixture.registry, store);
    const conflict = decodeDaConflictEvidenceV1Cbor(fixture.encoded);

    await expect(
      gossip.handleInboundMessage({
        type: "unsigned",
        topic: conflictTopic(),
        data: fixture.encoded,
      }),
    ).rejects.toThrow(/must be strictly signed/u);
    await expect(
      gossip.handleInboundMessage(
        signedMessage(fixture.encoded, UNKNOWN_PEER_ID),
      ),
    ).rejects.toThrow(/unknown DA libp2p peer/u);
    await expect(
      gossip.handleInboundMessage(
        signedMessage(
          encodeDaConflictEvidenceV1Cbor({
            ...conflict,
            deploymentFingerprint: Buffer.alloc(32, 0xcd),
          }),
        ),
      ),
    ).rejects.toThrow(/deployment does not match/u);
    await expect(
      gossip.handleInboundMessage(
        signedMessage(
          encodeDaConflictEvidenceV1Cbor({
            ...conflict,
            evidenceHash: Buffer.alloc(32, 0xee),
          }),
        ),
      ),
    ).rejects.toThrow(/hash does not match/u);
    await expect(
      gossip.handleInboundMessage(
        signedMessage(
          encodeDaConflictEvidenceV1Cbor({
            ...conflict,
            headerHash: Buffer.alloc(28, 0x01),
          }),
        ),
      ),
    ).rejects.toThrow(/header does not match/u);

    const equivocation = decodeDaConflictingSignatureHeaderEvidenceV1Cbor(
      conflict.compactEvidence!,
    );
    const forgedCompact = encodeDaConflictingSignatureHeaderEvidenceV1Cbor({
      ...equivocation,
      upperHeaderWitness: Buffer.concat([
        Buffer.from([equivocation.signerIndex]),
        Buffer.alloc(64, 0xff),
      ]),
    });
    await expect(
      gossip.handleInboundMessage(
        signedMessage(
          encodeDaConflictEvidenceV1Cbor({
            ...conflict,
            evidenceHash: computeDaSha256Hash(forgedCompact),
            compactEvidence: forgedCompact,
          }),
        ),
      ),
    ).rejects.toThrow(/invalid attestation signature/u);
    await expect(
      gossip.handleInboundMessage(
        signedMessage(Buffer.concat([fixture.encoded, Buffer.from([0])])),
      ),
    ).rejects.toThrow();

    await expect(store.listDaConflictEvidence()).resolves.toEqual([]);
  });
});

const conflictFixture = async () => {
  const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
  const config = libp2pConfig(signer.publicKeyHex);
  const registry = DaPeerRegistry.fromConfig(config);
  const compactEvidence = encodeDaConflictingSignatureHeaderEvidenceV1Cbor({
    signerIndex: 0,
    daVkey: Buffer.from(signer.publicKeyHex, "hex"),
    lowerHeaderHash: Buffer.from(LOWER_HEADER_HASH, "hex"),
    lowerHeaderWitness: Buffer.from(
      signDaAttestation({
        signer,
        signerIndex: 0,
        headerHash: LOWER_HEADER_HASH,
      }),
      "hex",
    ),
    upperHeaderHash: Buffer.from(UPPER_HEADER_HASH, "hex"),
    upperHeaderWitness: Buffer.from(
      signDaAttestation({
        signer,
        signerIndex: 0,
        headerHash: UPPER_HEADER_HASH,
      }),
      "hex",
    ),
  });
  const evidenceHash = computeDaSha256Hash(compactEvidence);
  const encoded = encodeDaConflictEvidenceV1Cbor({
    deploymentFingerprint: Buffer.from(DEPLOYMENT_FINGERPRINT, "hex"),
    headerHash: Buffer.from(LOWER_HEADER_HASH, "hex"),
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
      headerHash: LOWER_HEADER_HASH,
      conflictingHeaderHash: UPPER_HEADER_HASH,
      signerIndex: 0,
      evidenceKind: "equivocation",
      evidenceHash: evidenceHash.toString("hex"),
      compactEvidenceCborHex: compactEvidence.toString("hex"),
      reporterPeerId: REPORTER_PEER_ID,
      receivedAt: "2026-07-27T00:00:00.000Z",
    } as const,
  };
};

const conflictGossip = (
  registry: DaPeerRegistry,
  store: JsonFileWatcherStore,
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

const signedMessage = (
  data: Uint8Array,
  peerId = REPORTER_PEER_ID,
): DaPubsubMessage => ({
  type: "signed",
  from: { toString: () => peerId },
  topic: conflictTopic(),
  data,
});

const conflictTopic = (): string =>
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
