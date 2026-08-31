import {
  computeDaSha256Hash,
  DaGossipTopic,
  daGossipTopic,
  decodeDaConflictEvidenceV1Cbor,
  decodeDaConflictingSignatureHeaderEvidenceV1Cbor,
  encodeDaConflictEvidenceV1Cbor,
  encodeDaConflictingSignatureHeaderEvidenceV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it, vi } from "vitest";

import type { Libp2pDaTransportConfig } from "../src/config.js";
import { StoreBackedDaAttestationProtocol } from "../src/da/libp2p/attestations.js";
import { DaGossip, type DaPubsubMessage } from "../src/da/libp2p/DaGossip.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import { createDaTopicAllowlist } from "../src/da/libp2p/DaTopics.js";
import { classifyDaLocalSigningCommitmentV1 } from "../src/peer/signatures.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
} from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { createDaConflictEvidenceGossipHandler } from "../src/watcher.js";
import { tempDir } from "./helpers.js";

const DEPLOYMENT_FINGERPRINT = "ab".repeat(32);
const REPORTER_PEER_ID = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
const UNKNOWN_PEER_ID = "12D3KooWCQ8WRN84GxEkR7k8dV6gb4ca3bNqM5LmT3evQVfBPGwv";
const LOWER_HEADER_HASH = "11".repeat(28);
const UPPER_HEADER_HASH = LOWER_HEADER_HASH;

describe("DA conflict evidence V1 lifecycle", () => {
  it("persists authenticated conflicting signatures once and survives restart", async () => {
    const fixture = await conflictFixture();
    const directory = await tempDir();
    const store = await JsonFileWatcherStore.open(directory);
    try {
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
    } finally {
      await store.close();
    }

    const reopened = await JsonFileWatcherStore.open(directory);
    try {
      await expect(reopened.listDaConflictEvidence()).resolves.toEqual([
        fixture.record,
      ]);
    } finally {
      await reopened.close();
    }
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

  it("preserves and reports a valid conflicting commitment before excluding it from quorum", async () => {
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const payload = Buffer.from("public retained DA");
    const payloadHash = computeDaSha256Hash(payload).toString("hex");
    const headerHash = LOWER_HEADER_HASH;
    const expected = availabilityCommitment(headerHash, "44".repeat(28));
    const conflicting = availabilityCommitment(headerHash, "55".repeat(28));
    const store = await JsonFileWatcherStore.open(await tempDir());
    await store.saveDaPayload({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: payload.toString("hex"),
      payloadSha256: payloadHash,
      sourcePeerId: "fixture",
      fetchedAt: "2026-07-27T00:00:00.000Z",
      verifiedAt: "2026-07-27T00:00:00.000Z",
      validationStatus: "verified",
      conflictStatus: "none",
    });
    const committeeValidation = validateDaCommittee({
      daParams: {
        committeeHex: signer.publicKeyHex,
        committeeSignersHash: Buffer.from(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ).toString("hex"),
        threshold: 1,
      },
    });
    const protocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      localPeerId: REPORTER_PEER_ID,
      committeeValidation,
      availabilityCommitmentAuthority: {
        deploymentIdentity: "99".repeat(28),
        bondOwnerCredential: "44".repeat(28),
        responseGeometry: {
          chunkByteLength: 4_096,
          trancheByteLength: 4 * 1_024 * 1_024,
          maxTrancheCount: 16,
        },
      },
      store,
    });
    const publishConflict = vi.fn(async () => undefined);
    protocol.setConflictEvidencePublisher(publishConflict);

    await expect(
      protocol.acceptAttestation({
        record: signatureRecord({
          signer,
          commitment: expected,
          payloadHash,
          committeeSignersHash: committeeValidation.committeeSignersHash,
        }),
        sourcePeerId: REPORTER_PEER_ID,
      }),
    ).resolves.toEqual({ status: "accepted" });
    await expect(
      protocol.acceptAttestation({
        record: signatureRecord({
          signer,
          commitment: conflicting,
          payloadHash,
          committeeSignersHash: committeeValidation.committeeSignersHash,
        }),
        sourcePeerId: REPORTER_PEER_ID,
      }),
    ).resolves.toMatchObject({
      status: "rejected",
      reason: expect.stringMatching(/authenticated release parameters/u),
    });

    await expect(store.listDaSignatures(headerHash)).resolves.toHaveLength(2);
    await expect(store.listDaConflictEvidence()).resolves.toHaveLength(1);
    expect(publishConflict).toHaveBeenCalledOnce();
  });

  it("refuses a local second signature for a different commitment identity", async () => {
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const first = availabilityCommitment(LOWER_HEADER_HASH, "44".repeat(28));
    const second = availabilityCommitment(LOWER_HEADER_HASH, "55".repeat(28));
    const prior = signatureRecord({
      signer,
      commitment: first,
      payloadHash: "66".repeat(32),
      committeeSignersHash: "77".repeat(32),
    });

    expect(
      classifyDaLocalSigningCommitmentV1({
        records: [prior],
        signerIndex: 0,
        expectedCommitmentDigest: second.digest,
      }),
    ).toMatchObject({
      maySign: false,
      conflictingVariants: [prior],
    });
    expect(
      classifyDaLocalSigningCommitmentV1({
        records: [prior],
        signerIndex: 0,
        expectedCommitmentDigest: first.digest,
      }),
    ).toMatchObject({
      maySign: true,
      existingExact: prior,
      conflictingVariants: [],
    });
  });

  it("retains same-header signer variants across file-store restart without last-write-wins collapse", async () => {
    const directory = await tempDir();
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const variants = [
      availabilityCommitment(LOWER_HEADER_HASH, "44".repeat(28)),
      availabilityCommitment(LOWER_HEADER_HASH, "55".repeat(28)),
    ].map((commitment) =>
      signatureRecord({
        signer,
        commitment,
        payloadHash: "66".repeat(32),
        committeeSignersHash: "77".repeat(32),
      }),
    );
    const first = await JsonFileWatcherStore.open(directory);
    for (const record of variants) {
      await first.saveDaSignature(record);
    }
    await first.close();

    const reopened = await JsonFileWatcherStore.open(directory);
    try {
      await expect(
        reopened.listDaSignatures(LOWER_HEADER_HASH),
      ).resolves.toHaveLength(2);
      for (const record of variants) {
        await expect(
          reopened.getDaSignature({
            headerHash: LOWER_HEADER_HASH,
            availabilityCommitmentDigest: record.availabilityCommitmentDigest,
            signerIndex: 0,
          }),
        ).resolves.toEqual(record);
      }
    } finally {
      await reopened.close();
    }
  });
});

const conflictFixture = async () => {
  const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
  const config = libp2pConfig(signer.publicKeyHex);
  const registry = DaPeerRegistry.fromConfig(config);
  const commitments = [
    availabilityCommitment(LOWER_HEADER_HASH, "44".repeat(28)),
    availabilityCommitment(UPPER_HEADER_HASH, "55".repeat(28)),
  ].sort((left, right) => left.digest.localeCompare(right.digest));
  const lower = commitments[0]!;
  const upper = commitments[1]!;
  const compactEvidence = encodeDaConflictingSignatureHeaderEvidenceV1Cbor({
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
  const encoded = encodeDaConflictEvidenceV1Cbor({
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

const availabilityCommitment = (headerHash: string, bondOwner: string) => {
  const commitment = SDK.buildDaAvailabilityCommitmentV1({
    deploymentIdentity: "99".repeat(28),
    headerHash,
    payload: Buffer.from("public retained DA"),
    bondOwner,
    responseGeometry: SDK.availabilityResponseGeometryV1({
      chunkByteLength: 4_096,
      trancheByteLength: 4 * 1_024 * 1_024,
      maxTrancheCount: 16,
    }),
  });
  const cbor = SDK.encodeDaAvailabilityCommitmentV1(commitment);
  return {
    commitment,
    cbor,
    digest: computeDaSha256Hash(Buffer.from(cbor, "hex")).toString("hex"),
  };
};

const signatureRecord = ({
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
    payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
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
