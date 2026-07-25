import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  type DaAttestationExchange,
  DaLibp2pAttestationExchange,
  decodeDaAttestationGossip,
  encodeDaAttestationGossip,
  StoreBackedDaAttestationProtocol,
} from "../src/da/libp2p/attestations.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  HeaderV1,
  PayloadRootSet,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { PeerSignatureCoordinator } from "../src/peer/coordinator.js";
import { PeerSignaturePoller } from "../src/peer/poller.js";
import { resolveRemoteDaAttestationTargets } from "../src/peer/targets.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
  validateDaSignerMembership,
} from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { makePayloadFixture, tempDir } from "./helpers.js";

describe("PeerSignatureCoordinator", () => {
  it("filters the local DA peer out of remote attestation targets", () => {
    const targets = resolveRemoteDaAttestationTargets({
      localPeerId: "local-peer",
      signerIndex: 1,
      peers: [
        {
          peerId: "local-peer",
          signerIndex: 1,
          roles: ["committee"],
        },
        {
          peerId: "remote-peer",
          signerIndex: 2,
          roles: ["committee"],
        },
        {
          peerId: "producer-peer",
          signerIndex: 3,
          roles: ["producer"],
        },
      ],
    });

    expect(targets.localPeer).toMatchObject({
      peerId: "local-peer",
      signerIndex: 1,
    });
    expect(targets.remotePeers).toEqual([
      { peerId: "remote-peer", signerIndex: 2 },
    ]);
  });

  it("rejects a local DA signer index that disagrees with the manifest peer", () => {
    expect(() =>
      resolveRemoteDaAttestationTargets({
        localPeerId: "local-peer",
        signerIndex: 0,
        peers: [
          {
            peerId: "local-peer",
            signerIndex: 1,
            roles: ["committee"],
          },
        ],
      }),
    ).toThrow(/does not match/);
  });

  it("publishes signatures through the libp2p attestation exchange and persists broadcast state", async () => {
    const deploymentFingerprint = "dep";
    const headerHash = "01".repeat(28);
    const receiverSigner = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const senderSigner = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    const committeeHex =
      receiverSigner.publicKeyHex + senderSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const receiverValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: receiverSigner,
      signerIndex: 0,
    });
    const senderValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: senderSigner,
      signerIndex: 1,
    });
    const receiverStore = await JsonFileWatcherStore.open(await tempDir());
    const senderStore = await JsonFileWatcherStore.open(await tempDir());
    await receiverStore.saveDaPayload({
      deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: "aabb",
      payloadSha256: "22".repeat(32),
      sourcePeerId: "fixture",
      fetchedAt: new Date().toISOString(),
      verifiedAt: new Date().toISOString(),
      validationStatus: "verified",
      conflictStatus: "none",
    });
    const receiverProtocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "receiver-peer",
      committeeValidation: receiverValidation,
      store: receiverStore,
    });
    const coordinator = new PeerSignatureCoordinator({
      deploymentFingerprint,
      peers: [{ signerIndex: 0, peerId: "receiver-peer" }],
      attestationExchange: inMemoryExchange({
        senderPeerId: "sender-peer",
        protocols: new Map([["receiver-peer", receiverProtocol]]),
      }),
      signer: senderSigner,
      signerIndex: 1,
      signerValidation: senderValidation,
      store: senderStore,
      requestTimeoutMs: 1000,
      retryInitialDelayMs: 10,
      retryMaxDelayMs: 100,
      retryMaxAttempts: 3,
    });

    const result = await coordinator.publishSignature(
      signatureRecord({
        deploymentFingerprint,
        headerHash,
        signerIndex: 1,
        committeeSignersHash,
        signatureWitness: signDaAttestation({
          signer: senderSigner,
          signerIndex: 1,
          headerHash,
        }),
      }),
    );

    expect(result).toBe("posted");
    await expect(
      receiverStore.getDaSignature({ headerHash, signerIndex: 1 }),
    ).resolves.toMatchObject({
      source: "peer",
      sourcePeer: "sender-peer",
    });
    await expect(
      senderStore.listPeerBroadcasts(headerHash),
    ).resolves.toMatchObject([
      { peerId: "receiver-peer", status: "posted", attempts: 1 },
    ]);
    await expect(senderStore.listPeerHealth()).resolves.toMatchObject([
      { peerId: "receiver-peer", consecutiveFailures: 0 },
    ]);
  });

  it("keeps peer-not-ready broadcasts retryable without failing L1-backed publish", async () => {
    const deploymentFingerprint = "dep";
    const headerHash = "02".repeat(28);
    const receiverSigner = await loadDaSigner(`hex:${"00".repeat(31)}03`);
    const senderSigner = await loadDaSigner(`hex:${"00".repeat(31)}04`);
    const committeeHex =
      receiverSigner.publicKeyHex + senderSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const receiverValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: receiverSigner,
      signerIndex: 0,
    });
    const senderValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: senderSigner,
      signerIndex: 1,
    });
    const receiverStore = await JsonFileWatcherStore.open(await tempDir());
    const senderStore = await JsonFileWatcherStore.open(await tempDir());
    const receiverProtocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "receiver-peer",
      committeeValidation: receiverValidation,
      store: receiverStore,
    });
    const coordinator = new PeerSignatureCoordinator({
      deploymentFingerprint,
      peers: [{ signerIndex: 0, peerId: "receiver-peer" }],
      attestationExchange: inMemoryExchange({
        senderPeerId: "sender-peer",
        protocols: new Map([["receiver-peer", receiverProtocol]]),
      }),
      signer: senderSigner,
      signerIndex: 1,
      signerValidation: senderValidation,
      store: senderStore,
      requestTimeoutMs: 1000,
      retryInitialDelayMs: 10,
      retryMaxDelayMs: 100,
      retryMaxAttempts: 3,
      onChainCoordinator: {
        publishSignature: async () => "posted",
      },
    });
    const record = signatureRecord({
      deploymentFingerprint,
      headerHash,
      signerIndex: 1,
      committeeSignersHash,
      signatureWitness: signDaAttestation({
        signer: senderSigner,
        signerIndex: 1,
        headerHash,
      }),
    });

    await expect(coordinator.publishSignature(record)).resolves.toBe("posted");
    await expect(
      senderStore.listPeerBroadcasts(headerHash),
    ).resolves.toMatchObject([
      { peerId: "receiver-peer", status: "failed", attempts: 1 },
    ]);

    await receiverStore.saveDaPayload({
      deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: "aabb",
      payloadSha256: "22".repeat(32),
      sourcePeerId: "fixture",
      fetchedAt: new Date().toISOString(),
      verifiedAt: new Date().toISOString(),
      validationStatus: "verified",
      conflictStatus: "none",
    });
    await new Promise((resolve) => setTimeout(resolve, 30));

    await expect(coordinator.publishSignature(record)).resolves.toBe("posted");
    await expect(
      senderStore.listPeerBroadcasts(headerHash),
    ).resolves.toMatchObject([
      { peerId: "receiver-peer", status: "posted", attempts: 2 },
    ]);
    await expect(
      receiverStore.getDaSignature({ headerHash, signerIndex: 1 }),
    ).resolves.toMatchObject({ source: "peer" });
  });

  it("polls arbitrary signer-index witnesses over the libp2p attestation exchange without a local signer", async () => {
    const deploymentFingerprint = "dep";
    const headerHash = "03".repeat(28);
    const signers = await Promise.all(
      Array.from({ length: 5 }, (_, index) =>
        loadDaSigner(`hex:${"00".repeat(31)}${seedByte(index + 10)}`),
      ),
    );
    const committeeHex = signers.map((signer) => signer.publicKeyHex).join("");
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const committeeValidation = validateDaCommittee({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
    });
    const localStore = await JsonFileWatcherStore.open(await tempDir());
    await localStore.saveDaPayload({
      deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: "aabb",
      payloadSha256: "22".repeat(32),
      sourcePeerId: "fixture",
      fetchedAt: new Date().toISOString(),
      verifiedAt: new Date().toISOString(),
      validationStatus: "verified",
      conflictStatus: "none",
    });
    const exchange: DaAttestationExchange = {
      publishAttestation: async () => ({ status: "accepted" }),
      attestationsByHeader: async () => [
        signatureRecord({
          deploymentFingerprint,
          headerHash,
          signerIndex: 3,
          committeeSignersHash,
          signatureWitness: signDaAttestation({
            signer: signers[3]!,
            signerIndex: 3,
            headerHash,
          }),
        }),
        signatureRecord({
          deploymentFingerprint,
          headerHash,
          signerIndex: 4,
          committeeSignersHash,
          signatureWitness: signDaAttestation({
            signer: signers[4]!,
            signerIndex: 4,
            headerHash,
          }),
        }),
      ],
    };
    const poller = new PeerSignaturePoller({
      deploymentFingerprint,
      peers: [{ peerId: "committee-peer" }],
      attestationExchange: exchange,
      signerValidation: committeeValidation,
      store: localStore,
      requestTimeoutMs: 1000,
    });

    await poller.pollPeerSignatures(headerHash);

    await expect(
      localStore.listDaSignatures(headerHash),
    ).resolves.toMatchObject([
      { signerIndex: 3, source: "peer", sourcePeer: "committee-peer" },
      { signerIndex: 4, source: "peer", sourcePeer: "committee-peer" },
    ]);
  });

  it("retrieves attestations over the typed libp2p request/response protocol", async () => {
    const deploymentFingerprint = "11".repeat(32);
    const receiverSigner = await loadDaSigner(`hex:${"00".repeat(31)}30`);
    const senderSigner = await loadDaSigner(`hex:${"00".repeat(31)}31`);
    const committeeHex =
      receiverSigner.publicKeyHex + senderSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const committeeValidation = validateDaCommittee({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
    });
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(payloadCbor).toString("hex");
    const receiverStore = await JsonFileWatcherStore.open(await tempDir());
    const localStore = await JsonFileWatcherStore.open(await tempDir());
    await saveVerifiedPayload(receiverStore, {
      deploymentFingerprint,
      headerHash,
      payloadHash,
      payloadCbor,
      header,
    });
    await saveVerifiedPayload(localStore, {
      deploymentFingerprint,
      headerHash,
      payloadHash,
      payloadCbor,
      header,
    });
    await receiverStore.saveDaSignature(
      signatureRecord({
        deploymentFingerprint,
        headerHash,
        signerIndex: 0,
        committeeSignersHash,
        payloadHash,
        signatureWitness: signDaAttestation({
          signer: receiverSigner,
          signerIndex: 0,
          headerHash,
        }),
      }),
    );
    const receiverProtocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "receiver-peer",
      committeeValidation,
      store: receiverStore,
    });
    const localProtocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "local-peer",
      committeeValidation,
      store: localStore,
    });
    const exchange = new DaLibp2pAttestationExchange({
      deploymentFingerprint,
      localPeerId: "local-peer",
      node: {
        request: async ({ payload }) =>
          receiverProtocol.handleAttestationsByHeaderRequest(payload),
        publishGossip: async () => undefined,
      },
      registry: new DaPeerRegistry([
        {
          peerId: "receiver-peer",
          signerIndex: 0,
          daVkey: receiverSigner.publicKeyHex,
          roles: ["committee"],
          multiaddrs: ["/dns4/receiver.example/tcp/4001/p2p/receiver-peer"],
          bootstrap: false,
        },
      ]),
      protocol: localProtocol,
      committeeValidation,
      store: localStore,
      requestTimeoutMs: 1000,
    });

    const [record] = await exchange.attestationsByHeader({
      peer: { peerId: "receiver-peer", signerIndex: 0 },
      deploymentFingerprint,
      headerHash,
    });

    expect(record).toMatchObject({
      deploymentFingerprint,
      headerHash,
      signerIndex: 0,
      payloadHash,
      committeeSignersHash,
      source: "peer",
      sourcePeer: "receiver-peer",
      validation: {
        headerHash,
        rootsMatch: true,
        stateQueueOutRef: "state-queue#0",
      },
    });
  });

  it("builds canonical attestation gossip messages without changing the on-chain witness", async () => {
    const deploymentFingerprint = "11".repeat(32);
    const headerHash = "04".repeat(28);
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}20`);
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
    );
    const protocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "announcer-peer",
      committeeValidation: {
        committeeKeys: [signer.publicKeyHex],
        committeeSignersHash,
        threshold: 1,
      },
      store: await JsonFileWatcherStore.open(await tempDir()),
    });
    const signatureWitness = signDaAttestation({
      signer,
      signerIndex: 0,
      headerHash,
    });
    const gossip = protocol.gossipMessageFor(
      signatureRecord({
        deploymentFingerprint,
        headerHash,
        signerIndex: 0,
        committeeSignersHash,
        signatureWitness,
      }),
    );

    const decoded = decodeDaAttestationGossip(
      encodeDaAttestationGossip(gossip),
    );

    expect(decoded.signerIndex).toBe(0);
    expect(decoded.daVkey.toString("hex")).toBe(signer.publicKeyHex);
    expect(decoded.onChainWitness.toString("hex")).toBe(signatureWitness);
  });
});

const inMemoryExchange = ({
  senderPeerId,
  protocols,
}: {
  readonly senderPeerId: string;
  readonly protocols: ReadonlyMap<string, StoreBackedDaAttestationProtocol>;
}): DaAttestationExchange => ({
  publishAttestation: async ({ peer, record }) => {
    const protocol = protocols.get(peer.peerId);
    if (protocol === undefined) {
      return { status: "unavailable", reason: "peer is unavailable" };
    }
    return protocol.acceptAttestation({ record, sourcePeerId: senderPeerId });
  },
  attestationsByHeader: async ({ peer, deploymentFingerprint, headerHash }) => {
    const protocol = protocols.get(peer.peerId);
    if (protocol === undefined) {
      return [];
    }
    return protocol.attestationsByHeader({
      deploymentFingerprint,
      headerHash,
    });
  },
});

const seedByte = (value: number): string => {
  if (value < 0 || value > 255) {
    throw new Error("seed byte must be uint8");
  }
  return value.toString(16).padStart(2, "0");
};

const signatureRecord = ({
  deploymentFingerprint,
  headerHash,
  signerIndex,
  committeeSignersHash,
  payloadHash = "22".repeat(32),
  signatureWitness,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly signerIndex: number;
  readonly committeeSignersHash: string;
  readonly payloadHash?: string;
  readonly signatureWitness: string;
}): DaSignatureRecord => ({
  deploymentFingerprint,
  headerHash,
  signerIndex,
  signatureWitness,
  payloadHash,
  committeeSignersHash,
  signedAt: new Date().toISOString(),
  broadcastStatus: "local",
  l1ChainPoint: {},
  validation: {
    payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
    rootsMatch: true,
    stateQueueOutRef: "tx#0",
    headerHash,
    rootSummary: {
      utxosRoot: "44".repeat(32),
      transactionsRoot: "55".repeat(32),
      depositsRoot: "66".repeat(32),
      withdrawalsRoot: "77".repeat(32),
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    },
    countSummary: {
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
    },
    l1Header: {
      startTime: "1",
      endTime: "2",
      operatorVkey: "88".repeat(28),
      prevHeaderHash: "99".repeat(28),
      protocolVersion: "1",
    },
  },
});

const saveVerifiedPayload = async (
  store: JsonFileWatcherStore,
  {
    deploymentFingerprint,
    headerHash,
    payloadHash,
    payloadCbor,
    header,
  }: {
    readonly deploymentFingerprint: string;
    readonly headerHash: string;
    readonly payloadHash: string;
    readonly payloadCbor: Buffer;
    readonly header: HeaderV1;
  },
): Promise<void> => {
  await store.upsertStateQueueHeader(
    stateQueueRecord({ deploymentFingerprint, headerHash, header }),
  );
  await store.saveDaPayload({
    deploymentFingerprint,
    headerHash,
    payloadSchemaVersion: 1,
    payloadCborHex: payloadCbor.toString("hex"),
    payloadSha256: payloadHash,
    sourcePeerId: "fixture",
    fetchedAt: new Date().toISOString(),
    verifiedAt: new Date().toISOString(),
    rootSummary: rootSummaryFromHeader(header),
    validationStatus: "verified",
    conflictStatus: "none",
  } satisfies DaPayloadRecord);
};

const stateQueueRecord = ({
  deploymentFingerprint,
  headerHash,
  header,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly header: HeaderV1;
}): StateQueueHeaderRecord => ({
  deploymentFingerprint,
  headerHash,
  stateQueueOutRef: "state-queue#0",
  blockAssetName: `block-${headerHash}`,
  header,
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint: {
    slot: 1,
    blockHash: "aa".repeat(32),
    depth: 10,
    providerSource: "fixture",
  },
  finalized: true,
  status: "unattested",
  validationErrors: [],
  updatedAt: new Date().toISOString(),
});

const rootSummaryFromHeader = (header: HeaderV1): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
});
