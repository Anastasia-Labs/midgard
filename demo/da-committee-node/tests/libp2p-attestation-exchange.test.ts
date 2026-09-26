import { createServer } from "node:net";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  type DaAttestationGossip,
  DaGossipTopic,
  daGossipTopic,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { createLibp2p } from "libp2p";
import { afterEach, describe, expect, it, vi } from "vitest";

import { CommitteeService } from "../src/committee-service.js";
import type { Libp2pDaTransportConfig } from "../src/config.js";
import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import { countSetBits, setSignerBit } from "../src/coordinator/witnesses.js";
import {
  createDaLibp2pAttestationGossipHandlers,
  createDaLibp2pAttestationRequestHandlers,
  daAttestationGossipFromRecord,
  DaLibp2pAttestationExchange,
  encodeDaAttestationGossip,
  StoreBackedDaAttestationProtocol,
} from "../src/da/libp2p/attestations.js";
import {
  DaLibp2pNode,
  type DaLibp2pRuntimeNode,
} from "../src/da/libp2p/DaLibp2pNode.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import type {
  DaAttestationCandidateRecord,
  DaPayloadRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
  DaStoredPayloadRootSet,
  Header,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { PeerSignatureCoordinator } from "../src/peer/coordinator.js";
import {
  type DaAvailabilityCommitmentAuthority,
  deriveExpectedDaAvailabilityCommitment,
  validateDaSignatureRecord,
} from "../src/peer/signatures.js";
import {
  type DaCommitteeValidation,
  type DaSigner,
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
  validateDaSignerMembership,
} from "../src/signer.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from "./helpers.js";
import { withFinalSnapshot } from "./helpers/final-snapshot.js";

// The request deadline the live committee runs with. Every pull must finish
// far inside it; a response stream the server never closes ends only there.
const REQUEST_TIMEOUT_MS = 5_000;
const PROMPT_RESPONSE_MS = 2_000;
// Twenty pulls on one connection exceed this several times over, so any
// stream left half-open would exhaust the muxer.
const MAX_STREAMS_PER_PEER = 8;

const runningNodes: DaLibp2pNode[] = [];
const openedStores: JsonFileCommitteeStore[] = [];

afterEach(async () => {
  await Promise.all(runningNodes.splice(0).map((node) => node.stop()));
  await Promise.all(openedStores.splice(0).map((store) => store.close()));
});

describe("DA attestation exchange over real libp2p", () => {
  it("pulls a peer's attestations well inside the deadline and repeatedly without leaking streams", async () => {
    const committee = await makeCommittee(2);
    const [storeA, storeB] = await openStores(2);
    await saveVerifiedPayload(storeA!, committee);
    await saveVerifiedPayload(storeB!, committee);
    const signatureB = localSignature(committee, 1);
    await storeB!.saveDaSignature(signatureB);
    const [memberA, memberB] = await startMembers({
      committee,
      stores: [storeA!, storeB!],
    });

    const pull = (headerHash = committee.headerHash) =>
      memberA!.exchange.attestationsByHeader({
        peer: { peerId: memberB!.peerId, signerIndex: 1 },
        deploymentFingerprint: committee.deploymentFingerprint,
        headerHash,
      });

    const startedAt = performance.now();
    const records = await pull();
    expect(performance.now() - startedAt).toBeLessThan(PROMPT_RESPONSE_MS);
    expect(records).toEqual([
      expect.objectContaining({
        headerHash: committee.headerHash,
        signerIndex: 1,
        signatureWitness: signatureB.signatureWitness,
        source: "peer",
        sourcePeer: memberB!.peerId,
      }),
    ]);

    for (let attempt = 0; attempt < 20; attempt += 1) {
      const repeatStartedAt = performance.now();
      await expect(pull()).resolves.toHaveLength(1);
      expect(performance.now() - repeatStartedAt).toBeLessThan(
        PROMPT_RESPONSE_MS,
      );
    }

    // A not-found response closes its stream the same way.
    const unknownStartedAt = performance.now();
    await expect(pull("0f".repeat(28))).resolves.toEqual([]);
    expect(performance.now() - unknownStartedAt).toBeLessThan(
      PROMPT_RESPONSE_MS,
    );
  }, 60_000);

  it("stores a peer signature published on the attestations gossip topic", async () => {
    const committee = await makeCommittee(2);
    const [storeA, storeB] = await openStores(2);
    await saveVerifiedPayload(storeA!, committee);
    await saveVerifiedPayload(storeB!, committee);
    const signatureB = localSignature(committee, 1);
    const [memberA, memberB] = await startMembers({
      committee,
      stores: [storeA!, storeB!],
    });

    await expect(
      memberB!.exchange.publishAttestation({
        peer: { peerId: memberA!.peerId, signerIndex: 0 },
        record: signatureB,
      }),
    ).resolves.toEqual({ status: "accepted" });

    await vi.waitFor(
      async () => {
        expect(await storeA!.listDaSignatures(committee.headerHash)).toEqual([
          expect.objectContaining({
            signerIndex: 1,
            signatureWitness: signatureB.signatureWitness,
            source: "peer",
            sourcePeer: memberB!.peerId,
            broadcastStatus: "posted",
          }),
        ]);
      },
      { timeout: 10_000, interval: 50 },
    );
    const [stored] = await storeA!.listDaSignatures(committee.headerHash);
    // The stored record passes the authority check the coordinator applies.
    expect(
      validateDaSignatureRecord({
        body: stored!,
        headerHash: committee.headerHash,
        deploymentFingerprint: committee.deploymentFingerprint,
        signerValidation: committee.validation,
        verifiedPayload: await storeA!.getDaPayload(committee.headerHash),
        expectedAvailabilityCommitmentCbor: committee.commitment.commitmentCbor,
        expectedAvailabilityCommitmentDigest:
          committee.commitment.commitmentDigest,
      }),
    ).toBeUndefined();
    expect(memberA!.gossipErrors).toEqual([]);
  }, 60_000);

  it("rejects forged, misattributed, and unverifiable gossip attestations without storing them", async () => {
    // Signer index 2 belongs to a committee member that runs no node here.
    const committee = await makeCommittee(3);
    const [storeA, storeB] = await openStores(2);
    await saveVerifiedPayload(storeA!, committee);
    await saveVerifiedPayload(storeB!, committee);
    // Only the sender holds this second block; the receiver never verified it.
    const unverified = await makePayloadFixture(2);
    await saveVerifiedPayload(storeB!, {
      deploymentFingerprint: committee.deploymentFingerprint,
      headerHash: unverified.headerHash,
      header: unverified.header,
      payloadCbor: unverified.payloadCbor,
    });
    const [memberA, memberB] = await startMembers({
      committee,
      stores: [storeA!, storeB!],
    });
    const gossipOf = (
      record: DaSignatureRecord,
      overrides: Partial<DaAttestationGossip> = {},
    ): Buffer =>
      encodeDaAttestationGossip({
        ...daAttestationGossipFromRecord({
          record,
          daVkey: committee.validation.committeeKeys[record.signerIndex]!,
          announcedByPeerId: memberB!.peerId,
        }),
        ...overrides,
      });
    const corruptedWitness = Buffer.from(
      localSignature(committee, 1).signatureWitness,
      "hex",
    );
    corruptedWitness[corruptedWitness.length - 1]! ^= 0x01;
    const unverifiedPayloadHash = computeDaSha256Hash(
      unverified.payloadCbor,
    ).toString("hex");

    const forgeries: readonly {
      readonly name: string;
      readonly headerHash: string;
      readonly message: Buffer;
      readonly reason: RegExp;
    }[] = [
      {
        name: "bad signature",
        headerHash: committee.headerHash,
        message: gossipOf(localSignature(committee, 1), {
          onChainWitness: corruptedWitness,
        }),
        reason:
          /rejected DA attestation gossip from \S+: signature witness verification failed/,
      },
      {
        name: "valid signature of an absent committee member",
        headerHash: committee.headerHash,
        message: gossipOf(localSignature(committee, 2)),
        reason: /signer index 2 does not belong to authenticated peer/,
      },
      {
        name: "valid signature of the receiving member",
        headerHash: committee.headerHash,
        message: gossipOf(localSignature(committee, 0)),
        reason: /signer index 0 does not belong to authenticated peer/,
      },
      {
        name: "wrong deployment fingerprint",
        headerHash: committee.headerHash,
        message: gossipOf(localSignature(committee, 1), {
          deploymentFingerprint: Buffer.alloc(32, 0xee),
        }),
        reason: /deployment fingerprint mismatch/,
      },
      {
        name: "announced as another peer",
        headerHash: committee.headerHash,
        message: gossipOf(localSignature(committee, 1), {
          announcedByPeerId: memberA!.peerId,
        }),
        reason: /announcing peer does not match the authenticated peer/,
      },
      {
        name: "no verified payload",
        headerHash: unverified.headerHash,
        message: gossipOf(
          signatureRecord({
            deploymentFingerprint: committee.deploymentFingerprint,
            headerHash: unverified.headerHash,
            header: unverified.header,
            signerIndex: 1,
            signer: committee.signers[1]!,
            committeeSignersHash: committee.validation.committeeSignersHash,
            payloadHash: unverifiedPayloadHash,
            commitment: commitmentFor(
              unverified.headerHash,
              unverified.payloadCbor,
            ),
          }),
        ),
        reason: /verified payload or observed header is not available/,
      },
    ];

    for (const [index, forgery] of forgeries.entries()) {
      await memberB!.node.publishGossip(
        DaGossipTopic.attestations,
        forgery.message,
      );
      await vi.waitFor(
        () => {
          expect(memberA!.gossipErrors).toHaveLength(index + 1);
        },
        { timeout: 10_000, interval: 50 },
      );
      expect(errorMessage(memberA!.gossipErrors[index]), forgery.name).toMatch(
        forgery.reason,
      );
      await expect(
        storeA!.listDaSignatures(forgery.headerHash),
        forgery.name,
      ).resolves.toEqual([]);
    }

    // The channel stayed live throughout: an honest signature still lands.
    await memberB!.node.publishGossip(
      DaGossipTopic.attestations,
      gossipOf(localSignature(committee, 1)),
    );
    await vi.waitFor(
      async () => {
        expect(await storeA!.listDaSignatures(committee.headerHash)).toEqual([
          expect.objectContaining({ signerIndex: 1, source: "peer" }),
        ]);
      },
      { timeout: 10_000, interval: 50 },
    );
    expect(memberA!.gossipErrors).toHaveLength(forgeries.length);
  }, 60_000);
});

describe("DA committee reaches threshold after a single-witness add_signatures", () => {
  it("completes the attestation from a peer signature received by gossip alone", async () => {
    const calls = await runCommitteeLifecycle({
      memberA: { ingestGossip: true, serveAttestations: true },
      // Member B serves no pulls, so gossip is member A's only source.
      memberB: { ingestGossip: true, serveAttestations: false },
      peerSignatureSource: "gossip",
    });
    expect(calls).toEqual(["init", "add:0", "add:1", "apply"]);
  }, 90_000);

  it("completes the attestation from a peer signature pulled by attestationsByHeader alone", async () => {
    const calls = await runCommitteeLifecycle({
      // Member A ignores attestation gossip, so the pull is its only source.
      memberA: { ingestGossip: false, serveAttestations: true },
      memberB: { ingestGossip: true, serveAttestations: true },
      peerSignatureSource: "pull",
    });
    expect(calls).toEqual(["init", "add:0", "add:1", "apply"]);
  }, 90_000);
});

type Committee = {
  readonly deploymentFingerprint: string;
  readonly signerSeeds: readonly string[];
  readonly signers: readonly DaSigner[];
  readonly validation: DaCommitteeValidation;
  readonly header: Header;
  readonly headerHash: string;
  readonly payloadCbor: Buffer;
  readonly payloadHash: string;
  readonly commitment: ReturnType<typeof commitmentFor>;
};

type MemberOptions = {
  readonly ingestGossip: boolean;
  readonly serveAttestations: boolean;
};

type RunningMember = {
  readonly peerId: string;
  readonly node: DaLibp2pNode;
  readonly exchange: DaLibp2pAttestationExchange;
  readonly gossipErrors: unknown[];
};

// Matches `minimalConfig`, so committee services and nodes share a deployment.
const DEPLOYMENT_FINGERPRINT = "f".repeat(64);

// Matches the availability authority `minimalConfig` gives a service.
const availabilityCommitmentAuthority: DaAvailabilityCommitmentAuthority = {
  deploymentIdentity: "99".repeat(28),
  bondOwnerCredential: "76".repeat(28),
  responseGeometry: {
    chunkByteLength: 14_020,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  },
};

const commitmentFor = (headerHash: string, payloadCbor: Buffer) =>
  deriveExpectedDaAvailabilityCommitment({
    authority: availabilityCommitmentAuthority,
    headerHash,
    payloadCborHex: payloadCbor.toString("hex"),
  });

const makeCommittee = async (size: number): Promise<Committee> => {
  const signerSeeds = Array.from(
    { length: size },
    (_, index) => `${"00".repeat(31)}${(0xc1 + index).toString(16)}`,
  );
  const signers = await Promise.all(
    signerSeeds.map((seed) => loadDaSigner(`hex:${seed}`)),
  );
  const committeeHex = signers.map((signer) => signer.publicKeyHex).join("");
  const validation = validateDaCommittee({
    daParams: {
      committeeHex,
      committeeSignersHash: bytesToHex(
        blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
      ),
      threshold: 2,
    },
  });
  const { header, headerHash, payloadCbor } = await makePayloadFixture();
  return {
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    signerSeeds,
    signers,
    validation,
    header,
    headerHash,
    payloadCbor,
    payloadHash: computeDaSha256Hash(payloadCbor).toString("hex"),
    commitment: commitmentFor(headerHash, payloadCbor),
  };
};

const openStores = async (count: number): Promise<JsonFileCommitteeStore[]> => {
  const stores = await Promise.all(
    Array.from({ length: count }, async () =>
      JsonFileCommitteeStore.open(await tempDir()),
    ),
  );
  openedStores.push(...stores);
  return stores;
};

const localSignature = (
  committee: Committee,
  signerIndex: number,
): DaSignatureRecord =>
  signatureRecord({
    deploymentFingerprint: committee.deploymentFingerprint,
    headerHash: committee.headerHash,
    header: committee.header,
    signerIndex,
    signer: committee.signers[signerIndex]!,
    committeeSignersHash: committee.validation.committeeSignersHash,
    payloadHash: committee.payloadHash,
    commitment: committee.commitment,
  });

/**
 * Starts one real committee libp2p node per store (TCP on loopback, Noise,
 * Yamux, StrictSign GossipSub), wired the way `index.ts` wires a member, and
 * waits until every node sees the others on the attestations topic.
 */
const startMembers = async ({
  committee,
  stores,
  options = stores.map(() => ({
    ingestGossip: true,
    serveAttestations: true,
  })),
}: {
  readonly committee: Committee;
  readonly stores: readonly JsonFileCommitteeStore[];
  readonly options?: readonly MemberOptions[];
}): Promise<RunningMember[]> => {
  const keySources = stores.map(
    (_, index) => `seed:${"00".repeat(31)}${(0xa1 + index).toString(16)}`,
  );
  const identities = await Promise.all(
    keySources.map((source) => loadDaLibp2pIdentity(source)),
  );
  const ports = await Promise.all(stores.map(() => reserveLoopbackPort()));
  const multiaddrs = identities.map(
    (identity, index) =>
      `/ip4/127.0.0.1/tcp/${ports[index]!.toString()}/p2p/${identity.peerId}`,
  );
  const topicId = daGossipTopic(
    committee.deploymentFingerprint,
    DaGossipTopic.attestations,
  );
  const members = stores.map((store, index) => {
    const config: Libp2pDaTransportConfig = {
      kind: "libp2p",
      deploymentFingerprint: committee.deploymentFingerprint,
      noHttpDaTransport: true,
      threshold: 2,
      listenMultiaddrs: [`/ip4/127.0.0.1/tcp/${ports[index]!.toString()}`],
      announceMultiaddrs: [multiaddrs[index]!],
      bootstrapMultiaddrs: multiaddrs.filter((_, peer) => peer !== index),
      gossip: {
        strictSign: true,
        emitSelf: false,
        allowedTopicsOnly: true,
        maxGossipMessageBytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
      },
      limits: {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        maxStreamsPerPeer: MAX_STREAMS_PER_PEER,
        requestTimeoutMs: REQUEST_TIMEOUT_MS,
      },
      retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
      peers: identities.map((identity, peer) => ({
        signerIndex: peer,
        daVkey: committee.validation.committeeKeys[peer]!,
        peerId: identity.peerId,
        multiaddrs: [multiaddrs[peer]!],
        roles: ["committee", "retrieval"],
      })),
    };
    const memberOptions = options[index]!;
    const registry = DaPeerRegistry.fromConfig(config);
    const protocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint: committee.deploymentFingerprint,
      localPeerId: identities[index]!.peerId,
      committeeValidation: committee.validation,
      availabilityCommitmentAuthority,
      store,
    });
    const gossipErrors: unknown[] = [];
    let topicSubscribers: () => readonly string[] = () => [];
    const node = new DaLibp2pNode({
      config,
      registry,
      privateKeySource: keySources[index]!,
      requestHandlers: memberOptions.serveAttestations
        ? createDaLibp2pAttestationRequestHandlers({
            deploymentFingerprint: committee.deploymentFingerprint,
            protocol,
            limits: config.limits,
          })
        : new Map(),
      gossipHandlers: memberOptions.ingestGossip
        ? createDaLibp2pAttestationGossipHandlers({
            deploymentFingerprint: committee.deploymentFingerprint,
            registry,
            protocol,
            committeeValidation: committee.validation,
            store,
          })
        : new Map(),
      onGossipMessageError: (error) => gossipErrors.push(error),
      libp2pFactory: async (libp2pOptions) => {
        const runtime = await createLibp2p(libp2pOptions);
        const pubsub = (runtime.services as unknown as GossipSubscribers)
          .pubsub;
        topicSubscribers = () =>
          pubsub.getSubscribers(topicId).map((peer) => peer.toString());
        return runtime as unknown as DaLibp2pRuntimeNode;
      },
    });
    runningNodes.push(node);
    return {
      member: {
        peerId: identities[index]!.peerId,
        node,
        exchange: new DaLibp2pAttestationExchange({
          deploymentFingerprint: committee.deploymentFingerprint,
          localPeerId: identities[index]!.peerId,
          node,
          registry,
          protocol,
          committeeValidation: committee.validation,
          store,
          requestTimeoutMs: REQUEST_TIMEOUT_MS,
        }),
        gossipErrors,
      },
      topicSubscribers: () => topicSubscribers(),
    };
  });
  for (const { member } of members) {
    await member.node.start();
  }
  await vi.waitFor(
    () => {
      for (const [index, { topicSubscribers }] of members.entries()) {
        const others = identities
          .filter((_, peer) => peer !== index)
          .map((identity) => identity.peerId);
        expect([...topicSubscribers()].sort()).toEqual(others.sort());
      }
    },
    { timeout: 20_000, interval: 100 },
  );
  return members.map(({ member }) => member);
};

type GossipSubscribers = {
  readonly pubsub: {
    getSubscribers(topic: string): readonly { toString(): string }[];
  };
};

/**
 * Two committee services over real libp2p against one stateful fake L1.
 * Member A submits; its first tick attests with its own witness only, which
 * reproduces the live stall. Member B then signs, and member A's second tick
 * must pick up member B's witness from `peerSignatureSource` to finish.
 */
const runCommitteeLifecycle = async ({
  memberA,
  memberB,
  peerSignatureSource,
}: {
  readonly memberA: MemberOptions;
  readonly memberB: MemberOptions;
  readonly peerSignatureSource: "gossip" | "pull";
}): Promise<readonly string[]> => {
  const committee = await makeCommittee(2);
  const stores = await openStores(2);
  const members = await startMembers({
    committee,
    stores,
    options: [memberA, memberB],
  });
  const l1 = statefulL1(committee);
  const dir = await tempDir();
  const services = members.map((member, signerIndex) => {
    const signer = committee.signers[signerIndex]!;
    const store = stores[signerIndex]!;
    const daParams = {
      committeeHex: committee.signers.map((s) => s.publicKeyHex).join(""),
      committeeSignersHash: committee.validation.committeeSignersHash,
      threshold: 2,
    };
    const config = {
      ...minimalConfig({
        dir: `${dir}/member-${signerIndex.toString()}`,
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: committee.signerSeeds[signerIndex]!,
        signerPublicKey: signer.publicKeyHex,
      }),
      signerIndex,
      daParams,
      peerRequestTimeoutMs: REQUEST_TIMEOUT_MS,
    };
    const signerValidation = validateDaSignerMembership({
      daParams,
      signer,
      signerIndex,
    });
    const remote = members
      .map((peer, index) => ({ peerId: peer.peerId, signerIndex: index }))
      .filter((peer) => peer.signerIndex !== signerIndex);
    return new CommitteeService({
      config,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({
            header: committee.header,
            headerHash: committee.headerHash,
            depth: 10,
          }),
        ],
      }),
      payloadSource: payloadSourceFromBytes(
        committee.payloadCbor,
        "producer-peer",
      ),
      signer,
      signerValidation,
      coordinator: new PeerSignatureCoordinator({
        deploymentFingerprint: committee.deploymentFingerprint,
        peers: remote,
        localPeerId: member.peerId,
        signer,
        signerIndex,
        signerValidation,
        availabilityCommitmentAuthority,
        store,
        attestationExchange: member.exchange,
        requestTimeoutMs: REQUEST_TIMEOUT_MS,
        retryInitialDelayMs: 100,
        retryMaxDelayMs: 1_000,
        retryMaxAttempts: 3,
        // Only member A submits to L1.
        onChainCoordinator:
          signerIndex === 0
            ? new OnChainLifecycleCoordinator({
                threshold: 2,
                visibilityRetryCount: 0,
                raceRecoveryRetryCount: 0,
                chainReader: l1.chainReader,
                submitter: l1.submitter,
                peerSignaturesFor: (headerHash) =>
                  store.listDaSignatures(headerHash),
                recordCandidate: (record) =>
                  store.saveDaAttestationCandidate(record),
                recordSubmission: (record) => store.saveL1Submission(record),
              })
            : undefined,
      }),
    });
  });
  const [serviceA, serviceB] = services;
  const [storeA] = stores;
  await serviceA!.initialize();
  await serviceB!.initialize();

  await expect(serviceA!.tick()).resolves.toMatchObject({
    signedHeaders: 1,
    errors: [],
  });
  expect(l1.calls).toEqual(["init", "add:0"]);

  await expect(serviceB!.tick()).resolves.toMatchObject({
    signedHeaders: 1,
    errors: [],
  });
  const peerSignatureStored = async () =>
    (await storeA!.listDaSignatures(committee.headerHash)).some(
      (record) =>
        record.signerIndex === 1 &&
        record.source === "peer" &&
        record.sourcePeer === members[1]!.peerId,
    );
  if (peerSignatureSource === "gossip") {
    await vi.waitFor(
      async () => {
        expect(await peerSignatureStored()).toBe(true);
      },
      { timeout: 10_000, interval: 50 },
    );
  } else {
    // Member A ingests no gossip: only its next tick's pull can supply it.
    expect(await peerSignatureStored()).toBe(false);
  }

  await expect(serviceA!.tick()).resolves.toMatchObject({ errors: [] });
  return l1.calls;
};

/** An L1 whose single DA attestation candidate follows submitted txs. */
const statefulL1 = (committee: Committee) => {
  const calls: string[] = [];
  let candidate: DaAttestationCandidateRecord | undefined;
  const nextOutRef = () => `${bytesToHex(Buffer.alloc(32, calls.length))}#0`;
  const submitted = () => ({
    status: "submitted" as const,
    txHash: bytesToHex(Buffer.alloc(32, calls.length)),
  });
  return {
    calls,
    chainReader: {
      fetchDaAttestationCandidates: async () =>
        candidate === undefined ? [] : [candidate],
    },
    submitter: {
      initAttestation: async () => {
        calls.push("init");
        candidate = {
          deploymentFingerprint: committee.deploymentFingerprint,
          headerHash: committee.headerHash,
          outRef: nextOutRef(),
          datumCbor: "d87980",
          attestationCount: 0,
          threshold: 2,
          committeeSignersHash: committee.validation.committeeSignersHash,
          bitmap: "00".repeat(32),
          observedChainPoint: {},
          status: "initialized",
        };
        return submitted();
      },
      addSignatures: async ({
        candidate: current,
        signerIndexes,
      }: {
        readonly candidate: DaAttestationCandidateRecord;
        readonly signerIndexes: readonly number[];
      }) => {
        calls.push(`add:${signerIndexes.join(",")}`);
        const bitmap = signerIndexes.reduce(
          (bits, signerIndex) => setSignerBit(bits, signerIndex),
          current.bitmap,
        );
        const attestationCount = countSetBits(bitmap);
        candidate = {
          ...current,
          outRef: nextOutRef(),
          bitmap,
          attestationCount,
          status: attestationCount >= 2 ? "threshold" : "signed",
        };
        return submitted();
      },
      applyAttestation: async () => {
        calls.push("apply");
        candidate = undefined;
        return submitted();
      },
    },
  };
};

const signatureRecord = ({
  deploymentFingerprint,
  headerHash,
  header,
  signerIndex,
  signer,
  committeeSignersHash,
  payloadHash,
  commitment,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly header: Header;
  readonly signerIndex: number;
  readonly signer: DaSigner;
  readonly committeeSignersHash: string;
  readonly payloadHash: string;
  readonly commitment: ReturnType<typeof commitmentFor>;
}): DaSignatureRecordV1 => ({
  deploymentFingerprint,
  headerHash,
  signerIndex,
  signatureWitness: signDaAttestation({
    signer,
    signerIndex,
    availabilityCommitment: commitment.commitment,
  }),
  availabilityCommitmentCbor: commitment.commitmentCbor,
  availabilityCommitmentDigest: commitment.commitmentDigest,
  payloadHash,
  committeeSignersHash,
  signedAt: new Date().toISOString(),
  broadcastStatus: "posted",
  source: "local",
  l1ChainPoint: {},
  validation: {
    payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
    rootsMatch: true,
    stateQueueOutRef: "state-queue#0",
    headerHash,
    rootSummary: rootSummaryFromHeader(header),
    countSummary: {
      withdrawalCount: header.withdrawalCount,
      forcedTransactionCount: header.forcedTransactionCount,
      l2TransactionCount: header.l2TransactionCount,
      depositCount: header.depositCount,
      totalEventCount: header.totalEventCount,
      transitionStepCount: header.transitionStepCount,
      validationTraceCount: header.validationTraceCount,
    },
    l1Header: {
      startTime: header.startTime.toString(),
      endTime: header.endTime.toString(),
      operatorVkey: header.operatorVkey,
      prevHeaderHash: header.prevHeaderHash,
      protocolVersion: header.protocolVersion.toString(),
    },
  },
});

const saveVerifiedPayload = async (
  store: JsonFileCommitteeStore,
  {
    deploymentFingerprint,
    headerHash,
    header,
    payloadCbor,
  }: Pick<
    Committee,
    "deploymentFingerprint" | "headerHash" | "header" | "payloadCbor"
  >,
): Promise<void> => {
  await store.upsertStateQueueHeader(
    stateQueueRecord({ deploymentFingerprint, headerHash, header }),
  );
  await store.saveDaPayload({
    deploymentFingerprint,
    headerHash,
    payloadSchemaVersion: 1,
    payloadCborHex: payloadCbor.toString("hex"),
    payloadSha256: computeDaSha256Hash(payloadCbor).toString("hex"),
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
  readonly header: Header;
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

const rootSummaryFromHeader = (header: Header): DaStoredPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

const reserveLoopbackPort = (): Promise<number> =>
  new Promise((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      if (address === null || typeof address === "string") {
        reject(new Error("failed to reserve loopback port"));
        return;
      }
      const port = address.port;
      server.close((error) =>
        error === undefined ? resolve(port) : reject(error),
      );
    });
  });

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);
