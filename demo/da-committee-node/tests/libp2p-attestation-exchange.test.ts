import "node:net";
import "@al-ft/midgard-core/da-libp2p-identity";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-sdk";
import "@noble/hashes/blake2.js";
import "libp2p";
import "vitest";
import "../src/committee-service.js";
import "../src/coordinator/on-chain.js";
import "../src/coordinator/witnesses.js";
import "../src/da/libp2p/attestations.js";
import "../src/da/libp2p/DaLibp2pNode.js";
import "../src/da/libp2p/DaPeerRegistry.js";
import "../src/peer/coordinator.js";
import "../src/peer/signatures.js";
import "../src/signer.js";
import "../src/store.js";
import "../src/utils/hex.js";
import "./helpers.js";
import "./libp2p-attestation-exchange.state-queue-record.js";
import "./libp2p-attestation-exchange.da-attestation-exchange-over-real-libp2p.js";

import {
  computeDaSha256Hash,
  encodeDaAttestationsByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  type DaAttestationExchange,
  daAttestationGossipFromRecord,
  DaLibp2pAttestationExchange,
  StoreBackedDaAttestationProtocol,
} from "../src/da/libp2p/attestations.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import type { DaSignatureRecordV1 } from "../src/domain.js";
import { PeerSignaturePoller } from "../src/peer/poller.js";
import { validateDaSignatureRecord } from "../src/peer/signatures.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
} from "../src/signer.js";
import { type PostgresCommitteeStore } from "../src/store/postgres.js";
import { bytesToHex } from "../src/utils/hex.js";
import { makePayloadFixture } from "./helpers.js";
import {
  openTestCommitteeStore,
  saveHealthyL1SourceState,
} from "./helpers/committee-store.js";
import {
  availabilityCommitmentAuthority,
  commitmentFor,
  saveVerifiedPayload,
  signatureRecord,
} from "./peer-coordinator.signature-record.js";

const deploymentFingerprint = "11".repeat(32);

/** A two-member committee, threshold two, attesting one fixture block. */
const twoMemberCommittee = async () => {
  const signers = [
    await loadDaSigner(`hex:${"00".repeat(31)}50`),
    await loadDaSigner(`hex:${"00".repeat(31)}51`),
  ];
  const committeeHex = signers.map((signer) => signer.publicKeyHex).join("");
  const committeeSignersHash = bytesToHex(
    blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
  );
  const committeeValidation = validateDaCommittee({
    daParams: { committeeHex, committeeSignersHash, threshold: 2 },
  });
  const { header, headerHash, payloadCbor } = await makePayloadFixture();
  const payloadHash = computeDaSha256Hash(payloadCbor).toString("hex");
  const commitment = commitmentFor(headerHash, payloadCbor.toString("hex"));
  const signed = (signerIndex: number): DaSignatureRecordV1 =>
    signatureRecord({
      deploymentFingerprint,
      headerHash,
      signerIndex,
      committeeSignersHash,
      payloadHash,
      commitment,
      signatureWitness: signDaAttestation({
        signer: signers[signerIndex]!,
        signerIndex,
        availabilityCommitment: commitment.commitment,
      }),
    });
  const verify = (store: PostgresCommitteeStore) =>
    saveVerifiedPayload(store, {
      deploymentFingerprint,
      headerHash,
      payloadHash,
      payloadCbor,
      header,
    });
  const verifiedStore = async () => {
    const store = await saveHealthyL1SourceState(
      await openTestCommitteeStore(),
    );
    await verify(store);
    return store;
  };
  return {
    signers,
    committeeValidation,
    headerHash,
    signed,
    verify,
    verifiedStore,
  };
};

type Committee = Awaited<ReturnType<typeof twoMemberCommittee>>;

/**
 * The local member's pull exchange, wired to `respond` as the serving peer's
 * attestations-by-header handler over the typed request/response codec.
 */
const pullExchange = (
  committee: Committee,
  localStore: PostgresCommitteeStore,
  respond: (requestCbor: Uint8Array) => Promise<Buffer>,
) =>
  new DaLibp2pAttestationExchange({
    deploymentFingerprint,
    localPeerId: "local-peer",
    node: {
      request: async ({ payload }) => respond(payload),
      publishGossip: async () => undefined,
    },
    registry: new DaPeerRegistry([
      {
        peerId: "serving-peer",
        signerIndex: 1,
        daVkey: committee.signers[1]!.publicKeyHex,
        roles: ["committee"],
        multiaddrs: ["/dns4/serving.example/tcp/4001/p2p/serving-peer"],
        bootstrap: false,
      },
    ]),
    protocol: new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "local-peer",
      committeeValidation: committee.committeeValidation,
      availabilityCommitmentAuthority,
      store: localStore,
    }),
    committeeValidation: committee.committeeValidation,
    store: localStore,
    requestTimeoutMs: 1000,
  });

const pollerFor = (
  committee: Committee,
  store: PostgresCommitteeStore,
  attestationExchange: DaAttestationExchange,
) =>
  new PeerSignaturePoller({
    deploymentFingerprint,
    peers: [{ peerId: "serving-peer", signerIndex: 1 }],
    localPeerId: "local-peer",
    attestationExchange,
    signerValidation: committee.committeeValidation,
    availabilityCommitmentAuthority,
    store,
  });

describe("pulled attestations are judged one item at a time", () => {
  it("keeps a valid signer's attestation pulled next to an undecodable one", async () => {
    const committee = await twoMemberCommittee();
    const localStore = await committee.verifiedStore();
    const gossip = (record: DaSignatureRecordV1) =>
      daAttestationGossipFromRecord({
        record,
        daVkey: committee.signers[record.signerIndex]!.publicKeyHex,
        announcedByPeerId: "serving-peer",
      });
    const undecodable = {
      ...gossip(committee.signed(0)),
      availabilityCommitmentCbor: Buffer.from("deadbeef", "hex"),
      availabilityCommitmentDigest: computeDaSha256Hash(
        Buffer.from("deadbeef", "hex"),
      ),
    };
    const exchange = pullExchange(committee, localStore, async () =>
      encodeDaAttestationsByHeaderResponseCbor({
        status: "found",
        headerHash: Buffer.from(committee.headerHash, "hex"),
        attestations: [undecodable, gossip(committee.signed(1))],
        reasonCode: null,
      }),
    );

    await pollerFor(committee, localStore, exchange).pollPeerSignatures(
      committee.headerHash,
    );

    // The undecodable item is refused and never stored; its neighbour is.
    await expect(
      localStore.listDaSignatures(committee.headerHash),
    ).resolves.toMatchObject([{ signerIndex: 1, sourcePeer: "serving-peer" }]);
    await expect(localStore.listPeerHealth()).resolves.toMatchObject([
      { peerId: "serving-peer", consecutiveFailures: 0 },
    ]);
    expect(
      validateDaSignatureRecord({
        body: {
          ...committee.signed(0),
          availabilityCommitmentCbor: "deadbeef",
        },
        headerHash: committee.headerHash,
        deploymentFingerprint,
        signerValidation: committee.committeeValidation,
      }),
    ).toBe("invalid signature record");
  });
});

describe("peer signature polling before the local payload is verified", () => {
  const servingOnly = (
    records: () => readonly DaSignatureRecordV1[],
  ): DaAttestationExchange => ({
    publishAttestation: async () => ({ status: "accepted" }),
    attestationsByHeader: async () => records(),
    publishConflictEvidence: async () => undefined,
  });

  it("blames no peer while the payload is missing, then ingests once it is verified", async () => {
    const committee = await twoMemberCommittee();
    const store = await saveHealthyL1SourceState(
      await openTestCommitteeStore(),
    );
    // What a failed payload fetch leaves behind: a record with no bytes.
    await store.saveDaPayload({
      deploymentFingerprint,
      headerHash: committee.headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: "",
      payloadSha256: "",
      sourcePeerId: "",
      fetchedAt: new Date().toISOString(),
      payloadFetchStatus: "missing_da",
      validationStatus: "missing_da",
      validationError: "no candidate served the payload",
    });
    const poller = pollerFor(
      committee,
      store,
      servingOnly(() => [committee.signed(1)]),
    );

    await poller.pollPeerSignatures(committee.headerHash);
    await expect(store.listPeerHealth()).resolves.toEqual([]);
    await expect(store.listDaSignatures(committee.headerHash)).resolves.toEqual(
      [],
    );

    await committee.verify(store);
    await poller.pollPeerSignatures(committee.headerHash);
    await poller.pollPeerSignatures(committee.headerHash);
    await expect(
      store.listDaSignatures(committee.headerHash),
    ).resolves.toMatchObject([{ signerIndex: 1, sourcePeer: "serving-peer" }]);
    await expect(store.listPeerHealth()).resolves.toMatchObject([
      { peerId: "serving-peer", consecutiveFailures: 0 },
    ]);
  });

  it("still blames a failing peer and still refuses a forged signature", async () => {
    const committee = await twoMemberCommittee();
    const store = await committee.verifiedStore();
    const failing: DaAttestationExchange = {
      ...servingOnly(() => []),
      attestationsByHeader: async () => {
        throw new Error("peer stream reset");
      },
    };

    await pollerFor(committee, store, failing).pollPeerSignatures(
      committee.headerHash,
    );
    await expect(store.listPeerHealth()).resolves.toMatchObject([
      {
        peerId: "serving-peer",
        consecutiveFailures: 1,
        lastError: "peer stream reset",
      },
    ]);

    // Signer 1's slot carrying signer 0's signature does not verify.
    const genuine = committee.signed(1);
    const forged: DaSignatureRecordV1 = {
      ...genuine,
      signatureWitness: `01${committee.signed(0).signatureWitness.slice(2)}`,
    };
    expect(
      validateDaSignatureRecord({
        body: forged,
        headerHash: committee.headerHash,
        deploymentFingerprint,
        signerValidation: committee.committeeValidation,
      }),
    ).toBe("signature witness verification failed");
    await pollerFor(
      committee,
      store,
      servingOnly(() => [forged]),
    ).pollPeerSignatures(committee.headerHash);
    await expect(store.listDaSignatures(committee.headerHash)).resolves.toEqual(
      [],
    );
  });
});
