import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-sdk";
import "@noble/hashes/blake2.js";
import "vitest";
import "../src/committee-service.js";
import "../src/da/libp2p/attestations.js";
import "../src/da/libp2p/DaGossip.js";
import "../src/da/libp2p/DaPeerRegistry.js";
import "../src/da/libp2p/DaTopics.js";
import "../src/peer/signatures.js";
import "../src/signer.js";
import "../src/store.js";
import "./helpers.js";
import "./conflict-evidence.conflict-fixture.js";

import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  computeDaSha256Hash,
  decodeDaConflictEvidenceCbor,
  decodeDaConflictingSignatureHeaderEvidenceCbor,
  encodeDaConflictEvidenceCbor,
  encodeDaConflictingSignatureHeaderEvidenceCbor,
} from "@al-ft/midgard-core/da-transport";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it, vi } from "vitest";

import { StoreBackedDaAttestationProtocol } from "../src/da/libp2p/attestations.js";
import {
  buildDaSignatureConflictEvidence,
  classifyDaLocalSigningCommitment,
} from "../src/peer/signatures.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
} from "../src/signer.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import {
  availabilityCommitment,
  conflictFixture,
  conflictGossip,
  conflictTopic,
  DEPLOYMENT_FINGERPRINT,
  LOWER_HEADER_HASH,
  REPORTER_PEER_ID,
  SIBLING_HEADER_HASH,
  signatureRecord,
  signedMessage,
  UNKNOWN_PEER_ID,
} from "./conflict-evidence.conflict-fixture.js";
import { tempDir } from "./helpers.js";

describe("DA conflict evidence V1 lifecycle", () => {
  it("persists authenticated conflicting signatures once and survives restart", async () => {
    const fixture = await conflictFixture();
    const directory = await tempDir();
    const store = await JsonFileCommitteeStore.open(directory);
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

    const reopened = await JsonFileCommitteeStore.open(directory);
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
    const store = await JsonFileCommitteeStore.open(await tempDir());
    const gossip = conflictGossip(fixture.registry, store);
    const conflict = decodeDaConflictEvidenceCbor(fixture.encoded);

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
          encodeDaConflictEvidenceCbor({
            ...conflict,
            deploymentFingerprint: Buffer.alloc(32, 0xcd),
          }),
        ),
      ),
    ).rejects.toThrow(/deployment does not match/u);
    await expect(
      gossip.handleInboundMessage(
        signedMessage(
          encodeDaConflictEvidenceCbor({
            ...conflict,
            evidenceHash: Buffer.alloc(32, 0xee),
          }),
        ),
      ),
    ).rejects.toThrow(/hash does not match/u);
    await expect(
      gossip.handleInboundMessage(
        signedMessage(
          encodeDaConflictEvidenceCbor({
            ...conflict,
            headerHash: Buffer.alloc(28, 0x01),
          }),
        ),
      ),
    ).rejects.toThrow(/header does not match/u);

    const equivocation = decodeDaConflictingSignatureHeaderEvidenceCbor(
      conflict.compactEvidence!,
    );
    const forgedCompact = encodeDaConflictingSignatureHeaderEvidenceCbor({
      ...equivocation,
      upperHeaderWitness: Buffer.concat([
        Buffer.from([equivocation.signerIndex]),
        Buffer.alloc(64, 0xff),
      ]),
    });
    await expect(
      gossip.handleInboundMessage(
        signedMessage(
          encodeDaConflictEvidenceCbor({
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
    const expected = availabilityCommitment(headerHash, "99".repeat(28));
    const conflicting = availabilityCommitment(headerHash, "55".repeat(28));
    const store = await JsonFileCommitteeStore.open(await tempDir());
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
    const first = availabilityCommitment(LOWER_HEADER_HASH, "99".repeat(28));
    const second = availabilityCommitment(LOWER_HEADER_HASH, "55".repeat(28));
    const prior = signatureRecord({
      signer,
      commitment: first,
      payloadHash: "66".repeat(32),
      committeeSignersHash: "77".repeat(32),
    });

    expect(
      classifyDaLocalSigningCommitment({
        records: [prior],
        signerIndex: 0,
        expectedCommitmentDigest: second.digest,
      }),
    ).toMatchObject({
      maySign: false,
      conflictingVariants: [prior],
    });
    expect(
      classifyDaLocalSigningCommitment({
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
      availabilityCommitment(LOWER_HEADER_HASH, "99".repeat(28)),
      availabilityCommitment(LOWER_HEADER_HASH, "55".repeat(28)),
    ].map((commitment) =>
      signatureRecord({
        signer,
        commitment,
        payloadHash: "66".repeat(32),
        committeeSignersHash: "77".repeat(32),
      }),
    );
    const first = await JsonFileCommitteeStore.open(directory);
    for (const record of variants) {
      await first.saveDaSignature(record);
    }
    await first.close();

    const reopened = await JsonFileCommitteeStore.open(directory);
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

  // Owner ruling 2026-10-07: equivocation is one signer, one header hash and
  // two availability commitments. Signatures over sibling headers (same
  // parent, different header hashes) are truthful and never conflict.
  it("builds equivocation evidence for one header only, never for sibling headers", async () => {
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const record = (headerHash: string, deploymentIdentity: string) =>
      signatureRecord({
        signer,
        commitment: availabilityCommitment(headerHash, deploymentIdentity),
        payloadHash: "66".repeat(32),
        committeeSignersHash: "77".repeat(32),
      });
    const build = (
      first: ReturnType<typeof record>,
      second: ReturnType<typeof record>,
    ) =>
      buildDaSignatureConflictEvidence({
        first,
        second,
        daVkey: signer.publicKeyHex,
        reporterPeerId: REPORTER_PEER_ID,
        receivedAt: "2026-07-27T00:00:00.000Z",
      });
    const onA = record(LOWER_HEADER_HASH, "99".repeat(28));

    expect(build(onA, record(SIBLING_HEADER_HASH, "99".repeat(28)))).toBe(
      undefined,
    );
    expect(build(onA, onA)).toBe(undefined);
    const sameHeader = build(onA, record(LOWER_HEADER_HASH, "55".repeat(28)));
    expect(sameHeader?.record).toMatchObject({
      evidenceKind: "equivocation",
      headerHash: LOWER_HEADER_HASH,
      conflictingHeaderHash: LOWER_HEADER_HASH,
    });
  });

  it("refuses gossiped sibling-header evidence before storing it, and stores same-header evidence", async () => {
    const fixture = await conflictFixture();
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const sibling = availabilityCommitment(
      SIBLING_HEADER_HASH,
      "99".repeat(28),
    );
    // The shared codec refuses to encode a cross-header pair, so the
    // adversary's bytes are built by rewriting the upper half of a valid
    // same-header tuple: [signer, vkey, lowerHeader, lowerCommitment,
    // lowerWitness, upperHeader, upperCommitment, upperWitness].
    const conflict = decodeDaConflictEvidenceCbor(fixture.encoded);
    const tuple = decodeSingleCbor(conflict.compactEvidence!) as unknown[];
    const siblingCompact = encodeCbor([
      ...tuple.slice(0, 5),
      Buffer.from(SIBLING_HEADER_HASH, "hex"),
      Buffer.from(sibling.cbor, "hex"),
      Buffer.from(
        signDaAttestation({
          signer,
          signerIndex: 0,
          availabilityCommitment: sibling.commitment,
        }),
        "hex",
      ),
    ]);
    const store = await JsonFileCommitteeStore.open(await tempDir());
    try {
      const gossip = conflictGossip(fixture.registry, store);
      await expect(
        gossip.handleInboundMessage(
          signedMessage(
            encodeDaConflictEvidenceCbor({
              ...conflict,
              evidenceHash: computeDaSha256Hash(siblingCompact),
              compactEvidence: siblingCompact,
            }),
          ),
        ),
      ).rejects.toThrow(/evidence must name one header hash/u);
      await expect(store.listDaConflictEvidence()).resolves.toEqual([]);

      await expect(
        gossip.handleInboundMessage(signedMessage(fixture.encoded)),
      ).resolves.toBe(true);
      await expect(store.listDaConflictEvidence()).resolves.toEqual([
        fixture.record,
      ]);
    } finally {
      await store.close();
    }
  });
});
