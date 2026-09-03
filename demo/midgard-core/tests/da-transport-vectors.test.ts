import { describe, expect, it } from "vitest";

import { decodeSingleCbor, encodeCbor } from "../src/codec/cbor.js";
import {
  computeDaSha256Hash,
  DA_GOSSIP_SIGNATURE_LENGTH,
  DA_ON_CHAIN_ATTESTATION_DOMAIN,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
  type DaAttestationGossip,
  type DaAttestationsByHeaderRequest,
  type DaAttestationsByHeaderResponse,
  type DaCapabilitiesRequest,
  type DaCapabilitiesResponse,
  type DaConflictEvidence,
  DaConflictEvidenceKind,
  type DaConflictingSignatureHeaderEvidence,
  type DaEventToStepByEventRequest,
  type DaEventToStepByEventResponse,
  DaGenericFoundStatus,
  DaGossipTopic,
  daGossipTopic,
  DaLocalPayloadStatus,
  type DaMetadataByHeaderResponse,
  DaMetadataStatus,
  type DaPayloadAnnouncement,
  type DaPayloadByHeaderRequest,
  type DaPayloadByHeaderResponse,
  DaPayloadByHeaderStatus,
  type DaPayloadChunkManifest,
  type DaPayloadChunkRequest,
  type DaPayloadChunkResponse,
  DaPayloadSubmitMode,
  type DaPayloadSubmitRequest,
  type DaPayloadSubmitResponse,
  DaPayloadSubmitStatus,
  type DaProofBundleByHeaderRequest,
  type DaProofBundleByHeaderResponse,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  type DaTraceStepByIndexRequest,
  type DaTraceStepByIndexResponse,
  DaTransportSigningDomain,
  decodeDaAttestationGossipCbor,
  decodeDaAttestationsByHeaderRequestCbor,
  decodeDaAttestationsByHeaderResponseCbor,
  decodeDaCapabilitiesRequestCbor,
  decodeDaCapabilitiesResponseCbor,
  decodeDaConflictEvidenceCbor,
  decodeDaConflictingSignatureHeaderEvidenceCbor,
  decodeDaEventToStepByEventRequestCbor,
  decodeDaEventToStepByEventResponseCbor,
  decodeDaMetadataByHeaderResponseCbor,
  decodeDaPayloadAnnouncementCbor,
  decodeDaPayloadByHeaderRequestCbor,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadChunkManifestCbor,
  decodeDaPayloadChunkRequestCbor,
  decodeDaPayloadChunkResponseCbor,
  decodeDaPayloadSubmitRequestCbor,
  decodeDaPayloadSubmitResponseCbor,
  decodeDaProofBundleByHeaderRequestCbor,
  decodeDaProofBundleByHeaderResponseCbor,
  decodeDaTraceStepByIndexRequestCbor,
  decodeDaTraceStepByIndexResponseCbor,
  encodeDaAttestationGossipCbor,
  encodeDaAttestationPreimage,
  encodeDaAttestationsByHeaderRequestCbor,
  encodeDaAttestationsByHeaderResponseCbor,
  encodeDaCapabilitiesRequestCbor,
  encodeDaCapabilitiesResponseCbor,
  encodeDaConflictEvidenceCbor,
  encodeDaConflictingSignatureHeaderEvidenceCbor,
  encodeDaEventToStepByEventRequestCbor,
  encodeDaEventToStepByEventResponseCbor,
  encodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadAnnouncementCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadByHeaderResponseCbor,
  encodeDaPayloadChunkManifestCbor,
  encodeDaPayloadChunkRequestCbor,
  encodeDaPayloadChunkResponseCbor,
  encodeDaPayloadSubmitRequestCbor,
  encodeDaPayloadSubmitResponseCbor,
  encodeDaProofBundleByHeaderRequestCbor,
  encodeDaProofBundleByHeaderResponseCbor,
  encodeDaTraceStepByIndexRequestCbor,
  encodeDaTraceStepByIndexResponseCbor,
} from "../src/da-transport.js";

const h = (byte: string, count: number): string => byte.repeat(count);
const b = (byte: number, count: number): Buffer => Buffer.alloc(count, byte);

const deploymentFingerprint = b(0x01, 32);
const headerHash = b(0x02, 28);
const payloadHash = b(0x03, 32);
const rootSummaryHash = b(0x06, 32);
const proofBundleHash = b(0x07, 32);
const chunkHash = b(0x08, 32);
const signature = b(0x0b, 64);
const chunkManifest = {
  payloadHash,
  totalBytes: 5,
  chunkSize: 2,
  chunkHashes: [b(0x08, 32), b(0x09, 32)],
} satisfies DaPayloadChunkManifest;

describe("DA transport full message vectors", () => {
  it("pins sole V1 protocol IDs, topics, signing domains, and capabilities", () => {
    expect(DA_TRANSPORT_PROTOCOL_VERSION).toBe(1);
    expect(DA_TRANSPORT_LIMITS).toEqual({
      maxPayloadBytes: 67_108_864,
      maxInlineResponseBytes: 1_048_576,
      maxChunkBytes: 1_048_576,
      maxGossipMessageBytes: 65_536,
      maxStreamsPerPeer: 16,
      requestTimeoutMs: 15_000,
      minimumRetentionDays: 15,
    });
    expect(DA_GOSSIP_SIGNATURE_LENGTH).toBe(64);
    expect(DaTransportSigningDomain).toEqual({
      payloadAnnouncement: "MidgardDALibp2pPayloadAnnouncementV1",
      payloadSubmit: "MidgardDALibp2pPayloadSubmitV1",
      conflictEvidence: "MidgardDALibp2pConflictEvidenceV1",
    });
    expect(DaGossipTopic).toEqual({
      payloadAnnouncements: "payload-announcements",
      attestations: "attestations",
      conflicts: "conflicts",
    });
    expect(DaRequestResponseProtocol).toEqual({
      capabilities: "capabilities",
      payloadSubmit: "payload-submit",
      payloadByHeader: "payload-by-header",
      payloadChunk: "payload-chunk",
      metadataByHeader: "metadata-by-header",
      proofBundleByHeader: "proof-bundle-by-header",
      traceStepByIndex: "trace-step-by-index",
      eventToStepByEvent: "event-to-step-by-event",
      attestationsByHeader: "attestations-by-header",
    });

    const fingerprint = h("01", 32);
    expect(
      Object.values(DaGossipTopic).map((topic) =>
        daGossipTopic(deploymentFingerprint, topic),
      ),
    ).toEqual([
      `/midgard/${fingerprint}/da/payload-announcements/1`,
      `/midgard/${fingerprint}/da/attestations/1`,
      `/midgard/${fingerprint}/da/conflicts/1`,
    ]);
    expect(
      Object.values(DaRequestResponseProtocol).map((protocol) =>
        daRequestResponseProtocolId(deploymentFingerprint, protocol),
      ),
    ).toEqual([
      `/midgard/${fingerprint}/da/capabilities/1`,
      `/midgard/${fingerprint}/da/payload-submit/1`,
      `/midgard/${fingerprint}/da/payload-by-header/1`,
      `/midgard/${fingerprint}/da/payload-chunk/1`,
      `/midgard/${fingerprint}/da/metadata-by-header/1`,
      `/midgard/${fingerprint}/da/proof-bundle-by-header/1`,
      `/midgard/${fingerprint}/da/trace-step-by-index/1`,
      `/midgard/${fingerprint}/da/event-to-step-by-event/1`,
      `/midgard/${fingerprint}/da/attestations-by-header/1`,
    ]);

    assertVector(
      "DaCapabilitiesRequestV1",
      encodeDaCapabilitiesRequestCbor,
      decodeDaCapabilitiesRequestCbor,
      { deploymentFingerprint } satisfies DaCapabilitiesRequest,
      `815820${h("01", 32)}`,
    );
    assertVector(
      "DaCapabilitiesResponseV1",
      encodeDaCapabilitiesResponseCbor,
      decodeDaCapabilitiesResponseCbor,
      {
        deploymentFingerprint,
        transportProtocolVersion: 1,
        payloadSchemaVersions: [1],
        envelopeContentEncodings: [0, 1],
        maxPayloadBytes: 67_108_864,
        maxInlineResponseBytes: 1_048_576,
        maxChunkBytes: 1_048_576,
        maxStreamsPerPeer: 16,
        requestTimeoutMs: 15_000,
      } satisfies DaCapabilitiesResponse,
      [
        "89",
        `5820${h("01", 32)}`,
        "01",
        "8101",
        "820001",
        "1a04000000",
        "1a00100000",
        "1a00100000",
        "10",
        "193a98",
      ].join(""),
    );
  });

  it("matches golden vectors for remaining request-response and gossip tuples", () => {
    expect(DaPayloadSubmitMode).toEqual({ inline: 0, chunked: 1 });
    expect(DaPayloadSubmitStatus).toEqual({
      accepted: 0,
      duplicate: 1,
      conflict: 2,
      rejected: 3,
      deferred: 4,
    });
    expect(DaPayloadByHeaderStatus).toEqual({
      found_inline: 0,
      found_chunked: 1,
      not_found: 2,
      conflict: 3,
      rejected: 4,
    });
    expect(DaGenericFoundStatus).toEqual({
      found: 0,
      not_found: 1,
      rejected: 2,
    });
    expect(DaMetadataStatus).toEqual({
      found: 0,
      not_found: 1,
      conflict: 2,
      rejected: 3,
    });
    expect(DaLocalPayloadStatus).toEqual({
      staged: 0,
      verified: 1,
      signed: 2,
      conflict: 3,
    });

    assertVector(
      "DaPayloadAnnouncementV1",
      encodeDaPayloadAnnouncementCbor,
      decodeDaPayloadAnnouncementCbor,
      {
        deploymentFingerprint,
        headerHash,
        payloadHash,
        payloadSchemaVersion: 1,
        payloadBytes: 5,
        chunkSize: 2,
        chunkCount: 3,
        rootSummaryHash,
        announcedByPeerId: "peer-a",
        announcedAtSlot: 42,
        signature,
      } satisfies DaPayloadAnnouncement,
      [
        "8b",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "01",
        "05",
        "02",
        "03",
        `5820${h("06", 32)}`,
        "66706565722d61",
        "182a",
        `5840${h("0b", 64)}`,
      ].join(""),
    );

    assertVector(
      "PayloadChunkManifestV1",
      encodeDaPayloadChunkManifestCbor,
      decodeDaPayloadChunkManifestCbor,
      chunkManifest,
      [
        "84",
        `5820${h("03", 32)}`,
        "05",
        "02",
        "82",
        `5820${h("08", 32)}`,
        `5820${h("09", 32)}`,
      ].join(""),
    );

    assertVector(
      "PayloadSubmitRequestV1",
      encodeDaPayloadSubmitRequestCbor,
      decodeDaPayloadSubmitRequestCbor,
      {
        deploymentFingerprint,
        headerHash,
        payloadHash,
        payloadSchemaVersion: 1,
        mode: "inline",
        payloadBytes: Buffer.from("payload"),
        chunkManifest: null,
      } satisfies DaPayloadSubmitRequest,
      [
        "87",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "01",
        "00",
        "477061796c6f6164",
        "f6",
      ].join(""),
    );

    assertVector(
      "PayloadSubmitResponseV1",
      encodeDaPayloadSubmitResponseCbor,
      decodeDaPayloadSubmitResponseCbor,
      {
        status: "deferred",
        headerHash,
        payloadHash,
        reasonCode: "busy",
        retryAfterMs: 1500,
      } satisfies DaPayloadSubmitResponse,
      [
        "85",
        "04",
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "6462757379",
        "1905dc",
      ].join(""),
    );

    assertVector(
      "PayloadByHeaderResponseV1",
      encodeDaPayloadByHeaderResponseCbor,
      decodeDaPayloadByHeaderResponseCbor,
      {
        status: "found_inline",
        headerHash,
        payloadHash,
        payloadBytes: Buffer.from("payload"),
        chunkManifest: null,
        reasonCode: null,
      } satisfies DaPayloadByHeaderResponse,
      [
        "86",
        "00",
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "477061796c6f6164",
        "f6",
        "f6",
      ].join(""),
    );

    assertVector(
      "PayloadByHeaderRequestV1",
      encodeDaPayloadByHeaderRequestCbor,
      decodeDaPayloadByHeaderRequestCbor,
      {
        deploymentFingerprint,
        headerHash,
        acceptedPayloadHashes: [payloadHash, b(0x0d, 32)],
        maxInlineBytes: 1_048_576,
      } satisfies DaPayloadByHeaderRequest,
      [
        "84",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        "82",
        `5820${h("03", 32)}`,
        `5820${h("0d", 32)}`,
        "1a00100000",
      ].join(""),
    );

    assertVector(
      "MetadataByHeaderResponseV1",
      encodeDaMetadataByHeaderResponseCbor,
      decodeDaMetadataByHeaderResponseCbor,
      {
        status: "found",
        headerHash,
        payloadHash,
        payloadSchemaVersion: 1,
        payloadBytes: 5,
        rootSummaryHash,
        proofBundleHash,
        transitionTraceRoot: b(0x09, 32),
        eventToStepRoot: b(0x0a, 32),
        retainedUntilSlot: 42,
        localStatus: "verified",
      } satisfies DaMetadataByHeaderResponse,
      [
        "8b",
        "00",
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "01",
        "05",
        `5820${h("06", 32)}`,
        `5820${h("07", 32)}`,
        `5820${h("09", 32)}`,
        `5820${h("0a", 32)}`,
        "182a",
        "01",
      ].join(""),
    );

    assertVector(
      "PayloadChunkRequestV1",
      encodeDaPayloadChunkRequestCbor,
      decodeDaPayloadChunkRequestCbor,
      {
        deploymentFingerprint,
        headerHash,
        payloadHash,
        chunkIndex: 2,
      } satisfies DaPayloadChunkRequest,
      [
        "84",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "02",
      ].join(""),
    );

    assertVector(
      "PayloadChunkResponseV1",
      encodeDaPayloadChunkResponseCbor,
      decodeDaPayloadChunkResponseCbor,
      {
        status: "found",
        headerHash,
        payloadHash,
        chunkIndex: 2,
        chunkBytes: Buffer.from("chunk-2"),
        chunkHash,
      } satisfies DaPayloadChunkResponse,
      [
        "86",
        "00",
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "02",
        "476368756e6b2d32",
        `5820${h("08", 32)}`,
      ].join(""),
    );

    assertVector(
      "ProofBundleByHeaderRequestV1",
      encodeDaProofBundleByHeaderRequestCbor,
      decodeDaProofBundleByHeaderRequestCbor,
      {
        deploymentFingerprint,
        headerHash,
        maxInlineBytes: 1_048_576,
      } satisfies DaProofBundleByHeaderRequest,
      ["83", `5820${h("01", 32)}`, `581c${h("02", 28)}`, "1a00100000"].join(""),
    );

    assertVector(
      "ProofBundleByHeaderResponseV1",
      encodeDaProofBundleByHeaderResponseCbor,
      decodeDaProofBundleByHeaderResponseCbor,
      {
        status: "found_inline",
        headerHash,
        proofBundleHash,
        proofBundleBytes: Buffer.from("proof"),
        chunkManifest: null,
        reasonCode: null,
      } satisfies DaProofBundleByHeaderResponse,
      [
        "86",
        "00",
        `581c${h("02", 28)}`,
        `5820${h("07", 32)}`,
        "4570726f6f66",
        "f6",
        "f6",
      ].join(""),
    );

    assertVector(
      "TraceStepByIndexRequestV1",
      encodeDaTraceStepByIndexRequestCbor,
      decodeDaTraceStepByIndexRequestCbor,
      {
        deploymentFingerprint,
        headerHash,
        stepIndex: 7,
      } satisfies DaTraceStepByIndexRequest,
      ["83", `5820${h("01", 32)}`, `581c${h("02", 28)}`, "07"].join(""),
    );

    assertVector(
      "TraceStepByIndexResponseV1",
      encodeDaTraceStepByIndexResponseCbor,
      decodeDaTraceStepByIndexResponseCbor,
      {
        status: "found",
        headerHash,
        stepIndex: 7,
        transitionStepBytes: Buffer.from("step"),
        membershipProofBytes: Buffer.from("membership"),
      } satisfies DaTraceStepByIndexResponse,
      [
        "85",
        "00",
        `581c${h("02", 28)}`,
        "07",
        "4473746570",
        "4a6d656d62657273686970",
      ].join(""),
    );

    assertVector(
      "EventToStepByEventRequestV1",
      encodeDaEventToStepByEventRequestCbor,
      decodeDaEventToStepByEventRequestCbor,
      {
        deploymentFingerprint,
        headerHash,
        eventKey: Buffer.from("event"),
      } satisfies DaEventToStepByEventRequest,
      ["83", `5820${h("01", 32)}`, `581c${h("02", 28)}`, "456576656e74"].join(
        "",
      ),
    );

    assertVector(
      "EventToStepByEventResponseV1",
      encodeDaEventToStepByEventResponseCbor,
      decodeDaEventToStepByEventResponseCbor,
      {
        status: "found",
        headerHash,
        eventKey: Buffer.from("event"),
        eventToStepEntryBytes: Buffer.from("entry"),
        membershipOrNonmembershipProofBytes: Buffer.from("proof"),
      } satisfies DaEventToStepByEventResponse,
      [
        "85",
        "00",
        `581c${h("02", 28)}`,
        "456576656e74",
        "45656e747279",
        "4570726f6f66",
      ].join(""),
    );

    assertVector(
      "AttestationsByHeaderRequestV1",
      encodeDaAttestationsByHeaderRequestCbor,
      decodeDaAttestationsByHeaderRequestCbor,
      {
        deploymentFingerprint,
        headerHash,
        acceptedSignerIndexes: [0, 4],
        maxAttestations: 10,
      } satisfies DaAttestationsByHeaderRequest,
      [
        "84",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        "82",
        "00",
        "04",
        "0a",
      ].join(""),
    );

    assertVector(
      "AttestationsByHeaderResponseV1",
      encodeDaAttestationsByHeaderResponseCbor,
      decodeDaAttestationsByHeaderResponseCbor,
      {
        status: "found",
        headerHash,
        attestations: [
          {
            deploymentFingerprint,
            headerHash,
            payloadHash,
            availabilityCommitmentCbor: Buffer.from([0x80]),
            availabilityCommitmentDigest: b(0x0e, 32),
            signerIndex: 4,
            daVkey: b(0x0c, 32),
            onChainWitness: b(0x0d, 65),
            retentionUntilSlot: 42,
            announcedByPeerId: "peer-a",
          },
        ],
        reasonCode: null,
      } satisfies DaAttestationsByHeaderResponse,
      [
        "84",
        "00",
        `581c${h("02", 28)}`,
        "81",
        "8a",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "4180",
        `5820${h("0e", 32)}`,
        "04",
        `5820${h("0c", 32)}`,
        `5841${h("0d", 65)}`,
        "182a",
        "66706565722d61",
        "f6",
      ].join(""),
    );
  });

  it("freezes hash lengths and V1 helper preimages", () => {
    expect(encodeDaAttestationPreimage(headerHash).toString("hex")).toBe(
      `${Buffer.from(DA_ON_CHAIN_ATTESTATION_DOMAIN, "utf8").toString(
        "hex",
      )}${h("02", 28)}`,
    );
    expect(computeDaSha256Hash(Buffer.from("payload")).toString("hex")).toBe(
      "239f59ed55e737c77147cf55ad0c1b030b6d7ee748a7426952f9b852d5a935e5",
    );
    const equivocation = conflictingSignatureHeaderEvidence();
    assertVector(
      "DaConflictingSignatureHeaderEvidenceV1",
      encodeDaConflictingSignatureHeaderEvidenceCbor,
      decodeDaConflictingSignatureHeaderEvidenceCbor,
      equivocation,
      [
        "88",
        "04",
        `5820${h("0c", 32)}`,
        `581c${h("02", 28)}`,
        "4180",
        `584104${h("aa", 64)}`,
        `581c${h("03", 28)}`,
        "428100",
        `584104${h("bb", 64)}`,
      ].join(""),
    );
    const compactEvidence =
      encodeDaConflictingSignatureHeaderEvidenceCbor(equivocation);
    expect(
      decodeDaConflictEvidenceCbor(
        encodeDaConflictEvidenceCbor({
          deploymentFingerprint,
          headerHash: equivocation.lowerHeaderHash,
          evidenceKind: "equivocation",
          evidenceHash: computeDaSha256Hash(compactEvidence),
          compactEvidence,
        }),
      ),
    ).toEqual({
      deploymentFingerprint,
      headerHash: equivocation.lowerHeaderHash,
      evidenceKind: "equivocation",
      evidenceHash: computeDaSha256Hash(compactEvidence),
      compactEvidence,
    });
    expect(() =>
      encodeDaPayloadChunkRequestCbor({
        deploymentFingerprint: b(0x01, 31),
        headerHash,
        payloadHash,
        chunkIndex: 0,
      }),
    ).toThrow(/deployment_fingerprint must be 32 bytes/);
    expect(() =>
      encodeDaPayloadChunkRequestCbor({
        deploymentFingerprint,
        headerHash: b(0x02, 27),
        payloadHash,
        chunkIndex: 0,
      }),
    ).toThrow(/header_hash must be 28 bytes/);
    expect(() =>
      encodeDaPayloadChunkRequestCbor({
        deploymentFingerprint,
        headerHash,
        payloadHash: b(0x03, 31),
        chunkIndex: 0,
      }),
    ).toThrow(/payload_hash must be 32 bytes/);
    expect(() =>
      encodeDaAttestationGossipCbor({
        deploymentFingerprint,
        headerHash,
        payloadHash,
        availabilityCommitmentCbor: Buffer.from([0x80]),
        availabilityCommitmentDigest: b(0x0e, 32),
        signerIndex: 256,
        daVkey: b(0x0c, 32),
        onChainWitness: b(0x0d, 65),
        retentionUntilSlot: 42,
        announcedByPeerId: "peer-a",
      }),
    ).toThrow(/signer_index must be a uint8/);
  });

  it("rejects adjacent versions, languages, widths, and tuple arities", () => {
    expect(() =>
      daGossipTopic(deploymentFingerprint, "unknown" as never),
    ).toThrow(/gossip_topic is not supported by DA transport V1/u);
    expect(() =>
      daRequestResponseProtocolId(deploymentFingerprint, "unknown" as never),
    ).toThrow(/request_response_protocol is not supported by DA transport V1/u);

    const announcement = {
      deploymentFingerprint,
      headerHash,
      payloadHash,
      payloadSchemaVersion: 1,
      payloadBytes: 5,
      chunkSize: 2,
      chunkCount: 3,
      rootSummaryHash,
      announcedByPeerId: "peer-a",
      announcedAtSlot: 42,
      signature,
    } satisfies DaPayloadAnnouncement;
    expect(() =>
      encodeDaPayloadAnnouncementCbor({
        ...announcement,
        payloadSchemaVersion: 2,
      } as never),
    ).toThrow(/payload_schema_version must equal 1/u);
    expect(() =>
      encodeDaPayloadAnnouncementCbor({
        ...announcement,
        announcedByPeerId: "",
      }),
    ).toThrow(/announced_by_peer_id must be a non-empty string/u);
    expect(() =>
      encodeDaPayloadAnnouncementCbor({
        ...announcement,
        signature: b(0x0b, DA_GOSSIP_SIGNATURE_LENGTH - 1),
      }),
    ).toThrow(/signature must be 64 bytes/u);
    expect(() =>
      decodeDaPayloadAnnouncementCbor(
        replaceTupleItem(encodeDaPayloadAnnouncementCbor(announcement), 3, 2n),
      ),
    ).toThrow(/payload_schema_version must equal 1/u);
    expect(() =>
      decodeDaPayloadAnnouncementCbor(
        replaceTupleItem(encodeDaPayloadAnnouncementCbor(announcement), 8, ""),
      ),
    ).toThrow(/announced_by_peer_id must be a non-empty string/u);
    expect(() =>
      decodeDaPayloadAnnouncementCbor(
        replaceTupleItem(
          encodeDaPayloadAnnouncementCbor(announcement),
          10,
          b(0x0b, DA_GOSSIP_SIGNATURE_LENGTH - 1),
        ),
      ),
    ).toThrow(/signature must be 64 bytes/u);

    const submitRequest = {
      deploymentFingerprint,
      headerHash,
      payloadHash,
      payloadSchemaVersion: 1,
      mode: "chunked",
      payloadBytes: null,
      chunkManifest,
    } satisfies DaPayloadSubmitRequest;
    expect(() =>
      encodeDaPayloadSubmitRequestCbor({
        ...submitRequest,
        payloadSchemaVersion: 2,
      } as never),
    ).toThrow(/payload_schema_version must equal 1/u);
    expect(() =>
      encodeDaPayloadSubmitRequestCbor({
        ...submitRequest,
        mode: "unknown",
      } as never),
    ).toThrow(/mode has unsupported enum label/u);
    expect(() =>
      decodeDaPayloadSubmitRequestCbor(
        replaceTupleItem(
          encodeDaPayloadSubmitRequestCbor(submitRequest),
          4,
          2n,
        ),
      ),
    ).toThrow(/mode has unsupported enum code/u);

    const capabilities = {
      deploymentFingerprint,
      transportProtocolVersion: 1,
      payloadSchemaVersions: [1],
      envelopeContentEncodings: [0, 1],
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
      maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      requestTimeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    } satisfies DaCapabilitiesResponse;
    expect(() =>
      encodeDaCapabilitiesResponseCbor({
        ...capabilities,
        transportProtocolVersion: 2,
      } as never),
    ).toThrow(/transport_protocol_version must equal 1/u);
    for (const payloadSchemaVersions of [[2], [1, 2]]) {
      expect(() =>
        encodeDaCapabilitiesResponseCbor({
          ...capabilities,
          payloadSchemaVersions,
        } as never),
      ).toThrow(/must contain exactly DA payload schema V1/u);
    }
    for (const envelopeContentEncodings of [[], [0, 2], [1, 0]]) {
      expect(() =>
        encodeDaCapabilitiesResponseCbor({
          ...capabilities,
          envelopeContentEncodings,
        }),
      ).toThrow(/strictly increasing|only DA envelope V1 content encodings/u);
    }
    const encodedCapabilities = encodeDaCapabilitiesResponseCbor(capabilities);
    expect(() =>
      decodeDaCapabilitiesResponseCbor(
        replaceTupleItem(encodedCapabilities, 1, 2n),
      ),
    ).toThrow(/transport_protocol_version must equal 1/u);
    expect(() =>
      decodeDaCapabilitiesResponseCbor(
        replaceTupleItem(encodedCapabilities, 2, [2n]),
      ),
    ).toThrow(/must contain exactly DA payload schema V1/u);
    expect(() =>
      decodeDaCapabilitiesResponseCbor(
        replaceTupleItem(encodedCapabilities, 3, [0n, 2n]),
      ),
    ).toThrow(/only DA envelope V1 content encodings/u);

    expect(() =>
      encodeDaPayloadChunkManifestCbor({
        ...chunkManifest,
        chunkHashes: [b(0x08, 31)],
      }),
    ).toThrow(/chunk_manifest\.chunk_hashes\[0\] must be 32 bytes/u);
    expect(() =>
      encodeDaPayloadChunkRequestCbor({
        deploymentFingerprint,
        headerHash,
        payloadHash,
        chunkIndex: -1,
      }),
    ).toThrow(/chunk_index must be an unsigned integer/u);
    expect(() =>
      encodeDaTraceStepByIndexRequestCbor({
        deploymentFingerprint,
        headerHash,
        stepIndex: -1,
      }),
    ).toThrow(/step_index must be an unsigned integer/u);
    expect(() =>
      encodeDaProofBundleByHeaderResponseCbor({
        status: "found_inline",
        headerHash,
        proofBundleHash: b(0x07, 31),
        proofBundleBytes: Buffer.from("proof"),
        chunkManifest: null,
        reasonCode: null,
      }),
    ).toThrow(/proof_bundle_hash must be 32 bytes/u);

    const attestation = {
      deploymentFingerprint,
      headerHash,
      payloadHash,
      availabilityCommitmentCbor: Buffer.from([0x80]),
      availabilityCommitmentDigest: b(0x0e, 32),
      signerIndex: 4,
      daVkey: b(0x0c, 32),
      onChainWitness: b(0x0d, 65),
      retentionUntilSlot: 42,
      announcedByPeerId: "peer-a",
    } satisfies DaAttestationGossip;
    expect(() =>
      encodeDaAttestationGossipCbor({
        ...attestation,
        announcedByPeerId: "",
      }),
    ).toThrow(/announced_by_peer_id must be a non-empty string/u);
    expect(() =>
      decodeDaAttestationGossipCbor(
        replaceTupleItem(encodeDaAttestationGossipCbor(attestation), 9, ""),
      ),
    ).toThrow(/announced_by_peer_id must be a non-empty string/u);
    expect(() =>
      encodeDaAttestationGossipCbor({
        ...attestation,
        onChainWitness: b(0x0d, 64),
      }),
    ).toThrow(/on_chain_witness must be 65 bytes/u);

    const attestationsRequest = {
      deploymentFingerprint,
      headerHash,
      acceptedSignerIndexes: [0, 4],
      maxAttestations: 10,
    } satisfies DaAttestationsByHeaderRequest;
    for (const acceptedSignerIndexes of [
      [0, 0],
      [4, 0],
      [0, 256],
    ]) {
      expect(() =>
        encodeDaAttestationsByHeaderRequestCbor({
          ...attestationsRequest,
          acceptedSignerIndexes,
        }),
      ).toThrow(/strictly increasing|must be a uint8/u);
    }
    expect(() =>
      decodeDaAttestationsByHeaderRequestCbor(
        replaceTupleItem(
          encodeDaAttestationsByHeaderRequestCbor(attestationsRequest),
          2,
          [4n, 0n],
        ),
      ),
    ).toThrow(/accepted_signer_indexes must be strictly increasing/u);

    const conflictEvidence = {
      deploymentFingerprint,
      headerHash,
      evidenceKind: "equivocation",
      evidenceHash: b(0x0e, 32),
      compactEvidence: Buffer.from("0f10", "hex"),
    } satisfies DaConflictEvidence;
    expect(() =>
      encodeDaConflictEvidenceCbor({
        ...conflictEvidence,
        evidenceKind: "unknown",
      } as never),
    ).toThrow(/evidence_kind has unsupported enum label/u);
    expect(() =>
      decodeDaConflictEvidenceCbor(
        replaceTupleItem(
          encodeDaConflictEvidenceCbor(conflictEvidence),
          2,
          BigInt(Object.keys(DaConflictEvidenceKind).length),
        ),
      ),
    ).toThrow(/evidence_kind has unsupported enum code/u);
    expect(() =>
      encodeDaConflictEvidenceCbor({
        ...conflictEvidence,
        evidenceHash: b(0x0e, 31),
      }),
    ).toThrow(/evidence_hash must be 32 bytes/u);
    expect(() =>
      encodeDaConflictingSignatureHeaderEvidenceCbor({
        ...conflictingSignatureHeaderEvidence(),
        upperHeaderHash: headerHash,
        upperCommitmentCbor:
          conflictingSignatureHeaderEvidence().lowerCommitmentCbor,
      }),
    ).toThrow(/identities must be strictly ordered/u);
    expect(() =>
      encodeDaConflictingSignatureHeaderEvidenceCbor({
        ...conflictingSignatureHeaderEvidence(),
        upperHeaderWitness: Buffer.concat([Buffer.from([3]), b(0xbb, 64)]),
      }),
    ).toThrow(/witnesses must embed signer_index/u);
    expect(() =>
      decodeDaConflictingSignatureHeaderEvidenceCbor(
        withoutLastTupleItem(
          encodeDaConflictingSignatureHeaderEvidenceCbor(
            conflictingSignatureHeaderEvidence(),
          ),
        ),
      ),
    ).toThrow(/must have exactly 8 elements/u);

    const unknownStatusCases: readonly {
      readonly encoded: Buffer;
      readonly statusIndex: number;
      readonly unknownCode: bigint;
      readonly decode: (bytes: Uint8Array) => unknown;
    }[] = [
      {
        encoded: encodeDaPayloadSubmitResponseCbor({
          status: "accepted",
          headerHash,
          payloadHash,
          reasonCode: null,
          retryAfterMs: null,
        }),
        statusIndex: 0,
        unknownCode: 5n,
        decode: decodeDaPayloadSubmitResponseCbor,
      },
      {
        encoded: encodeDaPayloadByHeaderResponseCbor({
          status: "not_found",
          headerHash,
          payloadHash: null,
          payloadBytes: null,
          chunkManifest: null,
          reasonCode: null,
        }),
        statusIndex: 0,
        unknownCode: 5n,
        decode: decodeDaPayloadByHeaderResponseCbor,
      },
      {
        encoded: encodeDaPayloadChunkResponseCbor({
          status: "not_found",
          headerHash,
          payloadHash,
          chunkIndex: 0,
          chunkBytes: null,
          chunkHash: null,
        }),
        statusIndex: 0,
        unknownCode: 3n,
        decode: decodeDaPayloadChunkResponseCbor,
      },
      {
        encoded: encodeDaMetadataByHeaderResponseCbor({
          status: "not_found",
          headerHash,
          payloadHash: null,
          payloadSchemaVersion: null,
          payloadBytes: null,
          rootSummaryHash: null,
          proofBundleHash: null,
          transitionTraceRoot: null,
          eventToStepRoot: null,
          retainedUntilSlot: null,
          localStatus: null,
        }),
        statusIndex: 0,
        unknownCode: 4n,
        decode: decodeDaMetadataByHeaderResponseCbor,
      },
      {
        encoded: encodeDaProofBundleByHeaderResponseCbor({
          status: "not_found",
          headerHash,
          proofBundleHash: null,
          proofBundleBytes: null,
          chunkManifest: null,
          reasonCode: null,
        }),
        statusIndex: 0,
        unknownCode: 4n,
        decode: decodeDaProofBundleByHeaderResponseCbor,
      },
      {
        encoded: encodeDaTraceStepByIndexResponseCbor({
          status: "not_found",
          headerHash,
          stepIndex: 0,
          transitionStepBytes: null,
          membershipProofBytes: null,
        }),
        statusIndex: 0,
        unknownCode: 3n,
        decode: decodeDaTraceStepByIndexResponseCbor,
      },
      {
        encoded: encodeDaEventToStepByEventResponseCbor({
          status: "not_found",
          headerHash,
          eventKey: Buffer.from("event"),
          eventToStepEntryBytes: null,
          membershipOrNonmembershipProofBytes: null,
        }),
        statusIndex: 0,
        unknownCode: 3n,
        decode: decodeDaEventToStepByEventResponseCbor,
      },
      {
        encoded: encodeDaAttestationsByHeaderResponseCbor({
          status: "not_found",
          headerHash,
          attestations: [],
          reasonCode: null,
        }),
        statusIndex: 0,
        unknownCode: 3n,
        decode: decodeDaAttestationsByHeaderResponseCbor,
      },
    ];
    for (const unknownStatusCase of unknownStatusCases) {
      expect(() =>
        unknownStatusCase.decode(
          replaceTupleItem(
            unknownStatusCase.encoded,
            unknownStatusCase.statusIndex,
            unknownStatusCase.unknownCode,
          ),
        ),
      ).toThrow(/status has unsupported enum code/u);
    }

    const tupleCases: readonly {
      readonly label: string;
      readonly encoded: Buffer;
      readonly arity: number;
      readonly decode: (bytes: Uint8Array) => unknown;
    }[] = [
      {
        label: "announcement",
        encoded: encodeDaPayloadAnnouncementCbor(announcement),
        arity: 11,
        decode: decodeDaPayloadAnnouncementCbor,
      },
      {
        label: "submit request",
        encoded: encodeDaPayloadSubmitRequestCbor(submitRequest),
        arity: 7,
        decode: decodeDaPayloadSubmitRequestCbor,
      },
      {
        label: "submit response",
        encoded: encodeDaPayloadSubmitResponseCbor({
          status: "deferred",
          headerHash,
          payloadHash,
          reasonCode: "busy",
          retryAfterMs: 1500,
        }),
        arity: 5,
        decode: decodeDaPayloadSubmitResponseCbor,
      },
      {
        label: "capabilities request",
        encoded: encodeDaCapabilitiesRequestCbor({
          deploymentFingerprint,
        }),
        arity: 1,
        decode: decodeDaCapabilitiesRequestCbor,
      },
      {
        label: "capabilities response",
        encoded: encodeDaCapabilitiesResponseCbor(capabilities),
        arity: 9,
        decode: decodeDaCapabilitiesResponseCbor,
      },
      {
        label: "payload-by-header request",
        encoded: encodeDaPayloadByHeaderRequestCbor({
          deploymentFingerprint,
          headerHash,
          acceptedPayloadHashes: [payloadHash],
          maxInlineBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        }),
        arity: 4,
        decode: decodeDaPayloadByHeaderRequestCbor,
      },
      {
        label: "payload-by-header response",
        encoded: encodeDaPayloadByHeaderResponseCbor({
          status: "not_found",
          headerHash,
          payloadHash: null,
          payloadBytes: null,
          chunkManifest: null,
          reasonCode: "missing",
        }),
        arity: 6,
        decode: decodeDaPayloadByHeaderResponseCbor,
      },
      {
        label: "chunk manifest",
        encoded: encodeDaPayloadChunkManifestCbor(chunkManifest),
        arity: 4,
        decode: decodeDaPayloadChunkManifestCbor,
      },
      {
        label: "chunk request",
        encoded: encodeDaPayloadChunkRequestCbor({
          deploymentFingerprint,
          headerHash,
          payloadHash,
          chunkIndex: 2,
        }),
        arity: 4,
        decode: decodeDaPayloadChunkRequestCbor,
      },
      {
        label: "chunk response",
        encoded: encodeDaPayloadChunkResponseCbor({
          status: "found",
          headerHash,
          payloadHash,
          chunkIndex: 2,
          chunkBytes: Buffer.from("chunk-2"),
          chunkHash,
        }),
        arity: 6,
        decode: decodeDaPayloadChunkResponseCbor,
      },
      {
        label: "metadata response",
        encoded: encodeDaMetadataByHeaderResponseCbor({
          status: "not_found",
          headerHash,
          payloadHash: null,
          payloadSchemaVersion: null,
          payloadBytes: null,
          rootSummaryHash: null,
          proofBundleHash: null,
          transitionTraceRoot: null,
          eventToStepRoot: null,
          retainedUntilSlot: null,
          localStatus: null,
        }),
        arity: 11,
        decode: decodeDaMetadataByHeaderResponseCbor,
      },
      {
        label: "proof-bundle request",
        encoded: encodeDaProofBundleByHeaderRequestCbor({
          deploymentFingerprint,
          headerHash,
          maxInlineBytes: 4096,
        }),
        arity: 3,
        decode: decodeDaProofBundleByHeaderRequestCbor,
      },
      {
        label: "proof-bundle response",
        encoded: encodeDaProofBundleByHeaderResponseCbor({
          status: "found_inline",
          headerHash,
          proofBundleHash,
          proofBundleBytes: Buffer.from("proof"),
          chunkManifest: null,
          reasonCode: null,
        }),
        arity: 6,
        decode: decodeDaProofBundleByHeaderResponseCbor,
      },
      {
        label: "trace-step request",
        encoded: encodeDaTraceStepByIndexRequestCbor({
          deploymentFingerprint,
          headerHash,
          stepIndex: 7,
        }),
        arity: 3,
        decode: decodeDaTraceStepByIndexRequestCbor,
      },
      {
        label: "trace-step response",
        encoded: encodeDaTraceStepByIndexResponseCbor({
          status: "found",
          headerHash,
          stepIndex: 7,
          transitionStepBytes: Buffer.from("step"),
          membershipProofBytes: Buffer.from("proof"),
        }),
        arity: 5,
        decode: decodeDaTraceStepByIndexResponseCbor,
      },
      {
        label: "event-to-step request",
        encoded: encodeDaEventToStepByEventRequestCbor({
          deploymentFingerprint,
          headerHash,
          eventKey: Buffer.from("event"),
        }),
        arity: 3,
        decode: decodeDaEventToStepByEventRequestCbor,
      },
      {
        label: "event-to-step response",
        encoded: encodeDaEventToStepByEventResponseCbor({
          status: "found",
          headerHash,
          eventKey: Buffer.from("event"),
          eventToStepEntryBytes: Buffer.from("entry"),
          membershipOrNonmembershipProofBytes: Buffer.from("proof"),
        }),
        arity: 5,
        decode: decodeDaEventToStepByEventResponseCbor,
      },
      {
        label: "attestation gossip",
        encoded: encodeDaAttestationGossipCbor(attestation),
        arity: 10,
        decode: decodeDaAttestationGossipCbor,
      },
      {
        label: "attestations request",
        encoded: encodeDaAttestationsByHeaderRequestCbor(attestationsRequest),
        arity: 4,
        decode: decodeDaAttestationsByHeaderRequestCbor,
      },
      {
        label: "attestations response",
        encoded: encodeDaAttestationsByHeaderResponseCbor({
          status: "found",
          headerHash,
          attestations: [attestation],
          reasonCode: null,
        }),
        arity: 4,
        decode: decodeDaAttestationsByHeaderResponseCbor,
      },
      {
        label: "conflict evidence",
        encoded: encodeDaConflictEvidenceCbor(conflictEvidence),
        arity: 5,
        decode: decodeDaConflictEvidenceCbor,
      },
    ];
    for (const tupleCase of tupleCases) {
      expect(
        () => tupleCase.decode(withoutLastTupleItem(tupleCase.encoded)),
        tupleCase.label,
      ).toThrow(
        new RegExp(`must have exactly ${tupleCase.arity.toString()} elements`),
      );
    }
  });
});

const assertVector = <T>(
  label: string,
  encode: (value: T) => Buffer,
  decode: (bytes: Uint8Array) => T,
  value: T,
  hex: string,
): void => {
  const encoded = encode(value);
  expect(encoded.toString("hex"), label).toBe(hex);
  expect(decode(encoded)).toEqual(value);
};

const conflictingSignatureHeaderEvidence =
  (): DaConflictingSignatureHeaderEvidence => ({
    signerIndex: 4,
    daVkey: b(0x0c, 32),
    lowerHeaderHash: headerHash,
    lowerCommitmentCbor: Buffer.from([0x80]),
    lowerHeaderWitness: Buffer.concat([Buffer.from([4]), b(0xaa, 64)]),
    upperHeaderHash: b(0x03, 28),
    upperCommitmentCbor: Buffer.from([0x81, 0x00]),
    upperHeaderWitness: Buffer.concat([Buffer.from([4]), b(0xbb, 64)]),
  });

const withoutLastTupleItem = (encoded: Uint8Array): Buffer => {
  const decoded = decodeSingleCbor(encoded);
  if (!Array.isArray(decoded)) {
    throw new Error("expected a CBOR tuple");
  }
  return encodeCbor(decoded.slice(0, -1));
};

const replaceTupleItem = (
  encoded: Uint8Array,
  index: number,
  value: unknown,
): Buffer => {
  const decoded = decodeSingleCbor(encoded);
  if (!Array.isArray(decoded)) {
    throw new Error("expected a CBOR tuple");
  }
  const replaced = [...decoded];
  replaced[index] = value;
  return encodeCbor(replaced);
};
