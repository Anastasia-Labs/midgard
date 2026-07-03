import { describe, expect, it } from "vitest";

import { encodeCborMapRaw, encodeCborUnsigned } from "../src/codec/cbor.js";
import {
  DA_ON_CHAIN_ATTESTATION_V1_DOMAIN,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_PROTOCOL_VERSION,
  type DaAttestationGossipV1,
  DaConflictEvidenceKind,
  type DaConflictEvidenceV1,
  DaGossipTopic,
  daGossipTopic,
  DaLocalPayloadStatus,
  type DaMetadataByHeaderResponseV1,
  DaMetadataStatus,
  type DaPayloadByHeaderResponseV1,
  DaPayloadByHeaderStatus,
  type DaPayloadChunkManifestV1,
  DaPayloadSubmitMode,
  type DaPayloadSubmitRequestV1,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaAttestationGossipV1Cbor,
  decodeDaConflictEvidenceV1Cbor,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadChunkManifestCbor,
  decodeDaPayloadSubmitRequestV1Cbor,
  encodeDaAttestationGossipV1Cbor,
  encodeDaConflictEvidenceV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaPayloadChunkManifestCbor,
  encodeDaPayloadSubmitRequestV1Cbor,
  normalizeDaDeploymentFingerprintHex,
} from "../src/da-transport.js";

const h = (byte: string, count: number): string => byte.repeat(count);
const b = (byte: number, count: number): Buffer => Buffer.alloc(count, byte);
const deployment = b(0x01, 32);
const header = b(0x02, 28);
const payload = b(0x03, 32);

const chunkManifest: DaPayloadChunkManifestV1 = {
  payloadHash: payload,
  totalBytes: 5,
  chunkSize: 2,
  chunkHashes: [b(0x04, 32), b(0x05, 32)],
};

describe("DA libp2p transport protocol freeze", () => {
  it("freezes protocol IDs, topics, domains, limits, and enum codes", () => {
    expect(DA_TRANSPORT_PROTOCOL_VERSION).toBe(1);
    expect(DA_ON_CHAIN_ATTESTATION_V1_DOMAIN).toBe("MidgardDAAttestationV1");
    expect(DA_TRANSPORT_LIMITS_V1).toEqual({
      maxPayloadBytes: 67_108_864,
      maxInlineResponseBytes: 1_048_576,
      maxChunkBytes: 1_048_576,
      maxGossipMessageBytes: 65_536,
      maxStreamsPerPeer: 16,
      requestTimeoutMs: 15_000,
      minimumRetentionDays: 15,
    });
    expect(DaPayloadSubmitMode).toEqual({ inline: 0, chunked: 1 });
    expect(DaPayloadByHeaderStatus).toEqual({
      found_inline: 0,
      found_chunked: 1,
      not_found: 2,
      conflict: 3,
      rejected: 4,
    });
    expect(DaMetadataStatus.found).toBe(0);
    expect(DaLocalPayloadStatus.verified).toBe(1);
    expect(DaConflictEvidenceKind.equivocation).toBe(4);
    expect(daGossipTopic(deployment, DaGossipTopic.payloadAnnouncements)).toBe(
      `/midgard/${h("01", 32)}/da/payload-announcements/1`,
    );
    expect(
      daRequestResponseProtocolId(
        h("01", 32).toUpperCase(),
        DaRequestResponseProtocol.payloadSubmit,
      ),
    ).toBe(`/midgard/${h("01", 32)}/da/payload-submit/1`);
    expect(normalizeDaDeploymentFingerprintHex(h("AB", 32))).toBe(h("ab", 32));
  });

  it("matches frozen CBOR vectors for the Phase 0/1 message schemas", () => {
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
        `5820${h("04", 32)}`,
        `5820${h("05", 32)}`,
      ].join(""),
    );

    assertVector(
      "PayloadSubmitRequestV1",
      encodeDaPayloadSubmitRequestV1Cbor,
      decodeDaPayloadSubmitRequestV1Cbor,
      {
        deploymentFingerprint: deployment,
        headerHash: header,
        payloadHash: payload,
        payloadSchemaVersion: 2,
        mode: "chunked",
        payloadBytes: null,
        chunkManifest,
      } satisfies DaPayloadSubmitRequestV1,
      [
        "87",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "02",
        "01",
        "f6",
        "84",
        `5820${h("03", 32)}`,
        "05",
        "02",
        "82",
        `5820${h("04", 32)}`,
        `5820${h("05", 32)}`,
      ].join(""),
    );

    assertVector(
      "PayloadByHeaderResponseV1",
      encodeDaPayloadByHeaderResponseV1Cbor,
      decodeDaPayloadByHeaderResponseV1Cbor,
      {
        status: "not_found",
        headerHash: header,
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: "missing",
      } satisfies DaPayloadByHeaderResponseV1,
      [
        "86",
        "02",
        `581c${h("02", 28)}`,
        "f6",
        "f6",
        "f6",
        "676d697373696e67",
      ].join(""),
    );

    assertVector(
      "MetadataByHeaderResponseV1",
      encodeDaMetadataByHeaderResponseV1Cbor,
      decodeDaMetadataByHeaderResponseV1Cbor,
      {
        status: "found",
        headerHash: header,
        payloadHash: payload,
        payloadSchemaVersion: 2,
        payloadBytes: 5,
        rootSummaryHash: b(0x06, 32),
        proofBundleHash: b(0x07, 32),
        transitionTraceRoot: Buffer.from("0809", "hex"),
        eventToStepRoot: Buffer.from("0a0b", "hex"),
        retainedUntilSlot: 42,
        localStatus: "verified",
      } satisfies DaMetadataByHeaderResponseV1,
      [
        "8b",
        "00",
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "02",
        "05",
        `5820${h("06", 32)}`,
        `5820${h("07", 32)}`,
        "420809",
        "420a0b",
        "182a",
        "01",
      ].join(""),
    );

    assertVector(
      "DaAttestationGossipV1",
      encodeDaAttestationGossipV1Cbor,
      decodeDaAttestationGossipV1Cbor,
      {
        deploymentFingerprint: deployment,
        headerHash: header,
        payloadHash: payload,
        signerIndex: 4,
        daVkey: b(0x0c, 32),
        onChainWitness: b(0x0d, 65),
        retentionUntilSlot: 42,
        announcedByPeerId: "peer-a",
      } satisfies DaAttestationGossipV1,
      [
        "88",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "04",
        `5820${h("0c", 32)}`,
        `5841${h("0d", 65)}`,
        "182a",
        "66706565722d61",
      ].join(""),
    );

    assertVector(
      "ConflictEvidenceV1",
      encodeDaConflictEvidenceV1Cbor,
      decodeDaConflictEvidenceV1Cbor,
      {
        deploymentFingerprint: deployment,
        headerHash: header,
        evidenceKind: "equivocation",
        evidenceHash: b(0x0e, 32),
        compactEvidence: Buffer.from("0f10", "hex"),
      } satisfies DaConflictEvidenceV1,
      [
        "85",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        "04",
        `5820${h("0e", 32)}`,
        "420f10",
      ].join(""),
    );
  });

  it("rejects non-tuple, trailing, and unsupported enum CBOR", () => {
    expect(() =>
      decodeDaPayloadSubmitRequestV1Cbor(
        encodeCborMapRaw([[encodeCborUnsigned(0n), encodeCborUnsigned(1n)]]),
      ),
    ).toThrow(/PayloadSubmitRequestV1/);

    const encoded = encodeDaPayloadSubmitRequestV1Cbor({
      deploymentFingerprint: deployment,
      headerHash: header,
      payloadHash: payload,
      payloadSchemaVersion: 2,
      mode: "chunked",
      payloadBytes: null,
      chunkManifest,
    });
    expect(() =>
      decodeDaPayloadSubmitRequestV1Cbor(
        Buffer.concat([encoded, Buffer.from([0xf6])]),
      ),
    ).toThrow(/trailing bytes/);

    const unsupportedStatus = Buffer.concat([
      Buffer.from("86", "hex"),
      Buffer.from("09", "hex"),
      Buffer.from(`581c${h("02", 28)}`, "hex"),
      Buffer.from("f6f6f6", "hex"),
      Buffer.from("676d697373696e67", "hex"),
    ]);
    expect(() =>
      decodeDaPayloadByHeaderResponseV1Cbor(unsupportedStatus),
    ).toThrow(/unsupported enum code/);
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
