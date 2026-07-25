import { describe, expect, it } from "vitest";

import {
  computeDaSha256Hash,
  DA_ON_CHAIN_ATTESTATION_V1_DOMAIN,
  type DaAttestationsByHeaderRequestV1,
  type DaAttestationsByHeaderResponseV1,
  type DaEventToStepByEventRequestV1,
  type DaEventToStepByEventResponseV1,
  type DaPayloadAnnouncementV1,
  type DaPayloadByHeaderRequestV1,
  type DaPayloadChunkRequestV1,
  type DaPayloadChunkResponseV1,
  type DaPayloadSubmitResponseV1,
  type DaProofBundleByHeaderRequestV1,
  type DaProofBundleByHeaderResponseV1,
  type DaTraceStepByIndexRequestV1,
  type DaTraceStepByIndexResponseV1,
  decodeDaAttestationsByHeaderRequestV1Cbor,
  decodeDaAttestationsByHeaderResponseV1Cbor,
  decodeDaEventToStepByEventRequestV1Cbor,
  decodeDaEventToStepByEventResponseV1Cbor,
  decodeDaPayloadAnnouncementV1Cbor,
  decodeDaPayloadByHeaderRequestV1Cbor,
  decodeDaPayloadChunkRequestV1Cbor,
  decodeDaPayloadChunkResponseV1Cbor,
  decodeDaPayloadSubmitResponseV1Cbor,
  decodeDaProofBundleByHeaderRequestV1Cbor,
  decodeDaProofBundleByHeaderResponseV1Cbor,
  decodeDaTraceStepByIndexRequestV1Cbor,
  decodeDaTraceStepByIndexResponseV1Cbor,
  encodeDaAttestationGossipV1Cbor,
  encodeDaAttestationsByHeaderRequestV1Cbor,
  encodeDaAttestationsByHeaderResponseV1Cbor,
  encodeDaAttestationV1Preimage,
  encodeDaEventToStepByEventRequestV1Cbor,
  encodeDaEventToStepByEventResponseV1Cbor,
  encodeDaPayloadAnnouncementV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadChunkRequestV1Cbor,
  encodeDaPayloadChunkResponseV1Cbor,
  encodeDaPayloadSubmitResponseV1Cbor,
  encodeDaProofBundleByHeaderRequestV1Cbor,
  encodeDaProofBundleByHeaderResponseV1Cbor,
  encodeDaTraceStepByIndexRequestV1Cbor,
  encodeDaTraceStepByIndexResponseV1Cbor,
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

describe("DA transport full message vectors", () => {
  it("matches golden vectors for remaining request-response and gossip tuples", () => {
    assertVector(
      "DaPayloadAnnouncementV1",
      encodeDaPayloadAnnouncementV1Cbor,
      decodeDaPayloadAnnouncementV1Cbor,
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
      } satisfies DaPayloadAnnouncementV1,
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
      "PayloadSubmitResponseV1",
      encodeDaPayloadSubmitResponseV1Cbor,
      decodeDaPayloadSubmitResponseV1Cbor,
      {
        status: "deferred",
        headerHash,
        payloadHash,
        reasonCode: "busy",
        retryAfterMs: 1500,
      } satisfies DaPayloadSubmitResponseV1,
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
      "PayloadByHeaderRequestV1",
      encodeDaPayloadByHeaderRequestV1Cbor,
      decodeDaPayloadByHeaderRequestV1Cbor,
      {
        deploymentFingerprint,
        headerHash,
        acceptedPayloadHashes: [payloadHash, b(0x0d, 32)],
        maxInlineBytes: 1_048_576,
      } satisfies DaPayloadByHeaderRequestV1,
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
      "PayloadChunkRequestV1",
      encodeDaPayloadChunkRequestV1Cbor,
      decodeDaPayloadChunkRequestV1Cbor,
      {
        deploymentFingerprint,
        headerHash,
        payloadHash,
        chunkIndex: 2,
      } satisfies DaPayloadChunkRequestV1,
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
      encodeDaPayloadChunkResponseV1Cbor,
      decodeDaPayloadChunkResponseV1Cbor,
      {
        status: "found",
        headerHash,
        payloadHash,
        chunkIndex: 2,
        chunkBytes: Buffer.from("chunk-2"),
        chunkHash,
      } satisfies DaPayloadChunkResponseV1,
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
      encodeDaProofBundleByHeaderRequestV1Cbor,
      decodeDaProofBundleByHeaderRequestV1Cbor,
      {
        deploymentFingerprint,
        headerHash,
        maxInlineBytes: 1_048_576,
      } satisfies DaProofBundleByHeaderRequestV1,
      ["83", `5820${h("01", 32)}`, `581c${h("02", 28)}`, "1a00100000"].join(""),
    );

    assertVector(
      "ProofBundleByHeaderResponseV1",
      encodeDaProofBundleByHeaderResponseV1Cbor,
      decodeDaProofBundleByHeaderResponseV1Cbor,
      {
        status: "found_inline",
        headerHash,
        proofBundleHash,
        proofBundleBytes: Buffer.from("proof"),
        chunkManifest: null,
        reasonCode: null,
      } satisfies DaProofBundleByHeaderResponseV1,
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
      encodeDaTraceStepByIndexRequestV1Cbor,
      decodeDaTraceStepByIndexRequestV1Cbor,
      {
        deploymentFingerprint,
        headerHash,
        stepIndex: 7,
      } satisfies DaTraceStepByIndexRequestV1,
      ["83", `5820${h("01", 32)}`, `581c${h("02", 28)}`, "07"].join(""),
    );

    assertVector(
      "TraceStepByIndexResponseV1",
      encodeDaTraceStepByIndexResponseV1Cbor,
      decodeDaTraceStepByIndexResponseV1Cbor,
      {
        status: "found",
        headerHash,
        stepIndex: 7,
        transitionStepBytes: Buffer.from("step"),
        membershipProofBytes: Buffer.from("membership"),
      } satisfies DaTraceStepByIndexResponseV1,
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
      encodeDaEventToStepByEventRequestV1Cbor,
      decodeDaEventToStepByEventRequestV1Cbor,
      {
        deploymentFingerprint,
        headerHash,
        eventKey: Buffer.from("event"),
      } satisfies DaEventToStepByEventRequestV1,
      ["83", `5820${h("01", 32)}`, `581c${h("02", 28)}`, "456576656e74"].join(
        "",
      ),
    );

    assertVector(
      "EventToStepByEventResponseV1",
      encodeDaEventToStepByEventResponseV1Cbor,
      decodeDaEventToStepByEventResponseV1Cbor,
      {
        status: "found",
        headerHash,
        eventKey: Buffer.from("event"),
        eventToStepEntryBytes: Buffer.from("entry"),
        membershipOrNonmembershipProofBytes: Buffer.from("proof"),
      } satisfies DaEventToStepByEventResponseV1,
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
      encodeDaAttestationsByHeaderRequestV1Cbor,
      decodeDaAttestationsByHeaderRequestV1Cbor,
      {
        deploymentFingerprint,
        headerHash,
        acceptedSignerIndexes: [0, 4],
        maxAttestations: 10,
      } satisfies DaAttestationsByHeaderRequestV1,
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
      encodeDaAttestationsByHeaderResponseV1Cbor,
      decodeDaAttestationsByHeaderResponseV1Cbor,
      {
        status: "found",
        headerHash,
        attestations: [
          {
            deploymentFingerprint,
            headerHash,
            payloadHash,
            signerIndex: 4,
            daVkey: b(0x0c, 32),
            onChainWitness: b(0x0d, 65),
            retentionUntilSlot: 42,
            announcedByPeerId: "peer-a",
          },
        ],
        reasonCode: null,
      } satisfies DaAttestationsByHeaderResponseV1,
      [
        "84",
        "00",
        `581c${h("02", 28)}`,
        "81",
        "88",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
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
    expect(encodeDaAttestationV1Preimage(headerHash).toString("hex")).toBe(
      `${Buffer.from(DA_ON_CHAIN_ATTESTATION_V1_DOMAIN, "utf8").toString(
        "hex",
      )}${h("02", 28)}`,
    );
    expect(computeDaSha256Hash(Buffer.from("payload")).toString("hex")).toBe(
      "239f59ed55e737c77147cf55ad0c1b030b6d7ee748a7426952f9b852d5a935e5",
    );
    expect(() =>
      encodeDaPayloadChunkRequestV1Cbor({
        deploymentFingerprint: b(0x01, 31),
        headerHash,
        payloadHash,
        chunkIndex: 0,
      }),
    ).toThrow(/deployment_fingerprint must be 32 bytes/);
    expect(() =>
      encodeDaPayloadChunkRequestV1Cbor({
        deploymentFingerprint,
        headerHash: b(0x02, 27),
        payloadHash,
        chunkIndex: 0,
      }),
    ).toThrow(/header_hash must be 28 bytes/);
    expect(() =>
      encodeDaPayloadChunkRequestV1Cbor({
        deploymentFingerprint,
        headerHash,
        payloadHash: b(0x03, 31),
        chunkIndex: 0,
      }),
    ).toThrow(/payload_hash must be 32 bytes/);
    expect(() =>
      encodeDaAttestationGossipV1Cbor({
        deploymentFingerprint,
        headerHash,
        payloadHash,
        signerIndex: 256,
        daVkey: b(0x0c, 32),
        onChainWitness: b(0x0d, 65),
        retentionUntilSlot: 42,
        announcedByPeerId: "peer-a",
      }),
    ).toThrow(/signer_index must be a uint8/);
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
