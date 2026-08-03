import { describe, expect, it } from "vitest";

import {
  decodeSingleCbor,
  encodeCborMapRaw,
  encodeCborUnsigned,
} from "../src/codec/cbor.js";
import {
  DA_ON_CHAIN_ATTESTATION_V1_DOMAIN,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
  type DaAttestationGossipV1,
  type DaCapabilitiesResponseV1,
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
  decodeDaCapabilitiesRequestV1Cbor,
  decodeDaCapabilitiesResponseV1Cbor,
  decodeDaConflictEvidenceV1Cbor,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadChunkManifestCbor,
  decodeDaPayloadSubmitRequestV1Cbor,
  encodeDaAttestationGossipV1Cbor,
  encodeDaCapabilitiesRequestV1Cbor,
  encodeDaCapabilitiesResponseV1Cbor,
  encodeDaConflictEvidenceV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaPayloadChunkManifestCbor,
  encodeDaPayloadSubmitRequestV1Cbor,
  normalizeDaDeploymentFingerprintHex,
  parseDaLibp2pRuntimeManifest,
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
  it("rejects malformed UTF-8 and decoder-stripped BOMs structurally", () => {
    for (const malformed of ["61ff", "62c0af", "63eda080", "62e282"]) {
      expect(() => decodeSingleCbor(Buffer.from(malformed, "hex"))).toThrow(
        /valid UTF-8/u,
      );
    }
    expect(decodeSingleCbor(Buffer.from("63efbfbd", "hex"))).toBe("�");
    expect(() => decodeSingleCbor(Buffer.from("63efbbbf", "hex"))).toThrow(
      /UTF-8 BOM/u,
    );
  });

  it("freezes protocol IDs, topics, domains, limits, and enum codes", () => {
    expect(DA_TRANSPORT_V1_PROTOCOL_VERSION).toBe(1);
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

  it("parses the exact canonical producer and watcher runtime manifests", () => {
    const producer = parseDaLibp2pRuntimeManifest(runtimeManifestFixture());
    expect(producer).toMatchObject({
      network: "Preview",
      deployment: {
        fingerprint: h("ab", 32),
        contract_deployment_manifest_id: h("ab", 32),
      },
      runtime_topology: { target: "producer" },
    });

    const watcherFixture = runtimeManifestFixture();
    watcherFixture.runtime_topology = {
      target: "watcher",
      profile: "public",
      producer_peer_id: "peer-producer",
      local_signer_index: 0,
    };
    (watcherFixture.da_transport as Record<string, unknown>).retention_days =
      DA_TRANSPORT_LIMITS_V1.minimumRetentionDays + 1;
    const watcher = parseDaLibp2pRuntimeManifest(watcherFixture);
    expect(watcher.runtime_topology).toEqual({
      target: "watcher",
      profile: "public",
      producer_peer_id: "peer-producer",
      local_signer_index: 0,
    });
    expect(watcher.da_transport.retention_days).toBe(16);
  });

  it("rejects every unknown or missing root and nested runtime-manifest key", () => {
    const cases: readonly {
      readonly mutate: (manifest: Record<string, unknown>) => void;
      readonly error: RegExp;
    }[] = [
      {
        mutate: (manifest) => {
          manifest.unknown_root = true;
        },
        error: /unknown_root is unexpected/u,
      },
      {
        mutate: (manifest) => {
          delete manifest.network;
        },
        error: /network is required/u,
      },
      {
        mutate: (manifest) => {
          const gossip = (
            manifest.da_transport as Record<string, Record<string, unknown>>
          ).gossip;
          gossip.unknown_nested = true;
        },
        error: /gossip\.unknown_nested is unexpected/u,
      },
      {
        mutate: (manifest) => {
          const limits = (
            manifest.da_transport as Record<string, Record<string, unknown>>
          ).limits;
          delete limits.request_timeout_ms;
        },
        error: /limits\.request_timeout_ms is required/u,
      },
      {
        mutate: (manifest) => {
          const committee = manifest.da_committee as {
            members: Record<string, unknown>[];
          };
          committee.members[0]!.unknown_member = true;
        },
        error: /members\[0\]\.unknown_member is unexpected/u,
      },
      {
        mutate: (manifest) => {
          const committee = manifest.da_committee as {
            members: Record<string, unknown>[];
          };
          delete committee.members[0]!.roles;
        },
        error: /members\[0\]\.roles is required/u,
      },
    ];

    for (const testCase of cases) {
      const manifest = runtimeManifestFixture();
      testCase.mutate(manifest);
      expect(() => parseDaLibp2pRuntimeManifest(manifest)).toThrow(
        testCase.error,
      );
    }
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
        payloadSchemaVersion: 1,
        mode: "chunked",
        payloadBytes: null,
        chunkManifest,
      } satisfies DaPayloadSubmitRequestV1,
      [
        "87",
        `5820${h("01", 32)}`,
        `581c${h("02", 28)}`,
        `5820${h("03", 32)}`,
        "01",
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
        payloadSchemaVersion: 1,
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
        "01",
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
      payloadSchemaVersion: 1,
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

    const malformedUtf8Reason = Buffer.concat([
      Buffer.from("86", "hex"),
      Buffer.from("02", "hex"),
      Buffer.from(`581c${h("02", 28)}`, "hex"),
      Buffer.from("f6f6f6", "hex"),
      Buffer.from("6780697373696e67", "hex"),
    ]);
    expect(() =>
      decodeDaPayloadByHeaderResponseV1Cbor(malformedUtf8Reason),
    ).toThrow(/valid UTF-8/u);
  });

  it("reports submit-request decode timing without changing canonical bytes", () => {
    const stages: Array<{
      readonly stage: string;
      readonly durationMs: number;
    }> = [];
    let now = 10;
    const encoded = encodeDaPayloadSubmitRequestV1Cbor({
      deploymentFingerprint: deployment,
      headerHash: header,
      payloadHash: payload,
      payloadSchemaVersion: 1,
      mode: "chunked",
      payloadBytes: null,
      chunkManifest,
    });
    const decoded = decodeDaPayloadSubmitRequestV1Cbor(encoded, {
      monotonicNow: () => {
        now += 5;
        return now;
      },
      onStageTiming: (stage, durationMs) => stages.push({ stage, durationMs }),
    });

    expect(encodeDaPayloadSubmitRequestV1Cbor(decoded)).toEqual(encoded);
    expect(stages).toEqual([{ stage: "submit_request_decode", durationMs: 5 }]);
    expect(() =>
      decodeDaPayloadSubmitRequestV1Cbor(encoded, {
        monotonicNow: () => {
          throw new Error("clock unavailable");
        },
        onStageTiming: () => {
          throw new Error("sink unavailable");
        },
      }),
    ).not.toThrow();

    const nonMinimalArray = Buffer.concat([
      Buffer.from([0x98, 0x07]),
      encoded.subarray(1),
    ]);
    expect(() => decodeDaPayloadSubmitRequestV1Cbor(nonMinimalArray)).toThrow(
      /Non-minimal/u,
    );
  });

  it("round-trips deployment-scoped decoder capabilities canonically", () => {
    const request = { deploymentFingerprint: deployment };
    expect(
      decodeDaCapabilitiesRequestV1Cbor(
        encodeDaCapabilitiesRequestV1Cbor(request),
      ),
    ).toEqual(request);
    const response: DaCapabilitiesResponseV1 = {
      deploymentFingerprint: deployment,
      transportProtocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
      payloadSchemaVersions: [1],
      envelopeContentEncodings: [0, 1],
      maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      maxInlineResponseBytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
      maxChunkBytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
      maxStreamsPerPeer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
      requestTimeoutMs: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
    };
    expect(
      decodeDaCapabilitiesResponseV1Cbor(
        encodeDaCapabilitiesResponseV1Cbor(response),
      ),
    ).toEqual(response);
    expect(() =>
      encodeDaCapabilitiesResponseV1Cbor({
        ...response,
        payloadSchemaVersions: [999] as unknown as readonly [1],
      }),
    ).toThrow(/must contain exactly DA payload schema V1/);
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

const runtimeManifestFixture = (): Record<string, unknown> => ({
  schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
  network: "Preview",
  deployment: {
    fingerprint: h("AB", 32),
    contract_deployment_manifest_id: h("ab", 32),
    contract_deployment_info_sha256: h("cd", 32),
    identity_source: "contract_deployment_manifest_id",
  },
  runtime_topology: {
    target: "producer",
    profile: "public",
    producer_peer_id: "peer-producer",
  },
  da_transport: {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/39002"],
    announce_multiaddrs: ["/dns4/producer.example/tcp/39002/p2p/peer-producer"],
    bootstrap_multiaddrs: [],
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
    },
    limits: {
      max_payload_bytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      max_inline_response_bytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
      max_chunk_bytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
      max_streams_per_peer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
      request_timeout_ms: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
    },
    retention_days: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
  },
  public_retained_da: {
    profile: "public-retained-da-v1",
    access_policy: "any_noise_authenticated_peer",
    peer_id: "peer-public",
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/39003"],
    announce_multiaddrs: ["/dns4/public.example/tcp/39003/p2p/peer-public"],
    protocols: [
      "capabilities",
      "payload-by-header",
      "payload-chunk",
      "metadata-by-header",
      "proof-bundle-by-header",
      "trace-step-by-index",
      "event-to-step-by-event",
    ],
    limits: {
      max_streams_per_peer: 4,
      max_inflight_requests: 32,
      max_inflight_requests_per_peer: 2,
      max_inflight_proof_requests: 1,
      request_timeout_ms: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
    },
  },
  da_committee: {
    threshold: 1,
    members: [
      {
        signer_index: 0,
        da_vkey: h("01", 32),
        peer_id: "peer-a",
        multiaddrs: ["/dns4/da-a.example/tcp/39001/p2p/peer-a"],
        roles: ["committee"],
      },
    ],
  },
});
