import {
  DA_PAYLOAD_INNER_SCHEMA_VERSION,
  DaPayloadContentEncoding,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_PROTOCOL_VERSION,
  type DaCapabilitiesResponse,
  daDeploymentFingerprintFromHex,
  type DaPayloadByHeaderResponse,
  type DaPayloadChunkManifest,
  type DaPayloadChunkResponse,
  encodeDaCapabilitiesResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
  encodeDaPayloadChunkResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { beforeEach, describe, expect, it } from "vitest";

import type { WatcherConfig } from "../../src/runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  type WatcherPublicDaAttempt,
  WatcherPublicDaClient,
  WatcherPublicDaClientError,
  type WatcherPublicDaClock,
  type WatcherPublicDaLibp2pTransportV1,
} from "../../src/storage/public-da-client.js";
import {
  configOf,
  FINGERPRINT,
  HEADER_HASH,
  identityOf,
  makePayloadFixture,
  OTHER_FINGERPRINT,
  OTHER_HEADER_HASH,
  type PayloadFixture,
  PEERS,
  type PeerScript,
  rawConfig,
} from "./public-da-client.raw-config.js";

export const capabilitiesBytes = (
  overrides: Partial<DaCapabilitiesResponse> = {},
): Buffer =>
  encodeDaCapabilitiesResponseCbor({
    deploymentFingerprint: daDeploymentFingerprintFromHex(FINGERPRINT),
    transportProtocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
    payloadSchemaVersions: [DA_PAYLOAD_INNER_SCHEMA_VERSION],
    envelopeContentEncodings: [
      DaPayloadContentEncoding.identity,
      DaPayloadContentEncoding.zstd,
    ],
    maxPayloadBytes: 1_000_000,
    maxInlineResponseBytes: 500_000,
    maxChunkBytes: 250_000,
    maxStreamsPerPeer: 8,
    requestTimeoutMs: 10_000,
    ...overrides,
  });

export const payloadByHeaderBytes = (
  overrides: Partial<DaPayloadByHeaderResponse> = {},
): Buffer =>
  encodeDaPayloadByHeaderResponseCbor({
    status: "found_inline",
    headerHash: Buffer.from(HEADER_HASH, "hex"),
    payloadHash: null,
    payloadBytes: null,
    chunkManifest: null,
    reasonCode: null,
    ...overrides,
  });

export const chunkResponseBytes = (
  overrides: Partial<DaPayloadChunkResponse> &
    Pick<DaPayloadChunkResponse, "payloadHash" | "chunkIndex">,
): Buffer =>
  encodeDaPayloadChunkResponseCbor({
    status: "found",
    headerHash: Buffer.from(HEADER_HASH, "hex"),
    chunkBytes: null,
    chunkHash: null,
    ...overrides,
  });

export const chunksOf = (
  bytes: Buffer,
  chunkSize: number,
): {
  readonly chunks: Buffer[];
  readonly manifest: DaPayloadChunkManifest;
} => {
  const chunks: Buffer[] = [];
  for (let offset = 0; offset < bytes.length; offset += chunkSize) {
    chunks.push(
      bytes.subarray(offset, Math.min(offset + chunkSize, bytes.length)),
    );
  }
  return {
    chunks,
    manifest: {
      payloadHash: computeDaSha256Hash(bytes),
      totalBytes: bytes.length,
      chunkSize,
      chunkHashes: chunks.map((chunk) => computeDaSha256Hash(chunk)),
    },
  };
};

export const clientWith = (
  transport: WatcherPublicDaLibp2pTransportV1,
  configOptions?: Parameters<typeof rawConfig>[0],
  clock?: WatcherPublicDaClock,
): WatcherPublicDaClient =>
  new WatcherPublicDaClient({
    config: configOf(configOptions),
    deploymentIdentity: identityOf(),
    transport,
    ...(clock === undefined ? {} : { clock }),
  });

export const expectClientError = async (
  promise: Promise<unknown>,
): Promise<WatcherPublicDaClientError> => {
  try {
    await promise;
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherPublicDaClientError);
    return error as WatcherPublicDaClientError;
  }
  throw new Error("expected WatcherPublicDaClientErrorV1, request succeeded");
};

export const statuses = (
  attempts: readonly WatcherPublicDaAttempt[],
): readonly string[] => attempts.map((attempt) => attempt.status);

export let fixture: PayloadFixture;

export let otherHeaderFixture: PayloadFixture;

beforeEach(async () => {
  fixture = await makePayloadFixture(HEADER_HASH);
  otherHeaderFixture = await makePayloadFixture(OTHER_HEADER_HASH);
});

/** Single peer that negotiates cleanly and serves the canonical inline payload. */
export const honestInlineScript = (
  overrides: Partial<DaPayloadByHeaderResponse> = {},
  capabilityOverrides: Partial<DaCapabilitiesResponse> = {},
): Record<string, PeerScript> => ({
  [PEERS[0]!]: {
    capabilities: () => capabilitiesBytes(capabilityOverrides),
    "payload-by-header": () =>
      payloadByHeaderBytes({
        status: "found_inline",
        payloadHash: fixture.payloadHash,
        payloadBytes: fixture.envelope,
        ...overrides,
      }),
  },
});

// ---------------------------------------------------------------------------
// 1. Constructor configuration + cause chaining
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 construction", () => {
  const validTransport: WatcherPublicDaLibp2pTransportV1 = {
    request: async () => new Uint8Array([1]),
  };

  it("accepts a well-formed config, identity, and transport", () => {
    const client = new WatcherPublicDaClient({
      config: configOf(),
      deploymentIdentity: identityOf(),
      transport: validTransport,
    });
    expect(client.deploymentFingerprint).toBe(FINGERPRINT);
  });

  const constructionFailure = (options: {
    readonly config?: WatcherConfig;
    readonly deploymentIdentity?: VerifiedWatcherDeploymentIdentity;
    readonly transport?: WatcherPublicDaLibp2pTransportV1;
  }): WatcherPublicDaClientError => {
    try {
      new WatcherPublicDaClient({
        config: "config" in options ? options.config! : configOf(),
        deploymentIdentity:
          "deploymentIdentity" in options
            ? options.deploymentIdentity!
            : identityOf(),
        // `in` rather than `??` so an explicitly null/invalid transport is
        // passed through instead of being replaced by the valid default.
        transport: "transport" in options ? options.transport! : validTransport,
      });
    } catch (error) {
      expect(error).toBeInstanceOf(WatcherPublicDaClientError);
      return error as WatcherPublicDaClientError;
    }
    throw new Error("expected construction to fail");
  };

  it("rejects a target-network mismatch and preserves the cause", () => {
    const error = constructionFailure({
      deploymentIdentity: identityOf({ network: "Mainnet" }),
    });
    expect(error.code).toBe("invalid_configuration");
    expect(error.cause).toBeInstanceOf(Error);
    expect((error.cause as Error).message).toBe("target network mismatch");
    expect(error.message).toContain("target network mismatch");
  });

  it("rejects a mismatched durable deployment marker and preserves the cause", () => {
    const error = constructionFailure({
      deploymentIdentity: identityOf({
        manifestId: FINGERPRINT,
        markerManifestId: OTHER_FINGERPRINT,
      }),
    });
    expect(error.code).toBe("invalid_configuration");
    expect(error.cause).toBeInstanceOf(Error);
    expect((error.cause as Error).message).toContain(
      "deployment marker mismatch",
    );
  });

  it.each([
    ["null transport", null],
    ["non-object transport", 7],
    ["object without request()", { request: "not-a-function" }],
  ])("rejects an %s and preserves the cause", (_label, transport) => {
    const error = constructionFailure({
      transport: transport as unknown as WatcherPublicDaLibp2pTransportV1,
    });
    expect(error.code).toBe("invalid_configuration");
    expect(error.cause).toBeInstanceOf(Error);
    expect((error.cause as Error).message).toBe("invalid libp2p transport");
  });

  it("rejects an invalid watcher config and preserves the config error as cause", () => {
    const broken = rawConfig();
    (broken as { schemaVersion: string }).schemaVersion = "wrong-version";
    const error = constructionFailure({
      config: broken as unknown as WatcherConfig,
    });
    expect(error.code).toBe("invalid_configuration");
    expect(error.cause).toBeInstanceOf(Error);
    // A config fault must remain identifiable rather than collapsing into a
    // bare "invalid_configuration" with no explanation.
    expect((error.cause as Error).name).not.toBe(
      "WatcherPublicDaClientErrorV1",
    );
  });

  /**
   * A structurally valid identity carrying an unparseable manifest id: the
   * failure escapes from `daDeploymentFingerprintFromHex`, i.e. from code the
   * operator cannot influence. This stands in for a genuine internal defect.
   */
  const identityWithUnparseableManifestId =
    (): VerifiedWatcherDeploymentIdentity => ({
      ...identityOf(),
      manifestId: "not-a-fingerprint",
    });

  it("surfaces an unexpected internal failure as a distinguishable cause", () => {
    const error = constructionFailure({
      deploymentIdentity: identityWithUnparseableManifestId(),
    });
    expect(error.code).toBe("invalid_configuration");
    expect(error.cause).toBeInstanceOf(Error);
    expect((error.cause as Error).message).not.toBe("target network mismatch");
    expect((error.cause as Error).message).not.toBe("invalid libp2p transport");
  });

  it("gives every configuration failure branch a distinct cause", () => {
    const causes = [
      constructionFailure({
        deploymentIdentity: identityOf({ network: "Preview" }),
      }),
      constructionFailure({
        deploymentIdentity: identityOf({
          markerManifestId: OTHER_FINGERPRINT,
        }),
      }),
      constructionFailure({
        transport: null as unknown as WatcherPublicDaLibp2pTransportV1,
      }),
      constructionFailure({
        deploymentIdentity: identityWithUnparseableManifestId(),
      }),
    ].map((error) => (error.cause as Error).message);

    expect(new Set(causes).size).toBe(causes.length);
    for (const cause of causes) {
      expect(cause.length).toBeGreaterThan(0);
    }
  });
});
