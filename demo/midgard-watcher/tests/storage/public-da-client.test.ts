import {
  DA_PAYLOAD_INNER_SCHEMA_VERSION,
  DaPayloadContentEncoding,
  encodeDaPayloadEnvelope,
  wrapDaPayload,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_PROTOCOL_VERSION,
  type DaCapabilitiesResponse,
  daDeploymentFingerprintFromHex,
  type DaPayloadByHeaderResponse,
  type DaPayloadChunkManifest,
  type DaPayloadChunkResponse,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderRequestCbor,
  decodeDaPayloadChunkRequestCbor,
  encodeDaCapabilitiesResponseCbor,
  encodeDaEventToStepByEventResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
  encodeDaPayloadChunkResponseCbor,
  encodeDaProofBundleByHeaderResponseCbor,
  encodeDaTraceStepByIndexResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  DA_PAYLOAD_VERSION,
  type DaPayload,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
} from "@al-ft/midgard-sdk";
import { beforeEach, describe, expect, it, vi } from "vitest";

import type { WatcherConfig } from "../../src/runtime/config.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  type WatcherPublicDaAttempt,
  WatcherPublicDaClient,
  WatcherPublicDaClientError,
  type WatcherPublicDaClock,
  type WatcherPublicDaLibp2pTransportV1,
  type WatcherPublicDaRequest,
} from "../../src/storage/public-da-client.js";
import {
  encodeWatcherPublicDaFrame,
  readWatcherPublicDaFrames,
  WatcherPublicDaLibp2pTransport,
} from "../../src/storage/public-da-libp2p-transport.js";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const repeatHex = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

const FINGERPRINT = repeatHex(0x1a, 32);
const OTHER_FINGERPRINT = repeatHex(0x2b, 32);
const HEADER_HASH = repeatHex(0xab, 28);
const OTHER_HEADER_HASH = repeatHex(0xcd, 28);

const PEERS = ["da-peer-a", "da-peer-b", "da-peer-c", "da-peer-d", "da-peer-e"];

// Peer ids must stay inside the base58 alphabet the watcher config enforces.
const PEER_ID_SUFFIX = ["A", "B", "C", "D", "E"];

const multiaddrFor = (index: number): string =>
  `/dns4/da-${String.fromCharCode(97 + index)}.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz1234${PEER_ID_SUFFIX[index]!}`;

const rawConfig = (options?: {
  readonly peerCount?: number;
  readonly maxConcurrency?: number;
  readonly requestTimeoutMs?: number;
  readonly daFetchMs?: number;
  readonly targetNetwork?: string;
}): Record<string, unknown> => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: options?.targetNetwork ?? "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: repeatHex(0x11, 32),
          endpoint: "https://cardano-a.example",
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: repeatHex(0x22, 32),
          endpoint: "https://cardano-b.example",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: 15,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: 15,
      },
    },
  },
  da: {
    peers: Array.from({ length: options?.peerCount ?? 1 }, (_, index) => ({
      identity: PEERS[index],
      multiaddr: multiaddrFor(index),
    })),
    requestTimeoutMs: options?.requestTimeoutMs ?? 10_000,
    maxConcurrency: options?.maxConcurrency ?? 8,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
    rollbackAuthorityKeySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
    },
  },
  proverWallet: {
    keySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_PROVER_KEY",
    },
  },
  deadlines: {
    daFetchMs: options?.daFetchMs ?? 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});

const configOf = (options?: Parameters<typeof rawConfig>[0]): WatcherConfig =>
  rawConfig(options) as unknown as WatcherConfig;

const identityOf = (options?: {
  readonly manifestId?: string;
  readonly markerManifestId?: string;
  readonly network?: "Mainnet" | "Preprod" | "Preview";
}): VerifiedWatcherDeploymentIdentity => {
  const manifestId = options?.manifestId ?? FINGERPRINT;
  return {
    manifestId,
    network: options?.network ?? "Preprod",
    trustRootId: "trust-root-a",
    releaseEvidenceDigest: repeatHex(0x33, 32),
    ruleBundleCommitment: repeatHex(0x44, 32),
    programCommitments: {},
    durableMarker: makeDeploymentMarker(
      options?.markerManifestId ?? manifestId,
    ),
  };
};

const daPayload = (headerHash: string): DaPayload => {
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return {
    version: DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header: {
        prevUtxosRoot: repeatHex(0x01, 32),
        utxosRoot: repeatHex(0x02, 32),
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: repeatHex(0x03, 32),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: repeatHex(0x04, 32),
        eventToStepRoot: repeatHex(0x05, 32),
        validationTracesRoot: repeatHex(0x06, 32),
        ...counts,
        startTime: 1_000n,
        endTime: 1_999n,
        blockSlot: 42n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: repeatHex(0x07, 28),
        operatorVkey: repeatHex(0x08, 28),
        protocolVersion: 1n,
      },
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [[repeatHex(0x09, 32), repeatHex(0x0a, 40)]],
      transaction_preimages: [[repeatHex(0x09, 32), repeatHex(0x0b, 64)]],
      forced_transaction_preimages: [],
      cek_program_material: [],
      deposits: [],
      transition_trace: [[repeatHex(0x0c, 32), repeatHex(0x0d, 48)]],
      event_to_step: [[repeatHex(0x0e, 32), repeatHex(0x0f, 8)]],
      validation_traces: [[repeatHex(0x10, 32), repeatHex(0x11, 24)]],
      validation_trace_witnesses: [],
      counts,
    },
  };
};

type PayloadFixture = Readonly<{
  innerCbor: Buffer;
  envelope: Buffer;
  payloadHash: Buffer;
}>;

const makePayloadFixture = async (
  headerHash: string,
): Promise<PayloadFixture> => {
  const innerCbor = encodeDaPayload(daPayload(headerHash));
  const envelope = await wrapDaPayload(innerCbor, { mode: "identity" });
  return {
    innerCbor,
    envelope,
    payloadHash: computeDaSha256Hash(envelope),
  };
};

// ---------------------------------------------------------------------------
// Scripted libp2p transport
// ---------------------------------------------------------------------------

type ProtocolHandler = (
  request: WatcherPublicDaRequest,
) => Promise<Uint8Array> | Uint8Array;

type PeerScript = Partial<Record<DaRequestResponseProtocol, ProtocolHandler>>;

class ScriptedTransport implements WatcherPublicDaLibp2pTransportV1 {
  readonly calls: WatcherPublicDaRequest[] = [];

  constructor(private readonly script: Readonly<Record<string, PeerScript>>) {}

  async request(request: WatcherPublicDaRequest): Promise<Uint8Array> {
    this.calls.push(request);
    const peer = this.script[request.peerIdentity];
    if (peer === undefined) {
      throw new Error(`unscripted peer ${request.peerIdentity}`);
    }
    const handler = peer[request.protocol];
    if (handler === undefined) {
      throw new Error(
        `unscripted protocol ${request.protocol} for ${request.peerIdentity}`,
      );
    }
    return handler(request);
  }

  protocolsFor(peerIdentity: string): DaRequestResponseProtocol[] {
    return this.calls
      .filter((call) => call.peerIdentity === peerIdentity)
      .map((call) => call.protocol);
  }
}

/**
 * A deterministic replacement for the real clock and timer queue.
 *
 * Deadline behaviour must be decided by state, not by racing wall-clock
 * timers against peer failures: under full-suite load real timers drift in
 * both directions, and a fetch that has actually spent its budget can still
 * observe a sliver of remaining time and burn another peer on a clamped
 * 1ms dial (see #535). Virtual time only ever moves when the earliest pending
 * timer fires, so elapsed time is exactly the sum of the deadlines the client
 * itself chose. The drain runs on `setImmediate` so every pending microtask
 * settles between two firings — real time is used for ordering only, never
 * for measurement.
 */
const makeVirtualClock = (): WatcherPublicDaClock => {
  type VirtualTimer = { readonly dueAt: number; readonly callback: () => void };
  const timers = new Map<number, VirtualTimer>();
  let now = 0;
  let nextId = 0;
  let scheduled = false;

  const drain = (): void => {
    if (scheduled) {
      return;
    }
    scheduled = true;
    setImmediate(step);
  };

  const step = (): void => {
    scheduled = false;
    let dueId: number | undefined;
    let due: VirtualTimer | undefined;
    // Earliest deadline first; ties break on insertion order, which the Map
    // preserves, so the firing order is a pure function of the schedule.
    for (const [id, timer] of timers) {
      if (due === undefined || timer.dueAt < due.dueAt) {
        dueId = id;
        due = timer;
      }
    }
    if (dueId === undefined || due === undefined) {
      return;
    }
    timers.delete(dueId);
    now = Math.max(now, due.dueAt);
    due.callback();
    drain();
  };

  return {
    now: () => now,
    setTimeout: (callback: () => void, delayMs: number) => {
      const id = (nextId += 1);
      timers.set(id, { dueAt: now + delayMs, callback });
      drain();
      return id;
    },
    clearTimeout: (handle: unknown) => {
      timers.delete(handle as number);
    },
  };
};

/** A transport call that never settles until the peer aborts it. */
const hangUntilAborted: ProtocolHandler = async (request) =>
  new Promise<Uint8Array>((_, reject) => {
    request.signal.addEventListener("abort", () => {
      reject(new Error("aborted"));
    });
  });

const capabilitiesBytes = (
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

const payloadByHeaderBytes = (
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

const chunkResponseBytes = (
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

const chunksOf = (
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

const clientWith = (
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

const expectClientError = async (
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

const statuses = (
  attempts: readonly WatcherPublicDaAttempt[],
): readonly string[] => attempts.map((attempt) => attempt.status);

let fixture: PayloadFixture;
let otherHeaderFixture: PayloadFixture;

beforeEach(async () => {
  fixture = await makePayloadFixture(HEADER_HASH);
  otherHeaderFixture = await makePayloadFixture(OTHER_HEADER_HASH);
});

/** Single peer that negotiates cleanly and serves the canonical inline payload. */
const honestInlineScript = (
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

// ---------------------------------------------------------------------------
// 2. Request-argument validation
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 request validation", () => {
  const client = (): WatcherPublicDaClient =>
    clientWith(new ScriptedTransport({}));

  it.each([
    ["uppercase hex", HEADER_HASH.toUpperCase()],
    ["too short", repeatHex(0xab, 27)],
    ["too long", repeatHex(0xab, 29)],
    ["non-hex", "z".repeat(56)],
    ["empty", ""],
  ])(
    "rejects a %s header hash without dialing a peer",
    async (_label, hash) => {
      const transport = new ScriptedTransport({});
      const error = await expectClientError(
        clientWith(transport).fetchPayloadByHeader({ headerHash: hash }),
      );
      expect(error.code).toBe("invalid_request");
      expect(transport.calls).toHaveLength(0);
    },
  );

  it.each([
    ["empty", []],
    ["duplicated", [repeatHex(0x01, 32), repeatHex(0x01, 32)]],
    [
      "over the 64-entry cap",
      Array.from({ length: 65 }, (_, i) => repeatHex(i % 256, 32)),
    ],
    ["wrong length", [repeatHex(0x01, 31)]],
  ])(
    "rejects %s acceptedPayloadHashes",
    async (_label, acceptedPayloadHashes) => {
      const error = await expectClientError(
        client().fetchPayloadByHeader({
          headerHash: HEADER_HASH,
          acceptedPayloadHashes,
        }),
      );
      expect(error.code).toBe("invalid_request");
    },
  );

  it.each([-1, 1.5, Number.NaN, Number.MAX_SAFE_INTEGER + 2])(
    "rejects step index %s",
    async (stepIndex) => {
      const error = await expectClientError(
        client().fetchTraceStepByIndex({ headerHash: HEADER_HASH, stepIndex }),
      );
      expect(error.code).toBe("invalid_request");
    },
  );

  it.each([
    ["empty string", ""],
    ["odd-length hex", "abc"],
    ["uppercase hex", "ABCD"],
    ["empty bytes", new Uint8Array(0)],
    ["oversized bytes", new Uint8Array(4_097)],
  ])("rejects a %s event key", async (_label, eventKey) => {
    const error = await expectClientError(
      client().fetchEventToStepByEvent({ headerHash: HEADER_HASH, eventKey }),
    );
    expect(error.code).toBe("invalid_request");
  });
});

// ---------------------------------------------------------------------------
// 3. Capability negotiation
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 capability negotiation", () => {
  it("negotiates before requesting and clamps limits to the protocol ceiling", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });

    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
    expect(transport.protocolsFor(PEERS[0]!)).toEqual([
      "capabilities",
      "payload-by-header",
    ]);
    const inlineRequest = decodeDaPayloadByHeaderRequestCbor(
      transport.calls[1]!.requestCbor,
    );
    // maxInlineBytes echoes the negotiated (clamped) inline ceiling.
    expect(inlineRequest.maxInlineBytes).toBe(500_000);
    expect(inlineRequest.headerHash.toString("hex")).toBe(HEADER_HASH);
  });

  it("dials the deployment-scoped protocol id", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(transport.calls[0]!.protocolId).toBe(
      daRequestResponseProtocolId(FINGERPRINT, "capabilities"),
    );
    expect(transport.calls[0]!.multiaddr).toBe(multiaddrFor(0));
  });

  it("rejects capabilities announcing a foreign deployment fingerprint", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            deploymentFingerprint:
              daDeploymentFingerprintFromHex(OTHER_FINGERPRINT),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("capabilities");
  });

  it.each([
    ["zero maxPayloadBytes", { maxPayloadBytes: 0 }],
    ["zero maxChunkBytes", { maxChunkBytes: 0 }],
    ["zero maxStreamsPerPeer", { maxStreamsPerPeer: 0 }],
    ["zero requestTimeoutMs", { requestTimeoutMs: 0 }],
    [
      "inline ceiling above payload ceiling",
      { maxPayloadBytes: 1_000, maxInlineResponseBytes: 2_000 },
    ],
    [
      "chunk ceiling above payload ceiling",
      {
        maxPayloadBytes: 1_000,
        maxInlineResponseBytes: 500,
        maxChunkBytes: 2_000,
      },
    ],
  ])("rejects capabilities with %s", async (_label, overrides) => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes(overrides as Partial<DaCapabilitiesResponse>),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("rejects undecodable capability bytes", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => Buffer.from("not-cbor-at-all"),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});

// ---------------------------------------------------------------------------
// 4. Inline payload: SHA-256 verification, both directions
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 inline payload verification", () => {
  it("accepts an inline payload whose SHA-256 matches the announced hash", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });

    expect(result.schemaVersion).toBe("midgard-watcher-public-da-client-v1");
    expect(result.deploymentFingerprint).toBe(FINGERPRINT);
    expect(result.headerHash).toBe(HEADER_HASH);
    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
    expect(result.payloadEnvelopeCbor.equals(fixture.envelope)).toBe(true);
    expect(result.innerPayloadCbor.equals(fixture.innerCbor)).toBe(true);
    expect(result.sourcePeerIdentity).toBe(PEERS[0]);
    expect(result.durableInput.kind).toBe("da_payload");
    expect(result.durableInput.inputId).toBe(
      fixture.payloadHash.toString("hex"),
    );
    expect(result.durableInput.payload.cborHex).toBe(
      fixture.envelope.toString("hex"),
    );
    expect(statuses(result.attempts)).toEqual(["success"]);
  });

  it("REJECTS an inline payload whose bytes were corrupted after hashing", async () => {
    const corrupted = Buffer.from(fixture.envelope);
    corrupted[corrupted.length - 1] ^= 0xff;
    expect(corrupted.equals(fixture.envelope)).toBe(false);

    const transport = new ScriptedTransport(
      honestInlineScript({ payloadBytes: corrupted }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("payload-by-header");
  });

  it("REJECTS an honest payload announced under a foreign payload hash", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: computeDaSha256Hash(Buffer.from("x")),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an envelope whose declared inner SHA-256 does not match its body", async () => {
    const tamperedInner = Buffer.from(fixture.innerCbor);
    tamperedInner[tamperedInner.length - 1] ^= 0x01;
    // Envelope is internally inconsistent: body is tampered but innerSha256
    // still commits to the original inner bytes.
    const envelope = encodeDaPayloadEnvelope({
      version: 1,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: fixture.innerCbor.length,
      innerSha256: computeDaSha256Hash(fixture.innerCbor),
      body: tamperedInner,
    });

    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: computeDaSha256Hash(envelope),
        payloadBytes: envelope,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an envelope carrying a payload for a different header", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: otherHeaderFixture.payloadHash,
        payloadBytes: otherHeaderFixture.envelope,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a response echoing a different header hash", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        headerHash: Buffer.from(OTHER_HEADER_HASH, "hex"),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a payload not present in acceptedPayloadHashes", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({
        headerHash: HEADER_HASH,
        acceptedPayloadHashes: [repeatHex(0xee, 32)],
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts a payload present in acceptedPayloadHashes", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
      acceptedPayloadHashes: [
        repeatHex(0xee, 32),
        fixture.payloadHash.toString("hex"),
      ],
    });
    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
    const request = decodeDaPayloadByHeaderRequestCbor(
      transport.calls[1]!.requestCbor,
    );
    expect(request.acceptedPayloadHashes).toHaveLength(2);
  });

  it("REJECTS an inline response that also carries a chunk manifest", async () => {
    const { manifest } = chunksOf(fixture.envelope, 64);
    const transport = new ScriptedTransport(
      honestInlineScript({ chunkManifest: manifest }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunked response that also carries inline bytes", async () => {
    const { manifest } = chunksOf(fixture.envelope, 64);
    const transport = new ScriptedTransport(
      honestInlineScript({
        status: "found_chunked",
        chunkManifest: manifest,
        payloadBytes: fixture.envelope,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunked response with no manifest at all", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        status: "found_chunked",
        payloadBytes: null,
        chunkManifest: null,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});

// ---------------------------------------------------------------------------
// 5. Bounds checks
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 bounds enforcement", () => {
  it("REJECTS an inline payload above the negotiated inline ceiling", async () => {
    const inlineCeiling = fixture.envelope.length - 1;
    const transport = new ScriptedTransport(
      honestInlineScript(
        {},
        { maxPayloadBytes: 1_000_000, maxInlineResponseBytes: inlineCeiling },
      ),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts an inline payload exactly at the negotiated inline ceiling", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript(
        {},
        {
          maxPayloadBytes: 1_000_000,
          maxInlineResponseBytes: fixture.envelope.length,
        },
      ),
    );
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(result.payloadEnvelopeCbor).toHaveLength(fixture.envelope.length);
  });

  it("REJECTS an empty inline payload body", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: computeDaSha256Hash(Buffer.alloc(0)),
        payloadBytes: Buffer.alloc(0),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an empty transport response frame", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: { capabilities: () => new Uint8Array(0) },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a transport response that is not a byte array", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => "definitely-not-bytes" as unknown as Uint8Array,
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a trace step whose parts jointly exceed the payload ceiling", async () => {
    const half = Buffer.alloc(600, 0x5a);
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            maxPayloadBytes: 1_000,
            maxInlineResponseBytes: 1_000,
            maxChunkBytes: 1_000,
          }),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 0,
            transitionStepBytes: half,
            membershipProofBytes: half,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 0,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts a trace step that fits inside the payload ceiling", async () => {
    const step = Buffer.alloc(300, 0x5a);
    const proof = Buffer.alloc(400, 0x6b);
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            maxPayloadBytes: 1_000,
            maxInlineResponseBytes: 1_000,
            maxChunkBytes: 1_000,
          }),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 3,
            transitionStepBytes: step,
            membershipProofBytes: proof,
          }),
      },
    });
    const result = await clientWith(transport).fetchTraceStepByIndex({
      headerHash: HEADER_HASH,
      stepIndex: 3,
    });
    expect(result.stepIndex).toBe(3);
    expect(result.transitionStepSha256).toBe(
      computeDaSha256Hash(step).toString("hex"),
    );
    expect(result.membershipProofSha256).toBe(
      computeDaSha256Hash(proof).toString("hex"),
    );
  });

  /*
   * Zero-length (but non-null) parts are the one shape the joint size ceiling
   * cannot catch: an empty part only shrinks the sum. These pin the per-field
   * emptiness bound itself rather than a downstream duplicate of it.
   */
  it.each([
    ["transition step", Buffer.alloc(0), Buffer.alloc(32, 0x11)],
    ["membership proof", Buffer.alloc(32, 0x11), Buffer.alloc(0)],
  ])("REJECTS a trace step with an empty %s", async (_label, step, proof) => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 0,
            transitionStepBytes: step,
            membershipProofBytes: proof,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 0,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an event-to-step answer with empty proof bytes", async () => {
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: Buffer.alloc(16, 0x44),
            membershipOrNonmembershipProofBytes: Buffer.alloc(0),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an event-to-step answer with a present-but-empty entry", async () => {
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: Buffer.alloc(0),
            membershipOrNonmembershipProofBytes: Buffer.alloc(16, 0x55),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a proof bundle with empty bytes", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_inline",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(Buffer.alloc(0)),
            proofBundleBytes: Buffer.alloc(0),
            chunkManifest: null,
            reasonCode: null,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a trace step answering a different index", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 9,
            transitionStepBytes: Buffer.alloc(8, 1),
            membershipProofBytes: Buffer.alloc(8, 2),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 3,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});

// ---------------------------------------------------------------------------
// 6. Chunk manifest validation and chunked reassembly
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 chunked payload retrieval", () => {
  const CHUNK_SIZE = 64;

  const chunkedScript = (options?: {
    readonly manifest?: DaPayloadChunkManifest;
    readonly chunkFor?: (index: number, chunks: readonly Buffer[]) => Buffer;
    readonly chunkHashFor?: (
      index: number,
      chunks: readonly Buffer[],
    ) => Buffer;
    readonly chunkIndexFor?: (index: number) => number;
    readonly chunkStatus?: DaPayloadChunkResponse["status"];
  }): Record<string, PeerScript> => {
    const { chunks, manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    return {
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            maxPayloadBytes: 1_000_000,
            maxInlineResponseBytes: 128,
            maxChunkBytes: CHUNK_SIZE,
          }),
        "payload-by-header": () =>
          payloadByHeaderBytes({
            status: "found_chunked",
            payloadHash: fixture.payloadHash,
            payloadBytes: null,
            chunkManifest: options?.manifest ?? manifest,
          }),
        "payload-chunk": (request) => {
          const decoded = decodeDaPayloadChunkRequestCbor(request.requestCbor);
          const index = decoded.chunkIndex;
          return chunkResponseBytes({
            status: options?.chunkStatus ?? "found",
            payloadHash: fixture.payloadHash,
            chunkIndex: options?.chunkIndexFor?.(index) ?? index,
            chunkBytes: options?.chunkFor?.(index, chunks) ?? chunks[index]!,
            chunkHash:
              options?.chunkHashFor?.(index, chunks) ??
              computeDaSha256Hash(chunks[index]!),
          });
        },
      },
    };
  };

  it("reassembles a multi-chunk payload and verifies the whole-payload hash", async () => {
    const { chunks } = chunksOf(fixture.envelope, CHUNK_SIZE);
    expect(chunks.length).toBeGreaterThan(2);

    const transport = new ScriptedTransport(chunkedScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });

    expect(result.payloadEnvelopeCbor.equals(fixture.envelope)).toBe(true);
    expect(result.innerPayloadCbor.equals(fixture.innerCbor)).toBe(true);
    expect(transport.protocolsFor(PEERS[0]!)).toEqual([
      "capabilities",
      "payload-by-header",
      ...chunks.map(() => "payload-chunk" as const),
    ]);
  });

  it("requests every chunk index in order", async () => {
    const { chunks } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(chunkedScript());
    await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    const requested = transport.calls
      .filter((call) => call.protocol === "payload-chunk")
      .map(
        (call) => decodeDaPayloadChunkRequestCbor(call.requestCbor).chunkIndex,
      );
    expect(requested).toEqual(chunks.map((_, index) => index));
  });

  it("REJECTS a chunk whose bytes do not hash to the manifest entry", async () => {
    const transport = new ScriptedTransport(
      chunkedScript({
        chunkFor: (index, chunks) => {
          if (index !== 1) {
            return chunks[index]!;
          }
          const tampered = Buffer.from(chunks[1]!);
          tampered[0] ^= 0xff;
          return tampered;
        },
        // Peer still claims the honest manifest hash for the tampered chunk.
        chunkHashFor: (index, chunks) => computeDaSha256Hash(chunks[index]!),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("payload-chunk");
  });

  it("REJECTS a chunk announcing a hash that is not the manifest entry", async () => {
    const transport = new ScriptedTransport(
      chunkedScript({
        chunkHashFor: (index, chunks) =>
          index === 0
            ? computeDaSha256Hash(Buffer.from("wrong"))
            : computeDaSha256Hash(chunks[index]!),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunk answering the wrong index", async () => {
    const transport = new ScriptedTransport(
      chunkedScript({ chunkIndexFor: (index) => index + 1 }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a manifest committing to a different payload hash", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          payloadHash: computeDaSha256Hash(Buffer.from("other")),
        },
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("payload-by-header");
  });

  it("REJECTS a manifest whose chunk count contradicts totalBytes/chunkSize", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          chunkHashes: manifest.chunkHashes.slice(0, -1),
        },
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a manifest with an empty chunk list", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({ manifest: { ...manifest, chunkHashes: [] } }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a manifest whose chunkSize exceeds the negotiated chunk ceiling", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          chunkSize: CHUNK_SIZE * 4,
          chunkHashes: manifest.chunkHashes.slice(
            0,
            Math.ceil(manifest.totalBytes / (CHUNK_SIZE * 4)),
          ),
        },
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it.each([
    ["zero totalBytes", 0],
    ["totalBytes above the payload ceiling", 2_000_000],
  ])("REJECTS a manifest with %s", async (_label, totalBytes) => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({ manifest: { ...manifest, totalBytes } }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a final chunk of the wrong length", async () => {
    const { chunks, manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const lastIndex = chunks.length - 1;
    // Pad the final chunk and restate the manifest hash so only the declared
    // length check can catch it.
    const padded = Buffer.concat([chunks[lastIndex]!, Buffer.alloc(1, 0)]);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          chunkHashes: manifest.chunkHashes.map((hash, index) =>
            index === lastIndex ? computeDaSha256Hash(padded) : hash,
          ),
        },
        chunkFor: (index, allChunks) =>
          index === lastIndex ? padded : allChunks[index]!,
        chunkHashFor: (index, allChunks) =>
          index === lastIndex
            ? computeDaSha256Hash(padded)
            : computeDaSha256Hash(allChunks[index]!),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it.each([
    ["not_found", "not_found", "not_found"],
    ["rejected", "rejected", "peer_rejected"],
  ] as const)(
    "maps a %s chunk response to attempt status %s",
    async (_label, chunkStatus, expected) => {
      const transport = new ScriptedTransport(chunkedScript({ chunkStatus }));
      const error = await expectClientError(
        clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
      );
      expect(statuses(error.attempts)).toEqual([expected]);
    },
  );
});

// ---------------------------------------------------------------------------
// 7. Peer failover
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 peer failover", () => {
  const honestPeer = (identity: string): Record<string, PeerScript> => ({
    [identity]: {
      capabilities: () => capabilitiesBytes(),
      "payload-by-header": () =>
        payloadByHeaderBytes({
          status: "found_inline",
          payloadHash: fixture.payloadHash,
          payloadBytes: fixture.envelope,
        }),
    },
  });

  it("fails over from a transport-level failure to the next peer", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => {
          throw new Error("dial refused");
        },
      },
      ...honestPeer(PEERS[1]!),
    });
    const result = await clientWith(transport, {
      peerCount: 2,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(result.sourcePeerIdentity).toBe(PEERS[1]);
    expect(result.attempts).toHaveLength(2);
    expect(result.attempts[0]).toEqual({
      peerIdentity: PEERS[0],
      protocol: "capabilities",
      status: "transport_error",
    });
    expect(result.attempts[1]!.status).toBe("success");
  });

  it.each([
    ["not_found", "not_found"],
    ["conflict", "peer_conflict"],
    ["rejected", "peer_rejected"],
  ] as const)(
    "records a %s answer and fails over to an honest peer",
    async (status, expectedStatus) => {
      const transport = new ScriptedTransport({
        [PEERS[0]!]: {
          capabilities: () => capabilitiesBytes(),
          "payload-by-header": () => payloadByHeaderBytes({ status }),
        },
        ...honestPeer(PEERS[1]!),
      });
      const result = await clientWith(transport, {
        peerCount: 2,
      }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

      expect(result.sourcePeerIdentity).toBe(PEERS[1]);
      expect(statuses(result.attempts)).toEqual([expectedStatus, "success"]);
      expect(result.attempts[0]!.protocol).toBe("payload-by-header");
    },
  );

  it("fails over from a peer serving corrupted bytes to an honest peer", async () => {
    const corrupted = Buffer.from(fixture.envelope);
    corrupted[0] ^= 0xff;
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () =>
          payloadByHeaderBytes({
            status: "found_inline",
            payloadHash: fixture.payloadHash,
            payloadBytes: corrupted,
          }),
      },
      ...honestPeer(PEERS[1]!),
    });
    const result = await clientWith(transport, {
      peerCount: 2,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(statuses(result.attempts)).toEqual(["invalid_content", "success"]);
    expect(result.payloadEnvelopeCbor.equals(fixture.envelope)).toBe(true);
  });

  it("stops at the first success and does not dial later peers", async () => {
    const transport = new ScriptedTransport({
      ...honestPeer(PEERS[0]!),
      [PEERS[1]!]: {
        capabilities: () => {
          throw new Error("should never be dialed");
        },
      },
    });
    const result = await clientWith(transport, {
      peerCount: 2,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(result.attempts).toHaveLength(1);
    expect(transport.protocolsFor(PEERS[1]!)).toEqual([]);
  });

  it("reports all_peers_failed with one attempt per peer when every peer fails", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () =>
          payloadByHeaderBytes({ status: "not_found" }),
      },
      [PEERS[1]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () => payloadByHeaderBytes({ status: "rejected" }),
      },
      [PEERS[2]!]: {
        capabilities: () => {
          throw new Error("dial refused");
        },
      },
    });
    const error = await expectClientError(
      clientWith(transport, { peerCount: 3 }).fetchPayloadByHeader({
        headerHash: HEADER_HASH,
      }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual([
      "not_found",
      "peer_rejected",
      "transport_error",
    ]);
    expect(error.attempts.map((a) => a.peerIdentity)).toEqual([
      PEERS[0],
      PEERS[1],
      PEERS[2],
    ]);
  });

  it("records a per-peer timeout and continues to the next peer", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: { capabilities: hangUntilAborted },
      ...honestPeer(PEERS[1]!),
    });
    const result = await clientWith(transport, {
      peerCount: 2,
      requestTimeoutMs: 120,
      daFetchMs: 5_000,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(statuses(result.attempts)).toEqual(["timeout", "success"]);
  });

  it("freezes the returned attempt ledger", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(Object.isFrozen(result)).toBe(true);
    expect(Object.isFrozen(result.attempts)).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// 8. Deadline and permit concurrency control
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 deadline and permit control", () => {
  type Gate = {
    readonly waitUntilEntered: Promise<void>;
    readonly release: () => void;
    inFlight: number;
    maxInFlight: number;
    completed: string[];
  };

  const gatedScript = (
    gate: Gate,
    enteredResolve: () => void,
  ): Record<string, PeerScript> => ({
    [PEERS[0]!]: {
      capabilities: () => capabilitiesBytes(),
      "payload-by-header": async () => {
        gate.inFlight += 1;
        gate.maxInFlight = Math.max(gate.maxInFlight, gate.inFlight);
        enteredResolve();
        await gate.waitUntilEntered;
        gate.inFlight -= 1;
        return payloadByHeaderBytes({
          status: "found_inline",
          payloadHash: fixture.payloadHash,
          payloadBytes: fixture.envelope,
        });
      },
    },
  });

  const makeGate = (): { gate: Gate; entered: () => void } => {
    let release: () => void = () => undefined;
    const waitUntilEntered = new Promise<void>((resolve) => {
      release = resolve;
    });
    const gate: Gate = {
      waitUntilEntered,
      release,
      inFlight: 0,
      maxInFlight: 0,
      completed: [],
    };
    return { gate, entered: () => undefined };
  };

  it("serializes requests when maxConcurrency is 1", async () => {
    const { gate } = makeGate();
    const transport = new ScriptedTransport(gatedScript(gate, () => undefined));
    const client = clientWith(transport, { maxConcurrency: 1 });

    const first = client.fetchPayloadByHeader({ headerHash: HEADER_HASH });
    const second = client.fetchPayloadByHeader({ headerHash: HEADER_HASH });

    // Let the event loop run: only the first request may reach the transport.
    await new Promise((resolve) => setImmediate(resolve));
    expect(gate.inFlight).toBe(1);
    expect(
      transport.calls.filter((c) => c.protocol === "payload-by-header"),
    ).toHaveLength(1);

    gate.release();
    await Promise.all([first, second]);
    expect(gate.maxInFlight).toBe(1);
    expect(
      transport.calls.filter((c) => c.protocol === "payload-by-header"),
    ).toHaveLength(2);
  });

  it("runs requests in parallel up to maxConcurrency", async () => {
    const { gate } = makeGate();
    const transport = new ScriptedTransport(gatedScript(gate, () => undefined));
    const client = clientWith(transport, { maxConcurrency: 3 });

    const pending = [0, 1, 2].map(() =>
      client.fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    await new Promise((resolve) => setImmediate(resolve));
    await new Promise((resolve) => setImmediate(resolve));
    expect(gate.maxInFlight).toBe(3);

    gate.release();
    await Promise.all(pending);
  });

  it("drains the permit queue in submission order", async () => {
    const { gate } = makeGate();
    const order: number[] = [];
    const transport = new ScriptedTransport(gatedScript(gate, () => undefined));
    const client = clientWith(transport, { maxConcurrency: 1 });

    const pending = [0, 1, 2, 3].map((index) =>
      client
        .fetchPayloadByHeader({ headerHash: HEADER_HASH })
        .then(() => order.push(index)),
    );
    gate.release();
    await Promise.all(pending);

    expect(order).toEqual([0, 1, 2, 3]);
    expect(gate.maxInFlight).toBe(1);
  });

  it("releases the permit even when the fetch fails, so later requests proceed", async () => {
    let attempt = 0;
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () => {
          attempt += 1;
          return attempt === 1
            ? payloadByHeaderBytes({ status: "not_found" })
            : payloadByHeaderBytes({
                status: "found_inline",
                payloadHash: fixture.payloadHash,
                payloadBytes: fixture.envelope,
              });
        },
      },
    });
    const client = clientWith(transport, { maxConcurrency: 1 });

    const failure = await expectClientError(
      client.fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(failure.code).toBe("all_peers_failed");

    const result = await client.fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
  });

  it("clamps each per-request timeout to the configured request timeout", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    await clientWith(transport, {
      requestTimeoutMs: 2_500,
      daFetchMs: 60_000,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    for (const call of transport.calls) {
      expect(call.timeoutMs).toBeLessThanOrEqual(2_500);
      expect(call.timeoutMs).toBeGreaterThan(0);
    }
  });

  it("shrinks the per-request timeout as the fetch deadline is consumed", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: { capabilities: hangUntilAborted },
      [PEERS[1]!]: { capabilities: hangUntilAborted },
      [PEERS[2]!]: { capabilities: hangUntilAborted },
    });
    await expectClientError(
      clientWith(
        transport,
        {
          peerCount: 3,
          requestTimeoutMs: 400,
          daFetchMs: 1_000,
        },
        makeVirtualClock(),
      ).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );

    expect(transport.calls).toHaveLength(3);
    expect(transport.calls[0]!.timeoutMs).toBe(400);
    // Third dial can only receive whatever is left of the 1s fetch budget:
    // two 400ms dials are spent, so exactly 200ms remain.
    expect(transport.calls[2]!.timeoutMs).toBe(200);
  });

  it("aborts the in-flight transport call when a request times out", async () => {
    let aborted = false;
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: async (request) =>
          new Promise<Uint8Array>((_, reject) => {
            request.signal.addEventListener("abort", () => {
              aborted = true;
              reject(new Error("aborted"));
            });
          }),
      },
    });
    const error = await expectClientError(
      clientWith(
        transport,
        {
          requestTimeoutMs: 120,
          daFetchMs: 5_000,
        },
        makeVirtualClock(),
      ).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["timeout"]);
    expect(aborted).toBe(true);
  });

  it("fails the whole fetch with deadline_exceeded once the budget is spent", async () => {
    const transport = new ScriptedTransport(
      Object.fromEntries(
        PEERS.map((peer) => [peer, { capabilities: hangUntilAborted }]),
      ),
    );
    // Virtual time, so the budget is spent by the dials the client itself
    // chose (400 + 400 + 200 = the whole 1s), not by whatever the host
    // scheduler happened to do under load. Without it the last sliver of the
    // budget can be handed to another peer as a clamped sub-millisecond dial,
    // and the fetch reports the peer failure instead of the deadline (#535).
    const error = await expectClientError(
      clientWith(
        transport,
        {
          peerCount: 5,
          requestTimeoutMs: 400,
          daFetchMs: 1_000,
        },
        makeVirtualClock(),
      ).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );

    expect(error.code).toBe("deadline_exceeded");
    expect(statuses(error.attempts)).toEqual([
      "timeout",
      "timeout",
      "timeout",
      "deadline_exceeded",
    ]);
    // The budget is spent before every peer is dialed.
    expect(error.attempts.length).toBeLessThan(PEERS.length + 1);
  });
});

// ---------------------------------------------------------------------------
// 9. Proof bundle, trace step, and event-to-step surfaces
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 auxiliary DA surfaces", () => {
  const proofBundle = Buffer.alloc(96, 0x7c);

  it("accepts a proof bundle whose SHA-256 matches", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_inline",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(proofBundle),
            proofBundleBytes: proofBundle,
            chunkManifest: null,
            reasonCode: null,
          }),
      },
    });
    const result = await clientWith(transport).fetchProofBundleByHeader({
      headerHash: HEADER_HASH,
    });
    expect(result.proofBundleHash).toBe(
      computeDaSha256Hash(proofBundle).toString("hex"),
    );
    expect(result.proofBundleBytes.equals(proofBundle)).toBe(true);
    expect(result.durableInput.kind).toBe("proof_input");
  });

  it("REJECTS a proof bundle whose SHA-256 does not match", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_inline",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(Buffer.from("elsewhere")),
            proofBundleBytes: proofBundle,
            chunkManifest: null,
            reasonCode: null,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunked proof bundle (only inline is supported)", async () => {
    const { manifest } = chunksOf(proofBundle, 32);
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_chunked",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(proofBundle),
            proofBundleBytes: null,
            chunkManifest: manifest,
            reasonCode: null,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts an event-to-step membership answer and hashes both parts", async () => {
    const entry = Buffer.alloc(24, 0x21);
    const proof = Buffer.alloc(48, 0x22);
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: entry,
            membershipOrNonmembershipProofBytes: proof,
          }),
      },
    });
    const result = await clientWith(transport).fetchEventToStepByEvent({
      headerHash: HEADER_HASH,
      eventKey,
    });
    expect(result.eventToStepEntrySha256).toBe(
      computeDaSha256Hash(entry).toString("hex"),
    );
    expect(result.membershipOrNonmembershipProofSha256).toBe(
      computeDaSha256Hash(proof).toString("hex"),
    );
  });

  it("accepts a non-membership answer with a null entry", async () => {
    const proof = Buffer.alloc(48, 0x33);
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: null,
            membershipOrNonmembershipProofBytes: proof,
          }),
      },
    });
    const result = await clientWith(transport).fetchEventToStepByEvent({
      headerHash: HEADER_HASH,
      eventKey,
    });
    expect(result.eventToStepEntryBytes).toBeNull();
    expect(result.eventToStepEntrySha256).toBeNull();
  });

  it("REJECTS an event-to-step answer echoing a different event key", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from("ffffffff", "hex"),
            eventToStepEntryBytes: null,
            membershipOrNonmembershipProofBytes: Buffer.alloc(8, 1),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey: "0a1b2c3d",
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an event-to-step answer with no proof bytes", async () => {
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: Buffer.alloc(8, 1),
            membershipOrNonmembershipProofBytes: null,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});

// ---------------------------------------------------------------------------
// 10. Concrete TCP + Noise + Yamux transport framing and peer binding
// ---------------------------------------------------------------------------

describe("WatcherPublicDaLibp2pTransport", () => {
  const authenticatedPeerId =
    "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
  const requestFor = (
    signal = new AbortController().signal,
  ): WatcherPublicDaRequest => ({
    peerIdentity: "public-da-a",
    peerId: authenticatedPeerId,
    multiaddr: `/dns4/public-da.example/tcp/39003/p2p/${authenticatedPeerId}`,
    protocol: DaRequestResponseProtocol.capabilities,
    protocolId: daRequestResponseProtocolId(
      FINGERPRINT,
      DaRequestResponseProtocol.capabilities,
    ),
    requestCbor: Buffer.from([0xa0]),
    timeoutMs: 100,
    signal,
  });

  it("uses exact bounded framing across fragments and rejects adjacent responses", async () => {
    const frame = encodeWatcherPublicDaFrame(Buffer.from("response"), 32);
    const decoded = await collectBuffers(
      readWatcherPublicDaFrames(
        asAsyncIterable([frame.subarray(0, 3), frame.subarray(3)]),
        32,
      ),
    );
    expect(decoded).toEqual([Buffer.from("response")]);

    expect(() => encodeWatcherPublicDaFrame(Buffer.alloc(0), 32)).toThrow(
      /must not be empty/u,
    );
    expect(() => encodeWatcherPublicDaFrame(Buffer.alloc(33), 32)).toThrow(
      /exceeds configured bound/u,
    );
    await expect(
      collectBuffers(
        readWatcherPublicDaFrames(asAsyncIterable([Buffer.from([0, 0])]), 32),
      ),
    ).rejects.toThrow(/incomplete/u);
  });

  it("requires the Noise-authenticated connection peer to equal the configured peer", async () => {
    const sent: Uint8Array[] = [];
    const stream = {
      send: (frame: Uint8Array): boolean => {
        sent.push(frame);
        return true;
      },
      close: async (): Promise<void> => undefined,
      abort: (): void => undefined,
      async *[Symbol.asyncIterator](): AsyncGenerator<Uint8Array> {
        yield encodeWatcherPublicDaFrame(Buffer.from("response"));
      },
    };
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop: async (): Promise<void> => undefined,
        dialProtocol: async () => stream,
        getConnections: () => [
          { remotePeer: { toString: () => authenticatedPeerId } },
        ],
      }),
    });
    await transport.start();
    await expect(transport.request(requestFor())).resolves.toEqual(
      Buffer.from("response"),
    );
    expect(sent).toHaveLength(1);
    await transport.stop();

    const abortWrongPeerStream = vi.fn();
    const wrongPeerStream = {
      ...stream,
      abort: abortWrongPeerStream,
    };
    const wrongPeerTransport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop: async (): Promise<void> => undefined,
        dialProtocol: async () => wrongPeerStream,
        getConnections: () => [
          {
            remotePeer: {
              toString: () =>
                "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47",
            },
          },
        ],
      }),
    });
    await wrongPeerTransport.start();
    await expect(wrongPeerTransport.request(requestFor())).rejects.toThrow(
      /Noise-authenticated remote peer/u,
    );
    expect(abortWrongPeerStream).toHaveBeenCalledOnce();
    await wrongPeerTransport.stop();
  });

  it("honors an already-aborted request before it can dial", async () => {
    const controller = new AbortController();
    controller.abort(new Error("test cancellation"));
    const dialProtocol = vi.fn();
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop: async (): Promise<void> => undefined,
        dialProtocol,
        getConnections: () => [],
      }),
    });
    await transport.start();
    await expect(
      transport.request(requestFor(controller.signal)),
    ).rejects.toThrow(/test cancellation/u);
    expect(dialProtocol).not.toHaveBeenCalled();
    await transport.stop();
  });
});

const collectBuffers = async (
  iterable: AsyncIterable<Buffer>,
): Promise<Buffer[]> => {
  const values: Buffer[] = [];
  for await (const value of iterable) values.push(value);
  return values;
};

const asAsyncIterable = async function* (
  values: readonly Uint8Array[],
): AsyncGenerator<Uint8Array> {
  yield* values;
};
