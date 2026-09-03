import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherExternalProviderTransport,
  makeWatcherL1PublicBytes,
  normalizeWatcherL1Block,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistency as evaluateWatcherMultiProviderConsistencyRaw,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
} from "../../src/l1/multi-provider-consistency.js";

const canonicalJsonForTest = (
  value: unknown,
  ancestors = new WeakSet<object>(),
): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) throw new Error("unsupported number");
    return value.toString();
  }
  if (typeof value !== "object") throw new Error("unsupported value");
  if (ancestors.has(value)) throw new Error("cycle");
  ancestors.add(value);
  let result: string;
  if (Array.isArray(value)) {
    if (
      Object.getPrototypeOf(value) !== Array.prototype ||
      Reflect.ownKeys(value).length !== value.length + 1 ||
      Reflect.ownKeys(value).some(
        (key) =>
          key !== "length" &&
          (typeof key !== "string" ||
            !/^(?:0|[1-9][0-9]*)$/u.test(key) ||
            Number(key) >= value.length),
      )
    ) {
      throw new Error("unsupported array");
    }
    result = `[${value
      .map((member) => canonicalJsonForTest(member, ancestors))
      .join(",")}]`;
  } else {
    const record = value as Record<string, unknown>;
    const prototype = Object.getPrototypeOf(record);
    if (
      (prototype !== Object.prototype && prototype !== null) ||
      Reflect.ownKeys(record).length !== Object.keys(record).length
    ) {
      throw new Error("unsupported object");
    }
    result = `{${Object.keys(record)
      .sort()
      .map(
        (key) =>
          `${JSON.stringify(key)}:${canonicalJsonForTest(record[key], ancestors)}`,
      )
      .join(",")}}`;
  }
  ancestors.delete(value);
  return result;
};

const sha256CanonicalForTest = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJsonForTest(value), "utf8")
    .digest("hex");

const reorderObjectKeysForTest = (value: unknown): unknown => {
  if (Array.isArray(value)) return value.map(reorderObjectKeysForTest);
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.keys(value as Record<string, unknown>)
        .reverse()
        .map((key) => [
          key,
          reorderObjectKeysForTest((value as Record<string, unknown>)[key]),
        ]),
    );
  }
  return value;
};

const observationAttestations = new WeakMap<
  object,
  WatcherL1TransportAttestationContext
>();
const execFileAsync = promisify(execFile);
const transportContexts = new Map<
  string,
  WatcherL1TransportAttestationContext
>();
const tlsIdentities = new Map<string, string>();
let transportFixtureDirectory = "";
const tlsServers: Server[] = [];
const externalEndpoints = new Map<string, string>();

const listen = async (server: Server, target: string | number): Promise<void> =>
  await new Promise((resolve, reject) => {
    server.once("error", reject);
    const onListen = () => {
      server.off("error", reject);
      resolve();
    };
    if (typeof target === "string") server.listen(target, onListen);
    else server.listen(target, "127.0.0.1", onListen);
  });

const makeTlsFixture = async (name: string) => {
  const keyPath = join(transportFixtureDirectory, `${name}.key`);
  const certificatePath = join(transportFixtureDirectory, `${name}.crt`);
  await execFileAsync("openssl", [
    "req",
    "-x509",
    "-newkey",
    "rsa:2048",
    "-nodes",
    "-keyout",
    keyPath,
    "-out",
    certificatePath,
    "-days",
    "1",
    "-subj",
    "/CN=localhost",
    "-addext",
    "subjectAltName=DNS:localhost",
  ]);
  const [key, certificate] = await Promise.all([
    readFile(keyPath, "utf8"),
    readFile(certificatePath, "utf8"),
  ]);
  const server = createTlsServer({ key, cert: certificate });
  await listen(server, 0);
  tlsServers.push(server);
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("TLS fixture did not bind a TCP port");
  }
  const identitySha256 = createHash("sha256")
    .update(new X509Certificate(certificate).raw)
    .digest("hex");
  tlsIdentities.set(name, identitySha256);
  return { certificate, identitySha256, port: address.port };
};

beforeAll(async () => {
  transportFixtureDirectory = await mkdtemp(
    join(tmpdir(), "midgard-w10-consistency-"),
  );
  const fixtures = new Map<
    string,
    Awaited<ReturnType<typeof makeTlsFixture>>
  >();
  for (const identityByte of ["aa", "bb", "cc"]) {
    fixtures.set(identityByte, await makeTlsFixture(identityByte));
  }
  for (const [providerId, identityByte, operatorIdentityByte] of [
    ["provider-a", "aa", "aa"],
    ["provider-b", "bb", "bb"],
    ["provider-a", "bb", "bb"],
    ["provider-b", "aa", "bb"],
    ["provider-a", "aa", "ee"],
    ["provider-b", "bb", "ee"],
    ["provider-c", "cc", "cc"],
  ] as const) {
    const fixture = fixtures.get(identityByte)!;
    const endpoint = `https://localhost:${fixture.port.toString()}/${providerId}-${operatorIdentityByte}`;
    externalEndpoints.set(
      `${providerId}:${identityByte}:${operatorIdentityByte}`,
      endpoint,
    );
    transportContexts.set(
      `external:${providerId}:${identityByte}:${operatorIdentityByte}`,
      await establishWatcherExternalProviderTransport({
        network: "Preprod",
        providerId,
        operatorIdentitySha256: operatorIdentityByte.repeat(32),
        endpoint,
        caPem: fixture.certificate,
        expectedTlsPublicIdentitySha256: fixture.identitySha256,
        connectTimeoutMs: 2_000,
      }),
    );
  }
});

afterAll(async () => {
  for (const context of transportContexts.values()) {
    closeWatcherL1TransportAttestationContext(context);
  }
  for (const server of tlsServers) server.close();
  await rm(transportFixtureDirectory, { recursive: true, force: true });
});

const provider = (
  providerId: string,
  identityByte: string,
  operatorIdentityByte = identityByte,
) =>
  transportContexts.get(
    `external:${providerId}:${identityByte}:${operatorIdentityByte}`,
  )!;

const externalConfig = (network = "Preprod") => ({
  sourceMode: "external_providers",
  network,
  providers: [
    {
      providerId: "provider-a",
      operatorIdentitySha256: "aa".repeat(32),
      endpoint: externalEndpoints.get("provider-a:aa:aa")!,
    },
    {
      providerId: "provider-b",
      operatorIdentitySha256: "bb".repeat(32),
      endpoint: externalEndpoints.get("provider-b:bb:bb")!,
    },
  ],
});

const threeProviderExternalConfig = () => ({
  ...externalConfig(),
  providers: [
    ...externalConfig().providers,
    {
      providerId: "provider-c",
      operatorIdentitySha256: "cc".repeat(32),
      endpoint: externalEndpoints.get("provider-c:cc:cc")!,
    },
  ],
});

const localConfig = () => ({
  sourceMode: "local_node",
  network: "Preprod",
  authorityNodeId: "watcher-node-a",
  genesisIdentitySha256: "aa".repeat(32),
  chainSyncSocketPath: "/run/cardano/node.socket",
  queryServices: [],
});

const transaction = (bodySeedHex: string) => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    BigInt(`0x${bodySeedHex}`),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    true,
    undefined,
  );
  const bodyHex = body.to_canonical_cbor_hex();
  return {
    txHash: computeHash32(Buffer.from(bodyHex, "hex")).toString("hex"),
    fullTransaction: makeWatcherL1PublicBytes(
      fullTransaction.to_canonical_cbor_hex(),
    ),
    body: makeWatcherL1PublicBytes(bodyHex),
    witnessSet: makeWatcherL1PublicBytes(witnessSet.to_canonical_cbor_hex()),
    utxos: [],
    scripts: [],
    datums: [],
    redeemers: [],
  };
};

const observation = (
  providerId: string,
  identityByte: string,
  options: {
    blockHash?: string;
    parentBlockHash?: string | null;
    slot?: string;
    blockNo?: string;
    depth?: string;
    bodyHex?: string;
    operatorIdentityByte?: string;
  } = {},
): WatcherNormalizedL1Block => {
  const attestation = provider(
    providerId,
    identityByte,
    options.operatorIdentityByte ?? identityByte,
  );
  const normalized = normalizeWatcherL1Block(attestation, {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
    network: "Preprod",
    providerId,
    chainPoint: {
      blockHash: options.blockHash ?? "11".repeat(32),
      parentBlockHash: options.parentBlockHash ?? null,
      slot: options.slot ?? "1000",
      blockNo: options.blockNo ?? "100",
      depth: options.depth ?? "15",
    },
    transactions:
      options.bodyHex === undefined ? [] : [transaction(options.bodyHex)],
  });
  observationAttestations.set(normalized, attestation);
  return normalized;
};

const evaluateWatcherMultiProviderConsistency = (
  configuredSource: unknown,
  observations: unknown,
  explicitAttestations?: readonly WatcherL1TransportAttestationContext[],
) => {
  const inferred = Array.isArray(observations)
    ? observations.flatMap((candidate) => {
        if (typeof candidate !== "object" || candidate === null) {
          return [];
        }
        const context = observationAttestations.get(candidate);
        return context === undefined ? [] : [context];
      })
    : [];
  return evaluateWatcherMultiProviderConsistencyRaw(
    configuredSource,
    observations,
    explicitAttestations ?? [...new Set(inferred)],
  );
};

describe("fail-closed multi-provider consistency", () => {
  it("allows exact independently authenticated agreement with explicit minimum depth", () => {
    const first = observation("provider-a", "aa", { depth: "15" });
    const second = observation("provider-b", "bb", { depth: "12" });
    const result = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      first,
      second,
    ]);

    expect(result).toMatchObject({
      schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
      status: "agreed",
      protocolDecision: "allowed",
      sourceMode: "external_providers",
      configuredNetwork: "Preprod",
      authorityNodeId: null,
      authorityGenesisIdentitySha256: null,
      authorityChainSyncSocketPath: null,
      chainAuthorityObservationDigest: null,
      queryObservationCount: 0,
      observationCount: 2,
      independentProviderCount: 2,
      externalProviderBindings: [
        {
          providerId: "provider-a",
          operatorIdentitySha256: "aa".repeat(32),
          authenticationKind: "https_tls_identity_v1",
          publicIdentitySha256: tlsIdentities.get("aa"),
          endpoint: externalEndpoints.get("provider-a:aa:aa"),
        },
        {
          providerId: "provider-b",
          operatorIdentitySha256: "bb".repeat(32),
          authenticationKind: "https_tls_identity_v1",
          publicIdentitySha256: tlsIdentities.get("bb"),
          endpoint: externalEndpoints.get("provider-b:bb:bb"),
        },
      ],
      reasonCodes: ["providers_consistent"],
      alertCodes: [],
      rejectedObservationCount: 0,
      agreement: {
        pointDigest: first.chainPoint.pointDigest,
        blockHash: first.chainPoint.blockHash,
        slot: "1000",
        blockNo: "100",
        minimumDepth: "12",
        blockContentDigest: first.blockContentDigest,
      },
    });
    expect(result.consistencyDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(Object.isFrozen(result)).toBe(true);
    expect(Object.isFrozen(result.reasonCodes)).toBe(true);
    expect(Object.isFrozen(result.agreement)).toBe(true);
  });

  it("rejects endpoint aliases and quarantines live contexts bound to another configured location", () => {
    const aliases = {
      sourceMode: "external_providers",
      network: "Preprod",
      providers: [
        {
          providerId: "provider-a",
          operatorIdentitySha256: "aa".repeat(32),
          endpoint: "https://CARDANO.EXAMPLE:443/api/",
        },
        {
          providerId: "provider-b",
          operatorIdentitySha256: "bb".repeat(32),
          endpoint: "https://cardano.example/api",
        },
      ],
    };
    const aliasResult = evaluateWatcherMultiProviderConsistency(aliases, []);
    expect(aliasResult).toMatchObject({
      status: "quarantined",
      sourceMode: "external_providers",
      reasonCodes: [
        "insufficient_independent_providers",
        "invalid_configured_network",
      ],
    });

    const wrongEndpoint = externalConfig();
    wrongEndpoint.providers[0]!.endpoint =
      "https://localhost:65500/not-provider-a";
    const mismatch = evaluateWatcherMultiProviderConsistency(wrongEndpoint, [
      observation("provider-a", "aa"),
      observation("provider-b", "bb"),
    ]);
    expect(mismatch).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
    });
    expect(mismatch.reasonCodes).toContain("provider_transport_mismatch");
  });

  it("is byte-stable under provider arrival reordering", () => {
    const first = observation("provider-a", "aa");
    const second = observation("provider-b", "bb");

    const forward = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      first,
      second,
    ]);
    const reverse = evaluateWatcherMultiProviderConsistency(
      reorderObjectKeysForTest(externalConfig()),
      [reorderObjectKeysForTest(second), reorderObjectKeysForTest(first)],
      [
        observationAttestations.get(second)!,
        observationAttestations.get(first)!,
      ],
    );

    expect(reverse).toEqual(forward);
    expect(reverse.consistencyDigest).toBe(forward.consistencyDigest);
    const { consistencyDigest, ...canonical } = forward;
    expect(consistencyDigest).toBe(sha256CanonicalForTest(canonical));
    const reversedEvidence = {
      ...canonical,
      observationEvidenceDigests: [
        ...canonical.observationEvidenceDigests,
      ].reverse(),
    };
    expect(sha256CanonicalForTest(reversedEvidence)).not.toBe(
      consistencyDigest,
    );
  });

  it("is byte-stable when a configured provider id is bound to an unconfigured identity", () => {
    const first = observation("provider-a", "aa");
    const second = observation("provider-a", "bb");

    const forward = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      first,
      second,
    ]);
    const reverse = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      second,
      first,
    ]);

    expect(forward).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 1,
      reasonCodes: [
        "insufficient_independent_providers",
        "unconfigured_provider",
      ],
    });
    expect(reverse).toEqual(forward);
    expect(reverse.consistencyDigest).toBe(forward.consistencyDigest);
  });

  it("quarantines a single provider as insufficient", () => {
    const result = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      observation("provider-a", "aa"),
    ]);

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 1,
      reasonCodes: ["insufficient_independent_providers"],
      alertCodes: ["watcher_provider_quorum_unavailable"],
      agreement: null,
    });
  });

  it("quarantines duplicate provider and trust identities", () => {
    const duplicated = observation("provider-a", "aa");
    const exactDuplicate = evaluateWatcherMultiProviderConsistency(
      externalConfig(),
      [duplicated, duplicated],
    );
    expect(exactDuplicate).toMatchObject({
      status: "quarantined",
      independentProviderCount: 1,
      reasonCodes: [
        "insufficient_independent_providers",
        "duplicate_provider_id",
        "duplicate_trust_identity",
        "duplicate_operator_identity",
      ],
      alertCodes: [
        "watcher_provider_quorum_unavailable",
        "watcher_provider_identity_collision",
      ],
    });

    const sharedTrustConfig = externalConfig();
    sharedTrustConfig.providers[1]!.endpoint =
      externalEndpoints.get("provider-b:aa:bb")!;
    const sharedTrust = evaluateWatcherMultiProviderConsistency(
      sharedTrustConfig,
      [
        observation("provider-a", "aa"),
        observation("provider-b", "aa", { operatorIdentityByte: "bb" }),
      ],
    );
    expect(sharedTrust.reasonCodes).toEqual([
      "insufficient_independent_providers",
      "duplicate_trust_identity",
    ]);

    const sharedOperatorConfig = externalConfig();
    sharedOperatorConfig.providers[0]!.endpoint =
      externalEndpoints.get("provider-a:aa:ee")!;
    sharedOperatorConfig.providers[1]!.endpoint =
      externalEndpoints.get("provider-b:bb:ee")!;
    const sharedOperator = evaluateWatcherMultiProviderConsistency(
      sharedOperatorConfig,
      [
        observation("provider-a", "aa", { operatorIdentityByte: "ee" }),
        observation("provider-b", "bb", { operatorIdentityByte: "ee" }),
      ],
    );
    expect(sharedOperator.reasonCodes).toEqual([
      "insufficient_independent_providers",
      "unconfigured_provider",
    ]);
  });

  it("rejects detached evidence that rewrites its embedded transport identity", () => {
    const first = observation("provider-a", "aa");
    const second = observation("provider-b", "bb");
    const forged = [first, second].map((candidate) => ({
      ...structuredClone(candidate),
      provider: {
        ...structuredClone(candidate.provider),
        authentication: {
          ...structuredClone(candidate.provider.authentication),
          kind: "cardano_node_genesis_v1",
        },
      },
    }));
    const result = evaluateWatcherMultiProviderConsistencyRaw(
      externalConfig(),
      forged,
      [
        observationAttestations.get(first)!,
        observationAttestations.get(second)!,
      ],
    );

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 0,
      rejectedObservationCount: 2,
      reasonCodes: [
        "insufficient_independent_providers",
        "malformed_observation",
      ],
      externalProviderBindings: [],
      agreement: null,
    });
  });

  it("revalidates serialized observations only against separate sealed transport attestations", () => {
    const first = observation("provider-a", "aa");
    const second = observation("provider-b", "bb");
    const detached = JSON.parse(JSON.stringify([first, second]));
    const contexts = [
      observationAttestations.get(first)!,
      observationAttestations.get(second)!,
    ];

    const accepted = evaluateWatcherMultiProviderConsistencyRaw(
      externalConfig(),
      detached,
      contexts,
    );
    const serializedContexts = JSON.parse(JSON.stringify(contexts));
    const forged = evaluateWatcherMultiProviderConsistencyRaw(
      externalConfig(),
      detached,
      serializedContexts,
    );

    expect(accepted).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      independentProviderCount: 2,
      rejectedObservationCount: 0,
      reasonCodes: ["providers_consistent"],
    });
    expect(forged).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 0,
      reasonCodes: [
        "insufficient_independent_providers",
        "malformed_observation",
      ],
    });
  });

  it("quarantines observations from the wrong configured network", () => {
    const result = evaluateWatcherMultiProviderConsistency(
      externalConfig("Preview"),
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 0,
      reasonCodes: ["insufficient_independent_providers", "network_mismatch"],
      alertCodes: [
        "watcher_provider_quorum_unavailable",
        "watcher_provider_network_mismatch",
      ],
      agreement: null,
    });
  });

  it("keeps bounded provider lag pending and never allows a protocol decision", () => {
    const result = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      observation("provider-a", "aa"),
      observation("provider-b", "bb", {
        blockHash: "22".repeat(32),
        slot: "1001",
        blockNo: "101",
        depth: "0",
      }),
    ]);

    expect(result).toMatchObject({
      status: "pending",
      protocolDecision: "quarantined",
      reasonCodes: ["bounded_provider_lag"],
      alertCodes: ["watcher_provider_lag"],
      agreement: null,
    });
  });

  it("deduplicates canonical points before checking three-provider bounded lag", () => {
    const result = evaluateWatcherMultiProviderConsistency(
      threeProviderExternalConfig(),
      [
        observation("provider-a", "aa"),
        observation("provider-b", "bb"),
        observation("provider-c", "cc", {
          blockHash: "22".repeat(32),
          slot: "1001",
          blockNo: "101",
          depth: "0",
        }),
      ],
    );

    expect(result).toMatchObject({
      status: "pending",
      protocolDecision: "quarantined",
      independentProviderCount: 3,
      reasonCodes: ["bounded_provider_lag"],
      alertCodes: ["watcher_provider_lag"],
      agreement: null,
    });

    const mismatchedContent = evaluateWatcherMultiProviderConsistency(
      threeProviderExternalConfig(),
      [
        observation("provider-a", "aa", { bodyHex: "a100" }),
        observation("provider-b", "bb", { bodyHex: "a101" }),
        observation("provider-c", "cc", {
          blockHash: "22".repeat(32),
          slot: "1001",
          blockNo: "101",
          depth: "0",
        }),
      ],
    );
    expect(mismatchedContent).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      reasonCodes: ["bounded_provider_lag", "block_content_mismatch"],
      alertCodes: [
        "watcher_provider_lag",
        "watcher_provider_content_disagreement",
      ],
      agreement: null,
    });
  });

  it("quarantines stale and forked provider points", () => {
    const stale = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      observation("provider-a", "aa"),
      observation("provider-b", "bb", {
        blockHash: "22".repeat(32),
        slot: "1100",
        blockNo: "165",
      }),
    ]);
    expect(stale).toMatchObject({
      status: "quarantined",
      reasonCodes: ["stale_provider_observation"],
      alertCodes: ["watcher_provider_stale"],
    });

    const fork = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      observation("provider-a", "aa"),
      observation("provider-b", "bb", {
        blockHash: "22".repeat(32),
      }),
    ]);
    expect(fork).toMatchObject({
      status: "quarantined",
      reasonCodes: ["fork_disagreement"],
      alertCodes: ["watcher_provider_fork"],
    });
  });

  it("quarantines different provider-neutral content at the same point", () => {
    const result = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      observation("provider-a", "aa", { bodyHex: "a100" }),
      observation("provider-b", "bb", { bodyHex: "a101" }),
    ]);

    expect(result).toMatchObject({
      status: "quarantined",
      reasonCodes: ["block_content_mismatch"],
      alertCodes: ["watcher_provider_content_disagreement"],
      agreement: null,
    });
  });

  it("rejects a normalized transaction whose derived validity is spoofed", () => {
    const canonical = observation("provider-a", "aa", { bodyHex: "01" });
    const transaction = canonical.transactions[0]!;
    expect(transaction.isValid).toBe(true);

    const spoofed = {
      ...canonical,
      transactions: [
        {
          ...transaction,
          isValid: false,
        },
      ],
    };
    const result = evaluateWatcherMultiProviderConsistency(externalConfig(), [
      spoofed,
      observation("provider-b", "bb", { bodyHex: "01" }),
    ]);

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 1,
      rejectedObservationCount: 1,
      reasonCodes: [
        "insufficient_independent_providers",
        "malformed_observation",
      ],
      alertCodes: [
        "watcher_provider_quorum_unavailable",
        "watcher_provider_observation_rejected",
      ],
      agreement: null,
    });
  });

  it("quarantines malformed, unknown, and foreign input at a secret-safe boundary", () => {
    const secret = "https://operator:secret@example.invalid";
    const malformed = {
      ...observation("provider-a", "aa"),
      endpoint: secret,
    };
    const foreign = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(foreign, "provider", {
      enumerable: true,
      get: () => {
        throw new Error(secret);
      },
    });

    const malformedResult = evaluateWatcherMultiProviderConsistency(
      externalConfig(),
      [malformed, foreign],
    );
    const foreignBoundary = evaluateWatcherMultiProviderConsistency(
      externalConfig(),
      new Error(secret),
    );
    const unknownNetwork = evaluateWatcherMultiProviderConsistency(secret, [
      observation("provider-a", "aa"),
      observation("provider-b", "bb"),
    ]);
    const missingDiscriminator = evaluateWatcherMultiProviderConsistency(
      "Preprod",
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );
    const unsupportedConfig = structuredClone(externalConfig()) as Record<
      string,
      unknown
    >;
    (
      (unsupportedConfig.providers as Record<string, unknown>[])[0] as Record<
        string,
        unknown
      >
    ).providerId = 1n;
    const cycleConfig = structuredClone(externalConfig()) as Record<
      string,
      unknown
    >;
    cycleConfig.providers = [cycleConfig];
    const unsupportedResult = evaluateWatcherMultiProviderConsistency(
      unsupportedConfig,
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );
    const cycleResult = evaluateWatcherMultiProviderConsistency(cycleConfig, [
      observation("provider-a", "aa"),
      observation("provider-b", "bb"),
    ]);

    expect(malformedResult).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      rejectedObservationCount: 2,
      reasonCodes: [
        "insufficient_independent_providers",
        "malformed_observation",
      ],
      agreement: null,
    });
    expect(foreignBoundary.status).toBe("quarantined");
    expect(unknownNetwork.reasonCodes).toContain("invalid_configured_network");
    expect(missingDiscriminator).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      sourceMode: null,
      reasonCodes: ["invalid_configured_network"],
    });
    expect(unsupportedResult.status).toBe("quarantined");
    expect(cycleResult.status).toBe("quarantined");
    expect(
      JSON.stringify([
        malformedResult,
        foreignBoundary,
        unknownNetwork,
        missingDiscriminator,
      ]),
    ).not.toContain(secret);
  });

  it("never emits provider-quorum findings for a malformed local-node configuration", () => {
    const malformedLocal = {
      ...localConfig(),
      genesisIdentitySha256: "not-a-digest",
    };
    const result = evaluateWatcherMultiProviderConsistency(malformedLocal, []);

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      sourceMode: "local_node",
      reasonCodes: ["invalid_configured_network"],
      alertCodes: ["watcher_provider_observation_rejected"],
    });
    expect(result.reasonCodes).not.toContain(
      "insufficient_independent_providers",
    );
    expect(result.alertCodes).not.toContain(
      "watcher_provider_quorum_unavailable",
    );
  });

  it("retains fail-closed local-mode semantics without manufacturing a live authority", () => {
    const noAuthority = evaluateWatcherMultiProviderConsistency(
      localConfig(),
      [],
    );
    const externalSubstitution = evaluateWatcherMultiProviderConsistency(
      localConfig(),
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );

    expect(noAuthority).toMatchObject({
      status: "quarantined",
      sourceMode: "local_node",
      authorityNodeId: "watcher-node-a",
      reasonCodes: ["missing_chain_sync_authority"],
    });
    expect(externalSubstitution).toMatchObject({
      status: "quarantined",
      sourceMode: "local_node",
      reasonCodes: ["source_mode_mismatch", "missing_chain_sync_authority"],
    });
  });

  it("quarantines a hostile configured-source proxy without exposing its error", () => {
    const secret = "https://operator:secret@example.invalid";
    const hostileConfig = new Proxy(
      {},
      {
        getPrototypeOf: () => {
          throw new Error(secret);
        },
      },
    );

    const result = evaluateWatcherMultiProviderConsistency(hostileConfig, [
      observation("provider-a", "aa"),
      observation("provider-b", "bb"),
    ]);

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      sourceMode: null,
      reasonCodes: ["invalid_configured_network"],
      agreement: null,
    });
    expect(JSON.stringify(result)).not.toContain(secret);
  });
});
