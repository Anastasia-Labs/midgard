import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createServer as createNetServer, type Server } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  closeWatcherL1TransportAttestationContextV1,
  establishWatcherExternalProviderTransportV1,
  establishWatcherLocalNodeAuthorityTransportV1,
  establishWatcherLocalNodeQueryTransportV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContextV1,
  type WatcherNormalizedL1BlockV1,
} from "../src/l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistencyV1 as evaluateWatcherMultiProviderConsistencyV1Raw,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION,
} from "../src/multi-provider-consistency.js";

const observationAttestations = new WeakMap<
  object,
  WatcherL1TransportAttestationContextV1
>();
const execFileAsync = promisify(execFile);
const transportContexts = new Map<
  string,
  WatcherL1TransportAttestationContextV1
>();
const tlsIdentities = new Map<string, string>();
let transportFixtureDirectory = "";
let localServer: Server;
let localQueryServer: Server;
const tlsServers: Server[] = [];
let localGenesisIdentitySha256 = "";
let foreignGenesisIdentitySha256 = "";
let localNodeSocketPath = "";
let localQueryPort = 0;
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
  const socketPath = join(transportFixtureDirectory, "node.socket");
  localNodeSocketPath = socketPath;
  const genesisPath = join(transportFixtureDirectory, "genesis.json");
  const foreignGenesisPath = join(
    transportFixtureDirectory,
    "foreign-genesis.json",
  );
  const genesisBytes = Buffer.from('{"networkMagic":1}', "utf8");
  const foreignGenesisBytes = Buffer.from('{"networkMagic":2}', "utf8");
  await Promise.all([
    writeFile(genesisPath, genesisBytes),
    writeFile(foreignGenesisPath, foreignGenesisBytes),
  ]);
  localGenesisIdentitySha256 = createHash("sha256")
    .update(genesisBytes)
    .digest("hex");
  foreignGenesisIdentitySha256 = createHash("sha256")
    .update(foreignGenesisBytes)
    .digest("hex");
  localServer = createNetServer();
  await listen(localServer, socketPath);
  localQueryServer = createNetServer();
  await listen(localQueryServer, 0);
  const queryAddress = localQueryServer.address();
  if (queryAddress === null || typeof queryAddress === "string") {
    throw new Error("query fixture did not bind a TCP port");
  }
  localQueryPort = queryAddress.port;

  for (const [authorityNodeId, identityByte, selectedGenesisPath, digest] of [
    ["watcher-node-a", "cc", genesisPath, localGenesisIdentitySha256],
    ["watcher-node-b", "cc", genesisPath, localGenesisIdentitySha256],
    ["watcher-node-a", "aa", foreignGenesisPath, foreignGenesisIdentitySha256],
  ] as const) {
    const context = await establishWatcherLocalNodeAuthorityTransportV1({
      network: "Preprod",
      authorityNodeId,
      providerId: "chain-sync",
      nodeSocketPath: socketPath,
      genesisFilePath: selectedGenesisPath,
      expectedGenesisIdentitySha256: digest,
      connectTimeoutMs: 2_000,
    });
    transportContexts.set(
      `local:${authorityNodeId}:chain_sync:chain-sync:${identityByte}`,
      context,
    );
  }
  const authority = transportContexts.get(
    "local:watcher-node-a:chain_sync:chain-sync:cc",
  )!;
  for (const [surface, providerId] of [
    ["ogmios", "ogmios"],
    ["ogmios", "ogmios-a"],
    ["ogmios", "ogmios-b"],
    ["kupo", "kupo"],
    ["db_sync", "db-sync"],
    ["kupmios", "kupmios"],
  ] as const) {
    transportContexts.set(
      `local:watcher-node-a:${surface}:${providerId}:cc`,
      await establishWatcherLocalNodeQueryTransportV1(authority, {
        transportKind: "tcp",
        providerId,
        surface,
        endpoint: localQueryEndpoint(surface, providerId),
        connectTimeoutMs: 2_000,
      }),
    );
  }

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
      await establishWatcherExternalProviderTransportV1({
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
    closeWatcherL1TransportAttestationContextV1(context);
  }
  for (const server of [...tlsServers, localServer, localQueryServer])
    server.close();
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

const localProvider = (
  surface: "chain_sync" | "ogmios" | "kupo" | "kupmios" | "db_sync",
  identityByte: string,
  authorityNodeId = "watcher-node-a",
  providerId = surface.replace("_", "-"),
) => {
  const authorityIdentity = surface === "chain_sync" ? identityByte : "cc";
  return transportContexts.get(
    `local:${authorityNodeId}:${surface}:${providerId}:${authorityIdentity}`,
  )!;
};

const localQueryEndpoint = (surface: string, providerId: string): string =>
  `${surface === "db_sync" ? "postgresql" : "http"}://127.0.0.1:${localQueryPort.toString()}/${providerId}`;

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

const localConfig = (
  queryServices: readonly Readonly<{
    kind: "ogmios" | "kupo" | "kupmios" | "db_sync";
    providerId: string;
  }>[] = [],
) => ({
  sourceMode: "local_node",
  network: "Preprod",
  authorityNodeId: "watcher-node-a",
  genesisIdentitySha256: localGenesisIdentitySha256,
  chainSyncSocketPath: localNodeSocketPath,
  queryServices: queryServices.map((query) => ({
    ...query,
    endpoint: localQueryEndpoint(query.kind, query.providerId),
  })),
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
    fullTransaction: makeWatcherL1PublicBytesV1(
      fullTransaction.to_canonical_cbor_hex(),
    ),
    body: makeWatcherL1PublicBytesV1(bodyHex),
    witnessSet: makeWatcherL1PublicBytesV1(witnessSet.to_canonical_cbor_hex()),
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
): WatcherNormalizedL1BlockV1 => {
  const attestation = provider(
    providerId,
    identityByte,
    options.operatorIdentityByte ?? identityByte,
  );
  const normalized = normalizeWatcherL1BlockV1(attestation, {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
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

const localObservation = (
  surface: "chain_sync" | "ogmios" | "kupo" | "kupmios" | "db_sync",
  identityByte: string,
  options: {
    authorityNodeId?: string;
    blockHash?: string;
    parentBlockHash?: string | null;
    slot?: string;
    blockNo?: string;
    depth?: string;
    bodyHex?: string;
    providerId?: string;
  } = {},
): WatcherNormalizedL1BlockV1 => {
  const attestation = localProvider(
    surface,
    identityByte,
    options.authorityNodeId,
    options.providerId,
  );
  const normalized = normalizeWatcherL1BlockV1(attestation, {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: options.providerId ?? surface.replace("_", "-"),
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

const evaluateWatcherMultiProviderConsistencyV1 = (
  configuredSource: unknown,
  observations: unknown,
  explicitAttestations?: readonly WatcherL1TransportAttestationContextV1[],
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
  return evaluateWatcherMultiProviderConsistencyV1Raw(
    configuredSource,
    observations,
    explicitAttestations ?? [...new Set(inferred)],
  );
};

describe("fail-closed multi-provider consistency", () => {
  it("allows exact independently authenticated agreement with explicit minimum depth", () => {
    const first = observation("provider-a", "aa", { depth: "15" });
    const second = observation("provider-b", "bb", { depth: "12" });
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      first,
      second,
    ]);

    expect(result).toMatchObject({
      schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION,
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
    const aliasResult = evaluateWatcherMultiProviderConsistencyV1(aliases, []);
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
    const mismatch = evaluateWatcherMultiProviderConsistencyV1(wrongEndpoint, [
      observation("provider-a", "aa"),
      observation("provider-b", "bb"),
    ]);
    expect(mismatch).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
    });
    expect(mismatch.reasonCodes).toContain("provider_transport_mismatch");
  });

  it("quarantines local authority and query observations from wrong configured locations", () => {
    const authority = localObservation("chain_sync", "cc");
    const wrongSocket = evaluateWatcherMultiProviderConsistencyV1(
      {
        ...localConfig(),
        chainSyncSocketPath: "/var/run/cardano/another-node.socket",
      },
      [authority],
    );
    expect(wrongSocket.reasonCodes).toContain("provider_transport_mismatch");

    const query = localObservation("ogmios", "cc");
    const configured = localConfig([{ kind: "ogmios", providerId: "ogmios" }]);
    const wrongQueryEndpoint = {
      ...configured,
      queryServices: [
        {
          ...configured.queryServices[0]!,
          endpoint: "http://127.0.0.1:65500/not-ogmios",
        },
      ],
    };
    const mismatch = evaluateWatcherMultiProviderConsistencyV1(
      wrongQueryEndpoint,
      [authority, query],
    );
    expect(mismatch.reasonCodes).toContain("provider_transport_mismatch");
  });

  it("is byte-stable under provider arrival reordering", () => {
    const first = observation("provider-a", "aa");
    const second = observation("provider-b", "bb");

    const forward = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [first, second],
    );
    const reverse = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [second, first],
    );

    expect(reverse).toEqual(forward);
    expect(reverse.consistencyDigest).toBe(forward.consistencyDigest);
  });

  it("is byte-stable when a configured provider id is bound to an unconfigured identity", () => {
    const first = observation("provider-a", "aa");
    const second = observation("provider-a", "bb");

    const forward = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [first, second],
    );
    const reverse = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [second, first],
    );

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
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
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
    const exactDuplicate = evaluateWatcherMultiProviderConsistencyV1(
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
    const sharedTrust = evaluateWatcherMultiProviderConsistencyV1(
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
    const sharedOperator = evaluateWatcherMultiProviderConsistencyV1(
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
    const result = evaluateWatcherMultiProviderConsistencyV1Raw(
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

    const accepted = evaluateWatcherMultiProviderConsistencyV1Raw(
      externalConfig(),
      detached,
      contexts,
    );
    const serializedContexts = JSON.parse(JSON.stringify(contexts));
    const forged = evaluateWatcherMultiProviderConsistencyV1Raw(
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
    const result = evaluateWatcherMultiProviderConsistencyV1(
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
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
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
    const result = evaluateWatcherMultiProviderConsistencyV1(
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

    const mismatchedContent = evaluateWatcherMultiProviderConsistencyV1(
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
    const stale = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
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

    const fork = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
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
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
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

  it("allows one watcher-operated chain-sync authority without a provider quorum", () => {
    const chainSync = localObservation("chain_sync", "cc", { depth: "19" });
    const result = evaluateWatcherMultiProviderConsistencyV1(localConfig(), [
      chainSync,
    ]);

    expect(result).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      sourceMode: "local_node",
      configuredNetwork: "Preprod",
      authorityNodeId: "watcher-node-a",
      authorityGenesisIdentitySha256: localGenesisIdentitySha256,
      authorityChainSyncSocketPath: localNodeSocketPath,
      chainAuthorityObservationDigest: chainSync.observationDigest,
      queryObservationCount: 0,
      observationCount: 1,
      independentProviderCount: 1,
      reasonCodes: ["local_node_consistent"],
      alertCodes: [],
      agreement: {
        pointDigest: chainSync.chainPoint.pointDigest,
        minimumDepth: "19",
        blockContentDigest: chainSync.blockContentDigest,
      },
    });
    expect(result.reasonCodes).not.toContain(
      "insufficient_independent_providers",
    );
  });

  it("accepts aligned query surfaces sharing the local node and ignores transport identity duplication for independence", () => {
    const chainSync = localObservation("chain_sync", "cc");
    const ogmios = localObservation("ogmios", "dd");
    const kupo = localObservation("kupo", "dd");
    const configured = localConfig([
      { kind: "ogmios", providerId: "ogmios" },
      { kind: "kupo", providerId: "kupo" },
    ]);
    const forward = evaluateWatcherMultiProviderConsistencyV1(configured, [
      chainSync,
      ogmios,
      kupo,
    ]);
    const reverse = evaluateWatcherMultiProviderConsistencyV1(configured, [
      kupo,
      ogmios,
      chainSync,
    ]);

    expect(forward).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      independentProviderCount: 1,
      queryObservationCount: 2,
      localQueryServiceBindings: [
        {
          kind: "kupo",
          providerId: "kupo",
          endpoint: localQueryEndpoint("kupo", "kupo"),
          observationStatus: "aligned",
        },
        {
          kind: "ogmios",
          providerId: "ogmios",
          endpoint: localQueryEndpoint("ogmios", "ogmios"),
          observationStatus: "aligned",
        },
      ],
      reasonCodes: ["local_node_consistent"],
      alertCodes: [],
    });
    expect(reverse).toEqual(forward);
  });

  it("accepts distinct aligned query services of the same local surface kind", () => {
    const chainSync = localObservation("chain_sync", "cc");
    const ogmiosA = localObservation("ogmios", "dd", {
      providerId: "ogmios-a",
    });
    const ogmiosB = localObservation("ogmios", "ee", {
      providerId: "ogmios-b",
    });

    const result = evaluateWatcherMultiProviderConsistencyV1(
      localConfig([
        { kind: "ogmios", providerId: "ogmios-a" },
        { kind: "ogmios", providerId: "ogmios-b" },
      ]),
      [chainSync, ogmiosA, ogmiosB],
    );

    expect(result).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      independentProviderCount: 1,
      queryObservationCount: 2,
      reasonCodes: ["local_node_consistent"],
      alertCodes: [],
    });
  });

  it("enumerates every configured local query and quarantines omitted evidence", () => {
    const result = evaluateWatcherMultiProviderConsistencyV1(
      localConfig([
        { kind: "ogmios", providerId: "ogmios" },
        { kind: "kupo", providerId: "kupo" },
      ]),
      [localObservation("chain_sync", "cc"), localObservation("ogmios", "dd")],
    );

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      queryObservationCount: 1,
      reasonCodes: ["missing_local_query_evidence"],
      alertCodes: ["watcher_local_node_query_evidence_missing"],
      localQueryServiceBindings: [
        {
          kind: "kupo",
          providerId: "kupo",
          observationStatus: "unavailable",
          observationDigest: null,
        },
        {
          kind: "ogmios",
          providerId: "ogmios",
          observationStatus: "aligned",
        },
      ],
    });
  });

  it("fails closed when local query data is stale, forked, content-mismatched, or has not propagated a rollback", () => {
    const chainSync = localObservation("chain_sync", "cc");
    const stale = evaluateWatcherMultiProviderConsistencyV1(
      localConfig([{ kind: "ogmios", providerId: "ogmios" }]),
      [
        chainSync,
        localObservation("ogmios", "dd", {
          blockHash: "22".repeat(32),
          slot: "999",
          blockNo: "99",
        }),
      ],
    );
    const fork = evaluateWatcherMultiProviderConsistencyV1(
      localConfig([{ kind: "kupo", providerId: "kupo" }]),
      [
        chainSync,
        localObservation("kupo", "ee", {
          blockHash: "22".repeat(32),
        }),
      ],
    );
    const mismatchedBytes = evaluateWatcherMultiProviderConsistencyV1(
      localConfig([{ kind: "db_sync", providerId: "db-sync" }]),
      [
        localObservation("chain_sync", "cc", { bodyHex: "a100" }),
        localObservation("db_sync", "ff", { bodyHex: "a101" }),
      ],
    );
    const rollbackNotPropagated = evaluateWatcherMultiProviderConsistencyV1(
      localConfig([{ kind: "kupmios", providerId: "kupmios" }]),
      [
        chainSync,
        localObservation("kupmios", "11", {
          blockHash: "22".repeat(32),
          slot: "1001",
          blockNo: "101",
        }),
      ],
    );

    expect(stale).toMatchObject({
      status: "quarantined",
      reasonCodes: ["stale_provider_observation"],
      alertCodes: ["watcher_provider_stale"],
    });
    expect(fork).toMatchObject({
      status: "quarantined",
      reasonCodes: ["fork_disagreement"],
      alertCodes: ["watcher_provider_fork"],
    });
    expect(mismatchedBytes).toMatchObject({
      status: "quarantined",
      reasonCodes: ["block_content_mismatch"],
      alertCodes: ["watcher_provider_content_disagreement"],
    });
    expect(rollbackNotPropagated).toMatchObject({
      status: "quarantined",
      reasonCodes: ["rollback_not_propagated"],
      alertCodes: ["watcher_local_node_rollback_not_propagated"],
    });
  });

  it("rejects local authority, genesis, source-mode, and missing chain-sync substitutions", () => {
    const wrongAuthority = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [
        localObservation("chain_sync", "cc", {
          authorityNodeId: "watcher-node-b",
        }),
      ],
    );
    const wrongGenesis = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [localObservation("chain_sync", "aa")],
    );
    const missingChainSync = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [localObservation("ogmios", "dd")],
    );
    const externalSubstitution = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );

    expect(wrongAuthority.reasonCodes).toEqual([
      "local_node_authority_mismatch",
      "missing_chain_sync_authority",
    ]);
    expect(wrongGenesis.reasonCodes).toEqual([
      "local_node_genesis_mismatch",
      "missing_chain_sync_authority",
    ]);
    expect(missingChainSync.reasonCodes).toEqual([
      "local_node_authority_mismatch",
      "missing_chain_sync_authority",
    ]);
    expect(externalSubstitution.reasonCodes).toEqual([
      "source_mode_mismatch",
      "missing_chain_sync_authority",
    ]);
    expect(
      [
        wrongAuthority,
        wrongGenesis,
        missingChainSync,
        externalSubstitution,
      ].every(({ protocolDecision }) => protocolDecision === "quarantined"),
    ).toBe(true);
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
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
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

    const malformedResult = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [malformed, foreign],
    );
    const foreignBoundary = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      new Error(secret),
    );
    const unknownNetwork = evaluateWatcherMultiProviderConsistencyV1(secret, [
      observation("provider-a", "aa"),
      observation("provider-b", "bb"),
    ]);
    const missingDiscriminator = evaluateWatcherMultiProviderConsistencyV1(
      "Preprod",
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );

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
    const result = evaluateWatcherMultiProviderConsistencyV1(
      malformedLocal,
      [],
    );

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

    const result = evaluateWatcherMultiProviderConsistencyV1(hostileConfig, [
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
