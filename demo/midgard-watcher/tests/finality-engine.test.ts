import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createServer as createNetServer, type Server } from "node:net";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  makeDeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../src/config.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
  parseWatcherFinalityPolicyV1,
  parseWatcherFinalityStateV1,
  WATCHER_FINALITY_POLICY_V1_SCHEMA_VERSION,
  WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION,
  WATCHER_FINALITY_STATE_V1_SCHEMA_VERSION,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityStateV1,
} from "../src/finality-engine.js";
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
import { evaluateWatcherMultiProviderConsistencyV1 as evaluateWatcherMultiProviderConsistencyV1Raw } from "../src/multi-provider-consistency.js";

const hex32 = (byte: string): string => byte.repeat(32);
const execFileAsync = promisify(execFile);
const observationAttestations = new WeakMap<
  object,
  WatcherL1TransportAttestationContextV1
>();
const transportContexts = new Map<
  string,
  WatcherL1TransportAttestationContextV1
>();
let transportFixtureDirectory = "";
let localTransportServer: Server | undefined;
let localQueryTransportServer: Server | undefined;
const tlsTransportServers: Server[] = [];
let localGenesisIdentitySha256 = "";
let localNodeSocketPath = "";
let localQueryEndpoint = "";
const externalEndpoints = new Map<string, string>();

const listen = async (server: Server, target: string | number): Promise<void> =>
  await new Promise((resolve, reject) => {
    const onError = (error: Error) => {
      server.off("listening", onListen);
      reject(error);
    };
    const onListen = () => {
      server.off("error", onError);
      resolve();
    };
    server.once("error", onError);
    server.once("listening", onListen);
    if (typeof target === "string") server.listen(target);
    else server.listen(target, "127.0.0.1");
  });

const closeServer = async (server: Server): Promise<void> => {
  if (!server.listening) return;
  await new Promise<void>((resolve, reject) => {
    server.close((error) => {
      if (error === undefined) resolve();
      else reject(error);
    });
  });
};

const makeTlsTransport = async (identityByte: string) => {
  const keyPath = join(transportFixtureDirectory, `${identityByte}.key`);
  const certificatePath = join(
    transportFixtureDirectory,
    `${identityByte}.crt`,
  );
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
  tlsTransportServers.push(server);
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("TLS fixture did not bind a TCP port");
  }
  return {
    certificate,
    identitySha256: createHash("sha256")
      .update(new X509Certificate(certificate).raw)
      .digest("hex"),
    port: address.port,
  };
};

const cleanupTransportFixtures = async (): Promise<void> => {
  for (const context of transportContexts.values()) {
    closeWatcherL1TransportAttestationContextV1(context);
  }
  transportContexts.clear();
  const servers = [
    ...tlsTransportServers,
    ...(localTransportServer === undefined ? [] : [localTransportServer]),
    ...(localQueryTransportServer === undefined
      ? []
      : [localQueryTransportServer]),
  ];
  tlsTransportServers.length = 0;
  localTransportServer = undefined;
  localQueryTransportServer = undefined;
  externalEndpoints.clear();
  await Promise.all(servers.map(closeServer));
  if (transportFixtureDirectory !== "") {
    await rm(transportFixtureDirectory, { recursive: true, force: true });
    transportFixtureDirectory = "";
  }
};

beforeAll(async () => {
  try {
    transportFixtureDirectory = await mkdtemp(
      join("/dev/shm", "midgard-w12-finality-"),
    );
    const nodeSocketPath = join(transportFixtureDirectory, "node.socket");
    localNodeSocketPath = nodeSocketPath;
    const genesisFilePath = join(transportFixtureDirectory, "genesis.json");
    const genesisBytes = Buffer.from('{"networkMagic":1}', "utf8");
    await writeFile(genesisFilePath, genesisBytes);
    localGenesisIdentitySha256 = createHash("sha256")
      .update(genesisBytes)
      .digest("hex");
    localTransportServer = createNetServer();
    await listen(localTransportServer, nodeSocketPath);
    const authority = await establishWatcherLocalNodeAuthorityTransportV1({
      network: "Preprod",
      authorityNodeId: "cardano-node-a",
      providerId: "cardano-node-a",
      nodeSocketPath,
      genesisFilePath,
      expectedGenesisIdentitySha256: localGenesisIdentitySha256,
      connectTimeoutMs: 2_000,
    });
    transportContexts.set("local:chain_sync", authority);
    localQueryTransportServer = createNetServer();
    await listen(localQueryTransportServer, 0);
    const queryAddress = localQueryTransportServer.address();
    if (queryAddress === null || typeof queryAddress === "string") {
      throw new Error("local query fixture did not bind a TCP port");
    }
    localQueryEndpoint = `http://127.0.0.1:${queryAddress.port.toString()}/ogmios`;
    transportContexts.set(
      "local:ogmios",
      await establishWatcherLocalNodeQueryTransportV1(authority, {
        transportKind: "tcp",
        providerId: "cardano-node-a-ogmios",
        surface: "ogmios",
        endpoint: localQueryEndpoint,
        connectTimeoutMs: 2_000,
      }),
    );

    const fixtures = new Map<
      string,
      Awaited<ReturnType<typeof makeTlsTransport>>
    >();
    for (const identityByte of ["a1", "b2", "c3", "d4", "e5"]) {
      fixtures.set(identityByte, await makeTlsTransport(identityByte));
    }
    for (const [providerId, identityByte, operatorIdentityByte] of [
      ["provider-a", "a1", "a1"],
      ["provider-b", "b2", "b2"],
      ["provider-a", "c3", "a1"],
      ["provider-a", "c3", "d4"],
      ["provider-b", "e5", "f6"],
      ["provider-c", "c3", "c3"],
      ["provider-d", "d4", "d4"],
      ["provider-x", "c3", "d4"],
      ["provider-y", "e5", "f6"],
    ] as const) {
      const fixture = fixtures.get(identityByte)!;
      const endpoint = `https://localhost:${fixture.port.toString()}/${providerId}`;
      externalEndpoints.set(
        `${providerId}:${identityByte}:${operatorIdentityByte}`,
        endpoint,
      );
      transportContexts.set(
        `external:${providerId}:${identityByte}:${operatorIdentityByte}`,
        await establishWatcherExternalProviderTransportV1({
          network: "Preprod",
          providerId,
          operatorIdentitySha256: hex32(operatorIdentityByte),
          endpoint,
          caPem: fixture.certificate,
          expectedTlsPublicIdentitySha256: fixture.identitySha256,
          connectTimeoutMs: 2_000,
        }),
      );
    }
  } catch (error) {
    await cleanupTransportFixtures();
    throw error;
  }
}, 60_000);

afterAll(async () => {
  await cleanupTransportFixtures();
}, 30_000);
const externalSource = () =>
  ({
    sourceMode: "external_providers",
    network: "Preprod",
    providers: [
      {
        providerId: "provider-a",
        operatorIdentitySha256: hex32("a1"),
        endpoint: externalEndpoints.get("provider-a:a1:a1")!,
      },
      {
        providerId: "provider-b",
        operatorIdentitySha256: hex32("b2"),
        endpoint: externalEndpoints.get("provider-b:b2:b2")!,
      },
    ],
  }) as const;

const config = (depth = 3, rollbackDepth = depth) => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "development",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: hex32("a1"),
          endpoint: externalEndpoints.get("provider-a:a1:a1")!,
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: hex32("b2"),
          endpoint: externalEndpoints.get("provider-b:b2:b2")!,
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
    finality: {
      depth,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: rollbackDepth,
      },
    },
  },
  da: {
    peers: [
      {
        identity: "da-peer-a",
        multiaddr:
          "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
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
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});

const localConfig = (depth = 3, rollbackDepth = depth) => {
  const base = config(depth, rollbackDepth);
  return {
    ...base,
    l1: {
      ...base.l1,
      source: {
        sourceMode: "local_node" as const,
        authorityNodeId: "cardano-node-a",
        chainSync: {
          kind: "cardano_node_socket" as const,
          socketPath: localNodeSocketPath,
          genesisIdentitySha256: localGenesisIdentitySha256,
        },
        queryServices: [],
      },
    },
  };
};

const deploymentIdentity = (
  manifestByte = "11",
  releaseByte = "22",
  network: "Mainnet" | "Preprod" | "Preview" = "Preprod",
) => ({
  manifestId: hex32(manifestByte),
  network,
  trustRootId: hex32("33"),
  releaseEvidenceDigest: hex32(releaseByte),
  ruleBundleCommitment: hex32("44"),
  programCommitments: { validation: hex32("55") },
  durableMarker: makeDeploymentMarkerV1(hex32(manifestByte)),
});

const policy = (
  depth = 3,
  manifestByte = "11",
  releaseByte = "22",
  rollbackDepth = depth,
): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(
    config(depth, rollbackDepth),
    deploymentIdentity(manifestByte, releaseByte),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

const localPolicy = (depth = 3): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(
    localConfig(depth),
    deploymentIdentity(),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

const provider = (
  providerId: string,
  identityByte: string,
  operatorIdentityByte = identityByte,
) =>
  transportContexts.get(
    `external:${providerId}:${identityByte}:${operatorIdentityByte}`,
  )!;

const localNodeProvider = (surface: "chain_sync" | "ogmios" = "chain_sync") =>
  transportContexts.get(`local:${surface}`)!;

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

type ObservationOptions = Readonly<{
  blockHash?: string;
  parentBlockHash?: string | null;
  slot?: string;
  blockNo?: string;
  depth?: string;
  bodyHex?: string;
  operatorIdentityByte?: string;
}>;

const observation = (
  providerId: string,
  identityByte: string,
  options: ObservationOptions = {},
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
      blockHash: options.blockHash ?? hex32("aa"),
      parentBlockHash: options.parentBlockHash ?? null,
      slot: options.slot ?? "1000",
      blockNo: options.blockNo ?? "100",
      depth: options.depth ?? "0",
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
        if (typeof candidate !== "object" || candidate === null) return [];
        const attestation = observationAttestations.get(candidate);
        return attestation === undefined ? [] : [attestation];
      })
    : [];
  return evaluateWatcherMultiProviderConsistencyV1Raw(
    configuredSource,
    observations,
    explicitAttestations ?? [...new Set(inferred)],
  );
};

const agreement = (
  depth: string,
  options: ObservationOptions = {},
  reverse = false,
) => {
  const observations = [
    observation("provider-a", "a1", { ...options, depth }),
    observation("provider-b", "b2", { ...options, depth }),
  ];
  return evaluateWatcherMultiProviderConsistencyV1(
    externalSource(),
    reverse ? observations.reverse() : observations,
  );
};

const localAgreement = (
  depth: string,
  options: ObservationOptions = {},
  includeAlignedQuery = false,
  includeQueryObservation = includeAlignedQuery,
) => {
  const normalized = (
    surface: "chain_sync" | "ogmios",
  ): WatcherNormalizedL1BlockV1 => {
    const attestation = localNodeProvider(surface);
    const value = normalizeWatcherL1BlockV1(attestation, {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
      network: "Preprod",
      providerId:
        surface === "chain_sync"
          ? "cardano-node-a"
          : `cardano-node-a-${surface}`,
      chainPoint: {
        blockHash: options.blockHash ?? hex32("aa"),
        parentBlockHash: options.parentBlockHash ?? null,
        slot: options.slot ?? "1000",
        blockNo: options.blockNo ?? "100",
        depth,
      },
      transactions:
        options.bodyHex === undefined ? [] : [transaction(options.bodyHex)],
    });
    observationAttestations.set(value, attestation);
    return value;
  };
  return evaluateWatcherMultiProviderConsistencyV1(
    {
      sourceMode: "local_node",
      network: "Preprod",
      authorityNodeId: "cardano-node-a",
      genesisIdentitySha256: localGenesisIdentitySha256,
      chainSyncSocketPath: localNodeSocketPath,
      queryServices: includeAlignedQuery
        ? [
            {
              kind: "ogmios" as const,
              providerId: "cardano-node-a-ogmios",
              endpoint: localQueryEndpoint,
            },
          ]
        : [],
    },
    [
      normalized("chain_sync"),
      ...(includeQueryObservation ? [normalized("ogmios")] : []),
    ],
  );
};

const pendingAt = (
  finalityPolicy: WatcherFinalityPolicyV1,
  depth: string,
  options: ObservationOptions = {},
): WatcherFinalityStateV1 => {
  const result = evaluateWatcherFinalityV1(
    finalityPolicy,
    null,
    agreement(depth, options),
  );
  expect(result.action).toBe("observe_pending");
  return result.state as WatcherFinalityStateV1;
};

const finalizeAtThreshold = (
  finalityPolicy: WatcherFinalityPolicyV1,
  options: ObservationOptions = {},
): WatcherFinalityStateV1 => {
  const pending = pendingAt(finalityPolicy, "2", options);
  const result = evaluateWatcherFinalityV1(
    finalityPolicy,
    pending,
    agreement("3", options),
  );
  expect(result.action).toBe("finalize");
  return result.state as WatcherFinalityStateV1;
};

describe("canonical release-bound watcher finality", () => {
  it("binds W01 finality to the verified release and deployment marker", () => {
    const value = policy();

    expect(value).toMatchObject({
      schemaVersion: WATCHER_FINALITY_POLICY_V1_SCHEMA_VERSION,
      network: "Preprod",
      sourceMode: "external_providers",
      authorityNodeId: null,
      authorityGenesisIdentitySha256: null,
      authorityChainSyncSocketPath: null,
      externalProviders: [
        {
          providerId: "provider-a",
          operatorIdentitySha256: hex32("a1"),
          endpoint: externalEndpoints.get("provider-a:a1:a1"),
          authenticationKind: "https_tls_identity_v1",
        },
        {
          providerId: "provider-b",
          operatorIdentitySha256: hex32("b2"),
          endpoint: externalEndpoints.get("provider-b:b2:b2"),
          authenticationKind: "https_tls_identity_v1",
        },
      ],
      confirmationDepth: "3",
      maximumPreFinalityRollbackDepth: "3",
      maximumPostFinalityRecoveryDepth: "2160",
      beforeFinalityRollback: "rewind",
      afterFinalityRollback: "quarantine",
      releaseEvidenceDigest: hex32("22"),
      deploymentMarker: {
        schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
        manifestId: hex32("11"),
      },
    });
    expect(value.policyDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(parseWatcherFinalityPolicyV1(value)).toEqual(value);
    expect(Object.isFrozen(value)).toBe(true);
    expect(
      parseWatcherFinalityPolicyV1({
        ...value,
        maximumPostFinalityRecoveryDepth: "2161",
      }),
    ).toBeNull();
    const { policyDigest: _policyDigest, ...narrowedRecoveryPolicyCanonical } =
      {
        ...value,
        maximumPostFinalityRecoveryDepth: "2159",
      };
    expect(
      parseWatcherFinalityPolicyV1({
        ...narrowedRecoveryPolicyCanonical,
        policyDigest: createHash("sha256")
          .update(JSON.stringify(narrowedRecoveryPolicyCanonical), "utf8")
          .digest("hex"),
      }),
    ).toBeNull();

    const changedEndpointConfig = config();
    changedEndpointConfig.l1.source.providers[0]!.endpoint =
      "https://cardano-a-new.example";
    const changedEndpointPolicy = makeWatcherFinalityPolicyV1(
      changedEndpointConfig,
      deploymentIdentity(),
    );
    expect(changedEndpointPolicy).not.toBeNull();
    expect(changedEndpointPolicy?.policyDigest).not.toBe(value.policyDigest);

    const reorderedConfig = config();
    reorderedConfig.l1.source.providers.reverse();
    expect(
      makeWatcherFinalityPolicyV1(reorderedConfig, deploymentIdentity()),
    ).toEqual(value);

    const unauthenticatedTransportConfig = config();
    unauthenticatedTransportConfig.l1.source.providers[0]!.endpoint =
      "http://127.0.0.1:1442";
    expect(
      makeWatcherFinalityPolicyV1(
        unauthenticatedTransportConfig,
        deploymentIdentity(),
      ),
    ).toBeNull();

    expect(localPolicy()).toMatchObject({
      sourceMode: "local_node",
      authorityNodeId: "cardano-node-a",
      authorityGenesisIdentitySha256: localGenesisIdentitySha256,
      authorityChainSyncSocketPath: localNodeSocketPath,
    });
  });

  it("rejects configuration/deployment mismatches without emitting values", () => {
    const wrongNetwork = makeWatcherFinalityPolicyV1(
      config(),
      deploymentIdentity("11", "22", "Preview"),
    );
    const malformedMarker = {
      ...deploymentIdentity(),
      durableMarker: makeDeploymentMarkerV1(hex32("99")),
    };

    expect(wrongNetwork).toBeNull();
    expect(makeWatcherFinalityPolicyV1(config(), malformedMarker)).toBeNull();
  });

  it("accepts one authoritative local-node observation and rejects source substitution", () => {
    const finalityPolicy = localPolicy();
    const firstConsistency = localAgreement("2");
    const first = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      firstConsistency,
    );
    const finalized = evaluateWatcherFinalityV1(
      finalityPolicy,
      first.state,
      localAgreement("3"),
    );
    const externalSubstitution = evaluateWatcherFinalityV1(
      finalityPolicy,
      first.state,
      agreement("3"),
    );
    const foreignConfig = localConfig();
    const foreignPolicy = makeWatcherFinalityPolicyV1(
      {
        ...foreignConfig,
        l1: {
          ...foreignConfig.l1,
          source: {
            ...foreignConfig.l1.source,
            authorityNodeId: "cardano-node-b",
            chainSync: {
              ...foreignConfig.l1.source.chainSync,
              genesisIdentitySha256: hex32("c3"),
            },
          },
        },
      },
      deploymentIdentity(),
    );

    expect(firstConsistency).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      sourceMode: "local_node",
      authorityNodeId: "cardano-node-a",
      authorityGenesisIdentitySha256: localGenesisIdentitySha256,
      authorityChainSyncSocketPath: localNodeSocketPath,
      observationCount: 1,
      independentProviderCount: 1,
      queryObservationCount: 0,
      reasonCodes: ["local_node_consistent"],
    });
    expect(first).toMatchObject({
      action: "observe_pending",
      protocolDecision: "hold",
    });
    expect(finalized).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
      state: { phase: "finalized" },
    });
    expect(externalSubstitution).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_mode_mismatch"],
      state: first.state,
    });
    expect(
      evaluateWatcherFinalityV1(foreignPolicy, null, firstConsistency),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["source_authority_mismatch"],
    });
  });

  it("rejects agreement from two distinct providers outside the W01 allowlist", () => {
    const finalityPolicy = policy();
    const hostileAgreement = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [
        observation("provider-x", "c3", {
          operatorIdentityByte: "d4",
        }),
        observation("provider-y", "e5", {
          operatorIdentityByte: "f6",
        }),
      ],
    );

    expect(hostileAgreement.status).toBe("quarantined");
    expect(
      evaluateWatcherFinalityV1(finalityPolicy, null, hostileAgreement),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: { phase: "unobserved" },
    });
  });

  it("rejects configured provider labels with substituted operator identities", () => {
    const hostileAgreement = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [
        observation("provider-a", "c3", {
          operatorIdentityByte: "d4",
        }),
        observation("provider-b", "e5", {
          operatorIdentityByte: "f6",
        }),
      ],
    );

    expect(
      evaluateWatcherFinalityV1(policy(), null, hostileAgreement),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
    });
  });

  it("rejects an otherwise valid W11 agreement bound to another configured provider set", () => {
    const foreign = evaluateWatcherMultiProviderConsistencyV1(
      {
        sourceMode: "external_providers",
        network: "Preprod",
        providers: [
          {
            providerId: "provider-c",
            operatorIdentitySha256: hex32("c3"),
            endpoint: externalEndpoints.get("provider-c:c3:c3"),
          },
          {
            providerId: "provider-d",
            operatorIdentitySha256: hex32("d4"),
            endpoint: externalEndpoints.get("provider-d:d4:d4"),
          },
        ],
      },
      [
        observation("provider-c", "c3", { depth: "3" }),
        observation("provider-d", "d4", { depth: "3" }),
      ],
    );
    expect(foreign.status).toBe("agreed");
    expect(evaluateWatcherFinalityV1(policy(), null, foreign)).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_authority_mismatch"],
    });
  });

  it("keeps first visibility pending even when already above the threshold", () => {
    const first = evaluateWatcherFinalityV1(policy(), null, agreement("8"));

    expect(first).toMatchObject({
      schemaVersion: WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION,
      action: "observe_pending",
      protocolDecision: "hold",
      reasonCodes: ["first_visibility_pending"],
      alertCodes: ["watcher_finality_pending"],
      state: {
        schemaVersion: WATCHER_FINALITY_STATE_V1_SCHEMA_VERSION,
        phase: "pending",
        pending: {
          firstSeenDepth: "8",
          currentDepth: "8",
          visibilityCount: "1",
        },
      },
    });
    expect(first.resultDigest).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("finalizes threshold-1 to threshold exactly once", () => {
    const finalityPolicy = policy();
    const pending = pendingAt(finalityPolicy, "2");
    const finalized = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("3"),
    );
    const later = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized.state,
      agreement("4"),
    );

    expect(finalized).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
      reasonCodes: ["confirmation_depth_reached"],
      alertCodes: [],
      state: {
        phase: "finalized",
        pending: null,
        finalized: {
          firstSeenDepth: "2",
          currentDepth: "3",
          visibilityCount: "2",
        },
      },
    });
    expect(later).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["already_finalized"],
      state: finalized.state,
    });
  });

  it("makes exact duplicates idempotent before and after restart", () => {
    const finalityPolicy = policy();
    const evidence = agreement("1");
    const first = evaluateWatcherFinalityV1(finalityPolicy, null, evidence);
    const restarted = JSON.parse(
      JSON.stringify(first.state),
    ) as WatcherFinalityStateV1;
    const duplicate = evaluateWatcherFinalityV1(
      finalityPolicy,
      restarted,
      evidence,
    );

    expect(parseWatcherFinalityStateV1(restarted)).toEqual(first.state);
    expect(duplicate).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_observation"],
      state: first.state,
    });
    expect(duplicate.state?.stateDigest).toBe(first.state?.stateDigest);
  });

  it("advances below-threshold depth without irreversible state", () => {
    const finalityPolicy = policy(4);
    const pending = pendingAt(finalityPolicy, "1");
    const advanced = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2"),
    );

    expect(advanced).toMatchObject({
      action: "advance_pending",
      protocolDecision: "hold",
      reasonCodes: ["pending_depth_progress", "confirmation_depth_pending"],
      state: { phase: "pending", pending: { currentDepth: "2" } },
    });
  });

  it("emits a deterministic rewind for a pre-finality depth regression", () => {
    const finalityPolicy = policy(5);
    const pending = pendingAt(finalityPolicy, "3");
    const rewound = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2"),
    );
    const replay = evaluateWatcherFinalityV1(
      finalityPolicy,
      JSON.parse(JSON.stringify(pending)),
      agreement("2", {}, true),
    );

    expect(rewound).toMatchObject({
      action: "rewind_pending",
      protocolDecision: "rewind_required",
      reasonCodes: ["pending_depth_regression"],
      alertCodes: [
        "watcher_finality_pending",
        "watcher_finality_rewind_required",
      ].reverse(),
      state: { phase: "pending", pending: { currentDepth: "2" } },
      rewindInstruction: {
        kind: "pending_depth_regression",
        discardedStateDigest: pending.stateDigest,
        replacementDepth: "2",
      },
    });
    expect(replay).toEqual(rewound);
  });

  it("emits explicit rewinds for pre-finality fork and content mutation", () => {
    const finalityPolicy = policy(5);
    const pending = pendingAt(finalityPolicy, "1");
    const fork = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2", { bodyHex: "a100" }),
    );

    expect(fork).toMatchObject({
      action: "rewind_pending",
      reasonCodes: ["pending_point_changed"],
      rewindInstruction: { kind: "pending_point_changed" },
    });
    expect(content).toMatchObject({
      action: "rewind_pending",
      reasonCodes: ["pending_content_changed"],
      rewindInstruction: { kind: "pending_content_changed" },
    });
  });

  it("enforces the exact pre-finality rollback bound and adjacent excess", () => {
    const finalityPolicy = policy(5, "11", "22", 2);
    const depthTwo = pendingAt(finalityPolicy, "2");
    const exactDepth = evaluateWatcherFinalityV1(
      finalityPolicy,
      depthTwo,
      agreement("0"),
    );
    const depthThree = pendingAt(finalityPolicy, "3");
    const adjacentExcess = evaluateWatcherFinalityV1(
      finalityPolicy,
      depthThree,
      agreement("0"),
    );
    const exactFork = evaluateWatcherFinalityV1(
      finalityPolicy,
      pendingAt(finalityPolicy, "1"),
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const excessiveFork = evaluateWatcherFinalityV1(
      finalityPolicy,
      depthTwo,
      agreement("3", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );

    expect(exactDepth).toMatchObject({
      action: "rewind_pending",
      protocolDecision: "rewind_required",
      reasonCodes: ["pending_depth_regression"],
    });
    expect(adjacentExcess).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["pre_finality_rollback_depth_exceeded"],
      alertCodes: [
        "watcher_finality_input_rejected",
        "watcher_finality_rollback_limit_exceeded",
      ],
      state: depthThree,
    });
    expect(exactFork.action).toBe("rewind_pending");
    expect(excessiveFork.reasonCodes).toEqual([
      "pre_finality_rollback_depth_exceeded",
    ]);
  });

  it("fails closed when same-depth evidence arrives over a substituted endpoint", () => {
    const finalityPolicy = policy();
    const pending = pendingAt(finalityPolicy, "1");
    const freshTransportAgreement = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [
        observation("provider-a", "c3", {
          depth: "1",
          operatorIdentityByte: "a1",
        }),
        observation("provider-b", "b2", { depth: "1" }),
      ],
    );
    const stale = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      freshTransportAgreement,
    );

    expect(stale).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      state: pending,
    });
  });

  it("rejects W11 pending and quarantine without advancing state", () => {
    const finalityPolicy = policy();
    const initialPending = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [
        observation("provider-a", "a1", { depth: "0" }),
        observation("provider-b", "b2", {
          blockHash: hex32("bb"),
          slot: "1001",
          blockNo: "101",
          depth: "0",
        }),
      ],
    );
    const quarantined = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [observation("provider-a", "a1")],
    );

    expect(
      evaluateWatcherFinalityV1(finalityPolicy, null, initialPending),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_pending"],
      state: { phase: "unobserved" },
    });
    expect(
      evaluateWatcherFinalityV1(finalityPolicy, null, quarantined),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      state: { phase: "unobserved" },
    });
  });

  it("holds same-point post-finality depth regression without an incident and resumes on recovered depth", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const rolledBack = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      agreement("2"),
    );

    expect(rolledBack).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["post_finality_depth_regression"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: finalized,
    });
    expect(rolledBack.state?.phase).toBe("finalized");
    expect(rolledBack.state?.incident).toBeNull();
    expect(
      evaluateWatcherFinalityV1(
        finalityPolicy,
        rolledBack.state,
        agreement("4"),
      ),
    ).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["already_finalized"],
      state: finalized,
    });
  });

  it("opens incidents only for agreed point replacement and keeps same-point content or transient disagreement nonterminal", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const point = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      agreement("4", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      agreement("4", { bodyHex: "a100" }),
    );
    const pendingW11 = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [
        observation("provider-a", "a1"),
        observation("provider-b", "b2", {
          blockHash: hex32("bb"),
          slot: "1001",
          blockNo: "101",
        }),
      ],
    );
    const quasiRollback = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      pendingW11,
    );

    expect(point.reasonCodes).toContain("post_finality_point_changed");
    expect(content).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["post_finality_content_changed"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: finalized,
    });
    expect(quasiRollback).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_pending"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: finalized,
    });
    expect(point.state?.finalized).toEqual(finalized.finalized);
    expect(content.state).toEqual(finalized);
    expect(quasiRollback.state).toEqual(finalized);
    expect(
      evaluateWatcherFinalityV1(
        finalityPolicy,
        quasiRollback.state,
        agreement("4"),
      ),
    ).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["already_finalized"],
      state: finalized,
    });
  });

  it("keeps malformed input, bounded external lag, and local query unavailability transient after finality", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const boundedLag = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [
        observation("provider-a", "a1", { depth: "4" }),
        observation("provider-b", "b2", {
          blockHash: hex32("bb"),
          slot: "1001",
          blockNo: "101",
          depth: "0",
        }),
      ],
    );
    expect(boundedLag).toMatchObject({
      status: "pending",
      reasonCodes: ["bounded_provider_lag"],
    });
    for (const transient of [new Error("malformed"), boundedLag]) {
      const held = evaluateWatcherFinalityV1(
        finalityPolicy,
        finalized,
        transient,
      );
      expect(held).toMatchObject({
        action: "reject",
        protocolDecision: "quarantined",
        state: finalized,
      });
      expect(held.state?.phase).toBe("finalized");
      expect(held.state?.incident).toBeNull();
      expect(
        evaluateWatcherFinalityV1(finalityPolicy, held.state, agreement("4")),
      ).toMatchObject({
        action: "duplicate",
        protocolDecision: "hold",
        state: finalized,
      });
    }

    const localBase = localConfig();
    const localQueryPolicy = makeWatcherFinalityPolicyV1(
      {
        ...localBase,
        l1: {
          ...localBase.l1,
          source: {
            ...localBase.l1.source,
            queryServices: [
              {
                kind: "ogmios",
                identity: "cardano-node-a-ogmios",
                endpoint: localQueryEndpoint,
              },
            ],
          },
        },
      },
      deploymentIdentity(),
    ) as WatcherFinalityPolicyV1;
    expect(localQueryPolicy).not.toBeNull();
    const localPending = evaluateWatcherFinalityV1(
      localQueryPolicy,
      null,
      localAgreement("2", {}, true),
    ).state;
    const localFinalized = evaluateWatcherFinalityV1(
      localQueryPolicy,
      localPending,
      localAgreement("3", {}, true),
    ).state as WatcherFinalityStateV1;
    const unavailable = localAgreement("3", {}, true, false);
    expect(unavailable).toMatchObject({
      status: "quarantined",
      reasonCodes: ["missing_local_query_evidence"],
    });
    const held = evaluateWatcherFinalityV1(
      localQueryPolicy,
      localFinalized,
      unavailable,
    );
    expect(held).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      state: localFinalized,
    });
    expect(held.state?.incident).toBeNull();
    expect(
      evaluateWatcherFinalityV1(
        localQueryPolicy,
        held.state,
        localAgreement("4", {}, true),
      ),
    ).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      state: localFinalized,
    });
  });

  it("rejects stale policy state, deployment, and release bindings", () => {
    const originalPolicy = policy(3);
    const state = pendingAt(originalPolicy, "1");
    const stalePolicy = policy(4);
    const otherDeployment = policy(3, "99");
    const otherRelease = policy(3, "11", "99");

    expect(
      evaluateWatcherFinalityV1(stalePolicy, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["stale_state"],
      state: null,
    });
    expect(
      evaluateWatcherFinalityV1(otherDeployment, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["deployment_mismatch"],
      state: null,
    });
    expect(
      evaluateWatcherFinalityV1(otherRelease, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["release_evidence_mismatch"],
      state: null,
    });
  });

  it("rejects self-hashed restart states that bypass finality semantics", () => {
    const finalityPolicy = policy(5);
    const finalized = finalizeAtThreshold(policy());
    const impossible = structuredClone(finalized) as Record<string, unknown>;
    const bound = impossible.finalized as Record<string, unknown>;
    bound.firstSeenDepth = "0";
    bound.currentDepth = "0";
    bound.visibilityCount = "1";
    const canonical = { ...impossible };
    delete canonical.stateDigest;
    impossible.stateDigest = createHash("sha256")
      .update(JSON.stringify(canonical), "utf8")
      .digest("hex");

    expect(parseWatcherFinalityStateV1(impossible)).not.toBeNull();
    expect(parseWatcherFinalityStateV1(impossible, finalityPolicy)).toBeNull();
    expect(
      evaluateWatcherFinalityV1(finalityPolicy, impossible, agreement("6")),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["stale_state"],
      state: null,
    });

    const samePolicyImpossible = {
      ...impossible,
      policyDigest: finalityPolicy.policyDigest,
      releaseEvidenceDigest: finalityPolicy.releaseEvidenceDigest,
      deploymentMarker: finalityPolicy.deploymentMarker,
    } as Record<string, unknown>;
    const samePolicyCanonical = {
      ...samePolicyImpossible,
    } as Record<string, unknown>;
    delete samePolicyCanonical.stateDigest;
    samePolicyImpossible.stateDigest = createHash("sha256")
      .update(JSON.stringify(samePolicyCanonical), "utf8")
      .digest("hex");
    expect(
      evaluateWatcherFinalityV1(
        finalityPolicy,
        samePolicyImpossible,
        agreement("6"),
      ),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["invalid_state_semantics"],
      state: null,
    });
  });

  it("is deterministic across provider order, restart, and input key order", () => {
    const finalityPolicy = policy();
    const forwardEvidence = agreement("1");
    const reverseEvidence = agreement("1", {}, true);
    const forward = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      forwardEvidence,
    );
    const reorderedPolicy = Object.fromEntries(
      Object.entries(finalityPolicy).reverse(),
    );
    const reverse = evaluateWatcherFinalityV1(
      reorderedPolicy,
      null,
      reverseEvidence,
    );

    expect(reverseEvidence).toEqual(forwardEvidence);
    expect(reverse).toEqual(forward);
    expect(reverse.resultDigest).toBe(forward.resultDigest);
  });

  it("rejects malformed, unsafe, unknown, and uint64-overflow inputs", () => {
    const finalityPolicy = policy();
    const unsafe = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(unsafe, "status", {
      enumerable: true,
      get: () => {
        throw new Error("not public");
      },
    });
    const unknown = { ...agreement("1"), unknown: true };
    const overflow = structuredClone(agreement("1")) as Record<string, unknown>;
    (overflow.agreement as Record<string, unknown>).minimumDepth =
      "18446744073709551616";
    const withoutDigest = { ...overflow };
    delete withoutDigest.consistencyDigest;
    overflow.consistencyDigest = createHash("sha256")
      .update(JSON.stringify(withoutDigest), "utf8")
      .digest("hex");

    for (const malformed of [unsafe, unknown, overflow, new Error("no")]) {
      expect(
        evaluateWatcherFinalityV1(finalityPolicy, null, malformed),
      ).toMatchObject({
        action: "reject",
        protocolDecision: "quarantined",
        reasonCodes: ["malformed_provider_result"],
        state: { phase: "unobserved" },
      });
    }
  });

  it("uses value-free diagnostics for secret-bearing malformed inputs", () => {
    const secret = "postgres://operator:super-secret@example.invalid/watcher";
    const unsafePolicy = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(unsafePolicy, "releaseEvidenceDigest", {
      enumerable: true,
      get: () => {
        throw new Error(secret);
      },
    });
    const unsafeState = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(unsafeState, "stateDigest", {
      enumerable: true,
      get: () => {
        throw new Error(secret);
      },
    });
    const policyFailure = evaluateWatcherFinalityV1(
      unsafePolicy,
      null,
      new Error(secret),
    );
    const stateFailure = evaluateWatcherFinalityV1(
      policy(),
      unsafeState,
      new Error(secret),
    );

    expect(policyFailure.reasonCodes).toEqual(["malformed_policy"]);
    expect(stateFailure.reasonCodes).toEqual(["malformed_state"]);
    expect(JSON.stringify([policyFailure, stateFailure])).not.toContain(secret);
  });
});
