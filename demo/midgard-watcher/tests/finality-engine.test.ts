import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  makeDeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { WATCHER_CONFIG_SCHEMA_VERSION } from "../src/config.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
  parseWatcherFinalityPolicyV1,
  parseWatcherFinalityStateV1,
  WATCHER_FINALITY_POLICY_V1_SCHEMA_VERSION,
  WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION,
  WATCHER_FINALITY_STATE_V1_SCHEMA_VERSION,
  watcherFinalityConfiguredSourceV1,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityStateV1,
} from "../src/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContextV1,
  establishWatcherExternalProviderTransportV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContextV1,
  type WatcherNormalizedL1BlockV1,
} from "../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 as evaluateWatcherMultiProviderConsistencyV1Raw } from "../src/multi-provider-consistency.js";

const hex32 = (byte: string): string => byte.repeat(32);
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
const tlsTransportServers: Server[] = [];
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
  const servers = [...tlsTransportServers];
  tlsTransportServers.length = 0;
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
          "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
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

// A policy over an explicit configured provider set, so a test can name a
// provider allowlist that is strictly wider than the set a W11 record binds.
const policyOverProviders = (
  providers: readonly (readonly [string, string])[],
  depth = 3,
): WatcherFinalityPolicyV1 => {
  const base = config(depth, depth);
  const value = makeWatcherFinalityPolicyV1(
    {
      ...base,
      l1: {
        ...base.l1,
        source: {
          sourceMode: "external_providers",
          providers: providers.map(([identity, identityByte]) => ({
            identity,
            operatorIdentitySha256: hex32(identityByte),
            endpoint: externalEndpoints.get(
              `${identity}:${identityByte}:${identityByte}`,
            )!,
          })),
        },
      },
    },
    deploymentIdentity(),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

// Re-stamps a genuine W11 record onto another policy's configured source, so
// the source-authority binding cannot mask the provider-coverage question.
// Everything else in the record - bindings, counts, evidence, agreement - is
// exactly what W11 produced. Against a policy the record already matches this
// is the identity function, which the fully bound control asserts.
const rebindConsistencyToPolicy = (
  record: unknown,
  targetPolicy: WatcherFinalityPolicyV1,
): Record<string, unknown> => {
  const rebound: Record<string, unknown> = {
    ...(record as Record<string, unknown>),
    configuredSourceDigest: sha256CanonicalForTest(
      watcherFinalityConfiguredSourceV1(targetPolicy),
    ),
  };
  delete rebound.consistencyDigest;
  return { ...rebound, consistencyDigest: sha256CanonicalForTest(rebound) };
};

const provider = (
  providerId: string,
  identityByte: string,
  operatorIdentityByte = identityByte,
) =>
  transportContexts.get(
    `external:${providerId}:${identityByte}:${operatorIdentityByte}`,
  )!;

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
        policyDigest: sha256CanonicalForTest(narrowedRecoveryPolicyCanonical),
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
  });

  it("derives the local-node finality authority and Kupmios bindings from W01 config", () => {
    const base = config();
    const value = makeWatcherFinalityPolicyV1(
      {
        ...base,
        l1: {
          ...base.l1,
          source: {
            sourceMode: "local_node",
            authorityNodeId: "watcher-node",
            chainSync: {
              kind: "cardano_node_socket",
              socketPath: "/run/cardano/node.socket",
              nodeConfigPath: "/etc/cardano/node-config.json",
              genesisConfigPath: "/etc/cardano/shelley-genesis.json",
              genesisIdentitySha256: hex32("66"),
            },
            queryServices: [
              {
                kind: "ogmios",
                identity: "local-ogmios",
                endpoint: "ws://127.0.0.1:1337",
              },
              {
                kind: "kupo",
                identity: "local-kupo",
                endpoint: "http://127.0.0.1:1442",
              },
            ],
          },
        },
      },
      deploymentIdentity(),
    );

    expect(value).toMatchObject({
      sourceMode: "local_node",
      authorityNodeId: "watcher-node",
      authorityGenesisIdentitySha256: hex32("66"),
      authorityChainSyncSocketPath: "/run/cardano/node.socket",
      externalProviders: null,
      localQueryServices: [
        {
          kind: "kupo",
          providerId: "local-kupo",
          endpoint: "http://127.0.0.1:1442",
        },
        {
          kind: "ogmios",
          providerId: "local-ogmios",
          endpoint: "ws://127.0.0.1:1337",
        },
      ],
    });
    expect(parseWatcherFinalityPolicyV1(value)).toEqual(value);
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

  it("refuses finality while a configured external provider is unbound", () => {
    // provider-c is in the W01 allowlist and never observed. The W11 record is
    // a genuine two-provider agreement over provider-a/provider-b; every
    // binding it carries matches the policy, so `.every` over the binding list
    // is true and used to say nothing at all about provider-c.
    const finalityPolicy = policyOverProviders([
      ["provider-a", "a1"],
      ["provider-b", "b2"],
      ["provider-c", "c3"],
    ]);
    const belowThreshold = rebindConsistencyToPolicy(
      agreement("2"),
      finalityPolicy,
    );
    const atThreshold = rebindConsistencyToPolicy(
      agreement("3"),
      finalityPolicy,
    );
    expect(belowThreshold.status).toBe("agreed");
    expect((belowThreshold.externalProviderBindings as unknown[]).length).toBe(
      2,
    );
    expect(finalityPolicy.externalProviders).toHaveLength(3);

    const first = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      belowThreshold,
    );
    const second = evaluateWatcherFinalityV1(
      finalityPolicy,
      first.state,
      atThreshold,
    );

    expect(second.protocolDecision).toBe("quarantined");
    expect(second.action).toBe("reject");
    expect(second.reasonCodes).toEqual(["source_provider_binding_unrun"]);
    expect(first).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_provider_binding_unrun"],
      alertCodes: [
        "watcher_finality_input_rejected",
        "watcher_finality_configuration_mismatch",
      ],
    });
    expect(first.state?.phase).toBe("unobserved");
    expect(second.state?.phase).toBe("unobserved");
  });

  it("refuses finality when only part of the configured provider set ran", () => {
    // Half the allowlist - provider-c and provider-d - never ran, so the
    // operator, TLS-identity, and endpoint binding for both is unevaluated.
    const finalityPolicy = policyOverProviders([
      ["provider-a", "a1"],
      ["provider-b", "b2"],
      ["provider-c", "c3"],
      ["provider-d", "d4"],
    ]);
    const partial = rebindConsistencyToPolicy(agreement("8"), finalityPolicy);

    expect(finalityPolicy.externalProviders).toHaveLength(4);
    expect(
      evaluateWatcherFinalityV1(finalityPolicy, null, partial),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_provider_binding_unrun"],
    });
  });

  it("grants finality when every configured external provider is bound", () => {
    const finalityPolicy = policyOverProviders([
      ["provider-a", "a1"],
      ["provider-b", "b2"],
    ]);
    const belowThreshold = agreement("2");
    const atThreshold = agreement("3");

    // The re-stamp is the identity function on a record the policy already
    // matches: the rejections above differ only in the configured provider set.
    expect(rebindConsistencyToPolicy(atThreshold, finalityPolicy)).toEqual(
      atThreshold,
    );
    expect(finalityPolicy.policyDigest).toBe(policy().policyDigest);

    const first = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      belowThreshold,
    );
    const second = evaluateWatcherFinalityV1(
      finalityPolicy,
      first.state,
      atThreshold,
    );

    expect(first.action).toBe("observe_pending");
    expect(second).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
      reasonCodes: ["confirmation_depth_reached"],
      alertCodes: [],
      state: { phase: "finalized" },
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

  it("keeps malformed input and bounded external lag transient after finality", () => {
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
    impossible.stateDigest = sha256CanonicalForTest(canonical);

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
    samePolicyImpossible.stateDigest =
      sha256CanonicalForTest(samePolicyCanonical);
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
    const reverse = evaluateWatcherFinalityV1(
      reorderObjectKeysForTest(finalityPolicy),
      null,
      reorderObjectKeysForTest(reverseEvidence),
    );

    expect(reverseEvidence).toEqual(forwardEvidence);
    expect(reverse).toEqual(forward);
    expect(reverse.resultDigest).toBe(forward.resultDigest);
    const { policyDigest, ...policyCanonical } = finalityPolicy;
    expect(policyDigest).toBe(sha256CanonicalForTest(policyCanonical));
    const reversedProviders = {
      ...policyCanonical,
      externalProviders: [...policyCanonical.externalProviders!].reverse(),
    };
    expect(sha256CanonicalForTest(reversedProviders)).not.toBe(policyDigest);
    const { stateDigest, ...stateCanonical } = forward.state!;
    expect(stateDigest).toBe(sha256CanonicalForTest(stateCanonical));
    const { resultDigest, ...resultCanonical } = forward;
    expect(resultDigest).toBe(sha256CanonicalForTest(resultCanonical));
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
    overflow.consistencyDigest = sha256CanonicalForTest(withoutDigest);
    const arrayOrder = structuredClone(agreement("1")) as Record<
      string,
      unknown
    >;
    arrayOrder.observationEvidenceDigests = [
      ...(arrayOrder.observationEvidenceDigests as string[]),
    ].reverse();
    const unsupportedBigInt = structuredClone(agreement("1")) as Record<
      string,
      unknown
    >;
    (unsupportedBigInt.agreement as Record<string, unknown>).minimumDepth = 1n;
    const unsupportedDate = structuredClone(agreement("1")) as Record<
      string,
      unknown
    >;
    (unsupportedDate.agreement as Record<string, unknown>).minimumDepth =
      new Date(0);
    const cycle = structuredClone(agreement("1")) as Record<string, unknown>;
    cycle.agreement = cycle;

    for (const malformed of [
      unsafe,
      unknown,
      overflow,
      arrayOrder,
      unsupportedBigInt,
      unsupportedDate,
      cycle,
      new Error("no"),
    ]) {
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
