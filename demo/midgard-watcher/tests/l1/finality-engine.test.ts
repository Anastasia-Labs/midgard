import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  makeDeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityBootstrapState,
  makeWatcherFinalityPolicy,
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
  WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
  WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
  WATCHER_FINALITY_STATE_SCHEMA_VERSION,
  watcherFinalityConfiguredSource,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from "../../src/l1/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherExternalProviderTransport,
  makeWatcherL1PublicBytes,
  normalizeWatcherL1Block,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import type {
  WatcherLocalKupmiosNativeObservation,
  WatcherLocalKupmiosNativeObservationRuntime,
} from "../../src/l1/local-kupmios-native-observation.js";
import { evaluateWatcherMultiProviderConsistency as evaluateWatcherMultiProviderConsistencyRaw } from "../../src/l1/multi-provider-consistency.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { unsafeCreateWatcherChainCoordinatorForTest } from "../../src/runtime/chain-coordinator.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { WatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { sha256Canonical as sha256CanonicalForTest } from "../support/canonical-json.js";

const hex32 = (byte: string): string => byte.repeat(32);

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
  WatcherL1TransportAttestationContext
>();
const transportContexts = new Map<
  string,
  WatcherL1TransportAttestationContext
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
    closeWatcherL1TransportAttestationContext(context);
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
        await establishWatcherExternalProviderTransport({
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
  fundingProfileBundleDigest: "ab".repeat(32),
  blueprintHash: hex32(releaseByte),
  ruleBundleCommitment: hex32("44"),
  programCommitments: { validation: hex32("55") },
  durableMarker: makeDeploymentMarker(hex32(manifestByte)),
});

const policy = (
  depth = 3,
  manifestByte = "11",
  releaseByte = "22",
  rollbackDepth = depth,
): WatcherFinalityPolicy => {
  const value = makeWatcherFinalityPolicy(
    config(depth, rollbackDepth),
    deploymentIdentity(manifestByte, releaseByte),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicy;
};

// A policy over an explicit configured provider set, so a test can name a
// provider allowlist that is strictly wider than the set a W11 record binds.
const policyOverProviders = (
  providers: readonly (readonly [string, string])[],
  depth = 3,
): WatcherFinalityPolicy => {
  const base = config(depth, depth);
  const value = makeWatcherFinalityPolicy(
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
  return value as WatcherFinalityPolicy;
};

// Re-stamps a genuine W11 record onto another policy's configured source, so
// the source-authority binding cannot mask the provider-coverage question.
// Everything else in the record - bindings, counts, evidence, agreement - is
// exactly what W11 produced. Against a policy the record already matches this
// is the identity function, which the fully bound control asserts.
const rebindConsistencyToPolicy = (
  record: unknown,
  targetPolicy: WatcherFinalityPolicy,
): Record<string, unknown> => {
  const rebound: Record<string, unknown> = {
    ...(record as Record<string, unknown>),
    configuredSourceDigest: sha256CanonicalForTest(
      watcherFinalityConfiguredSource(targetPolicy),
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

const evaluateWatcherMultiProviderConsistency = (
  configuredSource: unknown,
  observations: unknown,
  explicitAttestations?: readonly WatcherL1TransportAttestationContext[],
) => {
  const inferred = Array.isArray(observations)
    ? observations.flatMap((candidate) => {
        if (typeof candidate !== "object" || candidate === null) return [];
        const attestation = observationAttestations.get(candidate);
        return attestation === undefined ? [] : [attestation];
      })
    : [];
  return evaluateWatcherMultiProviderConsistencyRaw(
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
  return evaluateWatcherMultiProviderConsistency(
    externalSource(),
    reverse ? observations.reverse() : observations,
  );
};

const pendingAt = (
  finalityPolicy: WatcherFinalityPolicy,
  depth: string,
  options: ObservationOptions = {},
): WatcherFinalityState => {
  const result = evaluateWatcherFinality(
    finalityPolicy,
    null,
    agreement(depth, options),
  );
  expect(result.action).toBe("observe_pending");
  return result.state as WatcherFinalityState;
};

const finalizeAtThreshold = (
  finalityPolicy: WatcherFinalityPolicy,
  options: ObservationOptions = {},
): WatcherFinalityState => {
  const pending = pendingAt(finalityPolicy, "2", options);
  const result = evaluateWatcherFinality(
    finalityPolicy,
    pending,
    agreement("3", options),
  );
  expect(result.action).toBe("finalize");
  return result.state as WatcherFinalityState;
};

describe("canonical release-bound watcher finality", () => {
  it("binds W01 finality to the verified release and deployment marker", () => {
    const value = policy();

    expect(value).toMatchObject({
      schemaVersion: WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
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
      blueprintHash: hex32("22"),
      deploymentMarker: {
        schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
        manifestId: hex32("11"),
      },
    });
    expect(value.policyDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(parseWatcherFinalityPolicy(value)).toEqual(value);
    expect(Object.isFrozen(value)).toBe(true);
    expect(
      parseWatcherFinalityPolicy({
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
      parseWatcherFinalityPolicy({
        ...narrowedRecoveryPolicyCanonical,
        policyDigest: sha256CanonicalForTest(narrowedRecoveryPolicyCanonical),
      }),
    ).toBeNull();

    const changedEndpointConfig = config();
    changedEndpointConfig.l1.source.providers[0]!.endpoint =
      "https://cardano-a-new.example";
    const changedEndpointPolicy = makeWatcherFinalityPolicy(
      changedEndpointConfig,
      deploymentIdentity(),
    );
    expect(changedEndpointPolicy).not.toBeNull();
    expect(changedEndpointPolicy?.policyDigest).not.toBe(value.policyDigest);

    const reorderedConfig = config();
    reorderedConfig.l1.source.providers.reverse();
    expect(
      makeWatcherFinalityPolicy(reorderedConfig, deploymentIdentity()),
    ).toEqual(value);

    const unauthenticatedTransportConfig = config();
    unauthenticatedTransportConfig.l1.source.providers[0]!.endpoint =
      "http://127.0.0.1:1442";
    expect(
      makeWatcherFinalityPolicy(
        unauthenticatedTransportConfig,
        deploymentIdentity(),
      ),
    ).toBeNull();
  });

  it("derives the local-node finality authority and Kupmios bindings from W01 config", () => {
    const base = config();
    const value = makeWatcherFinalityPolicy(
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
    expect(parseWatcherFinalityPolicy(value)).toEqual(value);
  });

  it("rejects configuration/deployment mismatches without emitting values", () => {
    const wrongNetwork = makeWatcherFinalityPolicy(
      config(),
      deploymentIdentity("11", "22", "Preview"),
    );
    const malformedMarker = {
      ...deploymentIdentity(),
      durableMarker: makeDeploymentMarker(hex32("99")),
    };

    expect(wrongNetwork).toBeNull();
    expect(makeWatcherFinalityPolicy(config(), malformedMarker)).toBeNull();
  });

  it("rejects agreement from two distinct providers outside the W01 allowlist", () => {
    const finalityPolicy = policy();
    const hostileAgreement = evaluateWatcherMultiProviderConsistency(
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
      evaluateWatcherFinality(finalityPolicy, null, hostileAgreement),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: { phase: "unobserved" },
    });
  });

  it("rejects configured provider labels with substituted operator identities", () => {
    const hostileAgreement = evaluateWatcherMultiProviderConsistency(
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
      evaluateWatcherFinality(policy(), null, hostileAgreement),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
    });
  });

  it("rejects an otherwise valid W11 agreement bound to another configured provider set", () => {
    const foreign = evaluateWatcherMultiProviderConsistency(
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
    expect(evaluateWatcherFinality(policy(), null, foreign)).toMatchObject({
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

    const first = evaluateWatcherFinality(finalityPolicy, null, belowThreshold);
    const second = evaluateWatcherFinality(
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
      evaluateWatcherFinality(finalityPolicy, null, partial),
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

    const first = evaluateWatcherFinality(finalityPolicy, null, belowThreshold);
    const second = evaluateWatcherFinality(
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

  it.each(["3", "8", "2161"])(
    "keeps first visibility pending at depth %s and requires real progress",
    (depth) => {
      const finalityPolicy = policy();
      const firstAgreement = agreement(depth);
      const first = evaluateWatcherFinality(
        finalityPolicy,
        null,
        firstAgreement,
      );

      expect(first).toMatchObject({
        schemaVersion: WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
        action: "observe_pending",
        protocolDecision: "hold",
        reasonCodes: ["first_visibility_pending"],
        alertCodes: ["watcher_finality_pending"],
        state: {
          schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
          phase: "pending",
          pending: {
            firstSeenDepth: depth,
            currentDepth: depth,
            firstSeenConsistencyDigest: firstAgreement.consistencyDigest,
            lastSeenConsistencyDigest: firstAgreement.consistencyDigest,
            visibilityCount: "1",
          },
        },
      });
      expect(first.resultDigest).toMatch(/^[0-9a-f]{64}$/u);
      const restored = parseWatcherFinalityState(
        JSON.parse(JSON.stringify(first.state)),
        finalityPolicy,
      );
      expect(restored).toEqual(first.state);
      expect(restored).not.toBeNull();
      expect(
        evaluateWatcherFinality(finalityPolicy, restored, firstAgreement),
      ).toMatchObject({
        action: "duplicate",
        protocolDecision: "hold",
        reasonCodes: ["duplicate_observation"],
        state: restored,
      });
      const greaterDepth = (BigInt(depth) + 1n).toString();
      const sameMinimum = evaluateWatcherMultiProviderConsistency(
        externalSource(),
        [
          observation("provider-a", "a1", { depth }),
          observation("provider-b", "b2", { depth: greaterDepth }),
        ],
      );
      expect(sameMinimum.status).toBe("agreed");
      expect(sameMinimum.agreement?.minimumDepth).toBe(depth);
      expect(
        evaluateWatcherFinality(finalityPolicy, restored, sameMinimum),
      ).toMatchObject({
        action: "reject",
        protocolDecision: "hold",
        reasonCodes: ["stale_observation"],
        state: restored,
      });
      const confirmed = evaluateWatcherFinality(
        finalityPolicy,
        restored,
        agreement(greaterDepth),
      );
      expect(confirmed).toMatchObject({
        action: "finalize",
        protocolDecision: "finality_granted",
        state: {
          phase: "finalized",
          pending: null,
          finalized: {
            firstSeenDepth: depth,
            currentDepth: greaterDepth,
            visibilityCount: "2",
          },
        },
      });
      expect(
        parseWatcherFinalityState(confirmed.state, finalityPolicy),
      ).toEqual(confirmed.state);
    },
  );

  it.each([
    { firstSeenDepth: "7" },
    { lastSeenConsistencyDigest: hex32("ff") },
    { visibilityCount: "2" },
    { firstSeenDepth: "7", visibilityCount: "2" },
  ])("rejects impossible pending visibility fields %j", (changes) => {
    const finalityPolicy = policy();
    const state = pendingAt(finalityPolicy, "8");
    const { stateDigest: _, ...content } = state;
    const canonical = {
      ...content,
      pending: { ...state.pending!, ...changes },
    };
    const malformed = {
      ...canonical,
      stateDigest: sha256CanonicalForTest(canonical),
    };
    expect(parseWatcherFinalityState(malformed)).not.toBeNull();
    expect(parseWatcherFinalityState(malformed, finalityPolicy)).toBeNull();
    expect(
      evaluateWatcherFinality(finalityPolicy, malformed, agreement("9")),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["invalid_state_semantics"],
      state: null,
    });
  });

  it("finalizes threshold-1 to threshold exactly once", () => {
    const finalityPolicy = policy();
    const pending = pendingAt(finalityPolicy, "2");
    const finalized = evaluateWatcherFinality(
      finalityPolicy,
      pending,
      agreement("3"),
    );
    const later = evaluateWatcherFinality(
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
    const first = evaluateWatcherFinality(finalityPolicy, null, evidence);
    const restarted = JSON.parse(
      JSON.stringify(first.state),
    ) as WatcherFinalityState;
    const duplicate = evaluateWatcherFinality(
      finalityPolicy,
      restarted,
      evidence,
    );

    expect(parseWatcherFinalityState(restarted)).toEqual(first.state);
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
    const advanced = evaluateWatcherFinality(
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
    const rewound = evaluateWatcherFinality(
      finalityPolicy,
      pending,
      agreement("2"),
    );
    const replay = evaluateWatcherFinality(
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
    const fork = evaluateWatcherFinality(
      finalityPolicy,
      pending,
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinality(
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
    const exactDepth = evaluateWatcherFinality(
      finalityPolicy,
      depthTwo,
      agreement("0"),
    );
    const depthThree = pendingAt(finalityPolicy, "3");
    const adjacentExcess = evaluateWatcherFinality(
      finalityPolicy,
      depthThree,
      agreement("0"),
    );
    const exactFork = evaluateWatcherFinality(
      finalityPolicy,
      pendingAt(finalityPolicy, "1"),
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const excessiveFork = evaluateWatcherFinality(
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
    const freshTransportAgreement = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [
        observation("provider-a", "c3", {
          depth: "1",
          operatorIdentityByte: "a1",
        }),
        observation("provider-b", "b2", { depth: "1" }),
      ],
    );
    const stale = evaluateWatcherFinality(
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
    const initialPending = evaluateWatcherMultiProviderConsistency(
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
    const quarantined = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [observation("provider-a", "a1")],
    );

    expect(
      evaluateWatcherFinality(finalityPolicy, null, initialPending),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_pending"],
      state: { phase: "unobserved" },
    });
    expect(
      evaluateWatcherFinality(finalityPolicy, null, quarantined),
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
    const rolledBack = evaluateWatcherFinality(
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
      evaluateWatcherFinality(finalityPolicy, rolledBack.state, agreement("4")),
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
    const point = evaluateWatcherFinality(
      finalityPolicy,
      finalized,
      agreement("4", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinality(
      finalityPolicy,
      finalized,
      agreement("4", { bodyHex: "a100" }),
    );
    const pendingW11 = evaluateWatcherMultiProviderConsistency(
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
    const quasiRollback = evaluateWatcherFinality(
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
      evaluateWatcherFinality(
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
    const boundedLag = evaluateWatcherMultiProviderConsistency(
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
      const held = evaluateWatcherFinality(
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
        evaluateWatcherFinality(finalityPolicy, held.state, agreement("4")),
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
      evaluateWatcherFinality(stalePolicy, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["stale_state"],
      state: null,
    });
    expect(
      evaluateWatcherFinality(otherDeployment, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["deployment_mismatch"],
      state: null,
    });
    expect(
      evaluateWatcherFinality(otherRelease, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["blueprint_mismatch"],
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

    expect(parseWatcherFinalityState(impossible)).not.toBeNull();
    expect(parseWatcherFinalityState(impossible, finalityPolicy)).toBeNull();
    expect(
      evaluateWatcherFinality(finalityPolicy, impossible, agreement("6")),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["stale_state"],
      state: null,
    });

    const samePolicyImpossible = {
      ...impossible,
      policyDigest: finalityPolicy.policyDigest,
      blueprintHash: finalityPolicy.blueprintHash,
      deploymentMarker: finalityPolicy.deploymentMarker,
    } as Record<string, unknown>;
    const samePolicyCanonical = {
      ...samePolicyImpossible,
    } as Record<string, unknown>;
    delete samePolicyCanonical.stateDigest;
    samePolicyImpossible.stateDigest =
      sha256CanonicalForTest(samePolicyCanonical);
    expect(
      evaluateWatcherFinality(
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
    const forward = evaluateWatcherFinality(
      finalityPolicy,
      null,
      forwardEvidence,
    );
    const reverse = evaluateWatcherFinality(
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
        evaluateWatcherFinality(finalityPolicy, null, malformed),
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
    Object.defineProperty(unsafePolicy, "blueprintHash", {
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
    const policyFailure = evaluateWatcherFinality(
      unsafePolicy,
      null,
      new Error(secret),
    );
    const stateFailure = evaluateWatcherFinality(
      policy(),
      unsafeState,
      new Error(secret),
    );

    expect(policyFailure.reasonCodes).toEqual(["malformed_policy"]);
    expect(stateFailure.reasonCodes).toEqual(["malformed_state"]);
    expect(JSON.stringify([policyFailure, stateFailure])).not.toContain(secret);
  });
});

describe("native coordinator with real finality decisions", () => {
  it.each(["fixed_tip", "one_tip_growth", "continued_tip_growth"] as const)(
    "releases buffered empty blocks at two increasing depths once in order: %s",
    async (mode) => {
      const finalityPolicy = policy(30);
      let state = makeWatcherFinalityBootstrapState(finalityPolicy);
      if (state === null) throw new Error("Fixture bootstrap rejected");
      const delivered: number[] = [];
      const hash = (height: number) => height.toString(16).padStart(64, "0");
      const block = (height: number): WatcherNativeBlockAdmission => ({
        schemaVersion: "midgard-watcher-native-block-admission-v1",
        blockType: "7",
        protocolMajor: "10",
        blockHash: hash(height),
        prevHash: hash(height - 1),
        slot: String(height * 10),
        blockNo: String(height),
        rawBlockCbor: "80",
        rawHeaderCbor: "80",
        transactionIds: [],
        transactionCbors: [],
      });
      const durable = {
        readFinality: () => state,
        read: () => ({
          currentFinalityState: state,
          authenticatedConsistencyHistory: [],
          currentStore: {},
        }),
        persistCanonicalProgress: async (
          observed: WatcherLocalKupmiosNativeObservation,
        ) => {
          if (state === null) throw new Error("Fixture state rejected");
          let evaluationState = state;
          const point = observed.block.chainPoint;
          // Reproduce durable canonical progress's direct-child/bootstrap
          // selection; the finality decision itself uses the real evaluator.
          if (
            state.phase === "finalized" &&
            state.finalized?.pointDigest !== point.pointDigest
          ) {
            const finalized = state.finalized;
            if (
              finalized === null ||
              point.parentBlockHash !== finalized.blockHash ||
              BigInt(point.blockNo) !== BigInt(finalized.blockNo) + 1n ||
              BigInt(point.slot) <= BigInt(finalized.slot)
            ) {
              throw new Error("Canonical progress is not a direct child");
            }
            const bootstrap = makeWatcherFinalityBootstrapState(finalityPolicy);
            if (bootstrap === null)
              throw new Error("Fixture bootstrap rejected");
            evaluationState = bootstrap;
          }
          const finalityResult = evaluateWatcherFinality(
            finalityPolicy,
            evaluationState,
            observed.consistency,
          );
          if (
            finalityResult.state === null ||
            ["reject", "rewind_pending", "quarantine_incident"].includes(
              finalityResult.action,
            )
          ) {
            throw new Error(
              `Canonical progress rejected: ${finalityResult.reasonCodes.join(",")}`,
            );
          }
          state = finalityResult.state;
          return {
            persistence:
              finalityResult.action === "duplicate" ? "unchanged" : "committed",
            finalityResult,
          };
        },
      } as unknown as WatcherDurableRuntime;
      const localObservation = {
        observe: async ({
          block: nativeBlock,
          depth,
        }: {
          readonly block: WatcherNativeBlockAdmission;
          readonly depth: string;
        }): Promise<WatcherLocalKupmiosNativeObservation> => {
          const options = {
            blockHash: nativeBlock.blockHash,
            parentBlockHash: nativeBlock.prevHash,
            slot: nativeBlock.slot,
            blockNo: nativeBlock.blockNo,
            depth,
          };
          const blocks = [
            observation("provider-a", "a1", options),
            observation("provider-b", "b2", options),
          ] as const;
          return {
            schemaVersion:
              "midgard-watcher-local-kupmios-native-observation-v1",
            block: blocks[0],
            ogmiosBlock: blocks[0],
            kupoCheckpoint: blocks[1],
            observations: blocks,
            transportAttestations: [
              provider("provider-a", "a1"),
              provider("provider-b", "b2"),
            ],
            consistency: evaluateWatcherMultiProviderConsistency(
              externalSource(),
              blocks,
            ),
          };
        },
        close: () => undefined,
      } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        {
          policy: finalityPolicy,
          durable,
          observation: localObservation,
          hooks: {
            onRollback: async () => undefined,
            onFinalized: async ({ nativeBlock }) => {
              delivered.push(Number(nativeBlock.blockNo));
            },
          },
        },
        { admitRollForward: (event) => block(Number(event.blockNo)) },
      );
      const send = async (height: number, tip: number) => {
        const admitted = block(height);
        await coordinator.handle({
          schemaVersion: "midgard-watcher-native-chain-sync-v1",
          kind: "roll_forward",
          blockHash: admitted.blockHash,
          blockType: admitted.blockType,
          prevHash: admitted.prevHash,
          slot: admitted.slot,
          blockNo: admitted.blockNo,
          rawBlockCbor: admitted.rawBlockCbor,
          tip: {
            kind: "point",
            blockHash: hash(tip),
            slot: String(tip * 10),
            blockNo: String(tip),
          },
        });
      };
      for (let height = 100; height <= 105; height += 1) {
        await send(height, height);
      }
      expect(delivered).toEqual([]);
      for (let height = 106; height <= 110; height += 1) {
        await send(height, 140);
      }
      // The earlier native arrivals supply the first real observation for
      // 100..105. Blocks106..110 have only been seen at tip140 and must wait.
      if (mode === "fixed_tip") {
        expect(delivered).toEqual([100, 101, 102, 103, 104, 105]);
        return;
      }
      await send(111, 141);
      if (mode === "one_tip_growth") {
        expect(delivered).toEqual(
          Array.from({ length: 11 }, (_, i) => 100 + i),
        );
        return;
      }
      for (let height = 112; height <= 118; height += 1) {
        await send(height, 140 + height - 110);
      }
      expect(delivered).toEqual(Array.from({ length: 18 }, (_, i) => 100 + i));
    },
  );

  it.each(["increasing_depth", "same_depth"] as const)(
    "requires real predecessor finality before recovering a nonempty pending prefix: %s",
    async (historyKind) => {
      const finalityPolicy = policy(30);
      const hash = (height: number) => height.toString(16).padStart(64, "0");
      const block = (height: number): WatcherNativeBlockAdmission => ({
        schemaVersion: "midgard-watcher-native-block-admission-v1",
        blockType: "7",
        protocolMajor: "10",
        blockHash: hash(height),
        prevHash: hash(height - 1),
        slot: String(height * 10),
        blockNo: String(height),
        rawBlockCbor: "80",
        rawHeaderCbor: "80",
        transactionIds: [],
        transactionCbors: [],
      });
      const pointOptions = (height: number): ObservationOptions => ({
        blockHash: hash(height),
        parentBlockHash: hash(height - 1),
        slot: String(height * 10),
        blockNo: String(height),
      });
      const history = [
        agreement("39", pointOptions(101)),
        agreement(
          historyKind === "increasing_depth" ? "40" : "39",
          pointOptions(101),
        ),
      ];
      const first = evaluateWatcherFinality(finalityPolicy, null, history[0]);
      const second = evaluateWatcherFinality(
        finalityPolicy,
        first.state,
        history[1],
      );
      expect(second.action).toBe(
        historyKind === "increasing_depth" ? "finalize" : "duplicate",
      );
      let state = pendingAt(finalityPolicy, "39", pointOptions(102));
      const delivered: string[] = [];
      const observationRuntime = {
        observe: async ({
          block: native,
          depth,
        }: {
          block: WatcherNativeBlockAdmission;
          depth: string;
        }): Promise<WatcherLocalKupmiosNativeObservation> => {
          const options = { ...pointOptions(Number(native.blockNo)), depth };
          const blocks = [
            observation("provider-a", "a1", options),
            observation("provider-b", "b2", options),
          ] as const;
          return {
            schemaVersion:
              "midgard-watcher-local-kupmios-native-observation-v1",
            block: blocks[0],
            ogmiosBlock: blocks[0],
            kupoCheckpoint: blocks[1],
            observations: blocks,
            transportAttestations: [
              provider("provider-a", "a1"),
              provider("provider-b", "b2"),
            ],
            consistency: evaluateWatcherMultiProviderConsistency(
              externalSource(),
              blocks,
            ),
          };
        },
        close: () => undefined,
      } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
      const intersection = {
        kind: "point" as const,
        blockHash: hash(99),
        slot: "990",
      };
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        {
          policy: finalityPolicy,
          restartIntersection: intersection,
          observation: observationRuntime,
          durable: {
            readFinality: () => state,
            read: () => ({
              currentFinalityState: state,
              authenticatedConsistencyHistory: history,
              currentStore: {},
            }),
            persistCanonicalProgress: async (
              observed: WatcherLocalKupmiosNativeObservation,
            ) => {
              expect(observed.block.chainPoint.blockNo).toBe("102");
              const finalityResult = evaluateWatcherFinality(
                finalityPolicy,
                state,
                observed.consistency,
              );
              expect(finalityResult.action).toBe("duplicate");
              if (finalityResult.state === null)
                throw new Error("Fixture state rejected");
              state = finalityResult.state;
              return { persistence: "unchanged", finalityResult };
            },
          } as unknown as WatcherDurableRuntime,
          hooks: {
            onRollback: async () => {
              throw new Error("Initial acknowledgement must not rewind");
            },
            onFinalized: async ({ nativeBlock }) => {
              delivered.push(nativeBlock.blockNo);
            },
          },
        },
        { admitRollForward: (event) => block(Number(event.blockNo)) },
      );
      const tip = {
        kind: "point" as const,
        blockHash: hash(140),
        slot: "1400",
        blockNo: "140",
      };
      await coordinator.handle({
        schemaVersion: "midgard-watcher-native-chain-sync-v1",
        kind: "roll_backward",
        point: intersection,
        tip,
      });
      const send = async (height: number) => {
        const native = block(height);
        await coordinator.handle({
          schemaVersion: "midgard-watcher-native-chain-sync-v1",
          kind: "roll_forward",
          blockHash: native.blockHash,
          prevHash: native.prevHash,
          slot: native.slot,
          blockNo: native.blockNo,
          blockType: native.blockType,
          rawBlockCbor: native.rawBlockCbor,
          tip,
        });
      };
      await send(100);
      expect(delivered).toEqual([]);
      await send(101);
      expect(delivered).toEqual([]);
      if (historyKind === "same_depth") {
        await expect(send(102)).rejects.toThrow();
        expect(delivered).toEqual([]);
      } else {
        await send(102);
        expect(delivered).toEqual(["100", "101"]);
        expect(state.phase).toBe("pending");
        await send(102);
        expect(delivered).toEqual(["100", "101"]);
      }
    },
  );
});
