import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterAll, beforeAll, expect } from "vitest";

import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherExternalProviderTransport,
  type WatcherL1TransportAttestationContext,
} from "../../src/l1/l1-adapter.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";

export const hex32 = (byte: string): string => byte.repeat(32);

export const reorderObjectKeysForTest = (value: unknown): unknown => {
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

export const observationAttestations = new WeakMap<
  object,
  WatcherL1TransportAttestationContext
>();

export const transportContexts = new Map<
  string,
  WatcherL1TransportAttestationContext
>();

let transportFixtureDirectory = "";

const tlsTransportServers: Server[] = [];

export const externalEndpoints = new Map<string, string>();

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

export const externalSource = () =>
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

export const config = (depth = 3, rollbackDepth = depth) => ({
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

export const deploymentIdentity = (
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

export const policy = (
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
