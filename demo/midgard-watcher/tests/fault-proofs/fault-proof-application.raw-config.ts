import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FraudProofL1Source,
  type ResolvedProverSigner,
  type StateQueueMutationLeaseCoordinator,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import type { LucidEvolution, Provider, UTxO } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  type WatcherFaultProofApplicationDependencies,
  type WatcherFaultProofInfrastructureAuthority,
} from "../../src/fault-proofs/fault-proof-application.js";
import type { WatcherFaultProofL1 } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import { WatcherPublicDaLibp2pTransport } from "../../src/storage/public-da-libp2p-transport.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

export const AUTHORITY = makeWatcherDeploymentAuthorityFixture();

/** The follower-backed L1 the application is given; no test here reads it. */
export const testFaultProofL1 = (): WatcherFaultProofL1 => ({
  source: (sourceId) => ({ sourceId }) as unknown as FraudProofL1Source,
  provider: {} as Provider,
});

export const DEPLOYMENT = AUTHORITY.result.manifestId;

export const HEADER = "ab".repeat(28);

const PEER_ID = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";

export const MANIFEST_PATH = "/etc/midgard/deployment-manifest-v1.json";

export const BLUEPRINT_PATH = "/etc/midgard/plutus.json";

export const DEPLOYMENT_INFO_PATH =
  "/etc/midgard/contract-deployment-info.json";

const ADDITIONAL_REFERENCE_CONTRACTS = [
  "fraudProofNativeScriptInvalidStep04",
  "fraudProofNativeScriptInvalidStep05",
] as const;

/**
 * The deployment's reference contracts, in the order the fixture resolver
 * assigns out-refs. Inverting that assignment lets a readiness roster entry be
 * traced back to the deployment contract it names, without transcribing any
 * workflow's roster into this file.
 */
const FIXTURE_REFERENCE_CONTRACT_NAMES: readonly string[] = [
  ...Object.keys(AUTHORITY.contracts),
  ...ADDITIONAL_REFERENCE_CONTRACTS,
];

export const contractNameForOutRef = (rosterOutRef: string): string => {
  const outputIndex = Number(rosterOutRef.split("#")[1]);
  const name = FIXTURE_REFERENCE_CONTRACT_NAMES[outputIndex];
  if (
    name === undefined ||
    rosterOutRef !==
      `${(outputIndex + 1).toString(16).padStart(64, "0")}#${outputIndex.toString()}`
  ) {
    throw new Error(
      `roster out-ref ${rosterOutRef} is not a reference script this deployment resolved`,
    );
  }
  return name;
};

/** Bound by every fault-proof workflow, whatever its physical step roster. */
export const SHARED_THREAD_REFERENCE_KEYS = [
  "computationThreadMint",
  "fraudProofMint",
] as const;

export const rawConfig = () => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "local_node",
      authorityNodeId: "watcher-node",
      chainSync: {
        kind: "cardano_node_socket",
        socketPath: "/run/cardano/node.socket",
        nodeConfigPath: "/etc/cardano/node-config.json",
        genesisConfigPath: "/etc/cardano/shelley-genesis.json",
        genesisIdentitySha256: "33".repeat(32),
      },
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
    },
  },
  da: {
    peers: [
      {
        identity: "da-peer-a",
        multiaddr: `/dns4/da-a.example/tcp/443/p2p/${PEER_ID}`,
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
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

export const infrastructure = (): WatcherFaultProofInfrastructureAuthority => ({
  manifestPath: MANIFEST_PATH,
  blueprintPath: BLUEPRINT_PATH,
  deploymentInfoPath: DEPLOYMENT_INFO_PATH,
});

export const invocation = (
  configPath: string,
  category: WorkflowAdapterReadinessInput["category"],
  overrides: Partial<WorkflowAdapterReadinessInput> = {},
): WorkflowAdapterReadinessInput => ({
  mode: "run",
  category,
  deploymentFingerprint: DEPLOYMENT,
  headerHash: HEADER,
  journalDirectory: "/var/lib/midgard-watcher/fraud-proof-journals",
  runtimeConfigPath: configPath,
  ...overrides,
});

export const hostileStructuralExecutionInvocation = (
  configPath: string,
  category: WorkflowAdapterRunnerInput["category"],
): WorkflowAdapterRunnerInput =>
  ({
    ...invocation(configPath, category),
    decisionDigest: "cd".repeat(32),
    actuationPermit: Object.freeze({
      permitVersion: "midgard-production-workflow-actuation-permit-v1",
    }),
  }) as unknown as WorkflowAdapterRunnerInput;

export const transportFactory = () => {
  const stop = vi.fn(async () => undefined);
  const transport = Object.create(
    WatcherPublicDaLibp2pTransport.prototype,
  ) as WatcherPublicDaLibp2pTransport;
  Object.defineProperties(transport, {
    request: { value: vi.fn(async () => new Uint8Array([0xf6])) },
    stop: { value: stop },
  });
  return { stop, factory: vi.fn(async () => transport) };
};

export const dependencies = (): WatcherFaultProofApplicationDependencies => {
  const signer: ResolvedProverSigner = {
    source: "test",
    address: "addr_test1vqpzry9x8gf2tvdw0s3jn54khce6mua7l0yp4rx3z9g4zpq0j52c7",
    paymentKeyHash: "01".repeat(28),
    selectWallet: vi.fn(),
  };
  const lease: StateQueueMutationLeaseCoordinator = {
    acquire: vi.fn(async () => {
      throw new Error("startup readiness must not acquire a mutation lease");
    }),
  };
  return {
    readText: vi.fn(async (path: string) => {
      if (path === MANIFEST_PATH) {
        return JSON.stringify(AUTHORITY.signedIdentity.manifest);
      }
      if (path === BLUEPRINT_PATH) return "{}";
      if (path === DEPLOYMENT_INFO_PATH) {
        return JSON.stringify({
          referenceScriptAuthPolicy: "02".repeat(28),
          contracts: AUTHORITY.contracts,
        });
      }
      throw new Error(`unexpected read ${path}`);
    }),
    canonicalPath: vi.fn(async (path: string) => path),
    makeLucid: vi.fn(async () => ({}) as LucidEvolution),
    resolveSigner: vi.fn(() => signer),
    resolveReferenceScript: vi.fn(async ({ contractName }) => {
      const referenceIndex = [
        ...Object.keys(AUTHORITY.contracts),
        ...ADDITIONAL_REFERENCE_CONTRACTS,
      ].indexOf(contractName);
      if (referenceIndex < 0) {
        throw new Error(`unknown deployment contract ${contractName}`);
      }
      return {
        txHash: (referenceIndex + 1).toString(16).padStart(64, "0"),
        outputIndex: referenceIndex,
        address: "addr_test1vq44",
        assets: { lovelace: 2_000_000n },
      } as UTxO;
    }),
    createLeaseCoordinator: vi.fn(() => lease),
  };
};
