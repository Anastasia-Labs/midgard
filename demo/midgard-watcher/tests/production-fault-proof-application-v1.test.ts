import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerInputV1,
  productionWorkflowReadinessReportV1,
  type ResolvedProverSigner,
  type StateQueueMutationLeaseCoordinator,
  unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTestV1,
} from "@al-ft/midgard-fault-proofs";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { WATCHER_CONFIG_SCHEMA_VERSION } from "../src/config.js";
import {
  unsafeCreateWatcherFaultProofProductionApplicationForTestV1,
  WATCHER_FAULT_PROOF_PRODUCTION_APPLICATION_V1,
  WATCHER_FAULT_PROOF_STARTUP_READINESS_V1,
  WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
  type WatcherFaultProofApplicationDependenciesV1,
  type WatcherFaultProofInfrastructureAuthorityV1,
} from "../src/production-fault-proof-application-v1.js";
import { WatcherPublicDaLibp2pTransport } from "../src/public-da-libp2p-transport.js";
import { makeWatcherDeploymentAuthorityFixtureV1 } from "./support/deployment-authority-fixture.js";

const AUTHORITY = makeWatcherDeploymentAuthorityFixtureV1();
const DEPLOYMENT = AUTHORITY.result.manifestId;
const HEADER = "ab".repeat(28);
const PEER_ID = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";
const MANIFEST_PATH = "/etc/midgard/deployment-manifest-v1.json";
const BLUEPRINT_PATH = "/etc/midgard/plutus.json";
const DEPLOYMENT_INFO_PATH = "/etc/midgard/contract-deployment-info.json";
const ADDITIONAL_REFERENCE_CONTRACTS = [
  "fraudProofNativeScriptInvalidStep04",
  "fraudProofNativeScriptInvalidStep05",
] as const;
const TEST_HISTORY_STORE =
  unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTestV1();

const rawConfig = () => ({
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
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: 30,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: 30,
      },
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

const infrastructure = (): WatcherFaultProofInfrastructureAuthorityV1 => ({
  manifestPath: MANIFEST_PATH,
  blueprintPath: BLUEPRINT_PATH,
  deploymentInfoPath: DEPLOYMENT_INFO_PATH,
  midgardNodeUrl: "http://127.0.0.1:3000",
  midgardNodeAdminKeySource: {
    kind: "environment",
    variable: "MIDGARD_NODE_ADMIN_KEY",
  },
  historicalNativeScriptHistory: {
    sourceMode: "external_provider_quorum",
    consistencyPolicy: "exact_bytes_all_providers_v1",
    providers: [
      {
        sourceId: "history-provider-a",
        operatorIdentitySha256: "71".repeat(32),
        authorityEndpoint: "https://history-a.example.test",
      },
      {
        sourceId: "history-provider-b",
        operatorIdentitySha256: "72".repeat(32),
        authorityEndpoint: "https://history-b.example.test",
      },
    ],
  },
  stateQueueLeaseTtlMs: 30_000,
});

const invocation = (
  configPath: string,
  category: ProductionWorkflowAdapterReadinessInputV1["category"],
  overrides: Partial<ProductionWorkflowAdapterReadinessInputV1> = {},
): ProductionWorkflowAdapterReadinessInputV1 => ({
  mode: "run",
  category,
  deploymentFingerprint: DEPLOYMENT,
  headerHash: HEADER,
  journalDirectory: "/var/lib/midgard-watcher/fraud-proof-journals",
  runtimeConfigPath: configPath,
  ...overrides,
});

const hostileStructuralExecutionInvocation = (
  configPath: string,
  category: ProductionWorkflowAdapterRunnerInputV1["category"],
): ProductionWorkflowAdapterRunnerInputV1 =>
  ({
    ...invocation(configPath, category),
    decisionDigest: "cd".repeat(32),
    actuationPermit: Object.freeze({
      permitVersion: "midgard-production-workflow-actuation-permit-v1",
    }),
  }) as unknown as ProductionWorkflowAdapterRunnerInputV1;

const transportFactory = () => {
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

const dependencies = (): WatcherFaultProofApplicationDependenciesV1 => {
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
    constructWorkflow: vi.fn(async (tagged) => ({
      binding: {
        deploymentFingerprint: DEPLOYMENT,
        definition: {
          category: tagged.category,
          headerHash: tagged.config.headerHash,
        },
      },
    })),
  };
};

describe("watcher production fault-proof application V1", () => {
  it("installs every compiled manifest-bound runner and preflights every reference roster read-only", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-proof-app-"));
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const transport = transportFactory();
    const deps = dependencies();
    try {
      const application =
        unsafeCreateWatcherFaultProofProductionApplicationForTestV1(
          {
            deploymentIdentity: AUTHORITY.result,
            infrastructure: infrastructure(),
            historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
            unsafeTransportFactoryForTest: transport.factory,
          },
          deps,
          {
            MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
            MIDGARD_NODE_ADMIN_KEY: "admin-key",
          },
        );

      expect(application.schemaVersion).toBe(
        WATCHER_FAULT_PROOF_PRODUCTION_APPLICATION_V1,
      );
      expect(application.installedCategories).toEqual(
        WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
      );
      expect(application.installedCategories).toEqual([
        "doubleSpend",
        "nonExistentInput",
        "nonExistentInputNoIndex",
        "invalidRange",
        "zeroInput",
        "daHashPreimage",
        "noReferenceInput",
        "referenceInputNoIdx",
        "invalidSignature",
        "missingSignature",
        "missingNativeScriptTx",
        "withdrawnReferenceInput",
        "canonicalDecodability",
        "committedFieldShape",
        "minFee",
        "doubleWithdraw",
        "l2TxMistag",
        "withdrawnInput",
        "inputSetUniqueness",
        "networkId",
        "nativeScriptInvalid",
      ]);
      expect(
        productionWorkflowReadinessReportV1(
          WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
          application.applicationRegistry,
        ),
      ).toMatchObject({
        deploymentFingerprint: DEPLOYMENT,
        installedCategoryCount: 21,
        requestedCategoryCount: 21,
        readyCategoryCount: 21,
        missingCategoryCount: 0,
      });

      for (const category of WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1) {
        const readiness = await application.assertStartupReady(
          invocation(configPath, category),
        );
        expect(readiness).toEqual(
          expect.objectContaining({
            schemaVersion: WATCHER_FAULT_PROOF_STARTUP_READINESS_V1,
            ready: true,
            category,
            deploymentFingerprint: DEPLOYMENT,
            headerHash: HEADER,
          }),
        );
        expect(Object.keys(readiness.referenceScriptOutRefs)).toEqual(
          Object.keys(
            category === "doubleSpend" ||
              category === "nonExistentInput" ||
              category === "noReferenceInput" ||
              category === "nonExistentInputNoIndex" ||
              category === "referenceInputNoIdx"
              ? {
                  step01: true,
                  step02: true,
                  step03: true,
                  step04: true,
                  claimRegistrySpend: true,
                  computationThreadMint: true,
                  fraudProofMint: true,
                  phasMembershipWithdraw: true,
                  chunkedVerifyWithdraw: true,
                  ...(category === "nonExistentInput" ||
                  category === "noReferenceInput"
                    ? { pexcludesWithdraw: true }
                    : {}),
                  fieldPreimageCertificateMint: true,
                }
              : category === "networkId"
                ? {
                    step01: true,
                    step02: true,
                    claimRegistrySpend: true,
                    computationThreadMint: true,
                    fraudProofMint: true,
                    phasMembershipWithdraw: true,
                    chunkedVerifyWithdraw: true,
                    pexcludesWithdraw: true,
                    fieldPreimageCertificateMint: true,
                  }
                : category === "nativeScriptInvalid"
                  ? {
                      step01: true,
                      step02: true,
                      step03: true,
                      step04: true,
                      step05: true,
                      claimRegistrySpend: true,
                      computationThreadMint: true,
                      fraudProofMint: true,
                      phasMembershipWithdraw: true,
                      chunkedVerifyWithdraw: true,
                      pexcludesWithdraw: true,
                      fieldPreimageCertificateMint: true,
                    }
                  : category === "missingNativeScriptTx"
                    ? {
                        step01: true,
                        step02: true,
                        step03: true,
                        step04: true,
                        step05: true,
                        step06: true,
                        step07: true,
                        step08: true,
                        claimRegistrySpend: true,
                        computationThreadMint: true,
                        fraudProofMint: true,
                        phasMembershipWithdraw: true,
                        chunkedVerifyWithdraw: true,
                        pexcludesWithdraw: true,
                        fieldPreimageCertificateMint: true,
                      }
                    : category === "invalidRange" ||
                        category === "zeroInput" ||
                        category === "l2TxMistag"
                      ? {
                          step01: true,
                          step02: true,
                          claimRegistrySpend: true,
                          computationThreadMint: true,
                          fraudProofMint: true,
                          phasMembershipWithdraw: true,
                          chunkedVerifyWithdraw: true,
                        }
                      : category === "minFee" ||
                          category === "invalidSignature" ||
                          category === "canonicalDecodability" ||
                          category === "inputSetUniqueness"
                        ? {
                            step01: true,
                            step02: true,
                            claimRegistrySpend: true,
                            computationThreadMint: true,
                            fraudProofMint: true,
                            phasMembershipWithdraw: true,
                            chunkedVerifyWithdraw: true,
                            fieldPreimageCertificateMint: true,
                          }
                        : category === "missingSignature"
                          ? {
                              step01: true,
                              step02: true,
                              step03: true,
                              step04: true,
                              claimRegistrySpend: true,
                              computationThreadMint: true,
                              fraudProofMint: true,
                              phasMembershipWithdraw: true,
                            }
                          : category === "withdrawnInput"
                            ? {
                                step01: true,
                                step02: true,
                                step03: true,
                                claimRegistrySpend: true,
                                computationThreadMint: true,
                                fraudProofMint: true,
                                phasMembershipWithdraw: true,
                                chunkedVerifyWithdraw: true,
                                fieldPreimageCertificateMint: true,
                              }
                            : category === "withdrawnReferenceInput"
                              ? {
                                  step01: true,
                                  step02: true,
                                  step03: true,
                                  claimRegistrySpend: true,
                                  computationThreadMint: true,
                                  fraudProofMint: true,
                                  phasMembershipWithdraw: true,
                                  fieldPreimageCertificateMint: true,
                                }
                              : {
                                  step01: true,
                                  step02: true,
                                  claimRegistrySpend: true,
                                  computationThreadMint: true,
                                  fraudProofMint: true,
                                  phasMembershipWithdraw: true,
                                },
          ),
        );
      }

      expect(deps.makeLucid).toHaveBeenCalledTimes(21);
      expect(transport.stop).toHaveBeenCalledTimes(21);
      await expect(
        application.runOrResume(
          hostileStructuralExecutionInvocation(configPath, "doubleSpend"),
        ),
      ).rejects.toThrow(
        "unsafe watcher fault-proof test application cannot execute transactions",
      );
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("fails closed on application/category, deployment, authority, and secret substitution", async () => {
    expect(() =>
      unsafeCreateWatcherFaultProofProductionApplicationForTestV1(
        {
          deploymentIdentity: AUTHORITY.result,
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          infrastructure: {
            ...infrastructure(),
            privateEvidenceUrl: "https://operator-private.example",
          } as never,
        },
        dependencies(),
      ),
    ).toThrow("unknown or missing fields");

    expect(() =>
      unsafeCreateWatcherFaultProofProductionApplicationForTestV1(
        {
          deploymentIdentity: AUTHORITY.result,
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          infrastructure: {
            ...infrastructure(),
            historicalNativeScriptHistory: {
              ...infrastructure().historicalNativeScriptHistory,
              providers: [
                ...infrastructure().historicalNativeScriptHistory.providers,
                {
                  sourceId: "history-provider-c",
                  operatorIdentitySha256: "71".repeat(32),
                  authorityEndpoint: "https://history-c.example.test",
                },
              ],
            },
          },
        },
        dependencies(),
      ),
    ).toThrow(
      "historical native-script providers must have distinct canonical identities and endpoints",
    );

    const directory = await mkdtemp(
      join(tmpdir(), "midgard-proof-app-hostile-"),
    );
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    try {
      const noSecrets =
        unsafeCreateWatcherFaultProofProductionApplicationForTestV1(
          {
            deploymentIdentity: AUTHORITY.result,
            historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
            infrastructure: infrastructure(),
            unsafeTransportFactoryForTest: transportFactory().factory,
          },
          dependencies(),
          {},
        );
      await expect(
        noSecrets.assertStartupReady(
          invocation(configPath, "doubleSpend", {
            deploymentFingerprint: DEPLOYMENT,
          }),
        ),
      ).rejects.toThrow("watcher prover wallet secret source is empty");

      const admitted =
        unsafeCreateWatcherFaultProofProductionApplicationForTestV1(
          {
            deploymentIdentity: AUTHORITY.result,
            historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
            infrastructure: infrastructure(),
            unsafeTransportFactoryForTest: transportFactory().factory,
          },
          dependencies(),
          {
            MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
            MIDGARD_NODE_ADMIN_KEY: "admin-key",
          },
        );
      await expect(
        admitted.assertStartupReady(
          invocation(configPath, "doubleSpend", {
            category: "withdrawalMistag",
          }),
        ),
      ).rejects.toThrow("no installed production workflow");
      await expect(
        admitted.assertStartupReady(
          invocation(configPath, "doubleSpend", {
            deploymentFingerprint: "ff".repeat(32),
          }),
        ),
      ).rejects.toThrow("differs from verified watcher authority");
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
