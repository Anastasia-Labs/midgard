import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES } from "@al-ft/midgard-fault-proofs";
import {
  type ResolvedProverSigner,
  type StateQueueMutationLeaseCoordinator,
  unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunnerInput,
  workflowFundingRequirementsForRunner,
  workflowReadinessReport,
} from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  unsafeCreateWatcherFaultProofApplicationForTest,
  WATCHER_FAULT_PROOF_APPLICATION,
  WATCHER_FAULT_PROOF_STARTUP_READINESS,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_MISSING_WORKFLOW_CATEGORIES,
  type WatcherFaultProofApplicationDependencies,
  type WatcherFaultProofInfrastructureAuthority,
} from "../../src/fault-proofs/fault-proof-application.js";
import {
  createWatcherWorkflowFundingProfileBundle,
  loadWatcherWorkflowFundingProfileOverlay,
} from "../../src/funding/workflow-funding-profile-overlay.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import { WatcherPublicDaLibp2pTransport } from "../../src/storage/public-da-libp2p-transport.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const AUTHORITY = makeWatcherDeploymentAuthorityFixture();
const DEPLOYMENT = AUTHORITY.result.manifestId;
const HEADER = "ab".repeat(28);
const PEER_ID = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";
const MANIFEST_PATH = "/etc/midgard/deployment-manifest-v1.json";
const BLUEPRINT_PATH = "/etc/midgard/plutus.json";
const DEPLOYMENT_INFO_PATH = "/etc/midgard/contract-deployment-info.json";
const ADDITIONAL_REFERENCE_CONTRACTS = [
  "fraudProofNativeScriptInvalidStep04",
  "fraudProofNativeScriptInvalidStep05",
  "fraudProofMissingNativeScriptUtxoStep06",
  "fraudProofMissingNativeScriptUtxoStep07",
] as const;
const TEST_HISTORY_STORE =
  unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest();

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

const contractNameForOutRef = (rosterOutRef: string): string => {
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
const SHARED_THREAD_REFERENCE_KEYS = [
  "computationThreadMint",
  "fraudProofMint",
] as const;

/**
 * The two families that accuse an L1-observed event rather than a committed L2
 * block leaf, and therefore never prove PHAS membership.
 */
const NON_PHAS_MEMBERSHIP_CATEGORIES = new Set<string>([
  "fabricatedDeposit",
  "fabricatedWithdrawal",
]);

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

const infrastructure = (): WatcherFaultProofInfrastructureAuthority => ({
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

const hostileStructuralExecutionInvocation = (
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

const dependencies = (): WatcherFaultProofApplicationDependencies => {
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
  it("installs every runner with an empty signed funding bundle while funding requests stay gated", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-empty-funding-"));
    try {
      const bundle = createWatcherWorkflowFundingProfileBundle({
        profiles: [],
      });
      const authority = makeWatcherDeploymentAuthorityFixture({
        fundingProfileBundleDigest: bundle.fundingProfileBundleDigest,
      });
      const bundlePath = join(directory, "funding.json");
      await writeFile(bundlePath, bundle.fundingProfileBundleBytes);
      const fundingProfileOverlay =
        await loadWatcherWorkflowFundingProfileOverlay({
          bundlePath,
          deploymentIdentity: authority.result,
        });
      const application = unsafeCreateWatcherFaultProofApplicationForTest(
        {
          deploymentIdentity: authority.result,
          infrastructure: infrastructure(),
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          unsafeTransportFactoryForTest: transportFactory().factory,
          fundingProfileOverlay,
        },
        dependencies(),
        {
          MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
          MIDGARD_NODE_ADMIN_KEY: "admin-key",
        },
      );
      expect(application.installedCategories).toEqual([
        ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      ]);
      for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
        expect(() =>
          workflowFundingRequirementsForRunner({
            category,
            runner: application.runners[category],
          }),
        ).toThrow("has no admitted measured funding profile");
      }
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("installs every compiled manifest-bound runner and preflights every reference roster read-only", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-proof-app-"));
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const transport = transportFactory();
    const deps = dependencies();
    try {
      const application = unsafeCreateWatcherFaultProofApplicationForTest(
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

      expect(application.schemaVersion).toBe(WATCHER_FAULT_PROOF_APPLICATION);
      // The catalogue in the SDK is the independent authority on which
      // families a watcher must be able to prove, and in which order.
      expect(application.installedCategories).toEqual([
        ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      ]);
      expect(WATCHER_INSTALLED_WORKFLOW_CATEGORIES).toEqual([
        ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      ]);
      expect(WATCHER_MISSING_WORKFLOW_CATEGORIES).toEqual([]);
      expect(
        workflowReadinessReport(
          WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
          application.applicationRegistry,
        ),
      ).toMatchObject({
        deploymentFingerprint: DEPLOYMENT,
        installedCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
        requestedCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
        readyCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
        missingCategoryCount: 0,
      });

      for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
        const resolvedBefore = vi.mocked(deps.resolveReferenceScript).mock.calls
          .length;
        const readiness = await application.assertStartupReady(
          invocation(configPath, category),
        );
        const resolvedContractNames = vi
          .mocked(deps.resolveReferenceScript)
          .mock.calls.slice(resolvedBefore)
          .map(([input]) => input.contractName);

        const { referenceScriptOutRefs, ...envelope } = readiness;
        expect(envelope).toEqual({
          schemaVersion: WATCHER_FAULT_PROOF_STARTUP_READINESS,
          ready: true,
          category,
          deploymentFingerprint: DEPLOYMENT,
          headerHash: HEADER,
        });

        const rosterKeys = Object.keys(referenceScriptOutRefs);
        const rosterOutRefs = Object.values(referenceScriptOutRefs);
        // Every rostered entry is a real resolved reference UTxO, not a
        // placeholder left behind by a reference script the workflow failed to
        // bind (which surfaces as `undefined#undefined`).
        for (const rosterOutRef of rosterOutRefs) {
          expect(rosterOutRef).toMatch(/^[0-9a-f]{64}#\d+$/u);
        }
        // One distinct deployment contract per rostered name, and the roster
        // accounts for exactly the reference scripts this workflow resolved:
        // nothing loaded but unreported, nothing reported but never resolved.
        expect(new Set(rosterOutRefs).size).toBe(rosterOutRefs.length);
        expect(rosterOutRefs.map(contractNameForOutRef).sort()).toEqual(
          [...resolvedContractNames].sort(),
        );
        // Every fault-proof workflow drives a computation thread and mints the
        // fraud proof; every family that accuses a committed L2 block leaf also
        // proves PHAS membership.
        expect(rosterKeys).toEqual(
          expect.arrayContaining([...SHARED_THREAD_REFERENCE_KEYS]),
        );
        expect(rosterKeys.includes("phasMembershipWithdraw")).toBe(
          !NON_PHAS_MEMBERSHIP_CATEGORIES.has(category),
        );
        expect(rosterKeys.length).toBeGreaterThan(
          SHARED_THREAD_REFERENCE_KEYS.length,
        );

        // A physical step may appear once, or once per committed polarity
        // (`step02Accepted` / `step02Forced`); either way its ordinal is bound.
        const stepOrdinals = [
          ...new Set(
            rosterKeys
              .map((key) => /^step(\d{2})(?:[A-Z]\w*)?$/u.exec(key)?.[1])
              .filter((ordinal): ordinal is string => ordinal !== undefined)
              .map(Number),
          ),
        ].sort((left, right) => left - right);
        if (category === "transitionTrace") {
          // The workflow module exports its own reference roster; that export
          // is the single source of truth for what startup must preflight.
          expect(rosterKeys).toEqual(
            TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES,
          );
          expect(stepOrdinals).toEqual([]);
        } else if (category === "validationTraceDispute") {
          // The interactive dispute game names its scripts by role in the
          // game, not by a physical step ordinal.
          expect(stepOrdinals).toEqual([]);
          expect(rosterKeys).toEqual(
            expect.arrayContaining([
              "opener",
              "source",
              "game",
              "boundary",
              "timeout",
              "award",
            ]),
          );
        } else {
          // The physical steps of a thread are contiguous from step01: a
          // dropped or renamed intermediate step leaves a gap here.
          expect(stepOrdinals.length).toBeGreaterThan(0);
          expect(stepOrdinals).toEqual(
            Array.from(
              { length: stepOrdinals.length },
              (_unused, index) => index + 1,
            ),
          );
        }
      }
      // The roster is complete, so this element type is `never` and the body
      // is unreachable. The guard stays so that re-opening a residue entry is
      // still required to fail closed at startup.
      for (const category of WATCHER_MISSING_WORKFLOW_CATEGORIES) {
        await expect(
          application.assertStartupReady(invocation(configPath, category)),
        ).rejects.toThrow(
          `watcher has no installed production workflow for ${String(category)}`,
        );
      }

      // One isolated Lucid instance and one stopped DA transport per readiness
      // preflight: startup must not leak a transport or share a wallet across
      // categories.
      expect(deps.makeLucid).toHaveBeenCalledTimes(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      );
      expect(transport.stop).toHaveBeenCalledTimes(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      );
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

  it("installs nativeScriptDecoding with all six manifest-bound physical steps", async () => {
    const directory = await mkdtemp(join(tmpdir(), "native-decoding-app-"));
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const transport = transportFactory();
    const deps = dependencies();
    try {
      const application = unsafeCreateWatcherFaultProofApplicationForTest(
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
      expect(application.installedCategories).toContain("nativeScriptDecoding");
      expect(WATCHER_MISSING_WORKFLOW_CATEGORIES).not.toContain(
        "nativeScriptDecoding",
      );
      expect(WATCHER_MISSING_WORKFLOW_CATEGORIES).not.toContain(
        "valueNotPreserved",
      );
      const readiness = await application.assertStartupReady(
        invocation(configPath, "nativeScriptDecoding"),
      );
      expect(readiness).toMatchObject({
        ready: true,
        category: "nativeScriptDecoding",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: HEADER,
      });
      // Six physical steps, each bound to a distinct deployment contract of
      // this family — a roster entry pointing at another family's script, or
      // two steps sharing one script, fails here.
      const stepEntries = Object.entries(
        readiness.referenceScriptOutRefs,
      ).filter(([key]) => /^step\d{2}$/u.test(key));
      expect(stepEntries.map(([key]) => key)).toEqual([
        "step01",
        "step02",
        "step03",
        "step04",
        "step05",
        "step06",
      ]);
      const stepContractNames = stepEntries.map(([, rosterOutRef]) =>
        contractNameForOutRef(rosterOutRef),
      );
      expect(new Set(stepContractNames).size).toBe(6);
      for (const contractName of stepContractNames) {
        expect(contractName).toMatch(/^fraudProofNativeScriptDecoding/u);
      }
      expect(transport.stop).toHaveBeenCalledTimes(1);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("fails closed on application/category, deployment, authority, and secret substitution", async () => {
    expect(() =>
      unsafeCreateWatcherFaultProofApplicationForTest(
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
      unsafeCreateWatcherFaultProofApplicationForTest(
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
      const noSecrets = unsafeCreateWatcherFaultProofApplicationForTest(
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

      const admitted = unsafeCreateWatcherFaultProofApplicationForTest(
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
      // Every catalogue category now has an installed workflow (54/54), so the
      // uninstalled-category guard is exercised with a forged non-catalogue
      // category string.
      await expect(
        admitted.assertStartupReady(
          invocation(configPath, "doubleSpend", {
            category:
              "forgedUninstalledCategory" as WorkflowAdapterReadinessInput["category"],
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
