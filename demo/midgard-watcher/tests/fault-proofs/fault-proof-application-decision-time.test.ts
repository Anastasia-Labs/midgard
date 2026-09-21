import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import {
  applyFamilyApplicationRecord,
  type CompleteCanonicalReplayContext,
  FAMILY_APPLICATION_REGISTRY,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type HeaderDecision,
  type ResolvedProverSigner,
  type StateQueueMutationLeaseCoordinator,
  unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservation,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  Header,
  type Header as HeaderType,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  unsafeCreateWatcherFaultProofApplicationForTest,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES,
  type WatcherFaultProofApplicationDependencies,
  type WatcherInstalledWorkflowCategory,
} from "../../src/fault-proofs/fault-proof-application.js";
import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherStateQueueHeaderObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import { WatcherPublicDaLibp2pTransport } from "../../src/storage/public-da-libp2p-transport.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

/**
 * The classifier module is the double here; everything watcher-side runs for
 * real: the application's `classifyHeader` entry, its decision-time
 * predecessor-authority check, the retention of a decision's replay context,
 * and the runtime loader that hands that context to the family record whose
 * `requires` names it. The double mirrors the real module's admission
 * discipline: a decision it did not issue has no replay context to read.
 */
const classifier = vi.hoisted(() => ({
  deploymentFingerprint: "",
  issued: new WeakSet<object>(),
  contexts: new WeakMap<object, CompleteCanonicalReplayContext>(),
  respond: undefined as undefined | (() => HeaderDecision),
  calls: 0,
}));

vi.mock("@al-ft/midgard-fault-proofs", async (load) => {
  const actual = await load<typeof import("@al-ft/midgard-fault-proofs")>();
  const bound = () => ({
    deploymentFingerprint: classifier.deploymentFingerprint,
  });
  return {
    ...actual,
    // The watcher's classifier loader binds the compiled blueprint; the
    // synthetic deployment fixture has none, so the bindings are tokens that
    // carry the one field the loader checks.
    bindFraudProofWorkflowDeployment: async () => bound(),
    createCrossBlockSettlementAuthority: () => bound(),
    createTransitionTraceEventAuthority: () => bound(),
    createHeaderClassifier: async (
      input: Parameters<typeof actual.createHeaderClassifier>[0],
    ) => ({
      classifierVersion: "midgard-production-header-classifier-v1",
      deploymentFingerprint: input.deploymentFingerprint,
      launchScope: input.replayer.launchScope,
    }),
    classifyHeader: async () => {
      classifier.calls += 1;
      if (classifier.respond === undefined) {
        throw new Error("test issued no classifier decision");
      }
      return classifier.respond();
    },
    headerDecisionReplayContext: (decision: HeaderDecision) => {
      if (!classifier.issued.has(decision)) {
        throw new Error("production header decision was not module-admitted");
      }
      return classifier.contexts.get(decision);
    },
  };
});

/** Hand-built queue observations stand in for the production source's admission. */
const admitted = vi.hoisted(() => ({
  observations: new WeakSet<object>(),
  headers: new WeakSet<object>(),
}));

vi.mock(
  "../../src/indexers/authenticated-state-queue-observation.js",
  async (load) => {
    const actual =
      await load<
        typeof import("../../src/indexers/authenticated-state-queue-observation.js")
      >();
    return {
      ...actual,
      assertWatcherStateQueueObservation: (observation: object) => {
        if (!admitted.observations.has(observation)) {
          throw new Error("test queue observation was not admitted");
        }
      },
      assertWatcherStateQueueHeaderObservation: (header: object) => {
        if (!admitted.headers.has(header)) {
          throw new Error("test queue header was not admitted");
        }
      },
    };
  },
);

const AUTHORITY = makeWatcherDeploymentAuthorityFixture();
const DEPLOYMENT = AUTHORITY.result.manifestId;
classifier.deploymentFingerprint = DEPLOYMENT;
const SOURCE_ID = "watcher-test-local-node";
const OBSERVED_SLOT = 900n;
const OBSERVED_BLOCK_HASH = "19".repeat(32);
const CONFIRMATION_DEPTH = 30;
const OBSERVATION_DIGEST = "11".repeat(32);
const MANIFEST_PATH = "/etc/midgard/deployment-manifest-v1.json";
const BLUEPRINT_PATH = "/etc/midgard/plutus.json";
const DEPLOYMENT_INFO_PATH = "/etc/midgard/contract-deployment-info.json";
const PEER_ID = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";

/** A challenged block with a predecessor: non-empty previous UTxO set. */
const header: HeaderType = {
  prevUtxosRoot: "01".repeat(32),
  transactionsRoot: "02".repeat(32),
  utxosRoot: "03".repeat(32),
  depositsRoot: "04".repeat(32),
  withdrawalsRoot: "05".repeat(32),
  forcedTransactionsRoot: "06".repeat(32),
  transitionTraceRoot: "07".repeat(32),
  eventToStepRoot: "08".repeat(32),
  validationTracesRoot: "09".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "0a".repeat(28),
  operatorVkey: "0b".repeat(28),
  protocolVersion: 1n,
};
const HEADER_CBOR = Data.to(header, Header);
const HEADER_HASH = computeHash28(Buffer.from(HEADER_CBOR, "hex")).toString(
  "hex",
);

const queueHeader: WatcherStateQueueHeaderObservation = Object.freeze({
  headerHash: HEADER_HASH,
  headerCborHex: HEADER_CBOR,
  stateQueueNodeCborHex: "d87980",
  linkedListDatumCborHex: "d87980",
  daAvailability: "Unattested",
  queueOutRef: `${"17".repeat(32)}#0`,
  nextHeaderHash: null,
  observedTransactionHash: "18".repeat(32),
  observedBlockHash: OBSERVED_BLOCK_HASH,
  observedSlot: OBSERVED_SLOT.toString(),
  observedBlockNo: "90",
  observedChainPointId: "20".repeat(32),
  finalityDepth: CONFIRMATION_DEPTH.toString(),
});
admitted.headers.add(queueHeader);

const queueObservation: WatcherAuthenticatedStateQueueObservation =
  Object.freeze({
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: DEPLOYMENT,
    protocolScriptAuthorityDigest: "10".repeat(32),
    stateQueuePolicyId: "11".repeat(28),
    hubOraclePolicyId: "12".repeat(28),
    nativePoint: Object.freeze({
      blockHash: "13".repeat(32),
      parentBlockHash: "14".repeat(32),
      slot: "1000",
      blockNo: "100",
      chainPointId: "15".repeat(32),
      finalityDepth: CONFIRMATION_DEPTH.toString(),
    }),
    sourceId: SOURCE_ID,
    previousObservationDigest: null,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze([
      Object.freeze({ headerHash: null, outRef: `${"16".repeat(32)}#0` }),
      Object.freeze({
        headerHash: HEADER_HASH,
        outRef: queueHeader.queueOutRef,
      }),
    ]),
    finalizedHeaders: Object.freeze([queueHeader]),
    finalizedCorrectionLock: null,
    correctionLockWitnesses: Object.freeze([]),
    observationDigest: OBSERVATION_DIGEST,
  });
admitted.observations.add(queueObservation);

const observation: AuthenticatedStateQueueHeaderObservation = Object.freeze({
  schemaVersion: "midgard-canonical-evidence-source-v1",
  sourceMode: "local_node",
  provenance: Object.freeze({
    trustClass: "authenticated_cardano_l1",
    sourceId: SOURCE_ID,
    grade: "security",
  }),
  chainPoint: Object.freeze({
    slot: OBSERVED_SLOT,
    blockHash: OBSERVED_BLOCK_HASH,
  }),
  confirmationDepth: CONFIRMATION_DEPTH,
  headerHash: HEADER_HASH,
  header,
});

const decisionDigestFor = (category: WatcherInstalledWorkflowCategory) =>
  `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category]}${"0".repeat(56)}`;

/**
 * A fault decision as the classifier module would seal it, optionally
 * carrying the predecessor replay context the module retains for live
 * decisions. The context's content is opaque to the watcher: the record's
 * `requires` gate and the loader only ask whether the host supplied one.
 */
const issue = (
  category: WatcherInstalledWorkflowCategory,
  replayContext: CompleteCanonicalReplayContext | undefined,
): HeaderDecision => {
  const decision: HeaderDecision = Object.freeze({
    schemaVersion: "midgard-production-header-decision-v1",
    classifierVersion: "midgard-production-header-classifier-v1",
    deploymentFingerprint: DEPLOYMENT,
    headerHash: HEADER_HASH,
    authenticatedObservationDigest: OBSERVATION_DIGEST,
    payloadEnvelopeSha256: "26".repeat(32),
    payloadSha256: "27".repeat(32),
    replayVersion: "midgard-complete-canonical-replay-v1",
    replayDigest: "28".repeat(32),
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    launchScopeDigest: "29".repeat(32),
    classificationDigest: "2a".repeat(32),
    decisionDigest: decisionDigestFor(category),
    decision: "fault_detected",
    category,
    violationId: `${category}_v1`,
    detectionId: `${category}_v1:0`,
    position: "0",
  });
  classifier.issued.add(decision);
  if (replayContext !== undefined)
    classifier.contexts.set(decision, replayContext);
  return decision;
};

const PREDECESSOR_CONTEXT = Object.freeze({}) as CompleteCanonicalReplayContext;

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

const dependencies = (): WatcherFaultProofApplicationDependencies => {
  const signer: ResolvedProverSigner = {
    source: "test",
    address: "addr_test1vqpzry9x8gf2tvdw0s3jn54khce6mua7l0yp4rx3z9g4zpq0j52c7",
    paymentKeyHash: "01".repeat(28),
    selectWallet: vi.fn(),
  };
  const lease: StateQueueMutationLeaseCoordinator = {
    acquire: vi.fn(async () => {
      throw new Error("classification must not acquire a mutation lease");
    }),
  };
  return {
    readText: vi.fn(async (path: string) => {
      if (path === MANIFEST_PATH) {
        return JSON.stringify(AUTHORITY.signedIdentity.manifest);
      }
      if (path === BLUEPRINT_PATH) return "{}";
      if (path.endsWith("/watcher.json")) return JSON.stringify(rawConfig());
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
      const referenceIndex = Object.keys(AUTHORITY.contracts).indexOf(
        contractName,
      );
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

const transportFactory = () => {
  const transport = Object.create(
    WatcherPublicDaLibp2pTransport.prototype,
  ) as WatcherPublicDaLibp2pTransport;
  Object.defineProperties(transport, {
    request: { value: vi.fn(async () => new Uint8Array([0xf6])) },
    stop: { value: vi.fn(async () => undefined) },
  });
  return vi.fn(async () => transport);
};

const PREDECESSOR_FAMILIES = WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES;
// The dispute family's decision path continues into transcript capture,
// which the replay-capture tests exercise against a real event runtime.
const OTHER_FAMILIES = WATCHER_INSTALLED_WORKFLOW_CATEGORIES.filter(
  (category) =>
    !PREDECESSOR_FAMILIES.includes(category) &&
    category !== "validationTraceDispute",
);

describe("watcher decision-time predecessor authority", () => {
  let directory: string;
  let configPath: string;
  let deps: WatcherFaultProofApplicationDependencies;
  let application: ReturnType<
    typeof unsafeCreateWatcherFaultProofApplicationForTest
  >;

  beforeAll(async () => {
    directory = await mkdtemp(join(tmpdir(), "midgard-decision-time-"));
    configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    deps = dependencies();
    application = unsafeCreateWatcherFaultProofApplicationForTest(
      {
        deploymentIdentity: AUTHORITY.result,
        historicalNativeScriptCheckpointStore:
          unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest(),
        infrastructure: {
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
        },
        unsafeTransportFactoryForTest: transportFactory(),
      },
      deps,
      {
        MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
        MIDGARD_NODE_ADMIN_KEY: "admin-key",
      },
    );
  });

  afterAll(async () => {
    await application?.close();
    await rm(directory, { recursive: true, force: true });
  });

  const classify = async (decision: HeaderDecision) => {
    classifier.respond = () => decision;
    try {
      return await application.classifyHeader({
        runtimeConfigPath: configPath,
        observation,
        stateQueueObservation: queueObservation,
        header: queueHeader,
        authenticatedObservationDigest: OBSERVATION_DIGEST,
      });
    } finally {
      classifier.respond = undefined;
    }
  };

  /**
   * What the family's runner would see: the watcher loads the runtime for
   * the decision and applies the family's real record, whose `requires`
   * gate is the only thing under test, so construction is stubbed.
   */
  const applyRecordFor = async <
    Category extends WatcherInstalledWorkflowCategory,
  >(
    category: Category,
  ) => {
    const execution = {
      mode: "run",
      category,
      deploymentFingerprint: DEPLOYMENT,
      headerHash: HEADER_HASH,
      journalDirectory: "/var/lib/midgard-watcher/fraud-proof-journals",
      runtimeConfigPath: configPath,
      decisionDigest: decisionDigestFor(category),
      actuationPermit: Object.freeze({
        permitVersion: "midgard-production-workflow-actuation-permit-v1",
      }),
    } as unknown as WorkflowAdapterRunnerInput;
    const loaded = await application.unsafeLoadRuntimeForTest({
      runtimeConfigPath: configPath,
      invocation: execution,
    });
    const resolvedBefore = vi.mocked(deps.resolveReferenceScript).mock.calls
      .length;
    const entry = FAMILY_APPLICATION_REGISTRY[category];
    const record = {
      ...entry,
      bindConfig: () => undefined,
      constructWorkflow: async () => ({
        binding: {
          deploymentFingerprint: DEPLOYMENT,
          definition: { category, headerHash: HEADER_HASH },
        },
        decisionDigest: execution.decisionDigest,
      }),
      execute: async () => undefined,
    } as unknown as FamilyApplicationRecord<
      Category,
      unknown,
      FamilyApplicationWorkflowIdentity<Category>
    >;
    try {
      const applied = await applyFamilyApplicationRecord({
        record,
        infrastructure: loaded.infrastructure,
        resolveReferenceScript: loaded.resolveReferenceScript,
        invocation: {
          deploymentFingerprint: DEPLOYMENT,
          category,
          headerHash: HEADER_HASH,
        },
      });
      return {
        applied,
        resolvedContractNames: vi
          .mocked(deps.resolveReferenceScript)
          .mock.calls.slice(resolvedBefore)
          .map(([input]) => input.contractName)
          .sort(),
      };
    } finally {
      await loaded.close();
    }
  };

  it.each(PREDECESSOR_FAMILIES)(
    "refuses a %s fault decision that omits predecessor authority, and retains nothing for its runner",
    async (category) => {
      const callsBefore = classifier.calls;
      await expect(classify(issue(category, undefined))).rejects.toThrow(
        `${category} classifier decision omitted predecessor authority`,
      );
      expect(classifier.calls).toBe(callsBefore + 1);
      // The refused decision left no replay context behind: the family's
      // record, applied for that decision digest, refuses at its own gate.
      await expect(applyRecordFor(category)).rejects.toThrow(
        `${category} application requires replayContext, which the host did not supply`,
      );
    },
  );

  it.each(PREDECESSOR_FAMILIES)(
    "admits a %s fault decision with predecessor authority and hands the context to the family at load time",
    async (category) => {
      const decision = issue(category, PREDECESSOR_CONTEXT);
      await expect(classify(decision)).resolves.toBe(decision);
      const { applied, resolvedContractNames } = await applyRecordFor(category);
      expect(Object.keys(applied.referenceScriptOutRefs)).toEqual(
        Object.keys(FAMILY_APPLICATION_REGISTRY[category].roster),
      );
      expect(resolvedContractNames).toEqual(
        Object.values(FAMILY_APPLICATION_REGISTRY[category].roster).sort(),
      );
      application.retainDecisionAuthorities(null);
    },
  );

  it("admits every other installed family's fault decision without predecessor authority", async () => {
    expect(OTHER_FAMILIES).toHaveLength(
      WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length -
        PREDECESSOR_FAMILIES.length -
        1,
    );
    for (const category of OTHER_FAMILIES) {
      const decision = issue(category, undefined);
      await expect(classify(decision)).resolves.toBe(decision);
    }
  });
});
