import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  applyFamilyApplicationRecord,
  type CompleteCanonicalReplayContext,
  FAMILY_APPLICATION_REGISTRY,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type FraudProofL1Source,
  type HeaderDecision,
  MemoryFraudProofWorkflowJournalStore,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservation,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  Header,
  type Header as HeaderType,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type Provider,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { watcherFaultProofSourceId } from "../../src/fault-proofs/fault-proof-application.build-common-infrastructure.js";
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
import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import { WATCHER_HISTORY_FIXTURE_BOUNDS } from "../support/deployment-authority-fixture.js";
import {
  admittedWatcherConfig,
  AUTHORITY,
  dependencies,
  DEPLOYMENT,
  FORWARDED_RUNTIME_CONFIG_PATH,
  infrastructure,
  rawConfig,
  testFaultProofL1,
  transportFactory,
} from "./fault-proof-application.raw-config.js";

/**
 * The classifier module is the double here; everything watcher-side runs for
 * real: the application's `classifyHeader` entry, its decision-time
 * predecessor-ledger check, the retention of a decision's replay context,
 * and the runtime loader that hands that context to the family record whose
 * `requires` names it. The double mirrors the real module's admission
 * discipline: a decision it did not issue has no replay context to read.
 *
 * Two facts are under test and they are not the same set. The decision-time
 * check guards the families whose proof opens `prev_utxos_root`: a fault
 * decision for one of them on a header with a non-empty previous ledger must
 * carry the authenticated predecessor. The records' `requires.replayContext`
 * guards the families whose artifact re-derives from the admitted context, a
 * strict superset the shared application loop enforces at load time.
 */
const classifier = vi.hoisted(() => ({
  deploymentFingerprint: "",
  issued: new WeakSet<object>(),
  contexts: new WeakMap<object, CompleteCanonicalReplayContext>(),
  respond: undefined as undefined | (() => HeaderDecision),
  calls: 0,
  releaseFinality: undefined as unknown,
}));

vi.mock("@al-ft/midgard-fault-proofs", async (load) => {
  const actual = await load<typeof import("@al-ft/midgard-fault-proofs")>();
  const bound = () => ({
    deploymentFingerprint: classifier.deploymentFingerprint,
    resolvedContracts: {
      contracts: Object.fromEntries(
        [
          ["fabricatedDeposit", "depositHistoryRetentionSpend"],
          ["fabricatedWithdrawal", "withdrawalHistoryRetentionSpend"],
        ].map(([category, retention]) => [
          category,
          {
            history: {
              inlineLimitBytes: BigInt(
                WATCHER_HISTORY_FIXTURE_BOUNDS.inlineLimitBytes,
              ),
              maxPayloadBytes: BigInt(
                WATCHER_HISTORY_FIXTURE_BOUNDS.maxPayloadBytes,
              ),
              maxPayloadNodes: BigInt(
                WATCHER_HISTORY_FIXTURE_BOUNDS.maxPayloadNodes,
              ),
              retentionAddress: validatorToAddress("Preprod", {
                type: "PlutusV3",
                script: AUTHORITY.contracts[retention!]!.contract.cborHex,
              }),
            },
          },
        ]),
      ),
    },
  });
  return {
    ...actual,
    // The watcher's classifier loader binds the compiled blueprint; the
    // synthetic deployment fixture has none, so the bindings are tokens that
    // carry its deployment identity and the explicit history configuration.
    bindFraudProofWorkflowDeployment: async () => bound(),
    bindFraudProofTerminalDeployment: async () => ({
      deploymentFingerprint: classifier.deploymentFingerprint,
      releaseFinality: classifier.releaseFinality,
    }),
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

classifier.deploymentFingerprint = DEPLOYMENT;
const SOURCE_ID = "watcher-test-local-node";
const OBSERVED_SLOT = 900n;
const OBSERVED_BLOCK_HASH = "19".repeat(32);
const CONFIRMATION_DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
const OBSERVATION_DIGEST = "11".repeat(32);

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

/** The genesis-ledger block: empty previous UTxO set, so no predecessor. */
const genesisHeader: HeaderType = {
  ...header,
  prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
};
const GENESIS_HEADER_CBOR = Data.to(genesisHeader, Header);
const GENESIS_HEADER_HASH = computeHash28(
  Buffer.from(GENESIS_HEADER_CBOR, "hex"),
).toString("hex");
const genesisQueueHeader: WatcherStateQueueHeaderObservation = Object.freeze({
  ...queueHeader,
  headerHash: GENESIS_HEADER_HASH,
  headerCborHex: GENESIS_HEADER_CBOR,
  queueOutRef: `${"1a".repeat(32)}#0`,
  observedTransactionHash: "1b".repeat(32),
});
admitted.headers.add(genesisQueueHeader);

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
      Object.freeze({
        headerHash: GENESIS_HEADER_HASH,
        outRef: genesisQueueHeader.queueOutRef,
      }),
    ]),
    finalizedHeaders: Object.freeze([queueHeader, genesisQueueHeader]),
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
const genesisObservation: AuthenticatedStateQueueHeaderObservation =
  Object.freeze({
    ...observation,
    headerHash: GENESIS_HEADER_HASH,
    header: genesisHeader,
  });

const decisionDigestFor = (category: WatcherInstalledWorkflowCategory) =>
  `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category]}${"0".repeat(56)}`;

/**
 * A fault decision as the classifier module would seal it, optionally
 * carrying the replay context the module retains for live decisions. The
 * watcher reads one field of it, `predecessor`, at decision time; the record's
 * `requires` gate and the loader only ask whether the host supplied a context.
 */
const issue = (
  category: WatcherInstalledWorkflowCategory,
  replayContext: CompleteCanonicalReplayContext | undefined,
  headerHash: string = HEADER_HASH,
): HeaderDecision => {
  const decision: HeaderDecision = Object.freeze({
    schemaVersion: "midgard-production-header-decision-v1",
    classifierVersion: "midgard-production-header-classifier-v1",
    deploymentFingerprint: DEPLOYMENT,
    headerHash,
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

/** A context carrying the authenticated predecessor; its content is opaque here. */
const PREDECESSOR_CONTEXT = Object.freeze({
  predecessor: Object.freeze({}),
}) as unknown as CompleteCanonicalReplayContext;
/** A context the classifier attached for another reason (settlements, events). */
const CONTEXT_WITHOUT_PREDECESSOR = Object.freeze(
  {},
) as CompleteCanonicalReplayContext;

const PREDECESSOR_FAMILIES = WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES;
const REPLAY_CONTEXT_FAMILIES = WATCHER_INSTALLED_WORKFLOW_CATEGORIES.filter(
  (category) =>
    FAMILY_APPLICATION_REGISTRY[category].requires.includes("replayContext"),
);
/** Artifact re-derives from the context, but tolerates an absent predecessor. */
const REPLAY_CONTEXT_ONLY_FAMILIES = REPLAY_CONTEXT_FAMILIES.filter(
  (category) => !PREDECESSOR_FAMILIES.includes(category),
);
const OTHER_FAMILIES = WATCHER_INSTALLED_WORKFLOW_CATEGORIES.filter(
  (category) => !PREDECESSOR_FAMILIES.includes(category),
);

describe("watcher decision-time predecessor ledger", () => {
  let deps: WatcherFaultProofApplicationDependencies;
  let application: ReturnType<
    typeof unsafeCreateWatcherFaultProofApplicationForTest
  >;

  beforeAll(() => {
    deps = dependencies();
    application = unsafeCreateWatcherFaultProofApplicationForTest(
      {
        l1: testFaultProofL1(),
        watcherConfig: admittedWatcherConfig(),
        deploymentIdentity: AUTHORITY.result,
        infrastructure: infrastructure(),
        unsafeTransportFactoryForTest: transportFactory().factory,
      },
      deps,
      {
        MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
      },
    );
  });

  afterAll(async () => {
    await application?.close();
  });

  const classify = async (
    decision: HeaderDecision,
    subject: Readonly<{
      observation: AuthenticatedStateQueueHeaderObservation;
      header: WatcherStateQueueHeaderObservation;
    }> = { observation, header: queueHeader },
  ) => {
    classifier.respond = () => decision;
    try {
      return await application.classifyHeader({
        observation: subject.observation,
        stateQueueObservation: queueObservation,
        header: subject.header,
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
      journal: new MemoryFraudProofWorkflowJournalStore(),
      runtimeConfigPath: FORWARDED_RUNTIME_CONFIG_PATH,
      decisionDigest: decisionDigestFor(category),
      actuationPermit: Object.freeze({
        permitVersion: "midgard-production-workflow-actuation-permit-v1",
      }),
    } as unknown as WorkflowAdapterRunnerInput;
    const loaded = await application.unsafeLoadRuntimeForTest({
      runtimeConfigPath: FORWARDED_RUNTIME_CONFIG_PATH,
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

  const REFUSAL = (category: string) =>
    `${category} classifier decision omitted the authenticated predecessor ledger`;

  it.each(PREDECESSOR_FAMILIES)(
    "refuses a %s fault decision that carries no replay context, and retains nothing for its runner",
    async (category) => {
      const callsBefore = classifier.calls;
      await expect(classify(issue(category, undefined))).rejects.toThrow(
        REFUSAL(category),
      );
      expect(classifier.calls).toBe(callsBefore + 1);
      // The refused decision left no replay context behind. A family that
      // also requires the context at load refuses there; one that does not
      // is applied, which is why the decision-time check exists.
      if (REPLAY_CONTEXT_FAMILIES.includes(category)) {
        await expect(applyRecordFor(category)).rejects.toThrow(
          `${category} application requires replayContext, which the host did not supply`,
        );
      } else {
        await expect(applyRecordFor(category)).resolves.toBeDefined();
      }
    },
  );

  it.each(PREDECESSOR_FAMILIES)(
    "refuses a %s fault decision whose replay context lacks the predecessor",
    async (category) => {
      await expect(
        classify(issue(category, CONTEXT_WITHOUT_PREDECESSOR)),
      ).rejects.toThrow(REFUSAL(category));
    },
  );

  it.each(PREDECESSOR_FAMILIES)(
    "admits a %s fault decision with the predecessor and hands the context to the family at load time",
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
      // The acting load builds its lease coordinator from nothing: removal is
      // coordinated locally and no Midgard node admin key is read.
      expect(deps.createLeaseCoordinator).toHaveBeenCalled();
      for (const call of vi.mocked(deps.createLeaseCoordinator).mock.calls) {
        expect(call).toEqual([]);
      }
      application.retainDecisionAuthorities(null);
    },
  );

  it.each(PREDECESSOR_FAMILIES)(
    "admits a %s fault decision on the genesis-ledger header without a predecessor",
    async (category) => {
      const decision = issue(category, undefined, GENESIS_HEADER_HASH);
      await expect(
        classify(decision, {
          observation: genesisObservation,
          header: genesisQueueHeader,
        }),
      ).resolves.toBe(decision);
      application.retainDecisionAuthorities(null);
    },
  );

  it.each(REPLAY_CONTEXT_ONLY_FAMILIES)(
    "admits a %s fault decision without a context at decision time; its record refuses at load",
    async (category) => {
      const decision = issue(category, undefined);
      await expect(classify(decision)).resolves.toBe(decision);
      await expect(applyRecordFor(category)).rejects.toThrow(
        `${category} application requires replayContext, which the host did not supply`,
      );
    },
  );

  it("admits every other installed family's fault decision without a replay context", async () => {
    expect(OTHER_FAMILIES).toHaveLength(
      WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length -
        PREDECESSOR_FAMILIES.length,
    );
    for (const category of OTHER_FAMILIES) {
      const decision = issue(category, undefined);
      await expect(classify(decision)).resolves.toBe(decision);
    }
  });
});

describe("watcher admitted runtime configuration", () => {
  it("uses the injected config for classify and verifyCompleted", async () => {
    const admittedConfig = rawConfig();
    const deps = dependencies();
    const sourceIds: string[] = [];
    const proverPhrase = "word ".repeat(24).trim();
    const application = unsafeCreateWatcherFaultProofApplicationForTest(
      {
        l1: {
          provider: {} as Provider,
          source: (sourceId) => {
            sourceIds.push(sourceId);
            return {
              sourceId,
              snapshotAuthority: () => {
                throw new Error("completion verification reached L1");
              },
            } as unknown as FraudProofL1Source;
          },
        },
        watcherConfig: admittedWatcherConfig(),
        deploymentIdentity: AUTHORITY.result,
        infrastructure: infrastructure(),
        unsafeTransportFactoryForTest: transportFactory().factory,
      },
      deps,
      { MIDGARD_WATCHER_PROVER_KEY: proverPhrase },
    );
    classifier.releaseFinality =
      await watcherDeploymentReleaseFinalityAuthority(
        AUTHORITY.result,
      ).verifyForWorkflow({ deploymentFingerprint: DEPLOYMENT });
    try {
      const category = OTHER_FAMILIES[0]!;
      const decision = issue(category, undefined);
      classifier.respond = () => decision;
      await expect(
        application.classifyHeader({
          observation,
          stateQueueObservation: queueObservation,
          header: queueHeader,
          authenticatedObservationDigest: OBSERVATION_DIGEST,
        }),
      ).resolves.toBe(decision);
      expect(deps.resolveSigner).toHaveBeenCalledWith({
        network: "Preprod",
        secret: proverPhrase,
      });

      await expect(
        application.verifyCompleted({
          category: "doubleSpend",
          headerHash: HEADER_HASH,
          decisionDigest: decisionDigestFor("doubleSpend"),
          entries: [],
          terminal: fundingTerminal(
            HEADER_HASH,
            "aa".repeat(32),
            "bb".repeat(32),
          ),
        }),
      ).rejects.toThrow("completion verification reached L1");
      expect(sourceIds).toContain(
        watcherFaultProofSourceId({
          category: "doubleSpend",
          manifestId: DEPLOYMENT,
          localL1Source: admittedConfig.l1.source,
        }),
      );
    } finally {
      classifier.respond = undefined;
      await application.close();
    }
  });
});
