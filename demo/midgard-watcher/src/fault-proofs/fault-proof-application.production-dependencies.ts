import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import { PREDECESSOR_LEDGER_PROOF_CATEGORIES } from "@al-ft/midgard-fault-proofs";
import {
  createLocalStateQueueMutationLeaseCoordinator,
  FAMILY_APPLICATION_REGISTRY,
  type FraudProofCompletedVerification,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  type HeaderDecision,
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
  type HistoricalNativeScriptProviderRoster,
  type HistoricalNativeScriptSourceRoster,
  parseContractDeploymentInfo,
  requireDeploymentReferenceScript,
  resolveProverSigner,
  type StateQueueMutationLeaseCoordinator,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
  type WorkflowAdapterRunnerInput,
  type WorkflowApplicationRegistry,
} from "@al-ft/midgard-fault-proofs";
import { type FraudProofL1Source } from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservation,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import {
  Lucid,
  type LucidEvolution,
  type Provider,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type WatcherWorkflowFundingProfileOverlay } from "../funding/workflow-funding-profile-overlay.js";
import {
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherUserEvents } from "../l1-follower/user-events.js";
import type { WatcherConfig } from "../runtime/config.js";
import { type VerifiedWatcherDeploymentAuthority } from "../runtime/deployment-authority.js";
import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import type { WatcherReplayTranscriptStore } from "../storage/replay-transcript-store.js";
import {
  type WatcherRetainedDaRuntimeOptions,
  type WatcherRetainedDaTransportStatus,
} from "../storage/retained-da-runtime.js";

export const WATCHER_FAULT_PROOF_APPLICATION =
  "midgard-watcher-fault-proof-production-application-v1" as const;

export const WATCHER_FAULT_PROOF_STARTUP_READINESS =
  "midgard-watcher-fault-proof-startup-readiness-v1" as const;

/**
 * Header hash carried by every startup readiness invocation. Readiness binds
 * the deployment and resolves the family roster for no particular header, and
 * nothing on that path checks the hash against L1; the shared readiness input
 * still requires one, so it is this fixed placeholder.
 */
export const WATCHER_STARTUP_READINESS_HEADER_HASH = "00".repeat(28);

export type WatcherHistoricalNativeScriptHistoryOverlay = Readonly<{
  sourceMode: "external_provider_quorum";
  consistencyPolicy: "exact_bytes_all_providers_v1";
  providers: readonly Readonly<{
    sourceId: string;
    operatorIdentitySha256: string;
    authorityEndpoint: string;
  }>[];
}>;

/**
 * The installed set is the family application registry's keys, read in the
 * catalogue's presentation order so that every launch-scope comparison in the
 * watcher (classifier, supervisor, decision bridge) sees the one order the SDK
 * catalogue defines. Nothing here names a family.
 */
export type WatcherInstalledWorkflowCategory =
  keyof typeof FAMILY_APPLICATION_REGISTRY;

const registeredWorkflowCategories: ReadonlySet<string> = new Set(
  Object.keys(FAMILY_APPLICATION_REGISTRY),
);

export const WATCHER_INSTALLED_WORKFLOW_CATEGORIES: readonly WatcherInstalledWorkflowCategory[] =
  Object.freeze(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter((category) =>
      registeredWorkflowCategories.has(category),
    ),
  );

/** Catalogue categories the registry does not carry: empty since #673. */
export const WATCHER_MISSING_WORKFLOW_CATEGORIES: readonly FraudProofCatalogueCategoryName[] =
  Object.freeze(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
      (category) => !registeredWorkflowCategories.has(category),
    ),
  );

if (
  registeredWorkflowCategories.size !==
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length
) {
  throw new Error(
    "family application registry names a category outside the fraud-proof catalogue",
  );
}

/**
 * The installed families whose proof opens the challenged header's
 * `prev_utxos_root`. The classifier decides `unprovable` for them when it
 * lacks the authenticated predecessor; the decision-time check re-checks that
 * invariant on every fault decision it receives, reading the same set the
 * classifier does. A record's `requires.replayContext` is a different fact
 * and the shared application loop enforces it at load time.
 */
export const WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES: readonly WatcherInstalledWorkflowCategory[] =
  Object.freeze(
    WATCHER_INSTALLED_WORKFLOW_CATEGORIES.filter((category) =>
      (
        PREDECESSOR_LEDGER_PROOF_CATEGORIES as readonly FraudProofCatalogueCategoryName[]
      ).includes(category),
    ),
  );

export type WatcherFaultProofInfrastructureAuthority = Readonly<{
  manifestPath: string;
  blueprintPath: string;
  deploymentInfoPath: string;
  historicalNativeScriptHistory: WatcherHistoricalNativeScriptHistoryOverlay;
}>;

/**
 * The application's L1: the watcher's chain follower. `source` is the
 * families' raw-read and signed-recovery source under one persisted source
 * id; `provider` is the Lucid provider every binding builds through.
 */
export type WatcherFaultProofL1 = Readonly<{
  source(sourceId: string): FraudProofL1Source;
  provider: Provider;
}>;

export type WatcherFaultProofApplicationOptions = Readonly<{
  l1: WatcherFaultProofL1;
  deploymentAuthority: VerifiedWatcherDeploymentAuthority;
  replayTranscriptStore: WatcherReplayTranscriptStore;
  userEvents: WatcherUserEvents;
  infrastructure: WatcherFaultProofInfrastructureAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  fundingProfileOverlay: WatcherWorkflowFundingProfileOverlay;
}>;

export type WatcherFaultProofApplicationConstructionOptions = Omit<
  WatcherFaultProofApplicationOptions,
  | "fundingProfileOverlay"
  | "historicalNativeScriptCheckpointStore"
  | "deploymentAuthority"
  | "replayTranscriptStore"
  | "userEvents"
> &
  Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    deploymentAuthority?: VerifiedWatcherDeploymentAuthority;
    replayTranscriptStore?: WatcherReplayTranscriptStore;
    userEvents?: WatcherUserEvents;
    historicalNativeScriptCheckpointStore?: HistoricalNativeScriptCheckpointStore;
    fundingProfileOverlay?: WatcherWorkflowFundingProfileOverlay;
    unsafeTransportOptionsForTest?: WatcherRetainedDaRuntimeOptions["unsafeTransportOptionsForTest"];
    unsafeTransportFactoryForTest?: WatcherRetainedDaRuntimeOptions["unsafeTransportFactoryForTest"];
  }>;

export type WatcherFaultProofStartupReadiness = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_STARTUP_READINESS;
  ready: true;
  category: WatcherInstalledWorkflowCategory;
  deploymentFingerprint: string;
  headerHash: string;
  referenceScriptOutRefs: Readonly<Record<string, string>>;
}>;

export type WatcherFaultProofHeaderClassificationInput = Readonly<{
  runtimeConfigPath: string;
  observation: AuthenticatedStateQueueHeaderObservation;
  stateQueueObservation: WatcherAuthenticatedStateQueueObservation;
  header: WatcherStateQueueHeaderObservation;
  authenticatedObservationDigest: string;
  /** Opaque local-node predecessor; its public retained DA is fetched here. */
  predecessor?: WatcherStateQueueHeaderObservation;
  retries?: number;
}>;

export type WatcherCompletedFaultProofInput = Readonly<{
  runtimeConfigPath: string;
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
  decisionDigest: string;
  entries: readonly FraudProofWorkflowJournalEntry[];
  terminal: FraudProofWorkflowTerminal;
}>;

export type WatcherCompletedFaultProofVerification =
  FraudProofCompletedVerification;

export type WatcherFaultProofApplication = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_APPLICATION;
  deploymentFingerprint: string;
  installedCategories: readonly WatcherInstalledWorkflowCategory[];
  runners: Readonly<
    Record<WatcherInstalledWorkflowCategory, WorkflowAdapterRunner>
  >;
  applicationRegistry: WorkflowApplicationRegistry;
  classifyHeader(
    input: WatcherFaultProofHeaderClassificationInput,
  ): Promise<HeaderDecision>;
  /** Retire private replay authority after target selection or invalidation. */
  retainDecisionAuthorities(decisionDigest: string | null): void;
  /** Whether an admitted decision retains capabilities of the live event head. */
  decisionUsesLocalEventHistory(decisionDigest: string): boolean;
  assertStartupReady(
    invocation: WorkflowAdapterReadinessInput,
  ): Promise<WatcherFaultProofStartupReadiness>;
  /** Live state of the shared retained-DA transport, for operations status. */
  retainedDaTransportStatus(): WatcherRetainedDaTransportStatus;
  verifyCompleted(
    input: WatcherCompletedFaultProofInput,
  ): Promise<WatcherCompletedFaultProofVerification>;
  runOrResume(invocation: WorkflowAdapterRunnerInput): Promise<unknown>;
  close(): Promise<void>;
}>;

export type WatcherHistoricalNativeScriptAuthority = Readonly<{
  checkpointStore: HistoricalNativeScriptCheckpointStore;
  providerRoster: HistoricalNativeScriptProviderRoster;
  historySource: HistoricalNativeScriptHistorySource;
  l1SourceRoster: Promise<HistoricalNativeScriptSourceRoster>;
}>;

export type WatcherFaultProofApplicationDependencies = Readonly<{
  readText(path: string): Promise<string>;
  canonicalPath(path: string): Promise<string>;
  makeLucid(input: {
    readonly network: WatcherConfig["targetNetwork"];
    readonly provider: Provider;
    readonly slotConfig?: NonNullable<
      WatcherConfig["customNetwork"]
    >["slotConfig"];
  }): Promise<LucidEvolution>;
  resolveSigner(input: {
    readonly network: WatcherConfig["targetNetwork"];
    readonly secret: string;
  }): ReturnType<typeof resolveProverSigner>;
  resolveReferenceScript(input: {
    readonly lucid: LucidEvolution;
    readonly deploymentInfo: ReturnType<typeof parseContractDeploymentInfo>;
    readonly contractName: string;
  }): Promise<UTxO>;
  /**
   * Non-tail removal is coordinated locally: the workflow orchestrator retries
   * a lost peel against a fresh authenticated L1 view until it confirms. The
   * watcher never talks to a Midgard node.
   */
  createLeaseCoordinator(): StateQueueMutationLeaseCoordinator;
}>;

export const productionDependencies: WatcherFaultProofApplicationDependencies =
  Object.freeze({
    readText: async (path) => await readFile(path, "utf8"),
    canonicalPath: realpath,
    makeLucid: async ({ network, provider, slotConfig }) =>
      await Lucid(
        provider,
        network,
        slotConfig === undefined ? {} : { slotConfig },
      ),
    resolveSigner: ({ network, secret }) =>
      resolveProverSigner(
        secret.startsWith("ed25519_sk")
          ? { network, walletPrivateKey: secret }
          : { network, walletSeedPhrase: secret },
        Object.freeze({}),
      ),
    resolveReferenceScript: async ({ lucid, deploymentInfo, contractName }) =>
      await requireDeploymentReferenceScript({
        lucid,
        deploymentInfo,
        name: contractName,
      }),
    createLeaseCoordinator: () =>
      createLocalStateQueueMutationLeaseCoordinator(),
  });

export const plainRecord = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  const result = value as Readonly<Record<string, unknown>>;
  for (const key of Object.keys(result)) {
    const descriptor = Object.getOwnPropertyDescriptor(result, key);
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      throw new Error(`${label} must not contain accessors`);
    }
  }
  return result;
};

export const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  keys: readonly string[],
  label: string,
): void => {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
};

export const canonicalAbsolutePath = (
  value: unknown,
  label: string,
): string => {
  if (
    typeof value !== "string" ||
    value.trim() !== value ||
    !isAbsolute(value)
  ) {
    throw new Error(`${label} must be a canonical absolute path`);
  }
  return value;
};
