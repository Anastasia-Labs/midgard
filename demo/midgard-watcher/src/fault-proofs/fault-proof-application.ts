import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  createTransitionTraceEventAuthority,
  PREDECESSOR_LEDGER_PROOF_CATEGORIES,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
  WORKFLOW_RUNNER_FACTORIES,
} from "@al-ft/midgard-fault-proofs";
import {
  bindFraudProofTerminalDeployment,
  bindFraudProofWorkflowDeployment,
  classifyHeader as classifyProductionHeaderV1,
  type CompleteCanonicalReplayContext,
  createCatalogueCompleteCanonicalReplay,
  createCrossBlockSettlementAuthority,
  createExternalHistoricalNativeScriptSourceRoster,
  createHeaderClassifier,
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createLocalKupmiosFraudProofRawL1SnapshotAuthority,
  createLocalKupmiosHttpOgmiosRawSource,
  createLocalStateQueueMutationLeaseCoordinator,
  FAMILY_APPLICATION_REGISTRY,
  type FamilyValidationChallengePort,
  type FraudProofCompletedVerification,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  type HeaderDecision,
  headerDecisionReplayContext,
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
  type HistoricalNativeScriptProviderRoster,
  type HistoricalNativeScriptSourceRoster,
  installWorkflowApplicationRegistry,
  journalJsonDigest,
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosExactPointNotCanonicalError,
  makeLucidForSubmit,
  normalizeJournalJson,
  parseContractDeploymentInfo,
  requireDeploymentReferenceScript,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveFamilyApplicationReferences,
  resolveProverSigner,
  restrictWorkflowFundingSigner,
  runFraudProofWorkflowCli,
  type StateQueueMutationLeaseCoordinator,
  verifyCompletedFraudProofWorkflow,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
  type WorkflowAdapterRunnerInput,
  type WorkflowApplicationRegistry,
  type WorkflowRuntimeLoader,
} from "@al-ft/midgard-fault-proofs";
import {
  CrossBlockDuplicateEventStep02DatumSchema,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  Header,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  assertWatcherWorkflowFundingProfileOverlay,
  type WatcherWorkflowFundingProfileOverlay,
  workflowFundingProfileFromOverlay,
} from "../funding/workflow-funding-profile-overlay.js";
import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type {
  WatcherConfig,
  WatcherWalletKeySource,
} from "../runtime/config.js";
import { parseWatcherConfig } from "../runtime/config.js";
import {
  assertWatcherVerifiedDeploymentAuthority,
  type VerifiedWatcherDeploymentAuthority,
} from "../runtime/deployment-authority.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherUserEventRuntime,
  type WatcherUserEventRuntime,
} from "../runtime/user-event-runtime.js";
import type { WatcherReplayTranscriptStore } from "../storage/replay-transcript-store.js";
import {
  createWatcherRetainedDaRuntimeOwner,
  createWatcherWorkflowRuntimeLoader,
  readAdmittedWatcherRuntimeConfig,
  type WatcherRetainedDaRuntimeOptions,
  type WatcherRetainedDaTransportStatus,
  type WatcherWorkflowInfrastructure,
} from "../storage/retained-da-runtime.js";
import {
  assertWatcherValidationReplayCaptureCurrent,
  captureWatcherValidationReplayTranscript,
  refreshWatcherValidationReplayCapture,
} from "./replay-transcript-capture.js";

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

export type WatcherFaultProofApplicationOptions = Readonly<{
  deploymentAuthority: VerifiedWatcherDeploymentAuthority;
  replayTranscriptStore: WatcherReplayTranscriptStore;
  userEventRuntime: WatcherUserEventRuntime;
  infrastructure: WatcherFaultProofInfrastructureAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  fundingProfileOverlay: WatcherWorkflowFundingProfileOverlay;
}>;

type WatcherFaultProofApplicationConstructionOptions = Omit<
  WatcherFaultProofApplicationOptions,
  | "fundingProfileOverlay"
  | "historicalNativeScriptCheckpointStore"
  | "deploymentAuthority"
  | "replayTranscriptStore"
  | "userEventRuntime"
> &
  Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    deploymentAuthority?: VerifiedWatcherDeploymentAuthority;
    replayTranscriptStore?: WatcherReplayTranscriptStore;
    userEventRuntime?: WatcherUserEventRuntime;
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

type WatcherHistoricalNativeScriptAuthority = Readonly<{
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
    readonly kupoHttpUrl: string;
    readonly ogmiosUrl: string;
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

const productionDependencies: WatcherFaultProofApplicationDependencies =
  Object.freeze({
    readText: async (path) => await readFile(path, "utf8"),
    canonicalPath: realpath,
    makeLucid: async ({ network, kupoHttpUrl, ogmiosUrl, slotConfig }) =>
      await makeLucidForSubmit(
        {
          network,
          provider: "Kupmios",
          slotConfig,
          kupoUrl: kupoHttpUrl,
          ogmiosUrl,
        },
        Object.freeze({}),
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

const plainRecord = (
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

const exactKeys = (
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

const canonicalAbsolutePath = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.trim() !== value ||
    !isAbsolute(value)
  ) {
    throw new Error(`${label} must be a canonical absolute path`);
  }
  return value;
};

const historicalProviderEndpoint = (value: unknown): string => {
  if (typeof value !== "string" || value.trim() !== value) {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  let endpoint: URL;
  try {
    endpoint = new URL(value);
  } catch {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  endpoint.pathname = endpoint.pathname.replace(/\/+$/u, "") || "/";
  if (
    endpoint.protocol !== "https:" ||
    endpoint.username.length !== 0 ||
    endpoint.password.length !== 0 ||
    endpoint.search.length !== 0 ||
    endpoint.hash.length !== 0 ||
    ["127.0.0.1", "localhost", "::1", "[::1]"].includes(
      endpoint.hostname.toLowerCase(),
    )
  ) {
    throw new Error(
      "historical native-script provider endpoint must be fixed external HTTPS",
    );
  }
  return endpoint.toString().replace(/\/$/u, "");
};

const admitHistoricalNativeScriptHistory = (
  value: unknown,
): WatcherHistoricalNativeScriptHistoryOverlay => {
  const input = plainRecord(value, "historical native-script history overlay");
  exactKeys(
    input,
    ["sourceMode", "consistencyPolicy", "providers"],
    "historical native-script history overlay",
  );
  if (
    input.sourceMode !== "external_provider_quorum" ||
    input.consistencyPolicy !== "exact_bytes_all_providers_v1" ||
    !Array.isArray(input.providers) ||
    input.providers.length < 2 ||
    input.providers.length > 4
  ) {
    throw new Error("historical native-script history overlay is invalid");
  }
  const sourceIds = new Set<string>();
  const operators = new Set<string>();
  const endpoints = new Set<string>();
  const providers = input.providers.map((value, index) => {
    const provider = plainRecord(
      value,
      `historical native-script provider ${index.toString()}`,
    );
    exactKeys(
      provider,
      ["sourceId", "operatorIdentitySha256", "authorityEndpoint"],
      `historical native-script provider ${index.toString()}`,
    );
    const endpoint = historicalProviderEndpoint(provider.authorityEndpoint);
    if (
      typeof provider.sourceId !== "string" ||
      provider.sourceId.trim() !== provider.sourceId ||
      provider.sourceId.length === 0 ||
      typeof provider.operatorIdentitySha256 !== "string" ||
      !/^[0-9a-f]{64}$/u.test(provider.operatorIdentitySha256) ||
      sourceIds.has(provider.sourceId) ||
      operators.has(provider.operatorIdentitySha256) ||
      endpoints.has(endpoint)
    ) {
      throw new Error(
        "historical native-script providers must have distinct canonical identities and endpoints",
      );
    }
    sourceIds.add(provider.sourceId);
    operators.add(provider.operatorIdentitySha256);
    endpoints.add(endpoint);
    return Object.freeze({
      sourceId: provider.sourceId,
      operatorIdentitySha256: provider.operatorIdentitySha256,
      authorityEndpoint: endpoint,
    });
  });
  return Object.freeze({
    sourceMode: "external_provider_quorum",
    consistencyPolicy: "exact_bytes_all_providers_v1",
    providers: Object.freeze(providers),
  });
};

const admitInfrastructure = (
  value: unknown,
): WatcherFaultProofInfrastructureAuthority => {
  const input = plainRecord(value, "fault-proof infrastructure authority");
  exactKeys(
    input,
    [
      "manifestPath",
      "blueprintPath",
      "deploymentInfoPath",
      "historicalNativeScriptHistory",
    ],
    "fault-proof infrastructure authority",
  );
  return Object.freeze({
    manifestPath: canonicalAbsolutePath(input.manifestPath, "manifestPath"),
    blueprintPath: canonicalAbsolutePath(input.blueprintPath, "blueprintPath"),
    deploymentInfoPath: canonicalAbsolutePath(
      input.deploymentInfoPath,
      "deploymentInfoPath",
    ),
    historicalNativeScriptHistory: admitHistoricalNativeScriptHistory(
      input.historicalNativeScriptHistory,
    ),
  });
};

const requireCanonicalFile = async (
  path: string,
  dependencies: WatcherFaultProofApplicationDependencies,
): Promise<string> => {
  const canonical = await dependencies.canonicalPath(path);
  if (canonical !== path) {
    throw new Error(
      `production fault-proof authority path must not traverse a symlink or non-canonical segment: ${path}`,
    );
  }
  return path;
};

const readSecret = async ({
  source,
  dependencies,
  environment,
  label,
}: {
  readonly source: WatcherWalletKeySource;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
  readonly label: string;
}): Promise<string> => {
  const raw =
    source.kind === "environment"
      ? environment[source.variable]
      : await dependencies.readText(
          await requireCanonicalFile(source.path, dependencies),
        );
  const secret = raw?.trim() ?? "";
  if (secret.length === 0) {
    throw new Error(`${label} secret source is empty`);
  }
  return secret;
};

/**
 * What binds the invocation to the verified deployment and reaches its
 * published scripts: the admitted local-node authority, the manifest,
 * blueprint and deployment info, a Lucid instance over Kupo/Ogmios and the
 * resolver a family's roster is resolved through. It reads no secret, so the
 * startup-readiness path can prove the deployment's published scripts were
 * found without holding the prover wallet or the node admin key. The
 * invocation's deployment fingerprint is checked by the runtime-config reader
 * both callers admit their configuration through.
 */
type WatcherDeploymentBinding = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  localL1Source: Extract<
    WatcherConfig["l1"]["source"],
    { sourceMode: "local_node" }
  >;
  kupoHttpUrl: string;
  ogmiosUrl: string;
  lucid: LucidEvolution;
  resolveReferenceScript: WatcherWorkflowInfrastructure["resolveReferenceScript"];
}>;

const bindWatcherDeploymentAuthority = async ({
  watcherConfig,
  infrastructure,
  dependencies,
}: {
  readonly watcherConfig: WatcherConfig;
  readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
}): Promise<WatcherDeploymentBinding> => {
  const localL1Source = watcherConfig.l1.source;
  if (localL1Source.sourceMode !== "local_node") {
    throw new Error(
      "watcher production workflows require the admitted local-node L1 source",
    );
  }
  const kupo = localL1Source.queryServices.find(
    (service) => service.kind === "kupo",
  );
  const ogmios = localL1Source.queryServices.find(
    (service) => service.kind === "ogmios",
  );
  if (kupo === undefined || ogmios === undefined) {
    throw new Error("watcher local-node authority omitted Kupo or Ogmios");
  }
  const [manifestPath, blueprintPath, deploymentInfoPath] = await Promise.all([
    requireCanonicalFile(infrastructure.manifestPath, dependencies),
    requireCanonicalFile(infrastructure.blueprintPath, dependencies),
    requireCanonicalFile(infrastructure.deploymentInfoPath, dependencies),
  ]);
  const [manifestJson, blueprintJson, deploymentInfoJson] = await Promise.all([
    dependencies.readText(manifestPath),
    dependencies.readText(blueprintPath),
    dependencies.readText(deploymentInfoPath),
  ]);
  let manifest: unknown;
  let deploymentInfoValue: unknown;
  try {
    manifest = JSON.parse(manifestJson) as unknown;
    deploymentInfoValue = JSON.parse(deploymentInfoJson) as unknown;
  } catch {
    throw new Error("watcher manifest/deployment-info input is not JSON");
  }
  const manifestRecord = plainRecord(manifest, "deployment manifest");
  const manifestFinality = plainRecord(
    manifestRecord.l1Finality,
    "deployment manifest l1Finality",
  );
  if (
    manifestFinality.confirmationDepth !== watcherConfig.l1.finality.depth ||
    manifestFinality.automaticRecoveryMaxDepth !==
      watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth
  ) {
    throw new Error(
      "watcher configured finality differs from the deployment manifest",
    );
  }
  const deploymentInfo = parseContractDeploymentInfo(deploymentInfoValue);
  const lucid = await dependencies.makeLucid({
    network: watcherConfig.targetNetwork,
    slotConfig: watcherConfig.customNetwork?.slotConfig,
    kupoHttpUrl: kupo.endpoint,
    ogmiosUrl: ogmios.endpoint,
  });
  return Object.freeze({
    manifest,
    blueprintJson,
    deploymentInfo: deploymentInfoValue,
    localL1Source,
    kupoHttpUrl: kupo.endpoint,
    ogmiosUrl: ogmios.endpoint,
    lucid,
    resolveReferenceScript: async ({ contractName }) =>
      await dependencies.resolveReferenceScript({
        lucid,
        deploymentInfo,
        contractName,
      }),
  });
};

/**
 * The watcher's one loader body for an acting invocation. On top of the
 * deployment binding it reads the prover wallet and the node admin key,
 * selects the (possibly funding-restricted) signer and hands over the optional
 * parts a family may require; the family's record then binds its own
 * manifest-bound config from these inside the shared runtime, so nothing here
 * is per family. Which optional parts a family needs is the record's
 * `requires`, refused by the shared application loop.
 */
const buildCommonInfrastructure = async ({
  watcherConfig,
  invocation,
  infrastructure,
  deploymentIdentity,
  historicalNativeScriptAuthority,
  replayContexts,
  validationChallenge,
  dependencies,
  environment,
}: {
  readonly watcherConfig: WatcherConfig;
  readonly invocation: WorkflowAdapterReadinessInput;
  readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly historicalNativeScriptAuthority: WatcherHistoricalNativeScriptAuthority;
  readonly replayContexts: ReadonlyMap<string, CompleteCanonicalReplayContext>;
  readonly validationChallenge: FamilyValidationChallengePort;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
}): Promise<WatcherWorkflowInfrastructure> => {
  const { category } = invocation;
  const [binding, proverSecret] = await Promise.all([
    bindWatcherDeploymentAuthority({
      watcherConfig,
      infrastructure,
      dependencies,
    }),
    readSecret({
      source: watcherConfig.proverWallet.keySource,
      dependencies,
      environment,
      label: "watcher prover wallet",
    }),
  ]);
  const resolvedSigner = dependencies.resolveSigner({
    network: watcherConfig.targetNetwork,
    secret: proverSecret,
  });
  const executionInvocation = invocation as Partial<WorkflowAdapterRunnerInput>;
  const decisionDigest = executionInvocation.decisionDigest;
  const replayContext =
    decisionDigest === undefined
      ? undefined
      : replayContexts.get(decisionDigest);
  const signer =
    executionInvocation.fundingReservationPermit === undefined
      ? resolvedSigner
      : restrictWorkflowFundingSigner({
          signer: resolvedSigner,
          permit: executionInvocation.fundingReservationPermit,
        });
  signer.selectWallet(binding.lucid);
  return Object.freeze({
    infrastructure: Object.freeze({
      manifest: binding.manifest,
      blueprintJson: binding.blueprintJson,
      deploymentInfo: binding.deploymentInfo,
      headerHash: invocation.headerHash,
      ...(decisionDigest === undefined ? {} : { decisionDigest }),
      lucid: binding.lucid,
      signer,
      source: Object.freeze({
        sourceId: [
          "watcher-fault-proof",
          category,
          deploymentIdentity.manifestId,
          binding.localL1Source.authorityNodeId,
          binding.localL1Source.chainSync.genesisIdentitySha256,
        ].join("/"),
        kupoHttpUrl: binding.kupoHttpUrl,
        ogmiosUrl: binding.ogmiosUrl,
        timeoutMs: watcherConfig.l1.requestTimeoutMs,
      }),
      stateQueueMutationLeaseCoordinator: dependencies.createLeaseCoordinator(),
      historicalNativeScriptAuthority: Object.freeze({
        ...historicalNativeScriptAuthority,
        l1SourceRoster: await historicalNativeScriptAuthority.l1SourceRoster,
      }),
      ...(replayContext === undefined ? {} : { replayContext }),
      validationChallenge,
    }),
    resolveReferenceScript: binding.resolveReferenceScript,
  });
};

const admittedApplications = new WeakSet<object>();

/** Retains the exact runner/classifier authority captured by this module. */
export const assertWatcherFaultProofApplication = (
  application: WatcherFaultProofApplication,
): void => {
  if (!admittedApplications.has(application)) {
    throw new Error(
      "watcher fault-proof production application is not module-admitted",
    );
  }
};

const predecessorObservationForClassifier = ({
  current,
  predecessor,
}: {
  readonly current: AuthenticatedStateQueueHeaderObservation;
  readonly predecessor: WatcherStateQueueHeaderObservation;
}): AuthenticatedStateQueueHeaderObservation => {
  assertWatcherStateQueueHeaderObservation(predecessor);
  const header = Data.from(predecessor.headerCborHex, Header);
  if (
    Data.to(header, Header) !== predecessor.headerCborHex ||
    predecessor.headerHash !== current.header.prevHeaderHash
  ) {
    throw new Error(
      "watcher predecessor observation differs from the challenged HeaderV1 link",
    );
  }
  if (
    !/^(?:0|[1-9][0-9]*)$/u.test(predecessor.observedSlot) ||
    !/^(?:0|[1-9][0-9]*)$/u.test(predecessor.finalityDepth)
  ) {
    throw new Error("watcher predecessor chain point is malformed");
  }
  const confirmationDepth = BigInt(predecessor.finalityDepth);
  if (confirmationDepth > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("watcher predecessor finality depth exceeds safe range");
  }
  return Object.freeze({
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: current.sourceMode,
    provenance: current.provenance,
    chainPoint: Object.freeze({
      slot: BigInt(predecessor.observedSlot),
      blockHash: predecessor.observedBlockHash,
    }),
    confirmationDepth: Number(confirmationDepth),
    headerHash: predecessor.headerHash,
    header,
  });
};

type ApplicationConstruction = {
  readonly options: WatcherFaultProofApplicationConstructionOptions;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
  readonly allowExecution: boolean;
};

/** A non-executing test application that also exposes its runtime loader. */
type WatcherFaultProofApplicationWithLoaderForTest =
  WatcherFaultProofApplication &
    Readonly<{ unsafeLoadRuntimeForTest: WorkflowRuntimeLoader }>;

function createApplication(
  input: ApplicationConstruction &
    Readonly<{ unsafeExposeRuntimeLoaderForTest: true }>,
): WatcherFaultProofApplicationWithLoaderForTest;
function createApplication(
  input: ApplicationConstruction,
): WatcherFaultProofApplication;
function createApplication({
  options,
  dependencies,
  environment,
  allowExecution,
  unsafeExposeRuntimeLoaderForTest = false,
}: ApplicationConstruction &
  Readonly<{
    unsafeExposeRuntimeLoaderForTest?: boolean;
  }>): WatcherFaultProofApplication {
  if (unsafeExposeRuntimeLoaderForTest && allowExecution) {
    throw new Error(
      "watcher runtime loader is exposed only to a non-executing test application",
    );
  }
  const deploymentIdentity = options.deploymentIdentity;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const deploymentAuthority = options.deploymentAuthority;
  const replayTranscriptStore = options.replayTranscriptStore;
  const userEventRuntime = options.userEventRuntime;
  if (allowExecution) {
    if (
      deploymentAuthority === undefined ||
      replayTranscriptStore === undefined ||
      userEventRuntime === undefined
    ) {
      throw new Error(
        "watcher execution requires deployment/rule authority and durable replay transcripts",
      );
    }
    assertWatcherVerifiedDeploymentAuthority(deploymentAuthority);
    assertWatcherUserEventRuntime(userEventRuntime);
    if (
      userEventRuntime.deploymentFingerprint !==
        deploymentIdentity.manifestId ||
      userEventRuntime.blueprintHash !== deploymentIdentity.blueprintHash
    ) {
      throw new Error("watcher event runtime deployment authority differs");
    }
    if (deploymentAuthority.deploymentIdentity !== deploymentIdentity) {
      throw new Error("watcher application deployment authorities differ");
    }
  }
  const infrastructure = admitInfrastructure(options.infrastructure);
  if (options.historicalNativeScriptCheckpointStore === undefined) {
    throw new Error(
      "watcher application requires its historical native-script checkpoint store",
    );
  }
  if (allowExecution && options.fundingProfileOverlay === undefined) {
    throw new Error(
      "watcher application requires its signed funding-profile overlay",
    );
  }
  if (options.fundingProfileOverlay !== undefined) {
    assertWatcherWorkflowFundingProfileOverlay(options.fundingProfileOverlay);
    if (
      options.fundingProfileOverlay.deploymentFingerprint !==
        deploymentIdentity.manifestId ||
      options.fundingProfileOverlay.blueprintHash !==
        deploymentIdentity.blueprintHash
    ) {
      throw new Error(
        "watcher funding-profile overlay changed deployment identity",
      );
    }
  }
  const fundingProfile = (category: WatcherInstalledWorkflowCategory) => {
    const overlay = options.fundingProfileOverlay;
    // Install every runner at startup. Funding is required when a category
    // requests a reservation, so missing measurements do not stop observation.
    if (overlay === undefined || overlay.profiles[category] === undefined) {
      return undefined;
    }
    return workflowFundingProfileFromOverlay({ overlay, category });
  };
  const providerRoster = createHistoricalNativeScriptProviderRoster({
    deploymentFingerprint: deploymentIdentity.manifestId,
    providers: infrastructure.historicalNativeScriptHistory.providers,
  });
  const historySource = createHistoricalNativeScriptHistorySource({
    providerRoster,
  });
  if (allowExecution) {
    requireHistoricalNativeScriptHistoryAuthority({
      deploymentFingerprint: deploymentIdentity.manifestId,
      checkpointStore: options.historicalNativeScriptCheckpointStore,
      historySource,
    });
  }
  const historicalNativeScriptAuthority: WatcherHistoricalNativeScriptAuthority =
    Object.freeze({
      checkpointStore: options.historicalNativeScriptCheckpointStore,
      providerRoster,
      historySource,
      l1SourceRoster: watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      )
        .verifyForWorkflow({
          deploymentFingerprint: deploymentIdentity.manifestId,
        })
        .then((releaseFinality) =>
          createExternalHistoricalNativeScriptSourceRoster({
            providerRoster,
            releaseFinality,
          }),
        ),
    });
  const environmentSnapshot = Object.freeze({ ...environment });
  const replayContexts = new Map<string, CompleteCanonicalReplayContext>();
  let authorityGeneration = 0;
  const validationCaptures = new Map<
    string,
    Awaited<ReturnType<typeof captureWatcherValidationReplayTranscript>>
  >();
  const retainedDaOptions = {
    deploymentIdentity,
    ...(options.unsafeTransportOptionsForTest === undefined
      ? {}
      : {
          unsafeTransportOptionsForTest: options.unsafeTransportOptionsForTest,
        }),
    ...(options.unsafeTransportFactoryForTest === undefined
      ? {}
      : {
          unsafeTransportFactoryForTest: options.unsafeTransportFactoryForTest,
        }),
  };
  const retainedDaOwner =
    createWatcherRetainedDaRuntimeOwner(retainedDaOptions);
  const loaderOptions = { ...retainedDaOptions, runtimeOwner: retainedDaOwner };
  /**
   * How the validation-trace dispute reaches the challenge its classifier
   * captured for this decision. The capture is refreshed and its currency
   * asserted here, at the moment the family binds its config, so a retired
   * or superseded decision cannot be disputed.
   */
  const validationChallenge: FamilyValidationChallengePort = Object.freeze({
    currentChallenge: async ({ headerHash, decisionDigest }) => {
      const capture = validationCaptures.get(decisionDigest);
      if (
        capture === undefined ||
        capture.transcript.headerHash !== headerHash
      ) {
        throw new Error(
          "validation execution has no freshly captured classifier transcript",
        );
      }
      await refreshWatcherValidationReplayCapture(capture);
      assertWatcherValidationReplayCaptureCurrent(capture);
      if (validationCaptures.get(decisionDigest) !== capture) {
        throw new Error(
          "validation decision authority was retired during workflow loading",
        );
      }
      return capture.challenge;
    },
  });
  /** One loader for all installed families; the record does the rest. */
  const loadRuntime = createWatcherWorkflowRuntimeLoader({
    ...loaderOptions,
    buildInfrastructure: async ({ watcherConfig, invocation }) =>
      await buildCommonInfrastructure({
        watcherConfig,
        invocation,
        infrastructure,
        deploymentIdentity,
        historicalNativeScriptAuthority,
        replayContexts,
        validationChallenge,
        dependencies,
        environment: environmentSnapshot,
      }),
  });
  const runners: WatcherFaultProofApplication["runners"] = Object.freeze(
    Object.fromEntries(
      WATCHER_INSTALLED_WORKFLOW_CATEGORIES.map(
        (
          category,
        ): readonly [
          WatcherInstalledWorkflowCategory,
          WorkflowAdapterRunner,
        ] => [
          category,
          WORKFLOW_RUNNER_FACTORIES[category](
            loadRuntime,
            fundingProfile(category),
          ),
        ],
      ),
    ) as Record<WatcherInstalledWorkflowCategory, WorkflowAdapterRunner>,
  );
  const applicationRegistry = installWorkflowApplicationRegistry({
    deploymentFingerprint: deploymentIdentity.manifestId,
    requiredInstalledCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    installations: WATCHER_INSTALLED_WORKFLOW_CATEGORIES.map((category) => ({
      category,
      deploymentFingerprint: deploymentIdentity.manifestId,
      runner: runners[category],
    })),
  });
  let classifierPromise: ReturnType<typeof createHeaderClassifier> | undefined;
  const loadClassifier = (watcherConfigValue: unknown, headerHash: string) => {
    classifierPromise ??= (async () => {
      const watcherConfig = parseWatcherConfig(watcherConfigValue);
      if (watcherConfig.l1.source.sourceMode !== "local_node")
        throw new Error(
          "cross-block settlement authority requires local-node source",
        );
      const kupo = watcherConfig.l1.source.queryServices.find(
        (service) => service.kind === "kupo",
      );
      const ogmios = watcherConfig.l1.source.queryServices.find(
        (service) => service.kind === "ogmios",
      );
      if (kupo === undefined || ogmios === undefined)
        throw new Error(
          "cross-block settlement authority requires Kupo and Ogmios",
        );
      const [manifestJson, blueprintJson, deploymentInfoJson] =
        await Promise.all(
          [
            infrastructure.manifestPath,
            infrastructure.blueprintPath,
            infrastructure.deploymentInfoPath,
          ].map(
            async (path) =>
              await dependencies.readText(
                await requireCanonicalFile(path, dependencies),
              ),
          ),
        );
      const binding = await bindFraudProofWorkflowDeployment({
        manifest: JSON.parse(manifestJson!),
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentInfoJson!),
        category: "crossBlockDuplicateEvent",
        headerHash,
        proverCredential: "00".repeat(28),
        stepDatumSchemas: [
          FraudProofComputationThreadStepDatum,
          CrossBlockDuplicateEventStep02DatumSchema,
        ],
      });
      if (binding.deploymentFingerprint !== deploymentIdentity.manifestId)
        throw new Error("cross-block settlement classifier changed deployment");
      const settlementAuthority = createCrossBlockSettlementAuthority({
        binding,
        source: {
          sourceId: `watcher-settlement-history/${deploymentIdentity.manifestId}`,
          kupoHttpUrl: kupo.endpoint,
          ogmiosUrl: ogmios.endpoint,
          timeoutMs: watcherConfig.l1.requestTimeoutMs,
        },
        historySource: historicalNativeScriptAuthority.historySource,
        checkpointStore: historicalNativeScriptAuthority.checkpointStore,
      });
      const transitionBinding = await bindFraudProofWorkflowDeployment({
        manifest: JSON.parse(manifestJson!),
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentInfoJson!),
        category: "transitionTrace",
        headerHash,
        proverCredential: "00".repeat(28),
        stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
      });
      if (
        transitionBinding.deploymentFingerprint !==
        deploymentIdentity.manifestId
      )
        throw new Error("transition trace classifier changed deployment");
      const transitionTraceEventAuthority = createTransitionTraceEventAuthority(
        {
          binding: transitionBinding,
          source: {
            sourceId: `watcher-transition-events/${deploymentIdentity.manifestId}`,
            kupoHttpUrl: kupo.endpoint,
            ogmiosUrl: ogmios.endpoint,
            timeoutMs: watcherConfig.l1.requestTimeoutMs,
          },
        },
      );
      await watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      ).verifyForWorkflow({
        deploymentFingerprint: deploymentIdentity.manifestId,
      });
      const lucid = await dependencies.makeLucid({
        network: watcherConfig.targetNetwork,
        slotConfig: watcherConfig.customNetwork?.slotConfig,
        kupoHttpUrl: kupo.endpoint,
        ogmiosUrl: ogmios.endpoint,
      });
      const proverSecret = await readSecret({
        source: watcherConfig.proverWallet.keySource,
        dependencies,
        environment: environmentSnapshot,
        label: "watcher prover wallet",
      });
      const signer = dependencies.resolveSigner({
        network: watcherConfig.targetNetwork,
        secret: proverSecret,
      });
      const replayer = createCatalogueCompleteCanonicalReplay({
        lucid,
        network: watcherConfig.targetNetwork,
        hubOraclePolicyId:
          watcherDeploymentProtocolScriptAuthority(deploymentIdentity)
            .protocolScriptHashes.hubOracleMint,
        minimumConfirmationDepth: 1,
        owner: signer.paymentKeyHash,
      });
      if (
        replayer.launchScope.length !==
          WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length ||
        replayer.launchScope.some(
          (category, index) =>
            category !== WATCHER_INSTALLED_WORKFLOW_CATEGORIES[index],
        )
      )
        throw new Error(
          "Watcher classifier differs from its exact installed workflow catalogue",
        );
      return await createHeaderClassifier({
        transitionTraceEventAuthority,
        deploymentFingerprint: deploymentIdentity.manifestId,
        replayer,
        releaseFinalityAuthority:
          watcherDeploymentReleaseFinalityAuthority(deploymentIdentity),
        settlementAuthority,
        historicalReplayAuthority: Object.freeze({
          checkpointStore: historicalNativeScriptAuthority.checkpointStore,
          historySource: historicalNativeScriptAuthority.historySource,
        }),
      });
    })();
    return classifierPromise;
  };
  const methods: WatcherFaultProofApplication = {
    close: async () => {
      admittedApplications.delete(application);
      authorityGeneration += 1;
      replayContexts.clear();
      validationCaptures.clear();
      await retainedDaOwner.close();
    },
    schemaVersion: WATCHER_FAULT_PROOF_APPLICATION,
    deploymentFingerprint: deploymentIdentity.manifestId,
    installedCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    runners,
    applicationRegistry,
    retainedDaTransportStatus: retainedDaOwner.transportStatus,
    decisionUsesLocalEventHistory: (decisionDigest) =>
      validationCaptures.has(decisionDigest),
    retainDecisionAuthorities: (decisionDigest) => {
      authorityGeneration += 1;
      for (const digest of replayContexts.keys()) {
        if (digest !== decisionDigest) replayContexts.delete(digest);
      }
      for (const digest of validationCaptures.keys()) {
        if (digest !== decisionDigest) validationCaptures.delete(digest);
      }
    },
    classifyHeader: async (request) => {
      const generation = authorityGeneration;
      const input = Object.freeze({
        ...request,
        observation: structuredClone(request.observation),
      });
      assertWatcherStateQueueObservation(input.stateQueueObservation);
      assertWatcherStateQueueHeaderObservation(input.header);
      if (
        input.stateQueueObservation.deploymentIdentityDigest !==
          deploymentIdentity.manifestId ||
        !input.stateQueueObservation.finalizedHeaders.includes(input.header) ||
        input.observation.sourceMode !== "local_node" ||
        input.observation.provenance.sourceId !==
          input.stateQueueObservation.sourceId ||
        input.observation.headerHash !== input.header.headerHash ||
        Data.to(input.observation.header, Header) !==
          input.header.headerCborHex ||
        input.observation.chainPoint.slot.toString() !==
          input.header.observedSlot ||
        input.observation.chainPoint.blockHash !==
          input.header.observedBlockHash ||
        input.observation.confirmationDepth.toString() !==
          input.header.finalityDepth
      ) {
        throw new Error(
          "classifier observation differs from its authenticated queue header",
        );
      }
      if (!admittedApplications.has(application)) {
        throw new Error(
          "watcher fault-proof production application is not admitted",
        );
      }
      const runtimeConfigPath = await requireCanonicalFile(
        input.runtimeConfigPath,
        dependencies,
      );
      const watcherConfigJson = await dependencies.readText(runtimeConfigPath);
      let watcherConfig: unknown;
      try {
        watcherConfig = JSON.parse(watcherConfigJson) as unknown;
      } catch {
        throw new Error("watcher runtime configuration is not JSON");
      }
      const retainedDa = await retainedDaOwner.createRuntime(watcherConfig);
      let completedDecision: HeaderDecision;
      let pendingCapture:
        | Awaited<ReturnType<typeof captureWatcherValidationReplayTranscript>>
        | undefined;
      try {
        if (
          retainedDa.deploymentFingerprint !== deploymentIdentity.manifestId
        ) {
          throw new Error(
            "watcher retained-DA runtime changed deployment identity",
          );
        }
        const decision = await classifyProductionHeaderV1({
          classifier: await loadClassifier(
            watcherConfig,
            input.observation.headerHash,
          ),
          observation: input.observation,
          authenticatedObservationDigest: input.authenticatedObservationDigest,
          sources: retainedDa.sources,
          ...(input.predecessor === undefined
            ? {}
            : {
                predecessorObservation: predecessorObservationForClassifier({
                  current: input.observation,
                  predecessor: input.predecessor,
                }),
              }),
          ...(input.retries === undefined ? {} : { retries: input.retries }),
        });
        const replayContext = headerDecisionReplayContext(decision);
        // A header committing a non-empty previous ledger can only be proved
        // against that ledger; the genesis-ledger header (empty root) has no
        // predecessor and the classifier forbids one.
        if (
          decision.decision === "fault_detected" &&
          WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES.includes(
            decision.category,
          ) &&
          input.observation.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT &&
          replayContext?.predecessor === undefined
        ) {
          throw new Error(
            `${decision.category} classifier decision omitted the authenticated predecessor ledger`,
          );
        }
        if (
          decision.decision === "fault_detected" &&
          decision.category === "validationTraceDispute"
        ) {
          if (
            deploymentAuthority === undefined ||
            replayTranscriptStore === undefined ||
            userEventRuntime === undefined
          ) {
            throw new Error(
              "validation classification requires live deployment authority and transcript storage",
            );
          }
          const identity = {
            deploymentFingerprint: deploymentIdentity.manifestId,
            headerHash: input.header.headerHash,
            inclusionPoint: {
              transactionHash: input.header.observedTransactionHash,
              blockHash: input.header.observedBlockHash,
              blockNo: input.header.observedBlockNo,
              slot: input.header.observedSlot,
              chainPointId: input.header.observedChainPointId,
            },
          };
          const archived = await replayTranscriptStore.read(identity);
          const capture = await captureWatcherValidationReplayTranscript({
            deploymentAuthority,
            stateQueueObservation: input.stateQueueObservation,
            header: input.header,
            decision,
            userEventRuntime,
            ...(archived === null
              ? {}
              : {
                  persistedTranscriptCborHex:
                    archived.persistedTranscriptCborHex,
                }),
          });
          if (
            !(await replayTranscriptStore.compareAndSwap({
              expectedTranscriptDigest: archived?.headTranscriptDigest ?? null,
              transcript: capture.transcript,
            }))
          ) {
            throw new Error(
              "validation transcript head changed during capture; classify again",
            );
          }
          pendingCapture = capture;
        }
        completedDecision = decision;
      } finally {
        await retainedDa.close();
      }
      if (pendingCapture !== undefined) {
        await refreshWatcherValidationReplayCapture(pendingCapture);
        assertWatcherValidationReplayCaptureCurrent(pendingCapture);
      }
      if (generation !== authorityGeneration) {
        throw new Error(
          "decision authority was invalidated during classification",
        );
      }
      if (pendingCapture !== undefined) {
        validationCaptures.set(
          completedDecision.decisionDigest,
          pendingCapture,
        );
      }
      const replayContext = headerDecisionReplayContext(completedDecision);
      if (replayContext !== undefined) {
        replayContexts.set(completedDecision.decisionDigest, replayContext);
      }
      return completedDecision;
    },
    assertStartupReady: async (invocation) => {
      if (!admittedApplications.has(application)) {
        throw new Error("watcher fault-proof application is closed");
      }
      if (
        !WATCHER_INSTALLED_WORKFLOW_CATEGORIES.includes(
          invocation.category as WatcherInstalledWorkflowCategory,
        )
      ) {
        throw new Error(
          `watcher has no installed production workflow for ${invocation.category}`,
        );
      }
      const category = invocation.category as WatcherInstalledWorkflowCategory;
      // Readiness binds the deployment and resolves the family's whole roster,
      // then stops: it binds no config, constructs no workflow and reads no
      // secret, so it can neither act nor need the optional infrastructure an
      // acting invocation must hold. The prover wallet is proven present by
      // the trusted-head startup phase, not here.
      const watcherConfig = await readAdmittedWatcherRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        deploymentFingerprint: invocation.deploymentFingerprint,
        deploymentIdentity,
      });
      const { resolveReferenceScript } = await bindWatcherDeploymentAuthority({
        watcherConfig,
        infrastructure,
        dependencies,
      });
      const { referenceScriptOutRefs } =
        await resolveFamilyApplicationReferences({
          record: FAMILY_APPLICATION_REGISTRY[category],
          resolveReferenceScript,
        });
      return Object.freeze({
        schemaVersion: WATCHER_FAULT_PROOF_STARTUP_READINESS,
        ready: true,
        category,
        deploymentFingerprint: deploymentIdentity.manifestId,
        headerHash: invocation.headerHash,
        referenceScriptOutRefs,
      });
    },
    verifyCompleted: async (input) => {
      if (!admittedApplications.has(application))
        throw new Error("watcher fault-proof application is not admitted");
      if (!WATCHER_INSTALLED_WORKFLOW_CATEGORIES.includes(input.category))
        throw new Error("completed workflow category is not installed");
      const [runtimeJson, manifestJson, blueprintJson, deploymentJson] =
        await Promise.all(
          [
            input.runtimeConfigPath,
            infrastructure.manifestPath,
            infrastructure.blueprintPath,
            infrastructure.deploymentInfoPath,
          ].map(async (path) =>
            dependencies.readText(
              await requireCanonicalFile(path, dependencies),
            ),
          ),
        );
      const config = parseWatcherConfig(JSON.parse(runtimeJson!));
      if (config.l1.source.sourceMode !== "local_node")
        throw new Error(
          "completed workflow verification requires local-node authority",
        );
      const kupo = config.l1.source.queryServices.find(
        ({ kind }) => kind === "kupo",
      );
      const ogmios = config.l1.source.queryServices.find(
        ({ kind }) => kind === "ogmios",
      );
      if (kupo === undefined || ogmios === undefined)
        throw new Error("completed workflow authority omitted Kupo or Ogmios");
      const binding = await bindFraudProofTerminalDeployment({
        manifest: JSON.parse(manifestJson!),
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentJson!),
        category: input.category,
        headerHash: input.headerHash,
        proverCredential: input.terminal.economics.proverCredential,
      });
      const finality = await watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      ).verifyForWorkflow({
        deploymentFingerprint: deploymentIdentity.manifestId,
      });
      if (
        binding.deploymentFingerprint !== deploymentIdentity.manifestId ||
        journalJsonDigest(normalizeJournalJson(binding.releaseFinality)) !==
          journalJsonDigest(normalizeJournalJson(finality))
      )
        throw new Error(
          "completed workflow changed its verified deployment release",
        );
      const source = createLocalKupmiosHttpOgmiosRawSource({
        sourceId: [
          "watcher-fault-proof",
          input.category,
          deploymentIdentity.manifestId,
          config.l1.source.authorityNodeId,
          config.l1.source.chainSync.genesisIdentitySha256,
        ].join("/"),
        kupoHttpUrl: kupo.endpoint,
        ogmiosUrl: ogmios.endpoint,
        timeoutMs: config.l1.requestTimeoutMs,
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      });
      const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
        source,
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      });
      try {
        return await verifyCompletedFraudProofWorkflow({
          binding,
          authority,
          entries: input.entries,
          terminal: input.terminal,
          decisionDigest: input.decisionDigest,
        });
      } catch (error) {
        if (
          error instanceof LocalKupmiosCheckpointChangedError ||
          error instanceof LocalKupmiosExactPointNotCanonicalError
        )
          return { kind: "pending", reason: "checkpoint_changed" };
        throw error;
      }
    },
    runOrResume: async (invocation) => {
      if (!allowExecution) {
        throw new Error(
          "unsafe watcher fault-proof test application cannot execute transactions",
        );
      }
      if (!admittedApplications.has(application)) {
        throw new Error(
          "watcher fault-proof production application is not admitted",
        );
      }
      return await runFraudProofWorkflowCli({
        ...invocation,
        applicationRegistry,
      });
    },
  };
  const application: WatcherFaultProofApplication = Object.freeze(
    unsafeExposeRuntimeLoaderForTest
      ? { ...methods, unsafeLoadRuntimeForTest: loadRuntime }
      : methods,
  );
  admittedApplications.add(application);
  return application;
}

export const createWatcherFaultProofApplication = (
  options: WatcherFaultProofApplicationOptions,
): WatcherFaultProofApplication =>
  createApplication({
    options: Object.freeze({
      deploymentIdentity: options.deploymentAuthority.deploymentIdentity,
      deploymentAuthority: options.deploymentAuthority,
      replayTranscriptStore: options.replayTranscriptStore,
      userEventRuntime: options.userEventRuntime,
      infrastructure: options.infrastructure,
      historicalNativeScriptCheckpointStore:
        options.historicalNativeScriptCheckpointStore,
      fundingProfileOverlay: options.fundingProfileOverlay,
    }),
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: true,
  });

/** Bind installed production workflows without exposing execution or classification. */
export const createWatcherFaultProofReadinessApplication = (
  options: Pick<
    WatcherFaultProofApplicationOptions,
    | "deploymentAuthority"
    | "infrastructure"
    | "historicalNativeScriptCheckpointStore"
    | "fundingProfileOverlay"
  >,
): Pick<
  WatcherFaultProofApplication,
  "installedCategories" | "assertStartupReady" | "close"
> => {
  assertWatcherVerifiedDeploymentAuthority(options.deploymentAuthority);
  const application = createApplication({
    options: {
      ...options,
      deploymentIdentity: options.deploymentAuthority.deploymentIdentity,
    },
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: false,
  });
  return Object.freeze({
    installedCategories: application.installedCategories,
    assertStartupReady: application.assertStartupReady,
    close: application.close,
  });
};

/**
 * Narrow test-only dependency seam. It cannot execute transactions, and it
 * exposes the one runtime loader its runners load through so a test can prove
 * what the watcher supplies to a family without holding an actuation permit.
 */
export const unsafeCreateWatcherFaultProofApplicationForTest = (
  options: WatcherFaultProofApplicationConstructionOptions,
  dependencies: WatcherFaultProofApplicationDependencies,
  environment: NodeJS.ProcessEnv = {},
): WatcherFaultProofApplicationWithLoaderForTest =>
  createApplication({
    options,
    dependencies,
    environment,
    allowExecution: false,
    unsafeExposeRuntimeLoaderForTest: true,
  });
