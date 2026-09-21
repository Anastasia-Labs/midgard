import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  workflowJournalIsReconciliationOnly,
} from "./actuation-permit.js";
import {
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
  type WorkflowAdapterRunnerInput,
} from "./adapters.js";
import {
  assertManifestBoundWorkflowIdentity,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
} from "./family-application.js";
import {
  FAMILY_APPLICATION_REGISTRY,
  type FamilyApplicationRegistryEntry,
} from "./family-application-registry.js";
import type { WorkflowFundingRequirements } from "./funding-requirements.js";
import { bindWorkflowFundingReservationJournal } from "./funding-reservation-permit.js";
import { DirectoryFraudProofWorkflowJournalStore } from "./journal.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
  resumeRecordedFraudProofWorkflow,
} from "./orchestrator.js";
import { continuePendingWorkflow } from "./pending-continuation.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  createAdmittedWorkflowRunner,
  WORKFLOW_ADAPTER_RUNNER,
} from "./runner-admission.js";

export const WORKFLOW_RUNTIME_CONFIG =
  "midgard-production-fraud-proof-runtime-config-v1" as const;

export type LoadedWorkflowRuntime<Config> = {
  readonly schemaVersion: typeof WORKFLOW_RUNTIME_CONFIG;
  /** Infrastructure and credentials only. Proof evidence is forbidden here. */
  readonly config: Config;
  readonly retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  /** Closes every transport/provider allocated while loading the runtime. */
  readonly close: () => Promise<void>;
};

export type WorkflowRuntimeConfigLoader<Config> = (input: {
  readonly runtimeConfigPath: string;
  /** Permit-free deployment identity/configuration; loading cannot actuate. */
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedWorkflowRuntime<Config>>;

type ManifestBoundWorkflowIdentity<
  Category extends FraudProofCatalogueCategoryName,
> = FamilyApplicationWorkflowIdentity<Category> & {
  readonly adapter?: FraudProofFamilyWorkflowAdapter;
  readonly terminalVerifier?: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority?: FraudProofReleaseFinalityAuthority;
};

const admitPublicDaSources = (
  sources: readonly DaLibp2pRetainedDaSource[],
): readonly RetainedDaPayloadSource[] => {
  if (
    sources.length === 0 ||
    sources.some((source) => !(source instanceof DaLibp2pRetainedDaSource))
  ) {
    throw new Error(
      "production workflow runtime requires concrete public retained-DA libp2p sources",
    );
  }
  return sources;
};

/**
 * Shared compiled runtime boundary. The loader may provide infrastructure,
 * credentials, and public-DA transports, but never prepared proof evidence.
 * The family constructor must independently bind its manifest, raw local L1
 * authority, signer, economics, finality, and exact reference-script roster.
 */
const createManifestBoundWorkflowRunOrResume =
  <
    Category extends FraudProofCatalogueCategoryName,
    Config,
    Workflow extends ManifestBoundWorkflowIdentity<Category>,
  >({
    category,
    loadRuntimeConfig,
    constructWorkflow,
    execute,
    bindsDecisionDigest = false,
  }: {
    readonly category: Category;
    readonly loadRuntimeConfig: WorkflowRuntimeConfigLoader<Config>;
    readonly constructWorkflow: (config: Config) => Promise<Workflow>;
    /**
     * Whether the constructed workflow must carry the invocation's admitted
     * decision digest. The families that check it today all check it here.
     */
    readonly bindsDecisionDigest?: boolean;
    readonly execute: (input: {
      readonly workflow: Workflow;
      readonly sources: readonly RetainedDaPayloadSource[];
      readonly journal: DirectoryFraudProofWorkflowJournalStore;
      readonly mode: "run" | "resume";
    }) => Promise<unknown>;
  }): WorkflowAdapterRunner["runOrResume"] =>
  async (invocation: WorkflowAdapterRunnerInput) => {
    if (invocation.category !== category) {
      throw new Error(
        `production workflow runner category mismatch: expected=${category} actual=${invocation.category}`,
      );
    }
    const journal = bindWorkflowFundingReservationJournal({
      permit: invocation.fundingReservationPermit,
      journal: bindWorkflowActuationJournal({
        journal: new DirectoryFraudProofWorkflowJournalStore(
          invocation.journalDirectory,
        ),
        permit: invocation.actuationPermit,
        decisionDigest: invocation.decisionDigest,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category,
        headerHash: invocation.headerHash,
      }),
    });
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint: invocation.deploymentFingerprint,
      category,
      headerHash: invocation.headerHash,
      checkpoint: "runner_start",
    });
    const loaded = await loadRuntimeConfig({
      runtimeConfigPath: invocation.runtimeConfigPath,
      invocation,
    });
    if (typeof loaded.close !== "function") {
      throw new Error(
        "production workflow runtime config omitted its transport disposer",
      );
    }
    try {
      if (loaded.schemaVersion !== WORKFLOW_RUNTIME_CONFIG) {
        throw new Error(
          "production workflow runtime config has an unsupported schema",
        );
      }
      const sources = admitPublicDaSources(loaded.retainedDaSources);
      const workflow = await constructWorkflow(loaded.config);
      assertManifestBoundWorkflowIdentity({
        workflow,
        category,
        deploymentFingerprint: invocation.deploymentFingerprint,
        headerHash: invocation.headerHash,
        bindsDecisionDigest,
        decisionDigest: invocation.decisionDigest,
      });
      return await continuePendingWorkflow({
        invocation,
        journal,
        execute: (mode) => {
          if (workflowJournalIsReconciliationOnly(journal)) {
            if (
              workflow.adapter === undefined ||
              workflow.terminalVerifier === undefined ||
              workflow.releaseFinalityAuthority === undefined
            )
              throw new Error(
                "manifest-bound workflow omitted its existing adapter recovery surface",
              );
            return resumeRecordedFraudProofWorkflow({
              deploymentFingerprint: invocation.deploymentFingerprint,
              category,
              headerHash: invocation.headerHash,
              journal,
              adapter: workflow.adapter,
              terminalVerifier: workflow.terminalVerifier,
              releaseFinalityAuthority: workflow.releaseFinalityAuthority,
            });
          }
          return execute({ workflow, sources, journal, mode });
        },
      });
    } finally {
      await loaded.close();
    }
  };

/**
 * Builds the shared manifest-bound runtime behavior for tests and downstream
 * composition. The result is intentionally not admitted for production
 * registry readiness: only the registry-derived factory table below can mint
 * that module-private identity.
 */
export const createManifestBoundWorkflowRunner = <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends ManifestBoundWorkflowIdentity<Category>,
>(
  input: Parameters<
    typeof createManifestBoundWorkflowRunOrResume<Category, Config, Workflow>
  >[0],
): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: createManifestBoundWorkflowRunOrResume(input),
  });

const runnerFunding = (
  fundingRequirements: WorkflowFundingRequirements | undefined,
): Readonly<{
  fundingRequirements?: WorkflowFundingRequirements;
}> =>
  fundingRequirements === undefined
    ? Object.freeze({})
    : Object.freeze({ fundingRequirements });

/**
 * Mints the admitted production runner for a family application record. The
 * record supplies the category, the construction and the launch route, so the
 * generic run-or-resume body above is the only place a manifest-bound family
 * is bound to an invocation. The public `createManifestBoundWorkflowRunner`
 * stays non-admissible: only this module can mint the registry identity.
 */
export const createFamilyApplicationWorkflowRunner = <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends ManifestBoundWorkflowIdentity<Category>,
>(
  record: FamilyApplicationRecord<Category, Config, Workflow>,
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<Config>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: record.category,
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: record.category,
      loadRuntimeConfig,
      constructWorkflow: record.constructWorkflow,
      execute: record.execute,
      bindsDecisionDigest: record.bindsDecisionDigest,
    }),
  });

/**
 * The factory a compiled host applies to install one family: its runtime
 * loader and, when measured, its funding profile. The loader is taken at
 * `unknown` because the registry states every record at one erased shape, so
 * the table cannot relate a host's loaded config to the family's own config
 * type; that pairing is checked at runtime, where the record's
 * `constructWorkflow` receives the loaded config. A host that binds its
 * config through the record's own `bindConfig` regains the check by
 * construction.
 */
export type FamilyWorkflowRunnerFactory = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<unknown>,
  fundingRequirements?: WorkflowFundingRequirements,
) => WorkflowAdapterRunner;

/**
 * The runner-table row of a registry entry: the record supplies the
 * construction, the launch route and the digest binding, so the row is the
 * record applied to a compiled host's loader. The entry's `constructWorkflow`
 * is stated at `never` by the registry's erasure; the row widens it to the
 * loader's `unknown`, which is the same erasure seen from the other side.
 */
const familyApplicationWorkflowRunnerFactory = <
  Category extends FraudProofCatalogueCategoryName,
>(
  entry: FamilyApplicationRegistryEntry<Category>,
): FamilyWorkflowRunnerFactory => {
  const record = entry as FamilyApplicationRecord<
    Category,
    unknown,
    FamilyApplicationWorkflowIdentity<Category>
  >;
  return (loadRuntimeConfig, fundingRequirements) =>
    createFamilyApplicationWorkflowRunner(
      record,
      loadRuntimeConfig,
      fundingRequirements,
    );
};

type WorkflowRunnerFactories = {
  readonly [Category in FraudProofCatalogueCategoryName]: FamilyWorkflowRunnerFactory;
};

/**
 * One admitted factory per catalogue family, derived from the registry: the
 * table is the registry mapped through the module-private admission, so
 * registering a family's record is what adds its row here. This is
 * deliberately separate from launch readiness: a factory is not ready until a
 * compiled application supplies its concrete public-libp2p runtime loader and
 * installs the resulting executable runner.
 */
export const WORKFLOW_RUNNER_FACTORIES: WorkflowRunnerFactories = Object.freeze(
  Object.fromEntries(
    Object.values(FAMILY_APPLICATION_REGISTRY).map(
      (
        entry,
      ): readonly [
        FraudProofCatalogueCategoryName,
        FamilyWorkflowRunnerFactory,
      ] => [entry.category, familyApplicationWorkflowRunnerFactory(entry)],
    ),
  ),
) as WorkflowRunnerFactories;
