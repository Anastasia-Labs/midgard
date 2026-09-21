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
  applyFamilyApplicationRecord,
  type FamilyApplicationInvocation,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type FamilyCommonInfrastructure,
  type FamilyReferenceScriptResolver,
} from "./family-application.js";
import {
  FAMILY_APPLICATION_REGISTRY,
  type FamilyApplicationRegistryEntry,
} from "./family-application-registry.js";
import type { WorkflowFundingRequirements } from "./funding-requirements.js";
import { bindWorkflowFundingReservationJournal } from "./funding-reservation-permit.js";
import { DirectoryFraudProofWorkflowJournalStore } from "./journal.js";
import { resumeRecordedFraudProofWorkflow } from "./orchestrator.js";
import { continuePendingWorkflow } from "./pending-continuation.js";
import {
  createAdmittedWorkflowRunner,
  WORKFLOW_ADAPTER_RUNNER,
} from "./runner-admission.js";

export const WORKFLOW_RUNTIME_CONFIG =
  "midgard-production-fraud-proof-runtime-config-v1" as const;

/**
 * What a compiled host loads for one invocation: the common infrastructure
 * every family draws from, the resolver that turns a roster entry into its
 * published reference UTxO, and the public retained-DA transports. It is the
 * same shape for all 55 families, so a host writes one loader. Proof evidence
 * is forbidden here; the family's record lays its own config out of these.
 */
export type LoadedWorkflowRuntime = {
  readonly schemaVersion: typeof WORKFLOW_RUNTIME_CONFIG;
  readonly infrastructure: FamilyCommonInfrastructure;
  readonly resolveReferenceScript: FamilyReferenceScriptResolver;
  readonly retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  /** Closes every transport/provider allocated while loading the runtime. */
  readonly close: () => Promise<void>;
};

export type WorkflowRuntimeLoader = (input: {
  readonly runtimeConfigPath: string;
  /** Permit-free deployment identity/configuration; loading cannot actuate. */
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedWorkflowRuntime>;

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
 * The loaded infrastructure is what the family binds its config from, so it
 * must describe the invocation being run and no other: a host loader that
 * ignored the invocation it was handed would otherwise bind a workflow to a
 * header or decision the permit never admitted.
 */
const assertLoadedInfrastructureMatchesInvocation = (
  infrastructure: FamilyCommonInfrastructure,
  invocation: WorkflowAdapterRunnerInput,
): void => {
  const differing = (
    [
      ["headerHash", infrastructure.headerHash, invocation.headerHash],
      [
        "decisionDigest",
        infrastructure.decisionDigest,
        invocation.decisionDigest,
      ],
    ] as const
  ).find(([, loaded, admitted]) => loaded !== admitted);
  if (differing !== undefined) {
    const [field, loaded, admitted] = differing;
    throw new Error(
      `production workflow runtime infrastructure differs from the invocation: ${field} loaded=${String(loaded)} admitted=${admitted}`,
    );
  }
};

/**
 * Shared compiled runtime boundary. The loader provides infrastructure,
 * credentials and public-DA transports, but never prepared proof evidence.
 * The record then resolves its own roster, binds its own manifest-bound
 * config and constructs its workflow through the shared application loop, so
 * a family is applied to an invocation in exactly one place.
 */
const createManifestBoundWorkflowRunOrResume =
  <
    Category extends FraudProofCatalogueCategoryName,
    Config,
    Workflow extends FamilyApplicationWorkflowIdentity<Category>,
  >({
    record,
    loadRuntime,
  }: {
    readonly record: FamilyApplicationRecord<Category, Config, Workflow>;
    readonly loadRuntime: WorkflowRuntimeLoader;
  }): WorkflowAdapterRunner["runOrResume"] =>
  async (invocation: WorkflowAdapterRunnerInput) => {
    const { category } = record;
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
    const loaded = await loadRuntime({
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
      assertLoadedInfrastructureMatchesInvocation(
        loaded.infrastructure,
        invocation,
      );
      const reconciliationOnly = workflowJournalIsReconciliationOnly(journal);
      const application: FamilyApplicationInvocation = {
        deploymentFingerprint: invocation.deploymentFingerprint,
        category,
        headerHash: invocation.headerHash,
        ...(reconciliationOnly
          ? { reconciliationAuthority: invocation.actuationPermit }
          : {}),
      };
      const { workflow } = await applyFamilyApplicationRecord({
        record,
        infrastructure: loaded.infrastructure,
        resolveReferenceScript: loaded.resolveReferenceScript,
        invocation: application,
      });
      return await continuePendingWorkflow({
        invocation,
        journal,
        execute: (mode) => {
          if (reconciliationOnly) {
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
          return record.execute({ workflow, sources, journal, mode });
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
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
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
 * record supplies the category, the roster, the binding, the construction and
 * the launch route; the host supplies one loader. The public
 * `createManifestBoundWorkflowRunner` stays non-admissible: only this module
 * can mint the registry identity.
 */
export const createFamilyApplicationWorkflowRunner = <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>(
  record: FamilyApplicationRecord<Category, Config, Workflow>,
  loadRuntime: WorkflowRuntimeLoader,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: record.category,
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      record,
      loadRuntime,
    }),
  });

/**
 * The factory a compiled host applies to install one family: the host's one
 * shared runtime loader and, when measured, the family's funding profile.
 */
export type FamilyWorkflowRunnerFactory = (
  loadRuntime: WorkflowRuntimeLoader,
  fundingRequirements?: WorkflowFundingRequirements,
) => WorkflowAdapterRunner;

/**
 * The runner-table row of a registry entry. The registry states every record
 * at one erased config shape (`constructWorkflow` takes `never`), so the row
 * restates it as a record whose config is unknown: the record's own
 * `bindConfig` is the only producer of that config and its own
 * `constructWorkflow` the only consumer, so no caller ever names it.
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
  return (loadRuntime, fundingRequirements) =>
    createFamilyApplicationWorkflowRunner(
      record,
      loadRuntime,
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
