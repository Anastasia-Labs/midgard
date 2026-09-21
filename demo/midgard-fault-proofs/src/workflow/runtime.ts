import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  createManifestBoundCrossBlockDuplicateEventWorkflow,
  type ManifestBoundCrossBlockDuplicateEventWorkflow,
  type ManifestBoundCrossBlockDuplicateEventWorkflowConfig,
  runOrResumeManifestBoundCrossBlockDuplicateEventWorkflow,
} from "../cross-block-duplicate-event/workflow.js";
import {
  createExecutionNativeScriptInvalidWorkflowRunnerSurface,
  type LoadExecutionNativeScriptInvalidWorkflow,
} from "../execution-native-script-invalid/v1.js";
import {
  createManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflowConfig,
  runOrResumeManifestBoundMinAdaWorkflow,
} from "../min-ada/workflow.js";
import {
  createManifestBoundMintAuthorizationWorkflow,
  type ManifestBoundMintAuthorizationWorkflow,
  type ManifestBoundMintAuthorizationWorkflowConfig,
  runOrResumeManifestBoundMintAuthorizationWorkflow,
} from "../mint-authorization/workflow.js";
import {
  createManifestBoundMissingNativeScriptTxWorkflow,
  type ManifestBoundMissingNativeScriptTxWorkflow,
  type ManifestBoundMissingNativeScriptTxWorkflowConfig,
  runOrResumeManifestBoundMissingNativeScriptTxWorkflow,
} from "../missing-native-script-tx/workflow.js";
import {
  createManifestBoundMissingNativeScriptUtxoWorkflow,
  type ManifestBoundMissingNativeScriptUtxoWorkflow,
  type ManifestBoundMissingNativeScriptUtxoWorkflowConfig,
  runOrResumeManifestBoundMissingNativeScriptUtxoWorkflow,
} from "../missing-native-script-utxo/workflow.js";
import {
  createManifestBoundNativeScriptDecodingWorkflow,
  type ManifestBoundNativeScriptDecodingWorkflow,
  type ManifestBoundNativeScriptDecodingWorkflowConfig,
  runOrResumeManifestBoundNativeScriptDecodingWorkflow,
} from "../native-script-decoding/workflow.js";
import {
  createManifestBoundNativeScriptInvalidWorkflow,
  type ManifestBoundNativeScriptInvalidWorkflow,
  type ManifestBoundNativeScriptInvalidWorkflowConfig,
  runOrResumeManifestBoundNativeScriptInvalidWorkflow,
} from "../native-script-invalid/workflow.js";
import {
  createManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflowConfig,
  runOrResumeManifestBoundNetworkIdWorkflow,
} from "../network-id/workflow-adapter.js";
import {
  createResolvedOutputNonCanonicalWorkflowRunnerSurface,
  type LoadResolvedOutputNonCanonicalWorkflow,
} from "../resolved-output-non-canonical/authenticated-workflow.js";
import {
  createSpendInputSignerMissingWorkflowRunnerSurface,
  type LoadSpendInputSignerMissingWorkflow,
} from "../spend-input-signer-missing/authenticated-workflow.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  createManifestBoundTransitionTraceWorkflow,
  type ManifestBoundTransitionTraceWorkflow,
  type ManifestBoundTransitionTraceWorkflowConfig,
  runOrResumeManifestBoundTransitionTraceWorkflow,
} from "../transition-trace/workflow.js";
import {
  createValidationTraceDisputeWorkflowRunnerSurface,
  type LoadValidationTraceDisputeWorkflow,
} from "../validation-dispute/workflow-v1.js";
import {
  createManifestBoundValueConservationWorkflow,
  type ManifestBoundValueConservationWorkflow,
  type ManifestBoundValueConservationWorkflowConfig,
  runOrResumeManifestBoundValueConservationWorkflow,
} from "../value-not-preserved/workflow.js";
import {
  createManifestBoundWithdrawalMistagWorkflow,
  type ManifestBoundWithdrawalMistagWorkflow,
  type ManifestBoundWithdrawalMistagWorkflowConfig,
  runOrResumeManifestBoundWithdrawalMistagWorkflow,
} from "../withdrawal-mistag/workflow.js";
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
  type ManifestBoundDaHashPreimageWorkflow,
  runOrResumeManifestBoundDaHashPreimageWorkflow,
} from "./da-hash-preimage.js";
import {
  createManifestBoundDoubleSpendWorkflow,
  type ManifestBoundDoubleSpendWorkflow,
  type ManifestBoundDoubleSpendWorkflowConfig,
  runOrResumeManifestBoundDoubleSpendWorkflow,
} from "./double-spend-adapter.js";
import {
  assertManifestBoundWorkflowIdentity,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
} from "./family-application.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD,
  EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_APPLICATION_RECORD,
  FIELD_PREIMAGE_LENGTH_MISMATCH_FAMILY_APPLICATION_RECORD,
  MINT_DECLARED_ASSET_LIMIT_FAMILY_APPLICATION_RECORD,
  MINT_ITEM_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  MISSING_REDEEMER_FAMILY_APPLICATION_RECORD,
  MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD,
  OBSERVER_ORDER_INVALID_FAMILY_APPLICATION_RECORD,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAMILY_APPLICATION_RECORD,
  OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD,
  RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD,
  REDEEMER_CANONICITY_FAMILY_APPLICATION_RECORD,
  SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD,
  SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_APPLICATION_RECORD,
  TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD,
  UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD,
  WITNESS_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
} from "./family-application-registry.js";
import type {
  FamilyDefinition,
  FaultProofWitnessRole,
  ManifestBoundFamilyWorkflow,
  ManifestBoundFamilyWorkflowConfig,
} from "./family-definition.js";
import type { WorkflowFundingRequirements } from "./funding-requirements.js";
import { bindWorkflowFundingReservationJournal } from "./funding-reservation-permit.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import { DirectoryFraudProofWorkflowJournalStore } from "./journal.js";
import {
  type AnyLinearFamilyDefinition,
  LINEAR_FAMILY_DEFINITIONS,
} from "./linear-family-definitions.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  createManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflowConfig,
  runOrResumeManifestBoundMissingSignatureWorkflow,
} from "./missing-signature.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
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
 * registry readiness: only the fixed-category family factories below can mint
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
 * The runner-table row of a family application record: the record supplies
 * the construction, the launch route and the digest binding, so the row is
 * the record applied to a compiled host's loader.
 */
const familyApplicationWorkflowRunnerFactory =
  <
    Category extends FraudProofCatalogueCategoryName,
    Config,
    Workflow extends ManifestBoundWorkflowIdentity<Category>,
  >(
    record: FamilyApplicationRecord<Category, Config, Workflow>,
  ) =>
  (
    loadRuntimeConfig: WorkflowRuntimeConfigLoader<Config>,
    fundingRequirements?: WorkflowFundingRequirements,
  ): WorkflowAdapterRunner =>
    createFamilyApplicationWorkflowRunner(
      record,
      loadRuntimeConfig,
      fundingRequirements,
    );

type AssembledLinearFamilyWorkflow = ManifestBoundFamilyWorkflow<
  LinearFamilyCategory,
  boolean
>;

type LinearFamilyLaunch = (input: {
  readonly workflow: AssembledLinearFamilyWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) => Promise<FraudProofWorkflowRunResult>;

/**
 * Q44 is the one linear family that does not launch through the generic
 * retained-DA runner: it fetches its raw source leaf through the dedicated
 * orchestrator entry, so no canonical replayer is consulted.
 */
const launchDaHashPreimage: LinearFamilyLaunch = async ({
  workflow,
  sources,
  journal,
}) =>
  await runOrResumeManifestBoundDaHashPreimageWorkflow({
    workflow: workflow as ManifestBoundDaHashPreimageWorkflow,
    sources,
    journal,
  });

export type LinearFamilyWorkflowRunnerFactory<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<
    ManifestBoundFamilyWorkflowConfig<Category, Witness, Certificate>
  >,
  fundingRequirements?: WorkflowFundingRequirements,
) => WorkflowAdapterRunner;

/**
 * One admitted factory row per linear family definition. The row is minted
 * through the module-private admission exactly like the fixed-category rows
 * below; the definition supplies the assembly and selects the launch route.
 * The definition is widened once here because a union member's polarity
 * cannot be correlated with its own callbacks at the type level; the exact
 * row types are restored by `LinearFamilyWorkflowRunnerFactories`.
 */
const linearFamilyWorkflowRunnerFactory =
  (definition: AnyLinearFamilyDefinition) =>
  (
    loadRuntimeConfig: WorkflowRuntimeConfigLoader<
      ManifestBoundFamilyWorkflowConfig<
        LinearFamilyCategory,
        FaultProofWitnessRole,
        boolean
      >
    >,
    fundingRequirements?: WorkflowFundingRequirements,
  ): WorkflowAdapterRunner => {
    const { category } = definition;
    const launch: LinearFamilyLaunch =
      category === "daHashPreimage"
        ? launchDaHashPreimage
        : runOrResumeManifestBoundFamilyWorkflow;
    return createAdmittedWorkflowRunner({
      category,
      ...runnerFunding(fundingRequirements),
      runOrResume: createManifestBoundWorkflowRunOrResume({
        category,
        loadRuntimeConfig,
        constructWorkflow: (config): Promise<AssembledLinearFamilyWorkflow> =>
          assembleManifestBoundFamilyWorkflow(
            definition as FamilyDefinition<
              LinearFamilyCategory,
              FaultProofWitnessRole,
              boolean
            >,
            config,
          ),
        execute: async ({ workflow, sources, journal }) =>
          await launch({ workflow, sources, journal }),
      }),
    });
  };

type LinearFamilyWorkflowRunnerFactories = {
  readonly [Category in LinearFamilyCategory]: (typeof LINEAR_FAMILY_DEFINITIONS)[Category] extends FamilyDefinition<
    Category,
    infer Witness,
    infer Certificate
  >
    ? LinearFamilyWorkflowRunnerFactory<Category, Witness, Certificate>
    : never;
};

const LINEAR_FAMILY_WORKFLOW_RUNNER_FACTORIES = Object.freeze(
  Object.fromEntries(
    Object.values(LINEAR_FAMILY_DEFINITIONS).map(
      (definition): readonly [LinearFamilyCategory, unknown] => [
        definition.category,
        linearFamilyWorkflowRunnerFactory(definition),
      ],
    ),
  ),
) as LinearFamilyWorkflowRunnerFactories;

export const createDoubleSpendWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundDoubleSpendWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "doubleSpend",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "doubleSpend",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundDoubleSpendWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundDoubleSpendWorkflow({
          workflow: workflow as ManifestBoundDoubleSpendWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createResolvedOutputNonCanonicalWorkflowRunner = (
  loadRuntimeConfig: LoadResolvedOutputNonCanonicalWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createResolvedOutputNonCanonicalWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "resolvedOutputNonCanonical",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createSpendInputSignerMissingWorkflowRunner = (
  loadRuntimeConfig: LoadSpendInputSignerMissingWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createSpendInputSignerMissingWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "spendInputSignerMissing",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createExecutionNativeScriptInvalidWorkflowRunner = (
  loadRuntimeConfig: LoadExecutionNativeScriptInvalidWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createExecutionNativeScriptInvalidWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "executionNativeScriptInvalid",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

/**
 * Production runner for the sole interactive family. The surface re-derives
 * the dispute cursor from live chain state on every invocation (ruling R6),
 * so the watcher always owns a move, a deadline, or completion.
 */
export const createValidationTraceDisputeWorkflowRunner = (
  loadRuntimeConfig: LoadValidationTraceDisputeWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createValidationTraceDisputeWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "validationTraceDispute",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createNetworkIdWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundNetworkIdWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "networkId",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "networkId",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNetworkIdWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNetworkIdWorkflow({
          workflow: workflow as ManifestBoundNetworkIdWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createMinAdaWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundMinAdaWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "minAda",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "minAda",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMinAdaWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMinAdaWorkflow({
          workflow: workflow as ManifestBoundMinAdaWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createTransitionTraceWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundTransitionTraceWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "transitionTrace",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "transitionTrace",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundTransitionTraceWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundTransitionTraceWorkflow({
          workflow: workflow as ManifestBoundTransitionTraceWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createValueConservationWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundValueConservationWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "valueNotPreserved",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "valueNotPreserved",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundValueConservationWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundValueConservationWorkflow({
          workflow: workflow as ManifestBoundValueConservationWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createMintAuthorizationWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundMintAuthorizationWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "mintAuthorization",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "mintAuthorization",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMintAuthorizationWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMintAuthorizationWorkflow({
          workflow: workflow as ManifestBoundMintAuthorizationWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createNativeScriptInvalidWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundNativeScriptInvalidWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "nativeScriptInvalid",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "nativeScriptInvalid",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNativeScriptInvalidWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNativeScriptInvalidWorkflow({
          workflow: workflow as ManifestBoundNativeScriptInvalidWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createNativeScriptDecodingWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundNativeScriptDecodingWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "nativeScriptDecoding",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "nativeScriptDecoding",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNativeScriptDecodingWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNativeScriptDecodingWorkflow({
          workflow: workflow as ManifestBoundNativeScriptDecodingWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createCrossBlockDuplicateEventWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundCrossBlockDuplicateEventWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "crossBlockDuplicateEvent",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "crossBlockDuplicateEvent",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundCrossBlockDuplicateEventWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundCrossBlockDuplicateEventWorkflow({
          workflow: workflow as ManifestBoundCrossBlockDuplicateEventWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createWithdrawalMistagWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundWithdrawalMistagWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "withdrawalMistag",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "withdrawalMistag",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundWithdrawalMistagWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundWithdrawalMistagWorkflow({
          workflow: workflow as ManifestBoundWithdrawalMistagWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createMissingNativeScriptUtxoWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundMissingNativeScriptUtxoWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "missingNativeScriptUtxo",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "missingNativeScriptUtxo",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMissingNativeScriptUtxoWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMissingNativeScriptUtxoWorkflow({
          workflow: workflow as ManifestBoundMissingNativeScriptUtxoWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createMissingSignatureWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundMissingSignatureWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "missingSignature",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "missingSignature",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMissingSignatureWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMissingSignatureWorkflow({
          workflow: workflow as ManifestBoundMissingSignatureWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createMissingNativeScriptTxWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundMissingNativeScriptTxWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "missingNativeScriptTx",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "missingNativeScriptTx",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMissingNativeScriptTxWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMissingNativeScriptTxWorkflow({
          workflow: workflow as ManifestBoundMissingNativeScriptTxWorkflow,
          sources,
          journal,
        }),
    }),
  });

/**
 * Factories for the current families whose complete shared workflow drivers
 * exist: every linear family's row is derived from its definition, every
 * registered decision-digest family's row from its application record, the
 * rest are the explicit rows below. This is deliberately separate from launch
 * readiness: a factory is not ready until a compiled application supplies its
 * concrete public-libp2p runtime loader and installs the resulting executable
 * runner.
 */
export const WORKFLOW_RUNNER_FACTORIES = Object.freeze({
  ...LINEAR_FAMILY_WORKFLOW_RUNNER_FACTORIES,
  doubleSpend: createDoubleSpendWorkflowRunner,
  missingSignature: createMissingSignatureWorkflowRunner,
  missingNativeScriptTx: createMissingNativeScriptTxWorkflowRunner,
  networkId: createNetworkIdWorkflowRunner,
  missingNativeScriptUtxo: createMissingNativeScriptUtxoWorkflowRunner,
  mintAuthorization: createMintAuthorizationWorkflowRunner,
  nativeScriptInvalid: createNativeScriptInvalidWorkflowRunner,
  nativeScriptDecoding: createNativeScriptDecodingWorkflowRunner,
  crossBlockDuplicateEvent: createCrossBlockDuplicateEventWorkflowRunner,
  withdrawalMistag: createWithdrawalMistagWorkflowRunner,
  minAda: createMinAdaWorkflowRunner,
  transitionTrace: createTransitionTraceWorkflowRunner,
  valueNotPreserved: createValueConservationWorkflowRunner,
  fieldPreimageLengthMismatch: familyApplicationWorkflowRunnerFactory(
    FIELD_PREIMAGE_LENGTH_MISMATCH_FAMILY_APPLICATION_RECORD,
  ),
  fieldItemWidthIllegal: familyApplicationWorkflowRunnerFactory(
    FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_APPLICATION_RECORD,
  ),
  witnessScriptDecoding: familyApplicationWorkflowRunnerFactory(
    WITNESS_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  ),
  scriptIntegrityHashMissing: familyApplicationWorkflowRunnerFactory(
    SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_APPLICATION_RECORD,
  ),
  transactionOutputNonCanonical: familyApplicationWorkflowRunnerFactory(
    TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  ),
  mintItemNonCanonical: familyApplicationWorkflowRunnerFactory(
    MINT_ITEM_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  ),
  resolvedOutputNonCanonical: createResolvedOutputNonCanonicalWorkflowRunner,
  mintDeclaredAssetLimit: familyApplicationWorkflowRunnerFactory(
    MINT_DECLARED_ASSET_LIMIT_FAMILY_APPLICATION_RECORD,
  ),
  spendInputSignerMissing: createSpendInputSignerMissingWorkflowRunner,
  protectedOutputSignerMissing: familyApplicationWorkflowRunnerFactory(
    PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD,
  ),
  observersForbiddenOnUntaggedNetwork: familyApplicationWorkflowRunnerFactory(
    OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAMILY_APPLICATION_RECORD,
  ),
  observerOrderInvalid: familyApplicationWorkflowRunnerFactory(
    OBSERVER_ORDER_INVALID_FAMILY_APPLICATION_RECORD,
  ),
  redeemerCanonicity: familyApplicationWorkflowRunnerFactory(
    REDEEMER_CANONICITY_FAMILY_APPLICATION_RECORD,
  ),
  outputReferenceScriptDecoding: familyApplicationWorkflowRunnerFactory(
    OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  ),
  executionSourceScriptDecoding: familyApplicationWorkflowRunnerFactory(
    EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  ),
  receivePurposeLanguage: familyApplicationWorkflowRunnerFactory(
    RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD,
  ),
  unusedScriptWitness: familyApplicationWorkflowRunnerFactory(
    UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD,
  ),
  missingScriptSource: familyApplicationWorkflowRunnerFactory(
    MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD,
  ),
  missingRedeemer: familyApplicationWorkflowRunnerFactory(
    MISSING_REDEEMER_FAMILY_APPLICATION_RECORD,
  ),
  unusedRedeemer: familyApplicationWorkflowRunnerFactory(
    UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD,
  ),
  executionNativeScriptInvalid:
    createExecutionNativeScriptInvalidWorkflowRunner,
  scriptIntegrityHashMismatch: familyApplicationWorkflowRunnerFactory(
    SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD,
  ),
  distinctAssetAccumulationLimit: familyApplicationWorkflowRunnerFactory(
    DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD,
  ),
  validationTraceDispute: createValidationTraceDisputeWorkflowRunner,
});
