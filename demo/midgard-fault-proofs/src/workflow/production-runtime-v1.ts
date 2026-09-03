import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  createDistinctAssetAccumulationWorkflowRunnerSurface,
  type LoadDistinctAssetAccumulationWorkflow,
} from "../distinct-asset-accumulation-limit/production-v1.js";
import {
  createExecutionNativeScriptInvalidWorkflowRunnerSurface,
  type LoadExecutionNativeScriptInvalidWorkflow,
} from "../execution-native-script-invalid/production-v1.js";
import {
  createExecutionSourceScriptDecodingWorkflowRunnerSurface,
  type LoadExecutionSourceScriptDecodingWorkflow,
} from "../execution-source-script-decoding/production-v1.js";
import {
  createFieldItemWidthIllegalWorkflowRunnerSurface,
  type LoadFieldItemWidthIllegalWorkflow,
} from "../field-item-width-illegal/production-workflow-v1.js";
import {
  createManifestBoundFieldPreimageLengthWorkflow,
  executeManifestBoundFieldPreimageLengthWorkflow,
  type ManifestBoundFieldPreimageLengthWorkflow,
  type ManifestBoundFieldPreimageLengthWorkflowConfig,
} from "../field-preimage-length-mismatch/production-workflow-v1.js";
import {
  createManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflowConfig,
  runOrResumeManifestBoundMinAdaWorkflow,
} from "../min-ada/production-workflow-v1.js";
import {
  createMintDeclaredAssetLimitWorkflowRunnerSurface,
  type LoadMintDeclaredAssetLimitWorkflow,
} from "../mint-declared-asset-limit/production-v1.js";
import {
  createManifestBoundMissingNativeScriptTxWorkflow,
  type ManifestBoundMissingNativeScriptTxWorkflow,
  type ManifestBoundMissingNativeScriptTxWorkflowConfig,
  runOrResumeManifestBoundMissingNativeScriptTxWorkflow,
} from "../missing-native-script-tx/production-workflow-v1.js";
import {
  createManifestBoundMissingNativeScriptUtxoWorkflow,
  type ManifestBoundMissingNativeScriptUtxoWorkflow,
  type ManifestBoundMissingNativeScriptUtxoWorkflowConfig,
  runOrResumeManifestBoundMissingNativeScriptUtxoWorkflow,
} from "../missing-native-script-utxo/production-workflow-v1.js";
import {
  createMissingRedeemerWorkflowRunnerSurface,
  type LoadMissingRedeemerWorkflow,
} from "../missing-redeemer/production-v1.js";
import {
  createMissingScriptSourceWorkflowRunnerSurface,
  type LoadMissingScriptSourceWorkflow,
} from "../missing-script-source/production-v1.js";
import {
  createManifestBoundNativeScriptInvalidWorkflow,
  type ManifestBoundNativeScriptInvalidWorkflow,
  type ManifestBoundNativeScriptInvalidWorkflowConfig,
  runOrResumeManifestBoundNativeScriptInvalidWorkflow,
} from "../native-script-invalid/production-workflow-v1.js";
import {
  createManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflowConfig,
  runOrResumeManifestBoundNetworkIdWorkflow,
} from "../network-id/workflow-adapter-v1.js";
import {
  createObserverOrderInvalidWorkflowRunnerSurface,
  type LoadObserverOrderInvalidWorkflow,
} from "../observer-order-invalid/production-v1.js";
import {
  createObserversForbiddenWorkflowRunnerSurface,
  type LoadObserversForbiddenWorkflow,
} from "../observers-forbidden-on-untagged-network/production-v1.js";
import {
  createOutputReferenceScriptDecodingWorkflowRunnerSurface,
  type LoadOutputReferenceScriptDecodingWorkflow,
} from "../output-reference-script-decoding/production-workflow-v1.js";
import {
  createProtectedOutputSignerMissingWorkflowRunnerSurface,
  type LoadProtectedOutputSignerMissingWorkflow,
} from "../protected-output-signer-missing/production-workflow-v1.js";
import {
  createReceivePurposeLanguageWorkflowRunnerSurface,
  type LoadReceivePurposeLanguageWorkflow,
} from "../receive-purpose-language/manifest-workflow-v1.js";
import {
  createRedeemerCanonicityWorkflowRunnerSurface,
  type LoadRedeemerCanonicityWorkflow,
} from "../redeemer-canonicity/production-runtime-v1.js";
import {
  createResolvedOutputNonCanonicalWorkflowRunnerSurface,
  type LoadResolvedOutputNonCanonicalWorkflow,
} from "../resolved-output-non-canonical/production-workflow-v1.js";
import {
  createScriptIntegrityHashMismatchWorkflowRunnerSurface,
  type LoadScriptIntegrityHashMismatchWorkflow,
} from "../script-integrity-hash-mismatch/production-v1.js";
import {
  createScriptIntegrityHashMissingWorkflowRunnerSurface,
  type LoadScriptIntegrityHashMissingWorkflow,
} from "../script-integrity-hash-missing/production-v1.js";
import {
  createSpendInputSignerMissingWorkflowRunnerSurface,
  type LoadSpendInputSignerMissingWorkflow,
} from "../spend-input-signer-missing/production-workflow-v1.js";
import {
  createTransactionOutputNonCanonicalWorkflowRunnerSurface,
  type LoadTransactionOutputNonCanonicalWorkflow,
} from "../transaction-output-non-canonical/production-workflow-v1.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  createUnusedRedeemerWorkflowRunnerSurface,
  type LoadUnusedRedeemerWorkflow,
} from "../unused-redeemer/production-v1.js";
import {
  createUnusedScriptWitnessWorkflowRunnerSurface,
  type LoadUnusedScriptWitnessWorkflow,
} from "../unused-script-witness/production-v1.js";
import {
  createWitnessScriptDecodingWorkflowRunnerSurface,
  type LoadWitnessScriptDecodingWorkflow,
} from "../witness-script-decoding/production-workflow-v1.js";
import {
  createManifestBoundDoubleSpendWorkflow,
  type ManifestBoundDoubleSpendWorkflow,
  type ManifestBoundDoubleSpendWorkflowConfig,
  runOrResumeManifestBoundDoubleSpendWorkflow,
} from "./double-spend-adapter-v1.js";
import { DirectoryFraudProofWorkflowJournalStore } from "./journal-v1.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "./production-actuation-permit-v1.js";
import {
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
  type WorkflowAdapterRunnerInput,
} from "./production-adapters-v1.js";
import {
  createManifestBoundCanonicalDecodabilityWorkflow,
  type ManifestBoundCanonicalDecodabilityWorkflow,
  type ManifestBoundCanonicalDecodabilityWorkflowConfig,
  runOrResumeManifestBoundCanonicalDecodabilityWorkflow,
} from "./production-canonical-decodability-v1.js";
import {
  createManifestBoundCommittedFieldShapeWorkflow,
  type ManifestBoundCommittedFieldShapeWorkflow,
  type ManifestBoundCommittedFieldShapeWorkflowConfig,
  runOrResumeManifestBoundCommittedFieldShapeWorkflow,
} from "./production-committed-field-shape-v1.js";
import {
  createManifestBoundDaHashPreimageWorkflow,
  type ManifestBoundDaHashPreimageWorkflow,
  type ManifestBoundDaHashPreimageWorkflowConfig,
  runOrResumeManifestBoundDaHashPreimageWorkflow,
} from "./production-da-hash-preimage-v1.js";
import {
  createManifestBoundDoubleWithdrawWorkflow,
  type ManifestBoundDoubleWithdrawWorkflow,
  type ManifestBoundDoubleWithdrawWorkflowConfig,
  runOrResumeManifestBoundDoubleWithdrawWorkflow,
} from "./production-double-withdraw-v1.js";
import {
  createManifestBoundFabricatedDepositWorkflow,
  type ManifestBoundFabricatedDepositWorkflow,
  type ManifestBoundFabricatedDepositWorkflowConfig,
  runOrResumeManifestBoundFabricatedDepositWorkflow,
} from "./production-fabricated-deposit-v1.js";
import {
  createManifestBoundFabricatedWithdrawalWorkflow,
  type ManifestBoundFabricatedWithdrawalWorkflow,
  type ManifestBoundFabricatedWithdrawalWorkflowConfig,
  runOrResumeManifestBoundFabricatedWithdrawalWorkflow,
} from "./production-fabricated-withdrawal-v1.js";
import type { WorkflowFundingRequirements } from "./production-funding-requirements-v1.js";
import { bindWorkflowFundingReservationJournal } from "./production-funding-reservation-permit-v1.js";
import {
  createManifestBoundInputNoIdxWorkflow,
  type ManifestBoundInputNoIdxWorkflow,
  type ManifestBoundInputNoIdxWorkflowConfig,
  runOrResumeManifestBoundInputNoIdxWorkflow,
} from "./production-input-no-idx-v1.js";
import {
  createManifestBoundInputSetUniquenessWorkflow,
  type ManifestBoundInputSetUniquenessWorkflow,
  type ManifestBoundInputSetUniquenessWorkflowConfig,
  runOrResumeManifestBoundInputSetUniquenessWorkflow,
} from "./production-input-set-uniqueness-v1.js";
import {
  createManifestBoundInvalidSignatureWorkflow,
  type ManifestBoundInvalidSignatureWorkflow,
  type ManifestBoundInvalidSignatureWorkflowConfig,
  runOrResumeManifestBoundInvalidSignatureWorkflow,
} from "./production-invalid-signature-v1.js";
import {
  createManifestBoundL2TxMistagWorkflow,
  type ManifestBoundL2TxMistagWorkflow,
  type ManifestBoundL2TxMistagWorkflowConfig,
  runOrResumeManifestBoundL2TxMistagWorkflow,
} from "./production-l2-tx-mistag-v1.js";
import {
  createManifestBoundMinFeeWorkflow,
  type ManifestBoundMinFeeWorkflow,
  type ManifestBoundMinFeeWorkflowConfig,
  runOrResumeManifestBoundMinFeeWorkflow,
} from "./production-min-fee-v1.js";
import {
  createManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflowConfig,
  runOrResumeManifestBoundMissingSignatureWorkflow,
} from "./production-missing-signature-v1.js";
import {
  createManifestBoundInvalidRangeWorkflow,
  createManifestBoundZeroInputWorkflow,
  type ManifestBoundInvalidRangeWorkflow,
  type ManifestBoundInvalidRangeWorkflowConfig,
  type ManifestBoundZeroInputWorkflow,
  type ManifestBoundZeroInputWorkflowConfig,
  runOrResumeManifestBoundInvalidRangeWorkflow,
  runOrResumeManifestBoundZeroInputWorkflow,
} from "./production-native-inclusion-two-step-v1.js";
import {
  createManifestBoundNoReferenceInputWorkflow,
  type ManifestBoundNoReferenceInputWorkflow,
  type ManifestBoundNoReferenceInputWorkflowConfig,
  runOrResumeManifestBoundNoReferenceInputWorkflow,
} from "./production-no-reference-input-v1.js";
import {
  createManifestBoundNonExistentInputWorkflow,
  type ManifestBoundNonExistentInputWorkflow,
  type ManifestBoundNonExistentInputWorkflowConfig,
  runOrResumeManifestBoundNonExistentInputWorkflow,
} from "./production-non-existent-input-v1.js";
import {
  createManifestBoundReferenceInputNoIdxWorkflow,
  type ManifestBoundReferenceInputNoIdxWorkflow,
  type ManifestBoundReferenceInputNoIdxWorkflowConfig,
  runOrResumeManifestBoundReferenceInputNoIdxWorkflow,
} from "./production-reference-input-no-idx-v1.js";
import {
  createAdmittedWorkflowRunner,
  WORKFLOW_ADAPTER_RUNNER,
} from "./production-runner-admission-v1.js";
import {
  createManifestBoundWithdrawnInputWorkflow,
  type ManifestBoundWithdrawnInputWorkflow,
  type ManifestBoundWithdrawnInputWorkflowConfig,
  runOrResumeManifestBoundWithdrawnInputWorkflow,
} from "./production-withdrawn-input-v1.js";
import {
  createManifestBoundWithdrawnReferenceInputWorkflow,
  type ManifestBoundWithdrawnReferenceInputWorkflow,
  type ManifestBoundWithdrawnReferenceInputWorkflowConfig,
  runOrResumeManifestBoundWithdrawnReferenceInputWorkflow,
} from "./production-withdrawn-reference-input-v1.js";

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
> = {
  readonly binding: {
    readonly deploymentFingerprint: string;
    readonly definition: {
      readonly category: Category;
      readonly headerHash: string;
    };
  };
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
  }: {
    readonly category: Category;
    readonly loadRuntimeConfig: WorkflowRuntimeConfigLoader<Config>;
    readonly constructWorkflow: (config: Config) => Promise<Workflow>;
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
      if (
        workflow.binding.deploymentFingerprint !==
          invocation.deploymentFingerprint ||
        workflow.binding.definition.category !== category ||
        workflow.binding.definition.headerHash !== invocation.headerHash
      ) {
        throw new Error(
          "manifest-bound workflow identity differs from the compiled CLI invocation",
        );
      }
      return await execute({
        workflow,
        sources,
        journal,
        mode: invocation.mode,
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

export const createFieldItemWidthIllegalWorkflowRunner = (
  loadRuntimeConfig: LoadFieldItemWidthIllegalWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createFieldItemWidthIllegalWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "fieldItemWidthIllegal",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createFieldPreimageLengthWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundFieldPreimageLengthWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "fieldPreimageLengthMismatch",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "fieldPreimageLengthMismatch",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundFieldPreimageLengthWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await executeManifestBoundFieldPreimageLengthWorkflow({
          workflow: workflow as ManifestBoundFieldPreimageLengthWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createScriptIntegrityHashMissingWorkflowRunner = (
  loadRuntimeConfig: LoadScriptIntegrityHashMissingWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createScriptIntegrityHashMissingWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "scriptIntegrityHashMissing",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createTransactionOutputNonCanonicalWorkflowRunner = (
  loadRuntimeConfig: LoadTransactionOutputNonCanonicalWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createTransactionOutputNonCanonicalWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "transactionOutputNonCanonical",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

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

export const createMintDeclaredAssetLimitWorkflowRunner = (
  loadRuntimeConfig: LoadMintDeclaredAssetLimitWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createMintDeclaredAssetLimitWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "mintDeclaredAssetLimit",
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

export const createProtectedOutputSignerMissingWorkflowRunner = (
  loadRuntimeConfig: LoadProtectedOutputSignerMissingWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createProtectedOutputSignerMissingWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "protectedOutputSignerMissing",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createObserversForbiddenOnUntaggedNetworkWorkflowRunner = (
  loadRuntimeConfig: LoadObserversForbiddenWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createObserversForbiddenWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "observersForbiddenOnUntaggedNetwork",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createOutputReferenceScriptDecodingWorkflowRunner = (
  loadRuntimeConfig: LoadOutputReferenceScriptDecodingWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createOutputReferenceScriptDecodingWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "outputReferenceScriptDecoding",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createWitnessScriptDecodingWorkflowRunner = (
  loadRuntimeConfig: LoadWitnessScriptDecodingWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createWitnessScriptDecodingWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "witnessScriptDecoding",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createExecutionSourceScriptDecodingWorkflowRunner = (
  loadRuntimeConfig: LoadExecutionSourceScriptDecodingWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createExecutionSourceScriptDecodingWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "executionSourceScriptDecoding",
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

export const createObserverOrderInvalidWorkflowRunner = (
  loadRuntimeConfig: LoadObserverOrderInvalidWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createObserverOrderInvalidWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "observerOrderInvalid",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createRedeemerCanonicityWorkflowRunner = (
  loadRuntimeConfig: LoadRedeemerCanonicityWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createRedeemerCanonicityWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "redeemerCanonicity",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createReceivePurposeLanguageWorkflowRunner = (
  loadRuntimeConfig: LoadReceivePurposeLanguageWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createReceivePurposeLanguageWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "receivePurposeLanguage",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createUnusedScriptWitnessWorkflowRunner = (
  loadRuntimeConfig: LoadUnusedScriptWitnessWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createUnusedScriptWitnessWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "unusedScriptWitness",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createMissingScriptSourceWorkflowRunner = (
  loadRuntimeConfig: LoadMissingScriptSourceWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createMissingScriptSourceWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "missingScriptSource",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createMissingRedeemerWorkflowRunner = (
  loadRuntimeConfig: LoadMissingRedeemerWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createMissingRedeemerWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "missingRedeemer",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createUnusedRedeemerWorkflowRunner = (
  loadRuntimeConfig: LoadUnusedRedeemerWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createUnusedRedeemerWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "unusedRedeemer",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createScriptIntegrityHashMismatchWorkflowRunner = (
  loadRuntimeConfig: LoadScriptIntegrityHashMismatchWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createScriptIntegrityHashMismatchWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "scriptIntegrityHashMismatch",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createDistinctAssetAccumulationWorkflowRunner = (
  loadRuntimeConfig: LoadDistinctAssetAccumulationWorkflow,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner => {
  const surface = createDistinctAssetAccumulationWorkflowRunnerSurface({
    loadRuntimeConfig,
  });
  return createAdmittedWorkflowRunner({
    category: "distinctAssetAccumulationLimit",
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

export const createDaHashPreimageWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundDaHashPreimageWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "daHashPreimage",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "daHashPreimage",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundDaHashPreimageWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundDaHashPreimageWorkflow({
          workflow: workflow as ManifestBoundDaHashPreimageWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createCommittedFieldShapeWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundCommittedFieldShapeWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "committedFieldShape",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "committedFieldShape",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundCommittedFieldShapeWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundCommittedFieldShapeWorkflow({
          workflow: workflow as ManifestBoundCommittedFieldShapeWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createCanonicalDecodabilityWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundCanonicalDecodabilityWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "canonicalDecodability",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "canonicalDecodability",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundCanonicalDecodabilityWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundCanonicalDecodabilityWorkflow({
          workflow: workflow as ManifestBoundCanonicalDecodabilityWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createDoubleWithdrawWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundDoubleWithdrawWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "doubleWithdraw",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "doubleWithdraw",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundDoubleWithdrawWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundDoubleWithdrawWorkflow({
          workflow: workflow as ManifestBoundDoubleWithdrawWorkflow,
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

export const createInvalidRangeWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundInvalidRangeWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "invalidRange",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "invalidRange",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInvalidRangeWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInvalidRangeWorkflow({
          workflow: workflow as ManifestBoundInvalidRangeWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createInvalidSignatureWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundInvalidSignatureWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "invalidSignature",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "invalidSignature",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInvalidSignatureWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInvalidSignatureWorkflow({
          workflow: workflow as ManifestBoundInvalidSignatureWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createInputNoIdxWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundInputNoIdxWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "nonExistentInputNoIndex",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "nonExistentInputNoIndex",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInputNoIdxWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInputNoIdxWorkflow({
          workflow: workflow as ManifestBoundInputNoIdxWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createNonExistentInputWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundNonExistentInputWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "nonExistentInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "nonExistentInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNonExistentInputWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNonExistentInputWorkflow({
          workflow: workflow as ManifestBoundNonExistentInputWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createNoReferenceInputWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundNoReferenceInputWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "noReferenceInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "noReferenceInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNoReferenceInputWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNoReferenceInputWorkflow({
          workflow: workflow as ManifestBoundNoReferenceInputWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createReferenceInputNoIdxWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundReferenceInputNoIdxWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "referenceInputNoIdx",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "referenceInputNoIdx",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundReferenceInputNoIdxWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundReferenceInputNoIdxWorkflow({
          workflow: workflow as ManifestBoundReferenceInputNoIdxWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createInputSetUniquenessWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundInputSetUniquenessWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "inputSetUniqueness",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "inputSetUniqueness",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInputSetUniquenessWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInputSetUniquenessWorkflow({
          workflow: workflow as ManifestBoundInputSetUniquenessWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createZeroInputWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundZeroInputWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "zeroInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "zeroInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundZeroInputWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundZeroInputWorkflow({
          workflow: workflow as ManifestBoundZeroInputWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createL2TxMistagWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundL2TxMistagWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "l2TxMistag",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "l2TxMistag",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundL2TxMistagWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundL2TxMistagWorkflow({
          workflow: workflow as ManifestBoundL2TxMistagWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createMinFeeWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundMinFeeWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "minFee",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "minFee",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMinFeeWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMinFeeWorkflow({
          workflow: workflow as ManifestBoundMinFeeWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createWithdrawnInputWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundWithdrawnInputWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "withdrawnInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "withdrawnInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundWithdrawnInputWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundWithdrawnInputWorkflow({
          workflow: workflow as ManifestBoundWithdrawnInputWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createFabricatedDepositWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundFabricatedDepositWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "fabricatedDeposit",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "fabricatedDeposit",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundFabricatedDepositWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundFabricatedDepositWorkflow({
          workflow: workflow as ManifestBoundFabricatedDepositWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createFabricatedWithdrawalWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundFabricatedWithdrawalWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "fabricatedWithdrawal",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "fabricatedWithdrawal",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundFabricatedWithdrawalWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundFabricatedWithdrawalWorkflow({
          workflow: workflow as ManifestBoundFabricatedWithdrawalWorkflow,
          sources,
          journal,
        }),
    }),
  });

export const createWithdrawnReferenceInputWorkflowRunner = (
  loadRuntimeConfig: WorkflowRuntimeConfigLoader<ManifestBoundWithdrawnReferenceInputWorkflowConfig>,
  fundingRequirements?: WorkflowFundingRequirements,
): WorkflowAdapterRunner =>
  createAdmittedWorkflowRunner({
    category: "withdrawnReferenceInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundWorkflowRunOrResume({
      category: "withdrawnReferenceInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundWithdrawnReferenceInputWorkflow,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundWithdrawnReferenceInputWorkflow({
          workflow: workflow as ManifestBoundWithdrawnReferenceInputWorkflow,
          sources,
          journal,
        }),
    }),
  });

/**
 * Explicit factories for the current families whose complete shared workflow
 * drivers exist. This is deliberately separate from launch readiness: a
 * factory is not ready until a compiled application supplies its concrete
 * public-libp2p runtime loader and installs the resulting executable runner.
 */
export const WORKFLOW_RUNNER_FACTORIES = Object.freeze({
  doubleSpend: createDoubleSpendWorkflowRunner,
  nonExistentInput: createNonExistentInputWorkflowRunner,
  nonExistentInputNoIndex: createInputNoIdxWorkflowRunner,
  invalidRange: createInvalidRangeWorkflowRunner,
  zeroInput: createZeroInputWorkflowRunner,
  daHashPreimage: createDaHashPreimageWorkflowRunner,
  noReferenceInput: createNoReferenceInputWorkflowRunner,
  referenceInputNoIdx: createReferenceInputNoIdxWorkflowRunner,
  invalidSignature: createInvalidSignatureWorkflowRunner,
  fabricatedDeposit: createFabricatedDepositWorkflowRunner,
  fabricatedWithdrawal: createFabricatedWithdrawalWorkflowRunner,
  withdrawnReferenceInput: createWithdrawnReferenceInputWorkflowRunner,
  canonicalDecodability: createCanonicalDecodabilityWorkflowRunner,
  committedFieldShape: createCommittedFieldShapeWorkflowRunner,
  minFee: createMinFeeWorkflowRunner,
  doubleWithdraw: createDoubleWithdrawWorkflowRunner,
  l2TxMistag: createL2TxMistagWorkflowRunner,
  withdrawnInput: createWithdrawnInputWorkflowRunner,
  missingSignature: createMissingSignatureWorkflowRunner,
  missingNativeScriptTx: createMissingNativeScriptTxWorkflowRunner,
  inputSetUniqueness: createInputSetUniquenessWorkflowRunner,
  networkId: createNetworkIdWorkflowRunner,
  missingNativeScriptUtxo: createMissingNativeScriptUtxoWorkflowRunner,
  nativeScriptInvalid: createNativeScriptInvalidWorkflowRunner,
  minAda: createMinAdaWorkflowRunner,
  fieldPreimageLengthMismatch: createFieldPreimageLengthWorkflowRunner,
  fieldItemWidthIllegal: createFieldItemWidthIllegalWorkflowRunner,
  witnessScriptDecoding: createWitnessScriptDecodingWorkflowRunner,
  scriptIntegrityHashMissing: createScriptIntegrityHashMissingWorkflowRunner,
  transactionOutputNonCanonical:
    createTransactionOutputNonCanonicalWorkflowRunner,
  resolvedOutputNonCanonical: createResolvedOutputNonCanonicalWorkflowRunner,
  mintDeclaredAssetLimit: createMintDeclaredAssetLimitWorkflowRunner,
  spendInputSignerMissing: createSpendInputSignerMissingWorkflowRunner,
  protectedOutputSignerMissing:
    createProtectedOutputSignerMissingWorkflowRunner,
  observersForbiddenOnUntaggedNetwork:
    createObserversForbiddenOnUntaggedNetworkWorkflowRunner,
  observerOrderInvalid: createObserverOrderInvalidWorkflowRunner,
  redeemerCanonicity: createRedeemerCanonicityWorkflowRunner,
  outputReferenceScriptDecoding:
    createOutputReferenceScriptDecodingWorkflowRunner,
  executionSourceScriptDecoding:
    createExecutionSourceScriptDecodingWorkflowRunner,
  receivePurposeLanguage: createReceivePurposeLanguageWorkflowRunner,
  unusedScriptWitness: createUnusedScriptWitnessWorkflowRunner,
  missingScriptSource: createMissingScriptSourceWorkflowRunner,
  missingRedeemer: createMissingRedeemerWorkflowRunner,
  unusedRedeemer: createUnusedRedeemerWorkflowRunner,
  scriptIntegrityHashMismatch: createScriptIntegrityHashMismatchWorkflowRunner,
  distinctAssetAccumulationLimit: createDistinctAssetAccumulationWorkflowRunner,
});
