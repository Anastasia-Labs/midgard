import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  createDistinctAssetAccumulationProductionWorkflowRunnerSurfaceV1,
  type LoadDistinctAssetAccumulationProductionWorkflowV1,
} from "../distinct-asset-accumulation-limit/production-v1.js";
import {
  createExecutionNativeScriptInvalidProductionWorkflowRunnerSurfaceV1,
  type LoadExecutionNativeScriptInvalidProductionWorkflowV1,
} from "../execution-native-script-invalid/production-v1.js";
import {
  createExecutionSourceScriptDecodingProductionWorkflowRunnerSurfaceV1,
  type LoadExecutionSourceScriptDecodingProductionWorkflowV1,
} from "../execution-source-script-decoding/production-v1.js";
import {
  createFieldItemWidthIllegalProductionWorkflowRunnerSurfaceV1,
  type LoadFieldItemWidthIllegalProductionWorkflowV1,
} from "../field-item-width-illegal/production-workflow-v1.js";
import {
  createManifestBoundFieldPreimageLengthWorkflowV1,
  executeManifestBoundFieldPreimageLengthWorkflowV1,
  type ManifestBoundFieldPreimageLengthWorkflowConfigV1,
  type ManifestBoundFieldPreimageLengthWorkflowV1,
} from "../field-preimage-length-mismatch/production-workflow-v1.js";
import {
  createManifestBoundMinAdaWorkflowV1,
  type ManifestBoundMinAdaWorkflowConfigV1,
  type ManifestBoundMinAdaWorkflowV1,
  runOrResumeManifestBoundMinAdaWorkflowV1,
} from "../min-ada/production-workflow-v1.js";
import {
  createMintDeclaredAssetLimitProductionWorkflowRunnerSurfaceV1,
  type LoadMintDeclaredAssetLimitProductionWorkflowV1,
} from "../mint-declared-asset-limit/production-v1.js";
import {
  createManifestBoundMissingNativeScriptTxWorkflowV1,
  type ManifestBoundMissingNativeScriptTxWorkflowConfigV1,
  type ManifestBoundMissingNativeScriptTxWorkflowV1,
  runOrResumeManifestBoundMissingNativeScriptTxWorkflowV1,
} from "../missing-native-script-tx/production-workflow-v1.js";
import {
  createManifestBoundMissingNativeScriptUtxoWorkflowV1,
  type ManifestBoundMissingNativeScriptUtxoWorkflowConfigV1,
  type ManifestBoundMissingNativeScriptUtxoWorkflowV1,
  runOrResumeManifestBoundMissingNativeScriptUtxoWorkflowV1,
} from "../missing-native-script-utxo/production-workflow-v1.js";
import {
  createMissingRedeemerProductionWorkflowRunnerSurfaceV1,
  type LoadMissingRedeemerProductionWorkflowV1,
} from "../missing-redeemer/production-v1.js";
import {
  createMissingScriptSourceProductionWorkflowRunnerSurfaceV1,
  type LoadMissingScriptSourceProductionWorkflowV1,
} from "../missing-script-source/production-v1.js";
import {
  createManifestBoundNativeScriptInvalidWorkflowV1,
  type ManifestBoundNativeScriptInvalidWorkflowConfigV1,
  type ManifestBoundNativeScriptInvalidWorkflowV1,
  runOrResumeManifestBoundNativeScriptInvalidWorkflowV1,
} from "../native-script-invalid/production-workflow-v1.js";
import {
  createManifestBoundNetworkIdWorkflowV1,
  type ManifestBoundNetworkIdWorkflowConfigV1,
  type ManifestBoundNetworkIdWorkflowV1,
  runOrResumeManifestBoundNetworkIdWorkflowV1,
} from "../network-id/workflow-adapter-v1.js";
import {
  createObserverOrderInvalidProductionWorkflowRunnerSurfaceV1,
  type LoadObserverOrderInvalidProductionWorkflowV1,
} from "../observer-order-invalid/production-v1.js";
import {
  createObserversForbiddenProductionWorkflowRunnerSurfaceV1,
  type LoadObserversForbiddenProductionWorkflowV1,
} from "../observers-forbidden-on-untagged-network/production-v1.js";
import {
  createOutputReferenceScriptDecodingProductionWorkflowRunnerSurfaceV1,
  type LoadOutputReferenceScriptDecodingProductionWorkflowV1,
} from "../output-reference-script-decoding/production-workflow-v1.js";
import {
  createProtectedOutputSignerMissingProductionWorkflowRunnerSurfaceV1,
  type LoadProtectedOutputSignerMissingProductionWorkflowV1,
} from "../protected-output-signer-missing/production-workflow-v1.js";
import {
  createReceivePurposeLanguageProductionWorkflowRunnerSurfaceV1,
  type LoadReceivePurposeLanguageProductionWorkflowV1,
} from "../receive-purpose-language/manifest-workflow-v1.js";
import {
  createRedeemerCanonicityProductionWorkflowRunnerSurfaceV1,
  type LoadRedeemerCanonicityProductionWorkflowV1,
} from "../redeemer-canonicity/production-runtime-v1.js";
import {
  createResolvedOutputNonCanonicalProductionWorkflowRunnerSurfaceV1,
  type LoadResolvedOutputNonCanonicalProductionWorkflowV1,
} from "../resolved-output-non-canonical/production-workflow-v1.js";
import {
  createScriptIntegrityHashMismatchProductionWorkflowRunnerSurfaceV1,
  type LoadScriptIntegrityHashMismatchProductionWorkflowV1,
} from "../script-integrity-hash-mismatch/production-v1.js";
import {
  createScriptIntegrityHashMissingProductionWorkflowRunnerSurfaceV1,
  type LoadScriptIntegrityHashMissingProductionWorkflowV1,
} from "../script-integrity-hash-missing/production-v1.js";
import {
  createSpendInputSignerMissingProductionWorkflowRunnerSurfaceV1,
  type LoadSpendInputSignerMissingProductionWorkflowV1,
} from "../spend-input-signer-missing/production-workflow-v1.js";
import {
  createTransactionOutputNonCanonicalProductionWorkflowRunnerSurfaceV1,
  type LoadTransactionOutputNonCanonicalProductionWorkflowV1,
} from "../transaction-output-non-canonical/production-workflow-v1.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  createUnusedRedeemerProductionWorkflowRunnerSurfaceV1,
  type LoadUnusedRedeemerProductionWorkflowV1,
} from "../unused-redeemer/production-v1.js";
import {
  createUnusedScriptWitnessProductionWorkflowRunnerSurfaceV1,
  type LoadUnusedScriptWitnessProductionWorkflowV1,
} from "../unused-script-witness/production-v1.js";
import {
  createWitnessScriptDecodingProductionWorkflowRunnerSurfaceV1,
  type LoadWitnessScriptDecodingProductionWorkflowV1,
} from "../witness-script-decoding/production-workflow-v1.js";
import {
  createManifestBoundDoubleSpendWorkflowV1,
  type ManifestBoundDoubleSpendWorkflowConfigV1,
  type ManifestBoundDoubleSpendWorkflowV1,
  runOrResumeManifestBoundDoubleSpendWorkflowV1,
} from "./double-spend-adapter-v1.js";
import { DirectoryFraudProofWorkflowJournalStoreV1 } from "./journal-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
} from "./production-actuation-permit-v1.js";
import {
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "./production-adapters-v1.js";
import {
  createManifestBoundCanonicalDecodabilityWorkflowV1,
  type ManifestBoundCanonicalDecodabilityWorkflowConfigV1,
  type ManifestBoundCanonicalDecodabilityWorkflowV1,
  runOrResumeManifestBoundCanonicalDecodabilityWorkflowV1,
} from "./production-canonical-decodability-v1.js";
import {
  createManifestBoundCommittedFieldShapeWorkflowV1,
  type ManifestBoundCommittedFieldShapeWorkflowConfigV1,
  type ManifestBoundCommittedFieldShapeWorkflowV1,
  runOrResumeManifestBoundCommittedFieldShapeWorkflowV1,
} from "./production-committed-field-shape-v1.js";
import {
  createManifestBoundDaHashPreimageWorkflowV1,
  type ManifestBoundDaHashPreimageWorkflowConfigV1,
  type ManifestBoundDaHashPreimageWorkflowV1,
  runOrResumeManifestBoundDaHashPreimageWorkflowV1,
} from "./production-da-hash-preimage-v1.js";
import {
  createManifestBoundDoubleWithdrawWorkflowV1,
  type ManifestBoundDoubleWithdrawWorkflowConfigV1,
  type ManifestBoundDoubleWithdrawWorkflowV1,
  runOrResumeManifestBoundDoubleWithdrawWorkflowV1,
} from "./production-double-withdraw-v1.js";
import {
  createManifestBoundFabricatedDepositWorkflowV1,
  type ManifestBoundFabricatedDepositWorkflowConfigV1,
  type ManifestBoundFabricatedDepositWorkflowV1,
  runOrResumeManifestBoundFabricatedDepositWorkflowV1,
} from "./production-fabricated-deposit-v1.js";
import {
  createManifestBoundFabricatedWithdrawalWorkflowV1,
  type ManifestBoundFabricatedWithdrawalWorkflowConfigV1,
  type ManifestBoundFabricatedWithdrawalWorkflowV1,
  runOrResumeManifestBoundFabricatedWithdrawalWorkflowV1,
} from "./production-fabricated-withdrawal-v1.js";
import type { ProductionWorkflowFundingRequirementsV1 } from "./production-funding-requirements-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "./production-funding-reservation-permit-v1.js";
import {
  createManifestBoundInputNoIdxWorkflowV1,
  type ManifestBoundInputNoIdxWorkflowConfigV1,
  type ManifestBoundInputNoIdxWorkflowV1,
  runOrResumeManifestBoundInputNoIdxWorkflowV1,
} from "./production-input-no-idx-v1.js";
import {
  createManifestBoundInputSetUniquenessWorkflowV1,
  type ManifestBoundInputSetUniquenessWorkflowConfigV1,
  type ManifestBoundInputSetUniquenessWorkflowV1,
  runOrResumeManifestBoundInputSetUniquenessWorkflowV1,
} from "./production-input-set-uniqueness-v1.js";
import {
  createManifestBoundInvalidSignatureWorkflowV1,
  type ManifestBoundInvalidSignatureWorkflowConfigV1,
  type ManifestBoundInvalidSignatureWorkflowV1,
  runOrResumeManifestBoundInvalidSignatureWorkflowV1,
} from "./production-invalid-signature-v1.js";
import {
  createManifestBoundL2TxMistagWorkflowV1,
  type ManifestBoundL2TxMistagWorkflowConfigV1,
  type ManifestBoundL2TxMistagWorkflowV1,
  runOrResumeManifestBoundL2TxMistagWorkflowV1,
} from "./production-l2-tx-mistag-v1.js";
import {
  createManifestBoundMinFeeWorkflowV1,
  type ManifestBoundMinFeeWorkflowConfigV1,
  type ManifestBoundMinFeeWorkflowV1,
  runOrResumeManifestBoundMinFeeWorkflowV1,
} from "./production-min-fee-v1.js";
import {
  createManifestBoundMissingSignatureWorkflowV1,
  type ManifestBoundMissingSignatureWorkflowConfigV1,
  type ManifestBoundMissingSignatureWorkflowV1,
  runOrResumeManifestBoundMissingSignatureWorkflowV1,
} from "./production-missing-signature-v1.js";
import {
  createManifestBoundInvalidRangeWorkflowV1,
  createManifestBoundZeroInputWorkflowV1,
  type ManifestBoundInvalidRangeWorkflowConfigV1,
  type ManifestBoundInvalidRangeWorkflowV1,
  type ManifestBoundZeroInputWorkflowConfigV1,
  type ManifestBoundZeroInputWorkflowV1,
  runOrResumeManifestBoundInvalidRangeWorkflowV1,
  runOrResumeManifestBoundZeroInputWorkflowV1,
} from "./production-native-inclusion-two-step-v1.js";
import {
  createManifestBoundNoReferenceInputWorkflowV1,
  type ManifestBoundNoReferenceInputWorkflowConfigV1,
  type ManifestBoundNoReferenceInputWorkflowV1,
  runOrResumeManifestBoundNoReferenceInputWorkflowV1,
} from "./production-no-reference-input-v1.js";
import {
  createManifestBoundNonExistentInputWorkflowV1,
  type ManifestBoundNonExistentInputWorkflowConfigV1,
  type ManifestBoundNonExistentInputWorkflowV1,
  runOrResumeManifestBoundNonExistentInputWorkflowV1,
} from "./production-non-existent-input-v1.js";
import {
  createManifestBoundReferenceInputNoIdxWorkflowV1,
  type ManifestBoundReferenceInputNoIdxWorkflowConfigV1,
  type ManifestBoundReferenceInputNoIdxWorkflowV1,
  runOrResumeManifestBoundReferenceInputNoIdxWorkflowV1,
} from "./production-reference-input-no-idx-v1.js";
import {
  createAdmittedProductionWorkflowRunnerV1,
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
} from "./production-runner-admission-v1.js";
import {
  createManifestBoundWithdrawnInputWorkflowV1,
  type ManifestBoundWithdrawnInputWorkflowConfigV1,
  type ManifestBoundWithdrawnInputWorkflowV1,
  runOrResumeManifestBoundWithdrawnInputWorkflowV1,
} from "./production-withdrawn-input-v1.js";
import {
  createManifestBoundWithdrawnReferenceInputWorkflowV1,
  type ManifestBoundWithdrawnReferenceInputWorkflowConfigV1,
  type ManifestBoundWithdrawnReferenceInputWorkflowV1,
  runOrResumeManifestBoundWithdrawnReferenceInputWorkflowV1,
} from "./production-withdrawn-reference-input-v1.js";

export const PRODUCTION_WORKFLOW_RUNTIME_CONFIG_V1 =
  "midgard-production-fraud-proof-runtime-config-v1" as const;

export type LoadedProductionWorkflowRuntimeV1<Config> = {
  readonly schemaVersion: typeof PRODUCTION_WORKFLOW_RUNTIME_CONFIG_V1;
  /** Infrastructure and credentials only. Proof evidence is forbidden here. */
  readonly config: Config;
  readonly retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  /** Closes every transport/provider allocated while loading the runtime. */
  readonly close: () => Promise<void>;
};

export type ProductionWorkflowRuntimeConfigLoaderV1<Config> = (input: {
  readonly runtimeConfigPath: string;
  /** Permit-free deployment identity/configuration; loading cannot actuate. */
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedProductionWorkflowRuntimeV1<Config>>;

type ManifestBoundWorkflowIdentityV1<
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
const createManifestBoundProductionWorkflowRunOrResumeV1 =
  <
    Category extends FraudProofCatalogueCategoryName,
    Config,
    Workflow extends ManifestBoundWorkflowIdentityV1<Category>,
  >({
    category,
    loadRuntimeConfig,
    constructWorkflow,
    execute,
  }: {
    readonly category: Category;
    readonly loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<Config>;
    readonly constructWorkflow: (config: Config) => Promise<Workflow>;
    readonly execute: (input: {
      readonly workflow: Workflow;
      readonly sources: readonly RetainedDaPayloadSource[];
      readonly journal: DirectoryFraudProofWorkflowJournalStoreV1;
      readonly mode: "run" | "resume";
    }) => Promise<unknown>;
  }): ProductionWorkflowAdapterRunnerV1["runOrResume"] =>
  async (invocation: ProductionWorkflowAdapterRunnerInputV1) => {
    if (invocation.category !== category) {
      throw new Error(
        `production workflow runner category mismatch: expected=${category} actual=${invocation.category}`,
      );
    }
    const journal = bindProductionWorkflowFundingReservationJournalV1({
      permit: invocation.fundingReservationPermit,
      journal: bindProductionWorkflowActuationJournalV1({
        journal: new DirectoryFraudProofWorkflowJournalStoreV1(
          invocation.journalDirectory,
        ),
        permit: invocation.actuationPermit,
        decisionDigest: invocation.decisionDigest,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category,
        headerHash: invocation.headerHash,
      }),
    });
    assertProductionWorkflowJournalActuationV1({
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
      if (loaded.schemaVersion !== PRODUCTION_WORKFLOW_RUNTIME_CONFIG_V1) {
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
export const createManifestBoundProductionWorkflowRunnerV1 = <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends ManifestBoundWorkflowIdentityV1<Category>,
>(
  input: Parameters<
    typeof createManifestBoundProductionWorkflowRunOrResumeV1<
      Category,
      Config,
      Workflow
    >
  >[0],
): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1(input),
  });

const runnerFunding = (
  fundingRequirements: ProductionWorkflowFundingRequirementsV1 | undefined,
): Readonly<{
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1;
}> =>
  fundingRequirements === undefined
    ? Object.freeze({})
    : Object.freeze({ fundingRequirements });

export const createDoubleSpendProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundDoubleSpendWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "doubleSpend",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "doubleSpend",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundDoubleSpendWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundDoubleSpendWorkflowV1({
          workflow: workflow as ManifestBoundDoubleSpendWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createFieldItemWidthIllegalProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadFieldItemWidthIllegalProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createFieldItemWidthIllegalProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "fieldItemWidthIllegal",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createFieldPreimageLengthProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundFieldPreimageLengthWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "fieldPreimageLengthMismatch",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "fieldPreimageLengthMismatch",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundFieldPreimageLengthWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await executeManifestBoundFieldPreimageLengthWorkflowV1({
          workflow: workflow as ManifestBoundFieldPreimageLengthWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createScriptIntegrityHashMissingProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadScriptIntegrityHashMissingProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createScriptIntegrityHashMissingProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "scriptIntegrityHashMissing",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createTransactionOutputNonCanonicalProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadTransactionOutputNonCanonicalProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createTransactionOutputNonCanonicalProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "transactionOutputNonCanonical",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createResolvedOutputNonCanonicalProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadResolvedOutputNonCanonicalProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createResolvedOutputNonCanonicalProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "resolvedOutputNonCanonical",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createMintDeclaredAssetLimitProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadMintDeclaredAssetLimitProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createMintDeclaredAssetLimitProductionWorkflowRunnerSurfaceV1(
    {
      loadRuntimeConfig,
    },
  );
  return createAdmittedProductionWorkflowRunnerV1({
    category: "mintDeclaredAssetLimit",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createSpendInputSignerMissingProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadSpendInputSignerMissingProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createSpendInputSignerMissingProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "spendInputSignerMissing",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createProtectedOutputSignerMissingProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadProtectedOutputSignerMissingProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createProtectedOutputSignerMissingProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "protectedOutputSignerMissing",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createObserversForbiddenOnUntaggedNetworkProductionWorkflowRunnerV1 =
  (
    loadRuntimeConfig: LoadObserversForbiddenProductionWorkflowV1,
    fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
  ): ProductionWorkflowAdapterRunnerV1 => {
    const surface = createObserversForbiddenProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
    return createAdmittedProductionWorkflowRunnerV1({
      category: "observersForbiddenOnUntaggedNetwork",
      ...runnerFunding(fundingRequirements),
      runOrResume: surface.runOrResume,
    });
  };

export const createOutputReferenceScriptDecodingProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadOutputReferenceScriptDecodingProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createOutputReferenceScriptDecodingProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "outputReferenceScriptDecoding",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createWitnessScriptDecodingProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadWitnessScriptDecodingProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createWitnessScriptDecodingProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "witnessScriptDecoding",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createExecutionSourceScriptDecodingProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadExecutionSourceScriptDecodingProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createExecutionSourceScriptDecodingProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "executionSourceScriptDecoding",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createExecutionNativeScriptInvalidProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadExecutionNativeScriptInvalidProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createExecutionNativeScriptInvalidProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "executionNativeScriptInvalid",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createObserverOrderInvalidProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadObserverOrderInvalidProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createObserverOrderInvalidProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "observerOrderInvalid",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createRedeemerCanonicityProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadRedeemerCanonicityProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createRedeemerCanonicityProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "redeemerCanonicity",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createReceivePurposeLanguageProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadReceivePurposeLanguageProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createReceivePurposeLanguageProductionWorkflowRunnerSurfaceV1(
    { loadRuntimeConfig },
  );
  return createAdmittedProductionWorkflowRunnerV1({
    category: "receivePurposeLanguage",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createUnusedScriptWitnessProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadUnusedScriptWitnessProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createUnusedScriptWitnessProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "unusedScriptWitness",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createMissingScriptSourceProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadMissingScriptSourceProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createMissingScriptSourceProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "missingScriptSource",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createMissingRedeemerProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadMissingRedeemerProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createMissingRedeemerProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "missingRedeemer",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createUnusedRedeemerProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadUnusedRedeemerProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface = createUnusedRedeemerProductionWorkflowRunnerSurfaceV1({
    loadRuntimeConfig,
  });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "unusedRedeemer",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createScriptIntegrityHashMismatchProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadScriptIntegrityHashMismatchProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createScriptIntegrityHashMismatchProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "scriptIntegrityHashMismatch",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createDistinctAssetAccumulationProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: LoadDistinctAssetAccumulationProductionWorkflowV1,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const surface =
    createDistinctAssetAccumulationProductionWorkflowRunnerSurfaceV1({
      loadRuntimeConfig,
    });
  return createAdmittedProductionWorkflowRunnerV1({
    category: "distinctAssetAccumulationLimit",
    ...runnerFunding(fundingRequirements),
    runOrResume: surface.runOrResume,
  });
};

export const createNetworkIdProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundNetworkIdWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "networkId",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "networkId",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNetworkIdWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNetworkIdWorkflowV1({
          workflow: workflow as ManifestBoundNetworkIdWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createMinAdaProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundMinAdaWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "minAda",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "minAda",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMinAdaWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMinAdaWorkflowV1({
          workflow: workflow as ManifestBoundMinAdaWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createNativeScriptInvalidProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundNativeScriptInvalidWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "nativeScriptInvalid",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "nativeScriptInvalid",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNativeScriptInvalidWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNativeScriptInvalidWorkflowV1({
          workflow: workflow as ManifestBoundNativeScriptInvalidWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createMissingNativeScriptUtxoProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundMissingNativeScriptUtxoWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "missingNativeScriptUtxo",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "missingNativeScriptUtxo",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMissingNativeScriptUtxoWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMissingNativeScriptUtxoWorkflowV1({
          workflow: workflow as ManifestBoundMissingNativeScriptUtxoWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createDaHashPreimageProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundDaHashPreimageWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "daHashPreimage",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "daHashPreimage",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundDaHashPreimageWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundDaHashPreimageWorkflowV1({
          workflow: workflow as ManifestBoundDaHashPreimageWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createCommittedFieldShapeProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundCommittedFieldShapeWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "committedFieldShape",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "committedFieldShape",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundCommittedFieldShapeWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundCommittedFieldShapeWorkflowV1({
          workflow: workflow as ManifestBoundCommittedFieldShapeWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createCanonicalDecodabilityProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundCanonicalDecodabilityWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "canonicalDecodability",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "canonicalDecodability",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundCanonicalDecodabilityWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundCanonicalDecodabilityWorkflowV1({
          workflow: workflow as ManifestBoundCanonicalDecodabilityWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createDoubleWithdrawProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundDoubleWithdrawWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "doubleWithdraw",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "doubleWithdraw",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundDoubleWithdrawWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundDoubleWithdrawWorkflowV1({
          workflow: workflow as ManifestBoundDoubleWithdrawWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createMissingSignatureProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundMissingSignatureWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "missingSignature",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "missingSignature",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMissingSignatureWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMissingSignatureWorkflowV1({
          workflow: workflow as ManifestBoundMissingSignatureWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createMissingNativeScriptTxProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundMissingNativeScriptTxWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "missingNativeScriptTx",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "missingNativeScriptTx",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMissingNativeScriptTxWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMissingNativeScriptTxWorkflowV1({
          workflow: workflow as ManifestBoundMissingNativeScriptTxWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createInvalidRangeProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundInvalidRangeWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "invalidRange",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "invalidRange",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInvalidRangeWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInvalidRangeWorkflowV1({
          workflow: workflow as ManifestBoundInvalidRangeWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createInvalidSignatureProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundInvalidSignatureWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "invalidSignature",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "invalidSignature",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInvalidSignatureWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInvalidSignatureWorkflowV1({
          workflow: workflow as ManifestBoundInvalidSignatureWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createInputNoIdxProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundInputNoIdxWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "nonExistentInputNoIndex",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "nonExistentInputNoIndex",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInputNoIdxWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInputNoIdxWorkflowV1({
          workflow: workflow as ManifestBoundInputNoIdxWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createNonExistentInputProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundNonExistentInputWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "nonExistentInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "nonExistentInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNonExistentInputWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNonExistentInputWorkflowV1({
          workflow: workflow as ManifestBoundNonExistentInputWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createNoReferenceInputProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundNoReferenceInputWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "noReferenceInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "noReferenceInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundNoReferenceInputWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundNoReferenceInputWorkflowV1({
          workflow: workflow as ManifestBoundNoReferenceInputWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createReferenceInputNoIdxProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundReferenceInputNoIdxWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "referenceInputNoIdx",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "referenceInputNoIdx",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundReferenceInputNoIdxWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundReferenceInputNoIdxWorkflowV1({
          workflow: workflow as ManifestBoundReferenceInputNoIdxWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createInputSetUniquenessProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundInputSetUniquenessWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "inputSetUniqueness",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "inputSetUniqueness",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundInputSetUniquenessWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundInputSetUniquenessWorkflowV1({
          workflow: workflow as ManifestBoundInputSetUniquenessWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createZeroInputProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundZeroInputWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "zeroInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "zeroInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundZeroInputWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundZeroInputWorkflowV1({
          workflow: workflow as ManifestBoundZeroInputWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createL2TxMistagProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundL2TxMistagWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "l2TxMistag",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "l2TxMistag",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundL2TxMistagWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundL2TxMistagWorkflowV1({
          workflow: workflow as ManifestBoundL2TxMistagWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createMinFeeProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundMinFeeWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "minFee",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "minFee",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundMinFeeWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundMinFeeWorkflowV1({
          workflow: workflow as ManifestBoundMinFeeWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createWithdrawnInputProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundWithdrawnInputWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "withdrawnInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "withdrawnInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundWithdrawnInputWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundWithdrawnInputWorkflowV1({
          workflow: workflow as ManifestBoundWithdrawnInputWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createFabricatedDepositProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundFabricatedDepositWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "fabricatedDeposit",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "fabricatedDeposit",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundFabricatedDepositWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundFabricatedDepositWorkflowV1({
          workflow: workflow as ManifestBoundFabricatedDepositWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createFabricatedWithdrawalProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundFabricatedWithdrawalWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "fabricatedWithdrawal",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "fabricatedWithdrawal",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundFabricatedWithdrawalWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundFabricatedWithdrawalWorkflowV1({
          workflow: workflow as ManifestBoundFabricatedWithdrawalWorkflowV1,
          sources,
          journal,
        }),
    }),
  });

export const createWithdrawnReferenceInputProductionWorkflowRunnerV1 = (
  loadRuntimeConfig: ProductionWorkflowRuntimeConfigLoaderV1<ManifestBoundWithdrawnReferenceInputWorkflowConfigV1>,
  fundingRequirements?: ProductionWorkflowFundingRequirementsV1,
): ProductionWorkflowAdapterRunnerV1 =>
  createAdmittedProductionWorkflowRunnerV1({
    category: "withdrawnReferenceInput",
    ...runnerFunding(fundingRequirements),
    runOrResume: createManifestBoundProductionWorkflowRunOrResumeV1({
      category: "withdrawnReferenceInput",
      loadRuntimeConfig,
      constructWorkflow: createManifestBoundWithdrawnReferenceInputWorkflowV1,
      execute: async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundWithdrawnReferenceInputWorkflowV1({
          workflow: workflow as ManifestBoundWithdrawnReferenceInputWorkflowV1,
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
export const PRODUCTION_WORKFLOW_RUNNER_FACTORIES_V1 = Object.freeze({
  doubleSpend: createDoubleSpendProductionWorkflowRunnerV1,
  nonExistentInput: createNonExistentInputProductionWorkflowRunnerV1,
  nonExistentInputNoIndex: createInputNoIdxProductionWorkflowRunnerV1,
  invalidRange: createInvalidRangeProductionWorkflowRunnerV1,
  zeroInput: createZeroInputProductionWorkflowRunnerV1,
  daHashPreimage: createDaHashPreimageProductionWorkflowRunnerV1,
  noReferenceInput: createNoReferenceInputProductionWorkflowRunnerV1,
  referenceInputNoIdx: createReferenceInputNoIdxProductionWorkflowRunnerV1,
  invalidSignature: createInvalidSignatureProductionWorkflowRunnerV1,
  fabricatedDeposit: createFabricatedDepositProductionWorkflowRunnerV1,
  fabricatedWithdrawal: createFabricatedWithdrawalProductionWorkflowRunnerV1,
  withdrawnReferenceInput:
    createWithdrawnReferenceInputProductionWorkflowRunnerV1,
  canonicalDecodability: createCanonicalDecodabilityProductionWorkflowRunnerV1,
  committedFieldShape: createCommittedFieldShapeProductionWorkflowRunnerV1,
  minFee: createMinFeeProductionWorkflowRunnerV1,
  doubleWithdraw: createDoubleWithdrawProductionWorkflowRunnerV1,
  l2TxMistag: createL2TxMistagProductionWorkflowRunnerV1,
  withdrawnInput: createWithdrawnInputProductionWorkflowRunnerV1,
  missingSignature: createMissingSignatureProductionWorkflowRunnerV1,
  missingNativeScriptTx: createMissingNativeScriptTxProductionWorkflowRunnerV1,
  inputSetUniqueness: createInputSetUniquenessProductionWorkflowRunnerV1,
  networkId: createNetworkIdProductionWorkflowRunnerV1,
  missingNativeScriptUtxo:
    createMissingNativeScriptUtxoProductionWorkflowRunnerV1,
  nativeScriptInvalid: createNativeScriptInvalidProductionWorkflowRunnerV1,
  minAda: createMinAdaProductionWorkflowRunnerV1,
  fieldPreimageLengthMismatch:
    createFieldPreimageLengthProductionWorkflowRunnerV1,
  fieldItemWidthIllegal: createFieldItemWidthIllegalProductionWorkflowRunnerV1,
  witnessScriptDecoding: createWitnessScriptDecodingProductionWorkflowRunnerV1,
  scriptIntegrityHashMissing:
    createScriptIntegrityHashMissingProductionWorkflowRunnerV1,
  transactionOutputNonCanonical:
    createTransactionOutputNonCanonicalProductionWorkflowRunnerV1,
  resolvedOutputNonCanonical:
    createResolvedOutputNonCanonicalProductionWorkflowRunnerV1,
  mintDeclaredAssetLimit:
    createMintDeclaredAssetLimitProductionWorkflowRunnerV1,
  spendInputSignerMissing:
    createSpendInputSignerMissingProductionWorkflowRunnerV1,
  protectedOutputSignerMissing:
    createProtectedOutputSignerMissingProductionWorkflowRunnerV1,
  observersForbiddenOnUntaggedNetwork:
    createObserversForbiddenOnUntaggedNetworkProductionWorkflowRunnerV1,
  observerOrderInvalid: createObserverOrderInvalidProductionWorkflowRunnerV1,
  redeemerCanonicity: createRedeemerCanonicityProductionWorkflowRunnerV1,
  outputReferenceScriptDecoding:
    createOutputReferenceScriptDecodingProductionWorkflowRunnerV1,
  executionSourceScriptDecoding:
    createExecutionSourceScriptDecodingProductionWorkflowRunnerV1,
  receivePurposeLanguage:
    createReceivePurposeLanguageProductionWorkflowRunnerV1,
  unusedScriptWitness: createUnusedScriptWitnessProductionWorkflowRunnerV1,
  missingScriptSource: createMissingScriptSourceProductionWorkflowRunnerV1,
  missingRedeemer: createMissingRedeemerProductionWorkflowRunnerV1,
  unusedRedeemer: createUnusedRedeemerProductionWorkflowRunnerV1,
  scriptIntegrityHashMismatch:
    createScriptIntegrityHashMismatchProductionWorkflowRunnerV1,
  distinctAssetAccumulationLimit:
    createDistinctAssetAccumulationProductionWorkflowRunnerV1,
});
