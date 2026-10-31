import {
  type DeploymentManifestEventHistoryBounds,
  type DeploymentManifestEventHistoryRetentionAddresses,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  type FraudProofCatalogueDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { Network, type Script } from "@lucid-evolution/lucid";

export type ContractDeploymentInfoEntry = {
  readonly scriptHash: string;
  readonly refScriptUTxO?: {
    readonly txHash: string;
    readonly outputIndex: number;
  } | null;
  readonly contract?: {
    readonly type: Script["type"];
    readonly cborHex: string;
  };
  readonly fraudProofCatalogue?: FraudProofCatalogueDeploymentInfo;
  readonly eventHistoryBounds?: DeploymentManifestEventHistoryBounds;
  readonly eventHistoryRetentionAddress?: string;
  readonly eventHistoryRetentionAddresses?: DeploymentManifestEventHistoryRetentionAddresses;
};

export type ContractDeploymentInfo = Readonly<
  Record<string, ContractDeploymentInfoEntry>
>;

export type InspectContractsParams = {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network?: Network;
};

export type InspectContractsFromFilesParams = {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly network?: Network;
};

export type InspectContractsOutput = {
  readonly network: Network;
  /**
   * Necessary-only L1 envelope audit over every parameterized spending
   * validator selected by the fault-proof contract builder. Passing this
   * audit does not satisfy proof-fit: the complete concrete transaction,
   * including framing, witnesses, redeemers, and outputs, must also fit.
   */
  readonly l1SpendingScriptEnvelopeNecessaryCondition: {
    readonly maxTransactionBytes: number;
    readonly appliedSpendingScriptCount: number;
    readonly allAppliedSpendingScriptsWithinEnvelope: boolean;
    readonly oversizedAppliedSpendingScripts: readonly InspectContractsOversizedSpendingScript[];
  };
  readonly computationThread: {
    readonly policyId: string;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly address: string;
    readonly spendingScriptHash: string;
  };
  readonly fraudProofCatalogue: {
    readonly root: string | null;
    readonly derivedRoot: string | null;
    readonly rootMatchesDerived: boolean | null;
    readonly doubleSpend: InspectContractsCatalogueCategoryOutput;
    readonly nonExistentInput: InspectContractsCatalogueCategoryOutput;
    readonly nonExistentInputNoIndex: InspectContractsCatalogueCategoryOutput;
    readonly invalidRange: InspectContractsCatalogueCategoryOutput;
    readonly zeroInput: InspectContractsCatalogueCategoryOutput;
    readonly transitionTrace: InspectContractsCatalogueCategoryOutput;
    readonly validationTraceDispute: InspectContractsCatalogueCategoryOutput;
    readonly daHashPreimage: InspectContractsCatalogueCategoryOutput;
    readonly noReferenceInput: InspectContractsCatalogueCategoryOutput;
    readonly referenceInputNoIdx: InspectContractsCatalogueCategoryOutput;
    readonly invalidSignature: InspectContractsCatalogueCategoryOutput;
    readonly categories: Readonly<
      Record<
        FraudProofCatalogueCategoryName,
        InspectContractsCatalogueCategoryOutput
      >
    >;
  };
  /**
   * Compiled step identities for every registered category. This offline
   * report does not establish publication or runtime readiness; deployment
   * admission and workflow preflight own those checks.
   */
  readonly registeredCategories: Readonly<
    Record<FraudProofCatalogueCategoryName, InspectContractsRegisteredCategory>
  >;
  readonly doubleSpend: {
    readonly categoryFirstStepHash: string;
    readonly deploymentDoubleSpendScriptHash: string | null;
    readonly deploymentDoubleSpendMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly nonExistentInput: {
    readonly categoryFirstStepHash: string;
    readonly deploymentNonExistentInputScriptHash: string | null;
    readonly deploymentNonExistentInputMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly invalidRange: {
    readonly categoryFirstStepHash: string;
    readonly deploymentInvalidRangeScriptHash: string | null;
    readonly deploymentInvalidRangeMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly zeroInput: {
    readonly categoryFirstStepHash: string;
    readonly deploymentZeroInputScriptHash: string | null;
    readonly deploymentZeroInputMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly daHashPreimage: {
    readonly categoryFirstStepHash: string;
    readonly deploymentDaHashPreimageScriptHash: string | null;
    readonly deploymentDaHashPreimageMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly noReferenceInput: {
    readonly categoryFirstStepHash: string;
    readonly deploymentNoReferenceInputScriptHash: string | null;
    readonly deploymentNoReferenceInputMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly referenceInputNoIdx: {
    readonly categoryFirstStepHash: string;
    readonly deploymentReferenceInputNoIdxScriptHash: string | null;
    readonly deploymentReferenceInputNoIdxMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly invalidSignature: {
    readonly categoryFirstStepHash: string;
    readonly deploymentInvalidSignatureScriptHash: string | null;
    readonly deploymentInvalidSignatureMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly nonExistentInputNoIndex: {
    readonly categoryFirstStepHash: string;
    readonly deploymentNonExistentInputNoIndexScriptHash: string | null;
    readonly deploymentNonExistentInputNoIndexMatchesFirstStep: boolean | null;
    /** Embedded deployment bytes still cross-checked against the applied hash. */
    readonly deploymentMatchesEmbeddedScriptBytes: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly transitionTrace: {
    readonly semanticYields: readonly {
      readonly name: string;
      readonly scriptHash: string;
      readonly standaloneScriptBytes: number;
      readonly withinL1TransactionByteEnvelopeNecessaryCondition: boolean;
    }[];
    readonly categoryFirstStepHash: string;
    readonly deploymentTransitionTraceScriptHash: string | null;
    readonly deploymentTransitionTraceMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly validationTraceDispute: {
    readonly categoryFirstStepHash: string;
    readonly deploymentValidationTraceDisputeScriptHash: string | null;
    readonly deploymentValidationTraceDisputeMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      ...InspectContractsStepOutput[],
    ];
  };
};

export type InspectContractsStepOutput = {
  readonly name:
    | "step01"
    | "step02"
    | "step03"
    | "step04"
    | "step05"
    | "step06"
    | "route"
    | "control"
    | "withdrawal"
    | "forced"
    | "accepted"
    | "deposit"
    | "l1Event"
    | "duplicate"
    | "dispute"
    | "source"
    | "game"
    | "boundary"
    | "timeout"
    | "award"
    | `semantic-resolver-${number}`
    | `prepare-resolver-${number}`;
  readonly scriptHash: string;
  readonly address: string;
  readonly standaloneScriptBytes: number;
  /**
   * A necessary condition only. The script must leave at least one byte for
   * the rest of the complete L1 transaction; full proof-fit is stricter.
   */
  readonly withinL1TransactionByteEnvelopeNecessaryCondition: boolean;
};

export type InspectContractsProofCategory = FraudProofCatalogueCategoryName;

export type InspectContractsRegisteredCategory = {
  readonly categoryFirstStepHash: string;
  readonly deploymentFirstStepScriptHash: string | null;
  readonly deploymentFirstStepMatches: boolean | null;
  readonly steps: readonly InspectContractsStepOutput[];
};

export type InspectContractsOversizedSpendingScript = {
  readonly category: InspectContractsProofCategory;
  readonly name: InspectContractsStepOutput["name"];
  readonly scriptHash: string;
  readonly standaloneScriptBytes: number;
};

export type InspectContractsCatalogueCategoryOutput = {
  readonly categoryId: string | null;
  readonly expectedCategoryId: string | null;
  readonly categoryIdMatchesExpected: boolean | null;
  readonly scriptHash: string | null;
  readonly scriptHashMatchesFirstStep: boolean | null;
  readonly membershipProofCbor: string | null;
  readonly membershipProofMatchesDerived: boolean | null;
};

export type ImplementedFraudProofCategoryName = FraudProofCatalogueCategoryName;

export const completeFraudProofCategoryRecord = <Value>(
  entries: readonly (readonly [FraudProofCatalogueCategoryName, Value])[],
): Readonly<Record<FraudProofCatalogueCategoryName, Value>> => {
  const result: Partial<Record<FraudProofCatalogueCategoryName, Value>> = {};
  for (const [category, value] of entries) {
    result[category] = value;
  }
  for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    if (result[category] === undefined) {
      throw new Error(
        `Failed to construct complete fraud-proof category record: ${category} is missing.`,
      );
    }
  }
  return result as Readonly<Record<FraudProofCatalogueCategoryName, Value>>;
};

export const expectedFraudProofCategoryId = (
  name: FraudProofCatalogueCategoryName,
): string => FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name];

export const DEFAULT_FAULT_PROOF_NETWORK: Network = "Preprod";

export const NETWORKS = new Set<Network>(["Mainnet", "Preview", "Preprod"]);
