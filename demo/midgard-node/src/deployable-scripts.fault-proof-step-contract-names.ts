import * as SDK from "@al-ft/midgard-sdk";
import { type ReferenceScriptAuthTokenTarget } from "@al-ft/midgard-sdk";
import { type Script } from "@lucid-evolution/lucid";

export const REFERENCE_SCRIPT_COMMAND_NAMES = [
  "node-runtime",
  "protocol-init",
  "reference-script-auth",
  "hub-oracle",
  "da",
  "state-queue",
  "scheduler",
  "registered-operators",
  "active-operators",
  "retired-operators",
  "deposit",
  "withdrawal",
  "settlement",
  "phas-membership",
  "reserve",
  "payout",
] as const;

export type ReferenceScriptCommandName =
  (typeof REFERENCE_SCRIPT_COMMAND_NAMES)[number];

/** `node-runtime` is every published script, so no entry lists it. */
export type DeployableScriptCommandName = Exclude<
  ReferenceScriptCommandName,
  "node-runtime"
>;

export type DeployableScriptPurpose = "spend" | "mint" | "withdraw";

export type DeployableScript = {
  /** Manifest contract name, e.g. `depositMint`. */
  readonly contract: string;
  /** Reference-script role; absent for scripts that are never published. */
  readonly role: ReferenceScriptAuthTokenTarget | undefined;
  readonly purpose: DeployableScriptPurpose;
  readonly commands: readonly DeployableScriptCommandName[];
  readonly script: Script;
  readonly scriptHash: string;
};

export type PublishedDeployableScript = DeployableScript & {
  readonly role: ReferenceScriptAuthTokenTarget;
};

// ---------------------------------------------------------------------------
// Fault-proof chain naming
// ---------------------------------------------------------------------------

/**
 * Linear fault-proof categories, in publication order. The manifest records
 * the same categories in a different order (see the `registeredChains*`
 * sections below).
 */
export const REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES = [
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "nativeScriptDecoding",
  "missingSignature",
  "missingNativeScriptTx",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "withdrawalMistag",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
  "valueNotPreserved",
  "inputSetUniqueness",
  "mintAuthorization",
  "networkId",
  "missingNativeScriptUtxo",
  "nativeScriptInvalid",
  "minAda",
  "fieldPreimageLengthMismatch",
  "fieldItemWidthIllegal",
  "witnessScriptDecoding",
  "scriptIntegrityHashMissing",
  "transactionOutputNonCanonical",
  "mintItemNonCanonical",
  "resolvedOutputNonCanonical",
  "mintDeclaredAssetLimit",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
  "outputReferenceScriptDecoding",
  "executionSourceScriptDecoding",
  "receivePurposeLanguage",
  "unusedScriptWitness",
  "missingScriptSource",
  "missingRedeemer",
  "unusedRedeemer",
  "executionNativeScriptInvalid",
  "scriptIntegrityHashMismatch",
  "distinctAssetAccumulationLimit",
] as const satisfies readonly (keyof SDK.FaultProofContractChains)[];

export type RegisteredLinearFaultProofCategory =
  (typeof REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES)[number];

/** Pre-registry fault-proof families, in manifest and publication order. */
export const LEGACY_FAULT_PROOF_FAMILIES = [
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "invalidRange",
  "zeroInput",
  "daHashPreimage",
  "noReferenceInput",
  "referenceInputNoIdx",
  "invalidSignature",
] as const satisfies readonly (keyof SDK.FaultProofContractChains)[];

export type LegacyFaultProofFamily =
  (typeof LEGACY_FAULT_PROOF_FAMILIES)[number];

export const TRANSITION_TRACE_FINAL_CONTRACT_NAMES = [
  "fraudProofTransitionTraceControl",
  "fraudProofTransitionTraceSource",
  "fraudProofTransitionTraceWithdrawal",
  "fraudProofTransitionTraceForced",
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
  "fraudProofTransitionTraceL1Event",
  "fraudProofTransitionTraceDuplicate",
] as const;

export const upperFirst = (value: string): string =>
  `${value.slice(0, 1).toUpperCase()}${value.slice(1)}`;

/**
 * Chains whose canonical contract names are NOT `…Step{index + 1}`.
 *
 * The generic rule below assumes a chain's Nth compiled step is the Nth
 * declared step, which stops being true the moment a family inserts a lettered
 * step (`step_02a`) or appends a set of named entries after its numbered ones.
 * Getting that wrong is worse than failing to publish: `abiRegisteredChainSteps`
 * only asks whether a NAME is declared, so an off-by-one still finds a real
 * role and publishes one family member's script under another member's role.
 *
 * Each array is the family's `steps` order from `midgard-sdk`, so index N here
 * is the validator at index N there. Adding a step to a chain means adding it
 * in the same position here.
 */
const FAULT_PROOF_STEP_CONTRACT_NAMES: Partial<
  Record<RegisteredLinearFaultProofCategory, readonly string[]>
> = {
  nativeScriptDecoding: [
    "fraudProofNativeScriptDecoding",
    "fraudProofNativeScriptDecodingStep02",
    "fraudProofNativeScriptDecodingStep03OpenSubject",
    "fraudProofNativeScriptDecodingStep03BindDescriptor",
    "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
    "fraudProofNativeScriptDecodingStep04",
  ],
  fieldPreimageLengthMismatch: [
    "fraudProofFieldPreimageLengthMismatch",
    "fraudProofFieldPreimageLengthMismatchStep02Accepted",
    "fraudProofFieldPreimageLengthMismatchStep02Forced",
    "fraudProofFieldPreimageLengthMismatchStep03",
  ],
  scriptIntegrityHashMissing: [
    "fraudProofScriptIntegrityHashMissing",
    "fraudProofScriptIntegrityHashMissingStep02",
    "fraudProofScriptIntegrityHashMissingStep03",
    "fraudProofScriptIntegrityHashMissingScriptGrammar",
    "fraudProofScriptIntegrityHashMissingScriptScan",
    "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
    "fraudProofScriptIntegrityHashMissingStep04",
  ],
  missingRedeemer: [
    "fraudProofMissingRedeemer",
    "fraudProofMissingRedeemerStep02",
    "fraudProofMissingRedeemerStep02a",
    "fraudProofMissingRedeemerStep02b",
    "fraudProofMissingRedeemerStep03",
    "fraudProofMissingRedeemerStep04",
    "fraudProofMissingRedeemerStep05",
  ],
  unusedRedeemer: [
    "fraudProofUnusedRedeemer",
    "fraudProofUnusedRedeemerStep02",
    "fraudProofUnusedRedeemerStep02a",
    "fraudProofUnusedRedeemerStep02b",
    "fraudProofUnusedRedeemerStep02c",
    "fraudProofUnusedRedeemerStep03",
    "fraudProofUnusedRedeemerStep04",
    "fraudProofUnusedRedeemerStep05",
    "fraudProofUnusedRedeemerStep06",
  ],
  executionNativeScriptInvalid: [
    "fraudProofExecutionNativeScriptInvalid",
    "fraudProofExecutionNativeScriptInvalidStep02",
    "fraudProofExecutionNativeScriptInvalidStep03",
    "fraudProofExecutionNativeScriptInvalidStep04",
    "fraudProofExecutionNativeScriptInvalidStep05",
    "fraudProofExecutionNativeScriptInvalidStep06",
    "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
    "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
    "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
    "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
    "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
    "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
    "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
  ],
};

/**
 * Manifest contract name of step `stepIndex` of a registered or legacy
 * fault-proof chain: `fraudProof<Chain>` for the first step and
 * `fraudProof<Chain>StepNN` after it, unless the chain is listed above.
 */
export const faultProofStepContractName = (
  chain: RegisteredLinearFaultProofCategory | LegacyFaultProofFamily,
  stepIndex: number,
): string => {
  const declared =
    FAULT_PROOF_STEP_CONTRACT_NAMES[
      chain as RegisteredLinearFaultProofCategory
    ];
  if (declared !== undefined) {
    const name = declared[stepIndex];
    if (name === undefined) {
      throw new Error(
        `${chain} exposes an unexpected step index ${stepIndex.toString()}`,
      );
    }
    return name;
  }
  return `fraudProof${upperFirst(chain)}${
    stepIndex === 0 ? "" : `Step${(stepIndex + 1).toString().padStart(2, "0")}`
  }`;
};

/**
 * Manifest contract names of every step of a registered or legacy chain, in
 * `steps` order. A chain listed in `FAULT_PROOF_STEP_CONTRACT_NAMES` has
 * exactly its declared names; any other chain has its step-01 name followed by
 * consecutive `…StepNN` names for as long as `isRecorded` admits them.
 */
export const recordedFaultProofStepContractNames = (
  chain: RegisteredLinearFaultProofCategory | LegacyFaultProofFamily,
  isRecorded: (contract: string) => boolean,
): readonly string[] => {
  const declared =
    FAULT_PROOF_STEP_CONTRACT_NAMES[
      chain as RegisteredLinearFaultProofCategory
    ];
  if (declared !== undefined) {
    return declared;
  }
  const names = [faultProofStepContractName(chain, 0)];
  for (
    let name = faultProofStepContractName(chain, names.length);
    isRecorded(name);
    name = faultProofStepContractName(chain, names.length)
  ) {
    names.push(name);
  }
  return names;
};

// ---------------------------------------------------------------------------
// Validation-trace dispute naming
// ---------------------------------------------------------------------------

export type ValidationTraceSemanticKey =
  keyof typeof SDK.VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics;

export type ValidationTraceYieldKey =
  keyof SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"]["yields"];

/** Titled semantic resolvers; `semanticResolvers[i]` is the one for key `i`. */
export const VALIDATION_TRACE_SEMANTIC_KEYS = Object.keys(
  SDK.VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
) as readonly ValidationTraceSemanticKey[];

export const VALIDATION_TRACE_SEMANTIC_CONTRACT_NAME_OVERRIDES: Partial<
  Record<ValidationTraceSemanticKey, string>
> = {
  phaseAScriptPreconditions:
    "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
};

export const validationTraceSemanticContractName = (
  key: ValidationTraceSemanticKey,
): string =>
  VALIDATION_TRACE_SEMANTIC_CONTRACT_NAME_OVERRIDES[key] ??
  `validationTraceDispute${upperFirst(key)}Semantic`;
