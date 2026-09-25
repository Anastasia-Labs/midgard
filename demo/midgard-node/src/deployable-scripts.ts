/**
 * The one ordered catalogue of every script a Midgard deployment carries.
 *
 * Each entry states the manifest contract name, the purpose it is compiled for
 * (spend, mint or withdraw), how its validator is selected from the resolved
 * SDK bundle (or the blueprint), whether it is published as a reference script,
 * and which reference-script commands need it. Reference-script roles are never
 * restated here: they come from the manifest's role map, so an entry is
 * published under exactly the role the manifest declares for its contract.
 *
 * Two consumers read the catalogue, in two historically different orders that
 * are both observable and therefore both pinned:
 *
 * - the manifest order (`manifestDeployableScripts`) is the key order of
 *   `ContractDeploymentInfo.contracts`, so it is baked into the bytes of
 *   `contract-deployment-info.json` and into its digest. It is the order the
 *   sections are declared in below.
 * - the publication order (`publishedDeployableScripts`) drives reference-script
 *   publication batching. It is `PUBLICATION_ORDER`, an explicit permutation of
 *   the same sections.
 *
 * Fault-proof families and the validation-trace dispute's stage sets are
 * generated from the SDK's own arrays and reference tables instead of being
 * indexed by hand.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthTokenTarget,
} from "@al-ft/midgard-sdk";
import { type Script, validatorToScriptHash } from "@lucid-evolution/lucid";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "./deployment-manifest.js";
import { loadPhasMembershipWithdrawalScript } from "./phas-membership.js";

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
type DeployableScriptCommandName = Exclude<
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

const upperFirst = (value: string): string =>
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

const VALIDATION_TRACE_SEMANTIC_CONTRACT_NAME_OVERRIDES: Partial<
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

/**
 * The SDK appends the shared redeemer-item envelope after the titled
 * semantic resolvers; it is published as the script-sources redeemer
 * normalization semantic.
 */
export const VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_INDEX =
  VALIDATION_TRACE_SEMANTIC_KEYS.length;

export const VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_CONTRACT =
  "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic";

/**
 * Key prefixes of the titled semantic resolvers the deployment records and
 * publishes, one list per catalogue section. The remaining titled resolvers
 * are never recorded in the manifest.
 */
const VALIDATION_TRACE_SCRIPT_SOURCES_SEMANTIC_PREFIXES = [
  "resolveInputs",
  "scriptSources",
] as const;
const VALIDATION_TRACE_PHASE_A_SEMANTIC_PREFIXES = [
  "phaseANativeScripts",
  "phaseAScriptPreconditions",
] as const;

const semanticKeyHasPrefix = (
  key: ValidationTraceSemanticKey,
  prefixes: readonly string[],
): boolean => prefixes.some((prefix) => key.startsWith(prefix));

/** Whether the manifest records the titled semantic resolver for `key`. */
export const isRecordedValidationTraceSemantic = (
  key: ValidationTraceSemanticKey,
): boolean =>
  semanticKeyHasPrefix(
    key,
    VALIDATION_TRACE_SCRIPT_SOURCES_SEMANTIC_PREFIXES,
  ) || semanticKeyHasPrefix(key, VALIDATION_TRACE_PHASE_A_SEMANTIC_PREFIXES);

/** Titled script-sources and ledger-output yields, in title order. */
const VALIDATION_TRACE_SCRIPT_SOURCES_YIELD_KEYS = (
  Object.keys(
    SDK.VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.yields,
  ) as ValidationTraceYieldKey[]
).filter(
  (key) => key.startsWith("scriptSources") || key.startsWith("ledgerOutput"),
);

/** CEK material and selection yields, in manifest order (not title order). */
const VALIDATION_TRACE_CEK_MATERIAL_YIELD_KEYS = [
  "cekMaterialProgramTask",
  "cekMaterialDataTask",
  "cekSelectionAuthenticate",
  "cekSelectionSuccessor",
  "cekSelectionMaterialProgram",
  "cekSelectionMaterialData",
  "phaseANativeItemNative",
  "phaseANativeItemForeign",
  "valueAndMintAssetFold",
] as const satisfies readonly ValidationTraceYieldKey[];

export const validationTraceYieldContractName = (
  key: ValidationTraceYieldKey,
): string => `validationTraceDispute${upperFirst(key)}Withdraw`;

/** Every validation-trace yield the manifest records. */
export const VALIDATION_TRACE_RECORDED_YIELD_KEYS: readonly ValidationTraceYieldKey[] =
  [
    ...VALIDATION_TRACE_SCRIPT_SOURCES_YIELD_KEYS,
    ...VALIDATION_TRACE_CEK_MATERIAL_YIELD_KEYS,
  ];

/**
 * CEK core stages in manifest order. Names come from the SDK's reference
 * table, whose key order differs (`semanticResult`/`semanticRoots` and
 * `blsRoots`/`blsBudget` are swapped there).
 */
const CEK_CORE_STAGE_ORDER = [
  "settle",
  "compute",
  "machine",
  "mapConversion",
  "directScalar",
  "directStructured",
  "directScalarBudget",
  "directStructuredBudget",
  "directScalarRoots",
  "directStructuredRoots",
  "semanticPair",
  "semanticListConstruct",
  "semanticListSelect",
  "semanticChoose",
  "semanticDataConstruct",
  "semanticDataScalar",
  "semanticDataMisc",
  "semanticBudget",
  "semanticRoots",
  "semanticResult",
  "mapStartNodes",
  "mapStartBudget",
  "mapStartRoots",
  "semanticFailureMaterial",
  "semanticFailureRoots",
  "blsFinal",
  "blsBudget",
  "blsRoots",
  "failureBudget",
  "failureKnown",
  "typeFailureKinds",
  "typeFailureRoots",
] as const satisfies readonly (keyof typeof SDK.CEK_CORE_STAGE_REFERENCES)[];

const TRANSITION_TRACE_YIELD_CONTRACT_NAMES = [
  ["l2Open", "fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw"],
  [
    "l2Summaries",
    "fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw",
  ],
  ["l2Replay", "fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw"],
  [
    "claimStructure",
    "fraudProofTransitionTraceAcceptedTransactionClaimStructureWithdraw",
  ],
  [
    "claimSource",
    "fraudProofTransitionTraceAcceptedTransactionClaimSourceWithdraw",
  ],
  [
    "claimEndpoints",
    "fraudProofTransitionTraceAcceptedTransactionClaimEndpointsWithdraw",
  ],
  ["depositProjection", "fraudProofTransitionTraceDepositProjectionWithdraw"],
  ["l1Event", "fraudProofTransitionTraceL1EventWithdraw"],
  ["forcedTiming", "fraudProofTransitionTraceForcedTimingWithdraw"],
  ["depositSummaries", "fraudProofTransitionTraceDepositSummariesWithdraw"],
  [
    "l2Assembly",
    "fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw",
  ],
  ["l2Scan", "fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw"],
  ["l2Value", "fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw"],
  ["depositAssembly", "fraudProofTransitionTraceDepositAssemblyWithdraw"],
  ["depositScan", "fraudProofTransitionTraceDepositScanWithdraw"],
  ["depositValue", "fraudProofTransitionTraceDepositValueWithdraw"],
  ["depositReplay", "fraudProofTransitionTraceDepositReplayWithdraw"],
] as const satisfies readonly (readonly [
  keyof SDK.FaultProofContractChains["transitionTrace"]["yields"],
  string,
])[];

const MIN_ADA_YIELD_CONTRACT_NAMES = [
  ["tx", "fraudProofMinAdaStep02TxWithdraw"],
  ["utxo", "fraudProofMinAdaStep02UtxoWithdraw"],
] as const satisfies readonly (readonly [
  keyof SDK.FaultProofContractChains["minAda"]["yields"],
  string,
])[];

// ---------------------------------------------------------------------------
// Roles
// ---------------------------------------------------------------------------

const REFERENCE_SCRIPT_ROLE_BY_CONTRACT: ReadonlyMap<
  string,
  ReferenceScriptAuthTokenTarget
> = new Map(
  Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
    ([role, contract]) => {
      if (!(role in REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)) {
        throw new Error(
          `Deployment manifest reference-script role is not registered by the SDK: ${role}`,
        );
      }
      return [contract, role as ReferenceScriptAuthTokenTarget] as const;
    },
  ),
);

/** The manifest's reference-script role for a contract, if it has one. */
export const referenceScriptRoleForContract = (
  contract: string,
): ReferenceScriptAuthTokenTarget | undefined =>
  REFERENCE_SCRIPT_ROLE_BY_CONTRACT.get(contract);

// ---------------------------------------------------------------------------
// Entry construction
// ---------------------------------------------------------------------------

type ScriptWithHash = { readonly script: Script; readonly scriptHash: string };

type DeployableScriptSpec = {
  readonly contract: string;
  readonly purpose: DeployableScriptPurpose;
  readonly referenceScript: boolean;
  readonly commands: readonly DeployableScriptCommandName[];
  /** Fault-proof chain the entry is a step of, for per-chain interleaving. */
  readonly chain?: string;
  readonly publishable: () => boolean;
  /** Deferred so an unpublishable entry's validator is never dereferenced. */
  readonly resolve: () => ScriptWithHash;
};

type EntryOptions = {
  readonly commands?: readonly DeployableScriptCommandName[];
  /** `false` for scripts the manifest records but never publishes. */
  readonly referenceScript?: boolean;
  readonly publishWhen?: (contracts: SDK.MidgardValidators) => boolean;
  readonly chain?: string;
};

type Section = (
  contracts: SDK.MidgardValidators,
) => readonly DeployableScriptSpec[];

type StaticEntry = (contracts: SDK.MidgardValidators) => DeployableScriptSpec;

const spending = (validator: SDK.SpendingValidator): ScriptWithHash => ({
  script: validator.spendingScript,
  scriptHash: validator.spendingScriptHash,
});

const minting = (validator: SDK.MintingValidator): ScriptWithHash => ({
  script: validator.mintingScript,
  scriptHash: validator.policyId,
});

const withdrawing = (validator: SDK.WithdrawalValidator): ScriptWithHash => ({
  script: validator.withdrawalScript,
  scriptHash: validator.withdrawalScriptHash,
});

const RESOLVE_BY_PURPOSE = {
  spend: spending,
  mint: minting,
  withdraw: withdrawing,
} as const;

type ValidatorForPurpose = {
  readonly spend: SDK.SpendingValidator;
  readonly mint: SDK.MintingValidator;
  readonly withdraw: SDK.WithdrawalValidator;
};

const entrySpec = (
  contracts: SDK.MidgardValidators,
  contract: string,
  purpose: DeployableScriptPurpose,
  resolve: () => ScriptWithHash,
  options: EntryOptions = {},
): DeployableScriptSpec => ({
  contract,
  purpose,
  referenceScript: options.referenceScript ?? true,
  commands: options.commands ?? [],
  ...(options.chain === undefined ? {} : { chain: options.chain }),
  publishable: () => options.publishWhen?.(contracts) ?? true,
  resolve,
});

/** A catalogue entry selected from the SDK bundle. */
const entry =
  <Purpose extends DeployableScriptPurpose>(
    purpose: Purpose,
    contract: string,
    select: (contracts: SDK.MidgardValidators) => ValidatorForPurpose[Purpose],
    options?: EntryOptions,
  ): StaticEntry =>
  (contracts) =>
    entrySpec(
      contracts,
      contract,
      purpose,
      () =>
        (
          RESOLVE_BY_PURPOSE[purpose] as (
            validator: ValidatorForPurpose[Purpose],
          ) => ScriptWithHash
        )(select(contracts)),
      options,
    );

const spend = (
  contract: string,
  select: (contracts: SDK.MidgardValidators) => SDK.SpendingValidator,
  options?: EntryOptions,
) => entry("spend", contract, select, options);

const mint = (
  contract: string,
  select: (contracts: SDK.MidgardValidators) => SDK.MintingValidator,
  options?: EntryOptions,
) => entry("mint", contract, select, options);

const withdraw = (
  contract: string,
  select: (contracts: SDK.MidgardValidators) => SDK.WithdrawalValidator,
  options?: EntryOptions,
) => entry("withdraw", contract, select, options);

const entries =
  (...staticEntries: readonly StaticEntry[]): Section =>
  (contracts) =>
    staticEntries.map((staticEntry) => staticEntry(contracts));

/** A generated family member whose validator is already in hand. */
const spendStep = (
  contracts: SDK.MidgardValidators,
  contract: string,
  validator: SDK.SpendingValidator,
  options?: EntryOptions,
): DeployableScriptSpec =>
  entrySpec(contracts, contract, "spend", () => spending(validator), options);

const withdrawStep = (
  contracts: SDK.MidgardValidators,
  contract: string,
  validator: SDK.WithdrawalValidator,
): DeployableScriptSpec =>
  entrySpec(contracts, contract, "withdraw", () => withdrawing(validator));

const categoriesFrom = (
  first: RegisteredLinearFaultProofCategory,
  last: RegisteredLinearFaultProofCategory,
): readonly RegisteredLinearFaultProofCategory[] =>
  REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.slice(
    REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.indexOf(first),
    REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.indexOf(last) + 1,
  );

const registeredChainSteps =
  (categories: readonly RegisteredLinearFaultProofCategory[]): Section =>
  (contracts) =>
    categories.flatMap((category) =>
      contracts.fraudProofContracts[category].steps.map(
        (validator, stepIndex) =>
          spendStep(
            contracts,
            faultProofStepContractName(category, stepIndex),
            validator,
          ),
      ),
    );

const legacyChainSteps =
  (which: "first" | "later"): Section =>
  (contracts) =>
    LEGACY_FAULT_PROOF_FAMILIES.flatMap((family) =>
      contracts.fraudProofContracts[family].steps.flatMap(
        (validator, stepIndex) =>
          (stepIndex === 0) === (which === "first")
            ? [
                spendStep(
                  contracts,
                  faultProofStepContractName(family, stepIndex),
                  validator,
                  { chain: family },
                ),
              ]
            : [],
      ),
    );

const semanticResolvers =
  (prefixes: readonly string[]): Section =>
  (contracts) =>
    VALIDATION_TRACE_SEMANTIC_KEYS.flatMap((key, index) =>
      semanticKeyHasPrefix(key, prefixes)
        ? [
            spendStep(
              contracts,
              validationTraceSemanticContractName(key),
              contracts.fraudProofContracts.validationTraceDispute
                .semanticResolvers[index],
            ),
          ]
        : [],
    );

const validationTraceYields =
  (keys: readonly ValidationTraceYieldKey[]): Section =>
  (contracts) =>
    keys.map((key) =>
      withdrawStep(
        contracts,
        validationTraceYieldContractName(key),
        contracts.fraudProofContracts.validationTraceDispute.yields[key],
      ),
    );

const vtd = (contracts: SDK.MidgardValidators) =>
  contracts.fraudProofContracts.validationTraceDispute;

const vtdControl = (contracts: SDK.MidgardValidators) =>
  contracts.fraudProofs.validationTraceDispute;

const vtdControlPresent = (contracts: SDK.MidgardValidators) =>
  contracts.fraudProofs.validationTraceDispute !== undefined;

const NOT_PUBLISHED = { referenceScript: false } as const;

// ---------------------------------------------------------------------------
// The catalogue, in manifest order
// ---------------------------------------------------------------------------

const DEPLOYABLE_SCRIPT_CATALOGUE = {
  referenceScriptAuth: entries(
    mint("referenceScriptAuthMint", (c) => c.referenceScriptAuth, {
      commands: ["protocol-init", "reference-script-auth"],
    }),
  ),
  hubOracle: entries(
    mint("hubOracleMint", (c) => c.hubOracle, {
      commands: ["protocol-init", "hub-oracle"],
    }),
  ),
  daParamsGovernor: entries(
    spend("daParamsGovernorSpend", (c) => c.daParamsGovernor, {
      commands: ["da"],
    }),
    mint("daParamsGovernorMint", (c) => c.daParamsGovernor, {
      commands: ["protocol-init", "da"],
    }),
  ),
  daAttestation: entries(
    spend("daAttestationSpend", (c) => c.daAttestation, { commands: ["da"] }),
    mint("daAttestationMint", (c) => c.daAttestation, {
      commands: ["protocol-init", "da"],
    }),
  ),
  stateQueue: entries(
    spend("stateQueueSpend", (c) => c.stateQueue, {
      commands: ["state-queue"],
    }),
    mint("stateQueueMint", (c) => c.stateQueue, {
      commands: ["protocol-init", "state-queue"],
    }),
    withdraw("stateQueueCommitWithdraw", (c) => c.stateQueue.yields.commit, {
      commands: ["protocol-init", "state-queue"],
    }),
    withdraw(
      "stateQueueUnattestedTimeoutWithdraw",
      (c) => c.stateQueue.yields.unattestedTimeout,
      { commands: ["protocol-init", "state-queue"] },
    ),
    withdraw(
      "stateQueueUnavailableTimeoutWithdraw",
      (c) => c.stateQueue.yields.unavailableTimeout,
      { commands: ["protocol-init", "state-queue"] },
    ),
    withdraw(
      "stateQueueFraudRemovalWithdraw",
      (c) => c.stateQueue.yields.fraudRemoval,
      { commands: ["protocol-init", "state-queue"] },
    ),
    withdraw("stateQueueMergeWithdraw", (c) => c.stateQueue.yields.merge, {
      commands: ["protocol-init", "state-queue"],
    }),
  ),
  scheduler: entries(
    spend("schedulerSpend", (c) => c.scheduler, { commands: ["scheduler"] }),
    mint("schedulerMint", (c) => c.scheduler, {
      commands: ["protocol-init", "scheduler"],
    }),
  ),
  registeredOperators: entries(
    spend("registeredOperatorsSpend", (c) => c.registeredOperators, {
      commands: ["registered-operators"],
    }),
    mint("registeredOperatorsMint", (c) => c.registeredOperators, {
      commands: ["protocol-init", "registered-operators"],
    }),
  ),
  activeOperators: entries(
    spend("activeOperatorsSpend", (c) => c.activeOperators, {
      commands: ["active-operators"],
    }),
    mint("activeOperatorsMint", (c) => c.activeOperators, {
      commands: ["protocol-init", "active-operators"],
    }),
  ),
  retiredOperators: entries(
    spend("retiredOperatorsSpend", (c) => c.retiredOperators, {
      commands: ["retired-operators"],
    }),
    mint("retiredOperatorsMint", (c) => c.retiredOperators, {
      commands: ["protocol-init", "retired-operators"],
    }),
  ),
  escapeHatch: entries(
    spend("escapeHatchSpend", (c) => c.escapeHatch, NOT_PUBLISHED),
    mint("escapeHatchMint", (c) => c.escapeHatch, NOT_PUBLISHED),
  ),
  fraudProofCatalogue: entries(
    spend(
      "fraudProofCatalogueSpend",
      (c) => c.fraudProofCatalogue,
      NOT_PUBLISHED,
    ),
    mint("fraudProofCatalogueMint", (c) => c.fraudProofCatalogue, {
      commands: ["protocol-init"],
    }),
  ),
  fraudProofToken: entries(
    spend("fraudProofSpend", (c) => c.fraudProof, NOT_PUBLISHED),
    mint("fraudProofMint", (c) => c.fraudProof),
  ),
  depositHistory: entries(
    spend(
      "depositHistoryRetentionSpend",
      (c) => SDK.requireEventHistoryContracts(c).deposit.retention,
      { commands: ["deposit"] },
    ),
    withdraw(
      "depositHistoryRetirementWithdraw",
      (c) => SDK.requireEventHistoryContracts(c).deposit.retirement,
      { commands: ["deposit"] },
    ),
  ),
  withdrawalHistory: entries(
    spend(
      "withdrawalHistoryRetentionSpend",
      (c) => SDK.requireEventHistoryContracts(c).withdrawal.retention,
      { commands: ["withdrawal"] },
    ),
    withdraw(
      "withdrawalHistoryRetirementWithdraw",
      (c) => SDK.requireEventHistoryContracts(c).withdrawal.retirement,
      { commands: ["withdrawal"] },
    ),
  ),
  depositSpend: entries(
    spend("depositSpend", (c) => c.deposit, { commands: ["deposit"] }),
  ),
  depositMint: entries(
    mint("depositMint", (c) => c.deposit, { commands: ["deposit"] }),
  ),
  withdrawalSpend: entries(
    spend("withdrawalSpend", (c) => c.withdrawal, {
      commands: ["withdrawal"],
    }),
  ),
  withdrawalMint: entries(
    mint("withdrawalMint", (c) => c.withdrawal, { commands: ["withdrawal"] }),
  ),
  txOrder: entries(
    spend("txOrderSpend", (c) => c.txOrder, NOT_PUBLISHED),
    mint("txOrderMint", (c) => c.txOrder, NOT_PUBLISHED),
  ),
  fieldPreimageCertificate: entries(
    spend("fieldPreimageCertificateSpend", (c) => c.fieldPreimageCertificate),
    mint("fieldPreimageCertificateMint", (c) => c.fieldPreimageCertificate),
  ),
  cekProgramMaterial: entries(
    spend("cekProgramMaterialSpend", (c) => c.cekProgramMaterial, {
      // Under the always-succeeds contract set the CEK validator is the
      // tx-order spend script. Do not publish that stand-in as a distinct
      // deployed script.
      publishWhen: (c) =>
        c.cekProgramMaterial.spendingScriptHash !==
        c.txOrder.spendingScriptHash,
    }),
  ),
  settlement: entries(
    spend("settlementSpend", (c) => c.settlement, NOT_PUBLISHED),
    mint("settlementMint", (c) => c.settlement, {
      commands: ["settlement"],
    }),
  ),
  payout: entries(
    spend("payoutSpend", (c) => c.payout, { commands: ["payout"] }),
    mint("payoutMint", (c) => c.payout, { commands: ["payout"] }),
  ),
  reserve: entries(
    spend("reserveSpend", (c) => c.reserve, { commands: ["reserve"] }),
    withdraw("reserveWithdraw", (c) => c.reserve, { commands: ["reserve"] }),
  ),
  phasMembership: (contracts) => [
    // Selected from the blueprint: the membership-proof observer is not part
    // of the resolved SDK bundle.
    entrySpec(
      contracts,
      "phasMembershipWithdraw",
      "withdraw",
      () => {
        const script = loadPhasMembershipWithdrawalScript();
        return { script, scriptHash: validatorToScriptHash(script) };
      },
      { commands: ["phas-membership"] },
    ),
  ],
  // Step 01 of each legacy family (`chain.steps[0]`, which IS the family's
  // `fraudProofs` entry). Later steps follow in `legacyFaultProofLaterSteps`;
  // publication interleaves the two per family.
  legacyFaultProofFirstSteps: legacyChainSteps("first"),
  validationTraceDisputeControl: entries(
    spend("validationTraceDispute", vtdControl, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeSource", (c) => vtdControl(c).source, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeGame", (c) => vtdControl(c).game, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeBoundary", (c) => vtdControl(c).boundary, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeTimeout", (c) => vtdControl(c).timeout, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeAward", (c) => vtdControl(c).award, {
      publishWhen: vtdControlPresent,
    }),
  ),
  validationTraceScriptSourcesSemantics: semanticResolvers(
    VALIDATION_TRACE_SCRIPT_SOURCES_SEMANTIC_PREFIXES,
  ),
  validationTraceRedeemerItem: (contracts) => [
    ...SDK.sharedRedeemerItemReferenceScripts(
      vtd(contracts).scriptSourcesStageOneRedeemerStages,
    ).map(({ deploymentEntry, validator }) =>
      spendStep(contracts, deploymentEntry, validator),
    ),
    spendStep(
      contracts,
      "validationTraceDisputeRedeemerItemSettlement",
      vtd(contracts).scriptSourcesStageOneRedeemerStages.settlement,
    ),
  ],
  validationTraceRedeemerNormalizationSemantic: (contracts) => [
    spendStep(
      contracts,
      VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_CONTRACT,
      vtd(contracts).semanticResolvers[
        VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_INDEX
      ],
    ),
  ],
  validationTraceScriptSourcesYields: validationTraceYields(
    VALIDATION_TRACE_SCRIPT_SOURCES_YIELD_KEYS,
  ),
  validationTracePhaseASemantics: semanticResolvers(
    VALIDATION_TRACE_PHASE_A_SEMANTIC_PREFIXES,
  ),
  // The manifest records the linear categories as [A, C, B]; publication
  // walks them in list order [A, B, C].
  registeredChainsA: registeredChainSteps(
    categoriesFrom("fabricatedDeposit", "unusedScriptWitness"),
  ),
  registeredChainsC: registeredChainSteps(
    categoriesFrom("unusedRedeemer", "distinctAssetAccumulationLimit"),
  ),
  registeredChainsB: registeredChainSteps(
    categoriesFrom("missingScriptSource", "missingRedeemer"),
  ),
  // Registered-chain members that are applied like steps but are deliberately
  // not members of `steps`, so the chain walk never reaches them. They still
  // spend from their own script addresses and carry their own roles.
  registeredChainAuxiliaries: (contracts) => {
    const chains = contracts.fraudProofContracts;
    const vnp = chains.valueNotPreserved;
    return [
      spendStep(
        contracts,
        "fraudProofTransitionTrace",
        chains.transitionTrace.route,
      ),
      ...chains.transitionTrace.finals.map((validator, index) => {
        const contract = TRANSITION_TRACE_FINAL_CONTRACT_NAMES[index];
        if (contract === undefined) {
          throw new Error(
            `transitionTrace exposes an unexpected final index ${index.toString()}`,
          );
        }
        return spendStep(contracts, contract, validator);
      }),
      ...(
        [
          [
            "fraudProofValueNotPreservedUnionAcceptedSource",
            vnp.unionAcceptedSource,
          ],
          [
            "fraudProofValueNotPreservedUnionForcedSource",
            vnp.unionForcedSource,
          ],
          ["fraudProofValueNotPreservedUnionEvent", vnp.unionEvent],
          ["fraudProofValueNotPreservedUnionPreState", vnp.unionPreState],
          ["fraudProofValueNotPreservedUnionInputs", vnp.unionInputs],
          ["fraudProofValueNotPreservedUnionInputValue", vnp.unionInputValue],
          ["fraudProofValueNotPreservedUnionAssets", vnp.unionAssets],
          [
            "fraudProofValueNotPreservedUnionFieldGrammar",
            vnp.unionFieldGrammar,
          ],
          ["fraudProofValueNotPreservedUnionOutputs", vnp.unionOutputs],
          ["fraudProofValueNotPreservedUnionOutputScan", vnp.unionOutputScan],
          ["fraudProofValueNotPreservedUnionMint", vnp.unionMint],
          ["fraudProofValueNotPreservedUnionUpdate", vnp.unionUpdate],
          ["fraudProofValueNotPreservedUnionTerminal", vnp.unionTerminal],
          [
            "fraudProofMissingSignatureForcedStep",
            chains.missingSignature.forcedStep,
          ],
          [
            "fraudProofMissingSignatureForcedSigner",
            chains.missingSignature.forcedSigner,
          ],
          [
            "fraudProofMissingSignatureForcedWitness",
            chains.missingSignature.forcedWitness,
          ],
          // The network-id forced (wrongful-rejection) door and the resumable
          // output scan it hands off to.
          ["fraudProofNetworkIdForcedStep", chains.networkId.forcedStep],
          ["fraudProofNetworkIdForcedScan", chains.networkId.forcedScan],
        ] as const
      ).map(([contract, validator]) =>
        spendStep(contracts, contract, validator),
      ),
    ];
  },
  computationThread: entries(
    mint("computationThreadMint", (c) => c.computationThread),
  ),
  chunkedVerify: entries(
    withdraw("chunkedVerifyWithdraw", (c) => c.chunkedVerify),
  ),
  pexcludes: entries(withdraw("pexcludesWithdraw", (c) => c.pexcludes)),
  legacyFaultProofLaterSteps: legacyChainSteps("later"),
  validationTraceCekContext: (contracts) =>
    SDK.cekContextReferenceScripts(
      vtd(contracts).cekContextStages,
      vtd(contracts).cekContextItemStages,
    ).map(({ deploymentEntry, validator }) =>
      spendStep(contracts, deploymentEntry, validator),
    ),
  validationTraceCekCore: (contracts) =>
    CEK_CORE_STAGE_ORDER.map((stage) =>
      spendStep(
        contracts,
        SDK.CEK_CORE_STAGE_REFERENCES[stage].deployment,
        vtd(contracts).cekCoreStages[stage],
      ),
    ),
  validationTraceCekMaterial: (contracts) => [
    spendStep(
      contracts,
      "validationTraceDisputeCekMaterialTraversal",
      vtd(contracts).cekMaterialTraversal,
    ),
    ...validationTraceYields(VALIDATION_TRACE_CEK_MATERIAL_YIELD_KEYS)(
      contracts,
    ),
  ],
  transitionTraceYields: (contracts) =>
    TRANSITION_TRACE_YIELD_CONTRACT_NAMES.map(([key, contract]) =>
      withdrawStep(
        contracts,
        contract,
        contracts.fraudProofContracts.transitionTrace.yields[key],
      ),
    ),
  minAdaYields: (contracts) =>
    MIN_ADA_YIELD_CONTRACT_NAMES.map(([key, contract]) =>
      withdrawStep(
        contracts,
        contract,
        contracts.fraudProofContracts.minAda.yields[key],
      ),
    ),
  correctionLock: entries(
    spend("correctionLockSpend", (c) => c.correctionLock, {
      commands: ["state-queue"],
    }),
  ),
  availabilityChallenge: entries(
    spend("availabilityChallengeSpend", (c) => c.availabilityChallenge),
    mint("availabilityChallengeMint", (c) => c.availabilityChallenge, {
      commands: ["da"],
    }),
    withdraw(
      "availabilityChallengeBondWithdraw",
      (c) => c.availabilityChallenge.yields.bond,
      { commands: ["da"] },
    ),
    withdraw(
      "availabilityChallengeOpenWithdraw",
      (c) => c.availabilityChallenge.yields.open,
    ),
    withdraw(
      "availabilityChallengeSettleWithdraw",
      (c) => c.availabilityChallenge.yields.settle,
    ),
    withdraw(
      "availabilityChallengeCloseWithdraw",
      (c) => c.availabilityChallenge.yields.close,
    ),
    withdraw(
      "availabilityChallengeTimeoutWithdraw",
      (c) => c.availabilityChallenge.yields.timeout,
    ),
  ),
} as const satisfies Record<string, Section>;

export type DeployableScriptSectionId =
  keyof typeof DEPLOYABLE_SCRIPT_CATALOGUE;

export const MANIFEST_ORDER = Object.keys(
  DEPLOYABLE_SCRIPT_CATALOGUE,
) as readonly DeployableScriptSectionId[];

type PublicationStep =
  | DeployableScriptSectionId
  | { readonly interleaveByChain: readonly DeployableScriptSectionId[] };

/** Reference-script publication order over the same sections. */
export const PUBLICATION_ORDER: readonly PublicationStep[] = [
  "referenceScriptAuth",
  "hubOracle",
  "daParamsGovernor",
  "daAttestation",
  "scheduler",
  "stateQueue",
  "registeredOperators",
  "activeOperators",
  "retiredOperators",
  "fraudProofCatalogue",
  "computationThread",
  "fraudProofToken",
  "chunkedVerify",
  "pexcludes",
  "depositHistory",
  "withdrawalHistory",
  "depositMint",
  "depositSpend",
  "withdrawalMint",
  "withdrawalSpend",
  "settlement",
  "phasMembership",
  "reserve",
  "payout",
  "availabilityChallenge",
  "fieldPreimageCertificate",
  "cekProgramMaterial",
  "validationTraceDisputeControl",
  // Each legacy family publishes step 01 followed by its later steps.
  {
    interleaveByChain: [
      "legacyFaultProofFirstSteps",
      "legacyFaultProofLaterSteps",
    ],
  },
  "registeredChainsA",
  "registeredChainsB",
  "registeredChainsC",
  "registeredChainAuxiliaries",
  "validationTraceCekCore",
  "validationTraceScriptSourcesSemantics",
  "validationTraceRedeemerItem",
  "validationTraceRedeemerNormalizationSemantic",
  "validationTraceScriptSourcesYields",
  "validationTracePhaseASemantics",
  "validationTraceCekContext",
  "validationTraceCekMaterial",
  "transitionTraceYields",
  "minAdaYields",
  "correctionLock",
  // Sections with no published script, listed so this order covers every
  // section of the catalogue.
  "escapeHatch",
  "txOrder",
];

const interleaveByChain = (
  specs: readonly DeployableScriptSpec[],
): readonly DeployableScriptSpec[] => {
  const chains: string[] = [];
  const byChain = new Map<string, DeployableScriptSpec[]>();
  for (const spec of specs) {
    const chain = spec.chain ?? "";
    let members = byChain.get(chain);
    if (members === undefined) {
      members = [];
      byChain.set(chain, members);
      chains.push(chain);
    }
    members.push(spec);
  }
  return chains.flatMap((chain) => byChain.get(chain) ?? []);
};

const sectionSpecs = (
  contracts: SDK.MidgardValidators,
  step: PublicationStep,
): readonly DeployableScriptSpec[] =>
  typeof step === "string"
    ? DEPLOYABLE_SCRIPT_CATALOGUE[step](contracts)
    : interleaveByChain(
        step.interleaveByChain.flatMap((id) =>
          DEPLOYABLE_SCRIPT_CATALOGUE[id](contracts),
        ),
      );

const referenceScriptRole = (
  spec: DeployableScriptSpec,
): ReferenceScriptAuthTokenTarget | undefined => {
  const role = REFERENCE_SCRIPT_ROLE_BY_CONTRACT.get(spec.contract);
  if (spec.referenceScript && role === undefined) {
    throw new Error(
      `Contract is missing a canonical reference-script role: ${spec.contract}`,
    );
  }
  if (!spec.referenceScript && role !== undefined) {
    throw new Error(
      `Contract ${spec.contract} is not published but carries reference-script role ${role}`,
    );
  }
  return role;
};

const resolveSpec = (spec: DeployableScriptSpec): DeployableScript => ({
  contract: spec.contract,
  role: referenceScriptRole(spec),
  purpose: spec.purpose,
  commands: spec.commands,
  ...spec.resolve(),
});

/** Every script the deployment manifest records, in manifest order. */
export const manifestDeployableScripts = (
  contracts: SDK.MidgardValidators,
): readonly DeployableScript[] =>
  MANIFEST_ORDER.flatMap((id) =>
    DEPLOYABLE_SCRIPT_CATALOGUE[id](contracts).map(resolveSpec),
  );

/** Every script published as a reference script, in publication order. */
export const publishedDeployableScripts = (
  contracts: SDK.MidgardValidators,
): readonly PublishedDeployableScript[] =>
  PUBLICATION_ORDER.flatMap((step) =>
    sectionSpecs(contracts, step).flatMap((spec) => {
      if (!spec.referenceScript || !spec.publishable()) {
        return [];
      }
      const resolved = resolveSpec(spec);
      return resolved.role === undefined
        ? []
        : [{ ...resolved, role: resolved.role }];
    }),
  );
