import * as SDK from "@al-ft/midgard-sdk";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthTokenTarget,
} from "@al-ft/midgard-sdk";
import { type Script } from "@lucid-evolution/lucid";

import {
  type DeployableScriptCommandName,
  type DeployableScriptPurpose,
  upperFirst,
  VALIDATION_TRACE_SEMANTIC_KEYS,
  type ValidationTraceSemanticKey,
  type ValidationTraceYieldKey,
} from "./deployable-scripts.fault-proof-step-contract-names.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "./deployment-manifest.js";

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
export const VALIDATION_TRACE_SCRIPT_SOURCES_SEMANTIC_PREFIXES = [
  "resolveInputs",
  "scriptSources",
] as const;

export const VALIDATION_TRACE_PHASE_A_SEMANTIC_PREFIXES = [
  "phaseANativeScripts",
  "phaseAScriptPreconditions",
] as const;

export const semanticKeyHasPrefix = (
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
  ) ||
  semanticKeyHasPrefix(key, VALIDATION_TRACE_PHASE_A_SEMANTIC_PREFIXES) ||
  key === "canonicalDecodeEmpty" ||
  key === "canonicalDecodeItem";

/** Titled script-sources and ledger-output yields, in title order. */
export const VALIDATION_TRACE_SCRIPT_SOURCES_YIELD_KEYS = (
  Object.keys(
    SDK.VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.yields,
  ) as ValidationTraceYieldKey[]
).filter(
  (key) => key.startsWith("scriptSources") || key.startsWith("ledgerOutput"),
);

/** CEK material and selection yields, in manifest order (not title order). */
export const VALIDATION_TRACE_CEK_MATERIAL_YIELD_KEYS = [
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
export const CEK_CORE_STAGE_ORDER = [
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

export const TRANSITION_TRACE_YIELD_CONTRACT_NAMES = [
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

export const MIN_ADA_YIELD_CONTRACT_NAMES = [
  ["tx", "fraudProofMinAdaStep02TxWithdraw"],
  ["utxo", "fraudProofMinAdaStep02UtxoWithdraw"],
] as const satisfies readonly (readonly [
  keyof SDK.FaultProofContractChains["minAda"]["yields"],
  string,
])[];

// ---------------------------------------------------------------------------
// Roles
// ---------------------------------------------------------------------------

export const REFERENCE_SCRIPT_ROLE_BY_CONTRACT: ReadonlyMap<
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

export type DeployableScriptSpec = {
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

export type EntryOptions = {
  readonly commands?: readonly DeployableScriptCommandName[];
  /** `false` for scripts the manifest records but never publishes. */
  readonly referenceScript?: boolean;
  readonly publishWhen?: (contracts: SDK.MidgardValidators) => boolean;
  readonly chain?: string;
};

export type Section = (
  contracts: SDK.MidgardValidators,
) => readonly DeployableScriptSpec[];

export type StaticEntry = (
  contracts: SDK.MidgardValidators,
) => DeployableScriptSpec;

export const spending = (validator: SDK.SpendingValidator): ScriptWithHash => ({
  script: validator.spendingScript,
  scriptHash: validator.spendingScriptHash,
});

const minting = (validator: SDK.MintingValidator): ScriptWithHash => ({
  script: validator.mintingScript,
  scriptHash: validator.policyId,
});

export const withdrawing = (
  validator: SDK.WithdrawalValidator,
): ScriptWithHash => ({
  script: validator.withdrawalScript,
  scriptHash: validator.withdrawalScriptHash,
});

export const RESOLVE_BY_PURPOSE = {
  spend: spending,
  mint: minting,
  withdraw: withdrawing,
} as const;

type ValidatorForPurpose = {
  readonly spend: SDK.SpendingValidator;
  readonly mint: SDK.MintingValidator;
  readonly withdraw: SDK.WithdrawalValidator;
};

export const entrySpec = (
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

export const spend = (
  contract: string,
  select: (contracts: SDK.MidgardValidators) => SDK.SpendingValidator,
  options?: EntryOptions,
) => entry("spend", contract, select, options);

export const mint = (
  contract: string,
  select: (contracts: SDK.MidgardValidators) => SDK.MintingValidator,
  options?: EntryOptions,
) => entry("mint", contract, select, options);

export const withdraw = (
  contract: string,
  select: (contracts: SDK.MidgardValidators) => SDK.WithdrawalValidator,
  options?: EntryOptions,
) => entry("withdraw", contract, select, options);
