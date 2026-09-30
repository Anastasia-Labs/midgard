import * as SDK from "@al-ft/midgard-sdk";

import {
  type DeployableScriptSpec,
  type EntryOptions,
  entrySpec,
  type Section,
  semanticKeyHasPrefix,
  spending,
  type StaticEntry,
  validationTraceYieldContractName,
  withdrawing,
} from "./deployable-scripts.cek-core-stage-order.js";
import {
  faultProofStepContractName,
  LEGACY_FAULT_PROOF_FAMILIES,
  REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES,
  type RegisteredLinearFaultProofCategory,
  VALIDATION_TRACE_SEMANTIC_KEYS,
  validationTraceSemanticContractName,
  type ValidationTraceYieldKey,
} from "./deployable-scripts.fault-proof-step-contract-names.js";

export const entries =
  (...staticEntries: readonly StaticEntry[]): Section =>
  (contracts) =>
    staticEntries.map((staticEntry) => staticEntry(contracts));

/** A generated family member whose validator is already in hand. */
export const spendStep = (
  contracts: SDK.MidgardValidators,
  contract: string,
  validator: SDK.SpendingValidator,
  options?: EntryOptions,
): DeployableScriptSpec =>
  entrySpec(contracts, contract, "spend", () => spending(validator), options);

export const withdrawStep = (
  contracts: SDK.MidgardValidators,
  contract: string,
  validator: SDK.WithdrawalValidator,
): DeployableScriptSpec =>
  entrySpec(contracts, contract, "withdraw", () => withdrawing(validator));

export const categoriesFrom = (
  first: RegisteredLinearFaultProofCategory,
  last: RegisteredLinearFaultProofCategory,
): readonly RegisteredLinearFaultProofCategory[] =>
  REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.slice(
    REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.indexOf(first),
    REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.indexOf(last) + 1,
  );

export const registeredChainSteps =
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

export const legacyChainSteps =
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

export const semanticResolvers =
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

export const validationTraceYields =
  (keys: readonly ValidationTraceYieldKey[]): Section =>
  (contracts) =>
    keys.map((key) =>
      withdrawStep(
        contracts,
        validationTraceYieldContractName(key),
        contracts.fraudProofContracts.validationTraceDispute.yields[key],
      ),
    );

export const vtd = (contracts: SDK.MidgardValidators) =>
  contracts.fraudProofContracts.validationTraceDispute;

export const vtdControl = (contracts: SDK.MidgardValidators) =>
  contracts.fraudProofs.validationTraceDispute;

export const vtdControlPresent = (contracts: SDK.MidgardValidators) =>
  contracts.fraudProofs.validationTraceDispute !== undefined;

export const NOT_PUBLISHED = { referenceScript: false } as const;
