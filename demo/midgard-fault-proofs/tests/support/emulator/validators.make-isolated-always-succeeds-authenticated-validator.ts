import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type AuthenticatedValidator,
  type FaultProofContractChains,
  type FraudProofCatalogueCategoryName,
  makeAuthenticatedValidator as makeSdkAuthenticatedValidator,
  makeMintingPolicy,
  makeSpendingValidator as makeSdkSpendingValidator,
  makeWithdrawalValidator as makeSdkWithdrawalValidator,
  type SpendingValidator as SdkSpendingValidator,
} from "@al-ft/midgard-sdk";
import { applyDoubleCborEncoding } from "@lucid-evolution/lucid";

import { type Blueprint, getCompiledScript, network } from "./blueprints.js";

type RepeatedValidatorTuple<
  Length extends number,
  Result extends readonly SdkSpendingValidator[] = readonly [],
> = Result["length"] extends Length
  ? Result
  : RepeatedValidatorTuple<Length, readonly [...Result, SdkSpendingValidator]>;

export const repeatValidator = <const Length extends number>(
  validator: SdkSpendingValidator,
  length: Length,
): RepeatedValidatorTuple<Length> =>
  Array.from(
    { length },
    () => validator,
  ) as unknown as RepeatedValidatorTuple<Length>;

type ScaffoldChain<Category extends FraudProofCatalogueCategoryName> = Pick<
  FaultProofContractChains[Category],
  "firstStep" | "steps"
>;

/** `firstStep` at every step of the category's deployment chain. */
export const scaffoldChain = <Category extends FraudProofCatalogueCategoryName>(
  firstStep: SdkSpendingValidator,
  category: Category,
): ScaffoldChain<Category> =>
  ({
    firstStep,
    steps: deploymentFamilyStepNames(category).map(() => firstStep),
  }) as unknown as ScaffoldChain<Category>;

/** `scaffoldChain` for each category, keyed and ordered as given. */
export const scaffoldChains = <
  Category extends FraudProofCatalogueCategoryName,
>(
  firstSteps: Readonly<Record<Category, SdkSpendingValidator>>,
): { readonly [Name in Category]: ScaffoldChain<Name> } =>
  Object.fromEntries(
    (Object.entries(firstSteps) as [Category, SdkSpendingValidator][]).map(
      ([category, firstStep]) => [category, scaffoldChain(firstStep, category)],
    ),
  ) as { readonly [Name in Category]: ScaffoldChain<Name> };

export const makeMintingValidator = makeMintingPolicy;

export const makeSpendingValidator = (
  spendingScriptCBOR: string,
): SdkSpendingValidator =>
  makeSdkSpendingValidator(network, spendingScriptCBOR);

export const makeWithdrawalValidator = makeSdkWithdrawalValidator;

export const makeAuthenticatedValidator = (
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator =>
  makeSdkAuthenticatedValidator(network, mintingScriptCBOR, spendingScriptCBOR);

/**
 * A test-only, uniquely hashed `\context -> ()` Plutus V3 program. Unlike the
 * optimized always-succeeds blueprint entries, it does not alias every other
 * scaffold validator's address and policy id. That isolation matters when a
 * topology loader filters UTxOs by both address and policy.
 */
export const makeIsolatedAlwaysSucceedsAuthenticatedValidator =
  (): AuthenticatedValidator => {
    // Flat UPLC 1.1.0 `lambda (con unit ())`, wrapped once as blueprint-style
    // CBOR before Lucid adds the ledger-facing second CBOR layer.
    const isolatedCompiledCode = "450101002499";
    const script = applyDoubleCborEncoding(isolatedCompiledCode);
    return makeAuthenticatedValidator(script, script);
  };

export const alwaysTitle = (
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  category === "midgard"
    ? `${category}.${baseName}_${purpose}.else`
    : `${category}.${baseName}.else`;

export const alwaysScript = (
  blueprint: Blueprint,
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  applyDoubleCborEncoding(
    getCompiledScript(blueprint, alwaysTitle(category, baseName, purpose)),
  );

export const alwaysAuthenticated = (
  blueprint: Blueprint,
  baseName: string,
): AuthenticatedValidator =>
  makeAuthenticatedValidator(
    alwaysScript(blueprint, "midgard", baseName, "mint"),
    alwaysScript(blueprint, "midgard", baseName, "spend"),
  );

// Test-only availability policy, mirroring the SDK state-queue harness. These
// emulator flows never open a challenge; the parameter supplies a distinct id.
export const dummyAvailabilityChallengePolicyId = (
  blueprint: Blueprint,
): string =>
  makeMintingValidator(
    alwaysScript(blueprint, "midgard", "escape_hatch", "mint"),
  ).policyId;
