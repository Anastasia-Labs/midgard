import {
  type AuthenticatedValidator,
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

export const scaffoldChain = <const Length extends number>(
  firstStep: SdkSpendingValidator,
  length: Length,
) => ({
  firstStep,
  steps: repeatValidator(firstStep, length),
});

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
