import * as SDK from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  SpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
  WithdrawalValidator,
} from "@lucid-evolution/lucid";
import { Effect, pipe } from "effect";
import { NoSuchElementException } from "effect/Cause";

import * as scripts from "../../blueprints/always-succeeds/plutus.json" with { type: "json" };

/**
 * Always-succeeds contract loader used for local development and testing.
 *
 * This service resolves validators from the blueprint bundle and converts them
 * into the SDK shapes expected by the rest of the node.
 */
export const NETWORK: Network = "Preprod";

type Category = "midgard" | "fraud_proofs";

type Purpose = "spend" | "mint" | "withdraw";

/**
 * Builds the validator title used inside the blueprint JSON bundle.
 */
const makeValidatorTitle = (
  category: Category,
  baseName: string,
  type: Purpose,
) =>
  category === "midgard"
    ? `${category}.${baseName}_${type}.else`
    : `${category}.${baseName}.else`;

/**
 * Looks up a compiled validator script by title inside the blueprint bundle.
 */
const getValidatorScript = (title: string) =>
  pipe(
    Effect.fromNullable(
      scripts.default.validators.find((v) => v.title === title),
    ),
    Effect.andThen((script) => script.compiledCode),
  );

/**
 * Constructs an SDK spending validator from the blueprint bundle.
 */
export const makeSpendingValidator = (
  category: Category,
  baseName: string,
  network: Network,
): Effect.Effect<SDK.SpendingValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const spendingScriptCBOR = yield* getValidatorScript(
      makeValidatorTitle(category, baseName, "spend"),
    );

    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(spendingScriptCBOR),
    };

    const spendingScriptAddress = validatorToAddress(network, spendingScript);
    const spendingScriptHash = validatorToScriptHash(spendingScript);

    return {
      spendingScriptCBOR,
      spendingScript,
      spendingScriptAddress,
      spendingScriptHash,
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

/**
 * Constructs an SDK minting validator from the blueprint bundle.
 */
export const makeMintingValidator = (
  category: Category,
  baseName: string,
): Effect.Effect<SDK.MintingValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const mintingScriptCBOR = yield* getValidatorScript(
      makeValidatorTitle(category, baseName, "mint"),
    );

    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(mintingScriptCBOR),
    };

    const policyId = mintingPolicyToId(mintingScript);

    return {
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

/**
 * Constructs an SDK withdrawal validator from the blueprint bundle.
 */
export const makeWithdrawalValidator = (
  category: Category,
  baseName: string,
): Effect.Effect<SDK.WithdrawalValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const withdrawalScriptCBOR = yield* getValidatorScript(
      makeValidatorTitle(category, baseName, "withdraw"),
    );

    const withdrawalScript: WithdrawalValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(withdrawalScriptCBOR),
    };

    const withdrawalScriptHash = validatorToScriptHash(withdrawalScript);

    return {
      withdrawalScriptCBOR,
      withdrawalScript,
      withdrawalScriptHash,
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

/**
 * Builds the authenticated validator bundle used by most Midgard state
 * machines.
 */
export const makeAuthenticatedValidator = (
  baseName: string,
  network: Network,
): Effect.Effect<SDK.AuthenticatedValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    return {
      ...(yield* makeSpendingValidator("midgard", baseName, network)),
      ...(yield* makeMintingValidator("midgard", baseName)),
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

export const makeMintOnlyAuthenticatedValidator = (
  baseName: string,
  network: Network,
): Effect.Effect<SDK.AuthenticatedValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const mintingValidator = yield* makeMintingValidator("midgard", baseName);
    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: mintingValidator.mintingScript.script,
    };

    return {
      spendingScriptCBOR: mintingValidator.mintingScriptCBOR,
      spendingScript,
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      spendingScriptHash: validatorToScriptHash(spendingScript),
      ...mintingValidator,
    };
  });

export const repeatedFaultProofChain = <Chain extends SDK.FraudProofChain>(
  validator: SDK.SpendingValidator,
  stepCount: number,
): Chain =>
  ({
    firstStep: validator,
    steps: Array.from({ length: stepCount }, () => validator),
  }) as unknown as Chain;
