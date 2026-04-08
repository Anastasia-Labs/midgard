import { Effect, pipe } from "effect";
import * as scripts from "../../blueprints/always-succeeds/plutus.json" with { type: "json" };
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
import * as SDK from "@al-ft/midgard-sdk";
import { NoSuchElementException } from "effect/Cause";

/**
 * Always-succeeds contract loader used for local development and testing.
 *
 * This service resolves validators from the blueprint bundle and converts them
 * into the SDK shapes expected by the rest of the node.
 */
const NETWORK: Network = "Preprod";

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
const makeSpendingValidator = (
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
const makeMintingValidator = (
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
const makeWithdrawalValidator = (
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
const makeAuthenticatedValidator = (
  baseName: string,
  network: Network,
): Effect.Effect<SDK.AuthenticatedValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const spendingValidator = yield* makeSpendingValidator(
      "midgard",
      baseName,
      network,
    );
    const mintingValidator = yield* makeMintingValidator("midgard", baseName);

    return {
      ...spendingValidator,
      ...mintingValidator,
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

/**
 * Resolves the full always-succeeds validator set used by test environments.
 */
const makeAlwaysSucceedsService: Effect.Effect<SDK.MidgardValidators> =
  Effect.gen(function* () {
    // Helpers
    /**
     * Builds the authenticated always-succeeds validator reference.
     */
    const mkAuthVal = (contract: string) =>
      makeAuthenticatedValidator(contract, NETWORK);
    /**
     * Builds the always-succeeds fraud-proof validator reference.
     */
    const mkFP = (fp: string) =>
      makeSpendingValidator("fraud_proofs", fp, NETWORK);

    // Midgard Contracts
    const hubOracle = yield* makeMintingValidator("midgard", "hub_oracle");
    const scheduler = yield* mkAuthVal("scheduler");
    const stateQueue = yield* mkAuthVal("state_queue");
    const registeredOperators = yield* mkAuthVal("registered_operators");
    const activeOperators = yield* mkAuthVal("active_operators");
    const retiredOperators = yield* mkAuthVal("retired_operators");
    const escapeHatch = yield* mkAuthVal("escape_hatch");
    const fraudProofCatalogue = yield* mkAuthVal("fraud_proof_catalogue");
    const fraudProof = yield* mkAuthVal("fraud_proof");
    const deposit = yield* mkAuthVal("deposit");
    const payout = yield* mkAuthVal("payout");
    const withdrawal = yield* mkAuthVal("withdrawal");
    const txOrder = yield* mkAuthVal("tx_order");
    const settlement = yield* mkAuthVal("settlement");

    // Reserve
    // TODO: Reserve is not entirely defined in specs. This might change.
    const reserveSpendingValidator = yield* makeSpendingValidator(
      "midgard",
      "reserve",
      NETWORK,
    );
    const reserveWithdawalValidator = yield* makeWithdrawalValidator(
      "midgard",
      "reserve",
    );
    const reserve = {
      ...reserveSpendingValidator,
      ...reserveWithdawalValidator,
    };

    // Fraud Proofs
    const doubleSpend = yield* mkFP("double_spend");
    const nonExistentInput = yield* mkFP("non_existent_input");
    const nonExistentInputNoIndex = yield* mkFP("non_existent_input_no_index");
    const invalidRange = yield* mkFP("invalid_range");

    const fraudProofs: SDK.FraudProofs = {
      doubleSpend,
      nonExistentInput,
      nonExistentInputNoIndex,
      invalidRange,
    };

    return {
      hubOracle,
      stateQueue,
      scheduler,
      registeredOperators,
      activeOperators,
      retiredOperators,
      escapeHatch,
      fraudProofCatalogue,
      fraudProof,
      deposit,
      withdrawal,
      txOrder,
      settlement,
      reserve,
      payout,
      fraudProofs,
    };
  }).pipe(Effect.orDie);

export class AlwaysSucceedsContract extends Effect.Service<AlwaysSucceedsContract>()(
  "AlwaysSucceedsContract",
  {
    effect: makeAlwaysSucceedsService,
  },
) {}
