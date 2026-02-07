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
import { NodeConfig } from "@/services/config.js";
import * as SDK from "@al-ft/midgard-sdk";
import { NoSuchElementException } from "effect/Cause";

const NETWORK: Network = "Preprod";

const MIDGARD_CATEGORY: string = "midgard";
const FRAUD_PROOFS_CATEGORY: string = "fraud_proofs";

const makeValidatorTitle = (
  category: string,
  baseName: string,
  type: "spend" | "mint" | "withdraw",
) => `${category}.${baseName}_${type}.else`;

const getValidatorScript = (title: string) =>
  pipe(
    Effect.fromNullable(
      scripts.default.validators.find((v) => v.title === title),
    ),
    Effect.andThen((script) => script.compiledCode),
  );

const makeSpendingValidator = (
  category: string,
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
  });

const makeMintingValidator = (
  category: string,
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
  });

const makeWithdrawalValidator = (
  category: string,
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
  });

const makeAuthenticatedValidator = (
  baseName: string,
  network: Network,
): Effect.Effect<SDK.AuthenticatedValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const spendingValidator = yield* makeSpendingValidator(
      MIDGARD_CATEGORY,
      baseName,
      network,
    );
    const mintingValidator = yield* makeMintingValidator(
      MIDGARD_CATEGORY,
      baseName,
    );

    return {
      ...spendingValidator,
      ...mintingValidator,
    };
  }).pipe(Effect.orDie);

const makeAlwaysSucceedsService: Effect.Effect<SDK.MidgardValidators> =
  Effect.gen(function* () {
    // Helpers
    const mkAuthVal = (contract: string) =>
      makeAuthenticatedValidator(contract, NETWORK);
    const mkFP = (fp: string) =>
      makeSpendingValidator(FRAUD_PROOFS_CATEGORY, fp, NETWORK);

    // Midgard Contracts
    const hubOracle = yield* makeMintingValidator(
      MIDGARD_CATEGORY,
      "hub_oracle",
    );
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
      MIDGARD_CATEGORY,
      "reserve",
      NETWORK,
    );
    const reserveWithdawalValidator = yield* makeWithdrawalValidator(
      MIDGARD_CATEGORY,
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
    dependencies: [NodeConfig.layer],
  },
) {}
