import { Effect, pipe } from "effect";
import * as scripts from "../../blueprints/always-succeeds/plutus.json" with { type: "json" };
import {
  applyDoubleCborEncoding,
  credentialToAddress,
  MintingPolicy,
  mintingPolicyToId,
  scriptHashToCredential,
  SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { NodeConfig } from "@/services/config.js";
import * as SDK from "@al-ft/midgard-sdk";

const makeValidatorTitle = (baseName: string, type: "spend" | "mint") =>
  `always_succeeds.${baseName}_${type}.else`;

const getValidatorScript = (title: string) =>
  pipe(
    Effect.fromNullable(
      scripts.default.validators.find((v) => v.title === title),
    ),
    Effect.andThen((script) => script.compiledCode),
  );

const makeSpendingValidator = (
  baseName: string,
): Effect.Effect<SDK.SpendingValidatorInfo, never, NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;

    const spendingCBOR = yield* getValidatorScript(
      makeValidatorTitle(baseName, "spend"),
    );

    const spendScript: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(spendingCBOR),
    };

    const spendScriptAddress = validatorToAddress(
      nodeConfig.NETWORK,
      spendScript,
    );

    return {
      spendingCBOR,
      spendScript,
      spendScriptAddress,
    };
  }).pipe(Effect.orDie);

const makeMintingValidator = (
  baseName: string,
): Effect.Effect<SDK.MintingValidatorInfo, never, NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;

    const mintingCBOR = yield* getValidatorScript(
      makeValidatorTitle(baseName, "mint"),
    );

    const mintScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(mintingCBOR),
    };

    const policyId = mintingPolicyToId(mintScript);

    return {
      mintingCBOR,
      mintScript,
      policyId,
    };
  }).pipe(Effect.orDie);

const makeAuthenticatedValidator = (
  baseName: string,
): Effect.Effect<SDK.AuthenticatedValidator, never, NodeConfig> =>
  Effect.gen(function* () {
    const spendingValidator = yield* makeSpendingValidator(baseName);
    const mintingValidator = yield* makeMintingValidator(baseName);

    return {
      ...spendingValidator,
      ...mintingValidator,
    };
  }).pipe(Effect.orDie);

const makeAlwaysSucceedsService: Effect.Effect<
  SDK.MidgardValidators,
  never,
  NodeConfig
> = Effect.gen(function* () {
  const hubOracleMintValidator = yield* makeMintingValidator("hub_oracle");
  const schedulerAuthValidator = yield* makeAuthenticatedValidator("scheduler");
  const stateQueueAuthValidator =
    yield* makeAuthenticatedValidator("state_queue");
  const registeredOperatorsAuthValidator = yield* makeAuthenticatedValidator(
    "registered_operators",
  );
  const activeOperatorsAuthValidator =
    yield* makeAuthenticatedValidator("active_operators");
  const retiredOperatorsAuthValidator =
    yield* makeAuthenticatedValidator("retired_operators");
  const escapeHatchAuthValidator =
    yield* makeAuthenticatedValidator("escape_hatch");
  const fraudProofCatalogueAuthValidator = yield* makeAuthenticatedValidator(
    "fraud_proof_catalogue",
  );
  const fraudProofAuthValidator =
    yield* makeAuthenticatedValidator("fraud_proof");
  const depositAuthValidator = yield* makeAuthenticatedValidator("deposit");
  const reserveAuthValidator = yield* makeAuthenticatedValidator("reserve");
  const payoutAuthValidator = yield* makeAuthenticatedValidator("payout");
  const withdrawalAuthValidator =
    yield* makeAuthenticatedValidator("withdrawal");
  const txOrderAuthValidator = yield* makeAuthenticatedValidator("tx_order");
  const settlementAuthValidator =
    yield* makeAuthenticatedValidator("settlement");

  return {
    hubOracleMintValidator,
    schedulerAuthValidator,
    stateQueueAuthValidator,
    registeredOperatorsAuthValidator,
    activeOperatorsAuthValidator,
    retiredOperatorsAuthValidator,
    escapeHatchAuthValidator,
    fraudProofCatalogueAuthValidator,
    fraudProofAuthValidator,
    depositAuthValidator,
    reserveAuthValidator,
    payoutAuthValidator,
    withdrawalAuthValidator,
    txOrderAuthValidator,
    settlementAuthValidator,
  };
}).pipe(Effect.orDie);

export class AlwaysSucceedsContract extends Effect.Service<AlwaysSucceedsContract>()(
  "AlwaysSucceedsContract",
  {
    effect: makeAlwaysSucceedsService,
    dependencies: [NodeConfig.layer],
  },
) {}
