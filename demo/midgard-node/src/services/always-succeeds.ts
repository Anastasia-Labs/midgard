import { Effect, pipe } from "effect";
import * as scripts from "../../blueprints/always-succeeds/plutus.json" with { type: "json" };
import {
  applyDoubleCborEncoding,
  MintingPolicy,
  mintingPolicyToId,
  Script,
  SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { NodeConfig } from "@/services/config.js";

export type AuthenticatedValidator = {
  spendingCBOR: string;
  spendScript: Script;
  spendScriptAddress: string;
  mintScript: Script;
  policyId: string;
};

export const makeAuthenticatedValidator = (
  mintingTitle: string,
  spendingTitle?: string,
): Effect.Effect<AuthenticatedValidator, never, NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const spendingCBOR: string = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find((v) => v.title === spendingTitle),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const spendScript: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(spendingCBOR),
    };
    const spendScriptAddress = validatorToAddress(
      nodeConfig.NETWORK,
      spendScript,
    );
    const mintingCBOR = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find((v) => v.title === mintingTitle),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const mintScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(mintingCBOR),
    };

    const policyId = mintingPolicyToId(mintScript);

    return {
      spendingCBOR,
      spendScript,
      spendScriptAddress,
      mintScript,
      policyId,
    };
  }).pipe(Effect.orDie);

const makeAlwaysSucceedsService: Effect.Effect<
  {
    hubOracleAuthValidator: AuthenticatedValidator;
    schedulerAuthValidator: AuthenticatedValidator;
    stateQueueAuthValidator: AuthenticatedValidator;
    settlementQueueAuthValidator: AuthenticatedValidator;
    registeredOperatorsAuthValidator: AuthenticatedValidator;
    activeOperatorsAuthValidator: AuthenticatedValidator;
    retiredOperatorsAuthValidator: AuthenticatedValidator;
    escapeHatchAuthValidator: AuthenticatedValidator;
    fraudProofCatalogueAuthValidator: AuthenticatedValidator;
    depositAuthValidator: AuthenticatedValidator;
  },
  never,
  NodeConfig
> = Effect.gen(function* () {
    const hubOracleAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.hub_oracle_mint.else",
      "always_succeeds.hub_oracle_spend.else",
    );
    const schedulerAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.scheduler_mint.else",
      "always_succeeds.scheduler_spend.else",
    );
    const stateQueueAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.state_queue_mint.else",
      "always_succeeds.state_queue_spend.else",
    );
    const settlementQueueAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.settlement_queue_mint.else",
      "always_succeeds.settlement_queue_spend.else",
    );
    const registeredOperatorsAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.registered_operators_mint.else",
    );
    const activeOperatorsAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.active_operators_mint.else",
      "always_succeeds.active_operators_spend.else",
    );
    const retiredOperatorsAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.retired_operators_mint.else",
    );
    const escapeHatchAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.escape_hatch_mint.else",
      "always_succeeds.escape_hatch_spend.else",
    );
    const fraudProofCatalogueAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.fraud_proof_catalogue_mint.else",
      "always_succeeds.fraud_proof_catalogue_spend.else",
    );
    const depositAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.deposit_mint.else",
      "always_succeeds.deposit_spend.else",
    );

    return {
      hubOracleAuthValidator,
      schedulerAuthValidator,
      stateQueueAuthValidator,
      settlementQueueAuthValidator,
      registeredOperatorsAuthValidator,
      activeOperatorsAuthValidator,
      retiredOperatorsAuthValidator,
      escapeHatchAuthValidator,
      fraudProofCatalogueAuthValidator,
      depositAuthValidator,
    };
  }).pipe(Effect.orDie);

export class AlwaysSucceedsContract extends Effect.Service<AlwaysSucceedsContract>()(
  "AlwaysSucceedsContract",
  {
    effect: makeAlwaysSucceedsService,
    dependencies: [NodeConfig.layer],
  },
) {}
