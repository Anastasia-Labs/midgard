import { Effect, pipe } from "effect";
import * as scripts from "../../blueprints/always-succeeds/plutus.json" with { type: "json" };
import {
  applyDoubleCborEncoding,
  MintingPolicy,
  mintingPolicyToId,
  SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { NodeConfig } from "@/services/config.js";
import { AuthenticatedValidator } from "@al-ft/midgard-sdk";

export const makeAuthenticatedValidator = (
  spendingTitle: string,
  mintingTitle: string,
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
    stateQueueAuthValidator: AuthenticatedValidator;
    registeredOperatorsAuthValidator: AuthenticatedValidator;
    activeOperatorsAuthValidator: AuthenticatedValidator;
    schedulerAuthValidator: AuthenticatedValidator;
    retiredOperatorsAuthValidator: AuthenticatedValidator;
    escapeHatchAuthValidator: AuthenticatedValidator;
    fraudProofCatalogueAuthValidator: AuthenticatedValidator;
    fraudProofAuthValidator: AuthenticatedValidator;
    depositAuthValidator: AuthenticatedValidator;
  },
  never,
  NodeConfig
> = Effect.gen(function* () {
  const hubOracleAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.hub_oracle_spend.else",
    "always_succeeds.hub_oracle_mint.else",
  );
  const schedulerAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.scheduler_spend.else",
    "always_succeeds.scheduler_mint.else",
  );
  const stateQueueAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.state_queue_spend.else",
    "always_succeeds.state_queue_mint.else",
  );
  const registeredOperatorsAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.registered_operators_spend.else",
    "always_succeeds.registered_operators_mint.else",
  );
  const activeOperatorsAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.active_operators_spend.else",
    "always_succeeds.active_operators_mint.else",
  );
  const retiredOperatorsAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.retired_operators_spend.else",
    "always_succeeds.retired_operators_mint.else",
  );
  const escapeHatchAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.escape_hatch_spend.else",
    "always_succeeds.escape_hatch_mint.else",
  );
  const fraudProofCatalogueAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.fraud_proof_catalogue_spend.else",
    "always_succeeds.fraud_proof_catalogue_mint.else",
  );
  const fraudProofAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.fraud_proof_spend.else",
    "always_succeeds.fraud_proof_mint.else",
  );
  const depositAuthValidator = yield* makeAuthenticatedValidator(
    "always_succeeds.deposit_spend.else",
    "always_succeeds.deposit_mint.else",
  );

  return {
    hubOracleAuthValidator,
    schedulerAuthValidator,
    stateQueueAuthValidator,
    registeredOperatorsAuthValidator,
    activeOperatorsAuthValidator,
    retiredOperatorsAuthValidator,
    escapeHatchAuthValidator,
    fraudProofCatalogueAuthValidator,
    fraudProofAuthValidator,
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
