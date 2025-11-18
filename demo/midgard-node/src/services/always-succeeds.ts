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
    stateQueueAuthValidator: AuthenticatedValidator;
    depositAuthValidator: AuthenticatedValidator;
    txOrderAuthValidator: AuthenticatedValidator;
    withdrawalAuthValidator: AuthenticatedValidator;
  },
  never,
  NodeConfig
> = Effect.gen(function* () {
  return yield* Effect.gen(function* () {
    const stateQueueAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.state_queue_spend.else",
      "always_succeeds.state_queue_mint.else",
    );

    const depositAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.deposit_spend.else",
      "always_succeeds.deposit_mint.else",
    );

    const txOrderAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.tx_order_spend.else",
      "always_succeeds.tx_order_mint.else",
    );

    const withdrawalAuthValidator = yield* makeAuthenticatedValidator(
      "always_succeeds.withdrawal_spend.else",
      "always_succeeds.withdrawal_mint.else",
    );

    return {
      stateQueueAuthValidator,
      depositAuthValidator,
      txOrderAuthValidator,
      withdrawalAuthValidator
    };
  }).pipe(Effect.orDie);
});

export class AlwaysSucceedsContract extends Effect.Service<AlwaysSucceedsContract>()(
  "AlwaysSucceedsContract",
  {
    effect: makeAlwaysSucceedsService,
    dependencies: [NodeConfig.layer],
  },
) {}
