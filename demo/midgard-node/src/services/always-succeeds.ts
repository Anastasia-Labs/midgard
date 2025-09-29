import { Context, Effect, Layer, pipe } from "effect";
import * as scripts from "../../blueprints/always-succeeds/plutus.json" with { type: "json" };
import {
  applyDoubleCborEncoding,
  MintingPolicy,
  mintingPolicyToId,
  Script,
  SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { NodeConfig, NodeConfigDep } from "@/config.js";

export type AuthenticatedValidator = {
  spendingCBOR: string,
  spendScript: Script,
  spendScriptAddress: string,
  mintScript: Script,
  policyId: string,
};

export const makeAuthenticatedValidator = (
    nodeConfig: NodeConfigDep,
    spendingTitle: string,
    mintingTitle: string,
  ) : Effect.Effect<AuthenticatedValidator, never, never>=>
  Effect.gen(function* () {
    const spendingCBOR: string = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === spendingTitle,
        ),
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
        scripts.default.validators.find(
          (v) => v.title === mintingTitle,
        ),
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



// TODO: remove all non-layer usages of this function
export const makeAlwaysSucceedsServiceFn = (nodeConfig: NodeConfigDep) =>
  Effect.gen(function* () {
    const stateQueueAuthValidator = yield* makeAuthenticatedValidator(
      nodeConfig,
      "always_succeeds.state_queue_spend.else",
      "always_succeeds.state_queue_mint.else"
    )

    const depositAuthValidator = yield* makeAuthenticatedValidator(
      nodeConfig,
      "always_succeeds.deposit_spend.else",
      "always_succeeds.deposit_mint.else"
    )

    return {
      stateQueueAuthValidator,
      depositAuthValidator,
    };
  }).pipe(Effect.orDie);

const makeAlwaysSucceedsService: Effect.Effect<
  {
    stateQueueAuthValidator: AuthenticatedValidator,
    depositAuthValidator: AuthenticatedValidator,
  },
  never,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  return yield* makeAlwaysSucceedsServiceFn(nodeConfig);
});

export class AlwaysSucceedsContract extends Context.Tag(
  "AlwaysSucceedsContract",
)<
  AlwaysSucceedsContract,
  Effect.Effect.Success<typeof makeAlwaysSucceedsService>
>() {
  static readonly layer = Layer.effect(
    AlwaysSucceedsContract,
    makeAlwaysSucceedsService,
  );
}
