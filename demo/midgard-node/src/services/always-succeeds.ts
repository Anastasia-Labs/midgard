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

// TODO: remove all non-layer usages of this function
export const makeAlwaysSucceedsServiceFn = (nodeConfig: NodeConfigDep) =>
  Effect.gen(function* () {
    const stateQueueSpendingCBOR: string = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === "always_succeeds.state_queue_spend.else",
        ),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const stateQueueSpendScript: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(stateQueueSpendingCBOR),
    };
    const stateQueueSpendScriptAddress = validatorToAddress(
      nodeConfig.NETWORK,
      stateQueueSpendScript,
    );
    const stateQueueMintingCBOR = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === "always_succeeds.state_queue_mint.else",
        ),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const stateQueueMintScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(stateQueueMintingCBOR),
    };

    const depositSpendingCBOR: string = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === "always_succeeds.deposit_spend.else",
        ),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const depositSpendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(depositSpendingCBOR),
    };
    const depositSpendingScriptAddress = validatorToAddress(
      nodeConfig.NETWORK,
      depositSpendingScript,
    );
    const mintingDepositCBOR = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === "always_succeeds.deposit_mint.else",
        ),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const depositSpendingMintScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(mintingDepositCBOR),
    };

    const stateQueuePolicyId = mintingPolicyToId(stateQueueMintScript);
    const depositSpendingPolicyId = mintingPolicyToId(
      depositSpendingMintScript,
    );

    return {
      stateQueueSpendingCBOR,
      stateQueueSpendScript,
      stateQueueSpendScriptAddress,
      stateQueueMintScript,
      stateQueuePolicyId,

      depositSpendingCBOR,
      depositSpendingScript,
      depositSpendingScriptAddress,
      depositSpendingMintScript,
      depositSpendingPolicyId,
    };
  }).pipe(Effect.orDie);

const makeAlwaysSucceedsService: Effect.Effect<
  {
  // TODO: rename to `AuthenticatedValidator`

    stateQueueSpendingCBOR: string;
    stateQueueSpendScript: Script;
    stateQueueSpendScriptAddress: string;
    stateQueueMintScript: Script;
    stateQueuePolicyId: string;

    depositSpendingCBOR: string;
    depositSpendingScript: Script;
    depositSpendingScriptAddress: string;
    depositSpendingMintScript: Script;
    depositSpendingPolicyId: string;
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
