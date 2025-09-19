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

export const makeAlwaysSucceedsServiceFn = (nodeConfig: NodeConfigDep) =>
  Effect.gen(function* () {
    const spendingCBOR : string = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === "always_succeeds.spend.else",
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
          (v) => v.title === "always_succeeds.mint.else",
        ),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const mintScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(mintingCBOR),
    };


    const spendingDepositCBOR : string = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === "always_succeeds.spend.else",
        ),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const spendDepositScript: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(spendingDepositCBOR),
    };
    const spendDepositScriptAddress = validatorToAddress(
      nodeConfig.NETWORK,
      spendDepositScript,
    );
    const mintingDepositCBOR = yield* pipe(
      Effect.fromNullable(
        scripts.default.validators.find(
          (v) => v.title === "always_succeeds.mint.else",
        ),
      ),
      Effect.andThen((script) => script.compiledCode),
    );
    const mintDepositScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(mintingDepositCBOR),
    };

    const policyId = mintingPolicyToId(mintScript);
    return {
      spendingCBOR,
      spendScript,
      spendScriptAddress,
      mintScript,

      spendingDepositCBOR,
      spendDepositScript,
      spendDepositScriptAddress,
      mintDepositScript,

      policyId,
    };
  }).pipe(Effect.orDie);

const makeAlwaysSucceedsService : Effect.Effect<{
    spendingCBOR: string;
    spendScript: Script;
    spendScriptAddress: string;
    mintScript: Script;
    policyId: string;
  }, never, NodeConfig> =
  Effect.gen(function* () {
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
