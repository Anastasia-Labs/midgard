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

const makeAlwaysSucceedsService: Effect.Effect<
  {
    spendingCBOR: string;
    spendScript: Script;
    spendScriptAddress: string;
    mintScript: Script;
    policyId: string;
  },
  never,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const spendingCBOR = yield* pipe(
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
  const policyId = mintingPolicyToId(mintScript);
  return {
    spendingCBOR,
    spendScript,
    spendScriptAddress,
    mintScript,
    policyId,
  };
}).pipe(Effect.orDie);

export class AlwaysSucceedsContract extends Effect.Service<AlwaysSucceedsContract>()(
  "AlwaysSucceedsContract",
  {
    effect: makeAlwaysSucceedsService,
    dependencies: [NodeConfig.layer],
  },
) {}
