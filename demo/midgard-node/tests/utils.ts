import { Effect } from "effect";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import { NodeConfig } from "../src/services/config.js";


export const provideValidatorTestLayers = <A, E, R>(
  eff: Effect.Effect<A, E, R>
) =>
  eff.pipe(
    Effect.provide(AlwaysSucceedsContract.Default),
    Effect.provide(NodeConfig.layer)
  );
