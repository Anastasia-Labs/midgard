import { Effect } from "effect";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

export const provideValidatorTestLayers = <A, E, R>(
  eff: Effect.Effect<A, E, R>,
) => eff.pipe(Effect.provide(AlwaysSucceedsContract.Default));
