import { Effect } from "effect";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import { NodeConfig } from "../src/services/config.js";
import dotenv from "dotenv";

// Ensure environment variables are loaded
dotenv.config({ path: ".env" });

export const provideValidatorTestLayers = <A, E, R>(
  eff: Effect.Effect<A, E, R>
) =>
  eff.pipe(
    Effect.provide(AlwaysSucceedsContract.Default),
    Effect.provide(NodeConfig.layer)
  );
