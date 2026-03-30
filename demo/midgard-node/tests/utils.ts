import dotenv from "dotenv";
import { NodeConfig } from "@/services/config.js";
import { Database } from "@/services/database.js";
import { Lucid } from "@/services/lucid.js";
import { Effect } from "effect";

dotenv.config({ path: ".env" });

export const provideDatabaseLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provide(Database.layer),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );
