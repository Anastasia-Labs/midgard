import { resolveL1ViewFatalMs } from "@al-ft/midgard-core";
import type * as SDKCore from "@al-ft/midgard-sdk/core";
import * as SDK from "@al-ft/midgard-sdk/state-queue";
import { Config, Context, Data, Effect, Layer, Option } from "effect";

import { daHardeningConfig } from "../da/hardening-config.js";
import { commitConfig } from "./config.commit.js";
import { databaseConfig } from "./config.database.js";
import { deploymentConfig } from "./config.deployment.js";
import { ingressConfig } from "./config.ingress.js";
import { l1Config } from "./config.l1.js";
import { mpfConfig } from "./config.mpf.js";
import { validationConfig } from "./config.validation.js";

/**
 * The node's settings, one table per concern. Each table reads, defaults and
 * checks its own settings; a setting resolved from another table's is
 * resolved here.
 */
const nodeConfig = Config.all([
  deploymentConfig,
  l1Config,
  validationConfig,
  ingressConfig,
  databaseConfig,
  commitConfig,
  mpfConfig,
  daHardeningConfig,
]).pipe(
  Config.mapAttempt(
    ([deployment, l1, validation, ingress, database, commit, mpf, da]) => {
      return {
        ...deployment,
        ...l1,
        L1_VIEW_FATAL_MS: resolveL1ViewFatalMs({
          value: Option.getOrUndefined(l1.L1_VIEW_FATAL_MS),
          defaultMs: Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
          pollIntervalMs: database.WAIT_BETWEEN_RETENTION_SWEEPS,
          fieldName: "L1_VIEW_FATAL_MS",
        }),
        ...validation,
        ...ingress,
        ...database,
        ...commit,
        ...mpf,
        ...da,
      };
    },
  ),
);

/** Fully-decoded runtime configuration required by the node. */
export type NodeConfigDep = Config.Config.Success<typeof nodeConfig>;

/**
 * Loads and normalizes the node's runtime configuration from environment
 * variables. A setting out of range refuses startup.
 */
export const makeConfig: Effect.Effect<NodeConfigDep> =
  Effect.orDie(nodeConfig);

/**
 * Effect service carrying the decoded node configuration.
 */
export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  NodeConfigDep
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}

/**
 * Tagged configuration error enriched with the relevant field/value pairs.
 */
export class ConfigError extends Data.TaggedError("ConfigError")<
  SDKCore.GenericErrorFields & {
    readonly fieldsAndValues: [string, string][];
  }
> {}
