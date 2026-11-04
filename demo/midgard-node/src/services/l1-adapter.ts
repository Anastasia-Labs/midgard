/**
 * Which L1-access adapter (`../l1-access.ts`) a process's Lucid service is
 * built over, and which one its chain parameters are read through. The Lucid
 * service requires `L1Adapter` and a derived contract bundle
 * `L1ChainParameters`, so every place that provides them chooses at compile
 * time:
 *
 * - Role processes only (`listen` and its workers): `FollowerL1AdapterLive`
 *   (`./l1-adapter.follower.ts`) opens the follower adapter over the node
 *   database's follower store with the node's index; the chain parameters
 *   (`FollowerL1ChainParametersLive`, here) are keyed ledger queries to the
 *   local node, with no store. Both refuse a non-follower L1 configuration
 *   at start.
 * - Every CLI command and tool (`../commands/l1-tool-adapter.ts`): node
 *   ledger, Kupmios or Blockfrost, chosen by `--l1`, for both.
 */
import { assertRoleL1Env } from "@al-ft/midgard-l1-follower";
import type * as LE from "@lucid-evolution/lucid";
import { Context, Effect, Layer, type Scope } from "effect";

import type { L1Access } from "../l1-access.js";
import { hasCauseCode } from "../provider-retry.js";
import { ConfigError } from "./config.js";
import type { NodeConfigDep } from "./config.node-config-dep.js";
import {
  NODE_L1_ACCESS_UNCONFIGURED,
  openNodeLedgerAccessFromConfig,
} from "./l1-node-ledger-access.js";
import {
  L1_NODE_CONFIG_PENDING,
  retryStartupStep,
  STARTUP_L1_NODE_BUDGET,
  startupStepFailed,
  type StartupStepFailedError,
} from "./startup-waiting.js";

/** The startup step a role's L1-access refusals are named under. */
export const L1_ACCESS_STARTUP_STEP = "l1_access";
/** `/readyz` reason: the role was started with a non-follower L1 setting. */
export const ROLE_NON_FOLLOWER_L1_CONFIG = "role_non_follower_l1_config";
/** `/readyz` reason: no local node is configured for the role's follower. */
export const L1_NODE_UNCONFIGURED = "l1_node_unconfigured";
/** `/readyz` reason: the node's L1 index cannot be built from its
 * deployment and configuration (`nodeIndexSpec`). */
export const L1_INDEX_UNCONFIGURED = "l1_index_unconfigured";

/** The adapter a Lucid service opens, closed with its scope. */
export class L1Adapter extends Context.Tag("L1Adapter")<
  L1Adapter,
  {
    /** `follower` in a role process, `tool` everywhere else. */
    readonly role: "follower" | "tool";
    readonly open: (
      config: NodeConfigDep,
    ) => Effect.Effect<L1Access, ConfigError, Scope.Scope>;
  }
>() {}

/**
 * The chain parameters a process reads (the slot mapping and era history):
 * keyed ledger queries, never through the node's index. Opened for the read
 * and closed with its scope.
 */
export class L1ChainParameters extends Context.Tag("L1ChainParameters")<
  L1ChainParameters,
  {
    readonly open: (
      config: NodeConfigDep,
    ) => Effect.Effect<Pick<L1Access, "slotConfig">, ConfigError, Scope.Scope>;
  }
>() {}

const asError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause), { cause });

/** The local node's configuration files are not there yet (a node still
 * starting writes them): the one open failure waited out. */
export const isL1NodeConfigPending = (error: unknown): boolean =>
  hasCauseCode(error, "ENOENT");

/** A step's terminal failure, as the Lucid service's `ConfigError`; the
 * startup still finds the step behind it (`findStartupStepFailure`). */
export const asStartupConfigError =
  (network: LE.Network) =>
  (failure: StartupStepFailedError): ConfigError =>
    new ConfigError({
      message: failure.message,
      cause: failure,
      fieldsAndValues: [["NETWORK", network]],
    });

/**
 * Opens an adapter with `open`, closed with the scope. Opening reads only the
 * node's config files when it reads anything (the node and follower adapters,
 * for the network magic); while they are not there yet this waits under
 * `l1_node_config_pending` for at most the L1 node budget. Any other failure
 * (a wrong network magic, an unconfigured node, a refused selection) fails at
 * once.
 */
export const openAdapterScoped = (
  config: NodeConfigDep,
  open: () => Promise<L1Access>,
): Effect.Effect<L1Access, ConfigError, Scope.Scope> =>
  Effect.acquireRelease(
    retryStartupStep(Effect.tryPromise({ try: open, catch: asError }), {
      key: "l1_node_config",
      reason: L1_NODE_CONFIG_PENDING,
      retryable: isL1NodeConfigPending,
      budget: { maxElapsed: STARTUP_L1_NODE_BUDGET },
      initialMs: 500,
    }).pipe(Effect.mapError(asStartupConfigError(config.NETWORK))),
    (opened) => Effect.promise(opened.close),
  );

/** A role's `l1_access` refusal under `reason`. */
export const refuseRoleL1Access =
  (network: LE.Network) =>
  (reason: string, cause: unknown): ConfigError =>
    asStartupConfigError(network)(
      startupStepFailed({ step: L1_ACCESS_STARTUP_STEP, reason, cause }),
    );

/**
 * A role's L1 refusals, before it opens anything: a process configured with
 * a non-follower L1 access (Kupmios, Blockfrost or a tool `L1_ACCESS`),
 * naming the setting, and one with no local node. Both are deterministic
 * startup failures no restart repairs: each is a failed `l1_access` step
 * with its own reason, so `listen` holds up and unready and names it on
 * `/readyz` and in its log (plan §7.5).
 */
export const refuseNonFollowerL1 = (
  config: NodeConfigDep,
): Effect.Effect<void, ConfigError> =>
  Effect.gen(function* () {
    const refuse = refuseRoleL1Access(config.NETWORK);
    yield* Effect.try({
      try: () => assertRoleL1Env(process.env, "midgard-node listen"),
      catch: (cause) => refuse(ROLE_NON_FOLLOWER_L1_CONFIG, cause),
    });
    if (config.L1_NATIVE_LEDGER === undefined)
      return yield* Effect.fail(
        refuse(
          L1_NODE_UNCONFIGURED,
          new Error(
            `The node's L1 provider needs ${NODE_L1_ACCESS_UNCONFIGURED}`,
          ),
        ),
      );
  });

/**
 * A role's chain parameters: keyed ledger queries to the local node over
 * its transport, with no store (`openNodeLedgerAccessFromConfig`), after
 * the role's refusals (`refuseNonFollowerL1`).
 */
export const FollowerL1ChainParametersLive = Layer.succeed(L1ChainParameters, {
  open: (config) =>
    Effect.zipRight(
      refuseNonFollowerL1(config),
      openAdapterScoped(config, () => openNodeLedgerAccessFromConfig(config)),
    ),
});
