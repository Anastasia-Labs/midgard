/**
 * The local node's network magic for the node's follower (F1 finding 4): read
 * from the node's config and Shelley genesis files only, never its socket, so
 * a node that is still starting does not stop the follower from starting.
 * While the files are not there yet, the follower state is the named
 * transient reason `l1_node_config_unreadable` and the read is retried, for
 * at most `STARTUP_L1_NODE_BUDGET`. Past it, or on a read that fails in a
 * way waiting does not fix, the state is `l1_node_config_failed` and the read
 * stops.
 */
import type { NativeLedgerNetwork } from "@al-ft/midgard-core/native-reward-account";
import {
  Cause,
  Duration,
  Effect,
  Either,
  Ref,
  Schedule,
  type Scope,
} from "effect";
import type { UnknownException } from "effect/Cause";

import { hasCauseCode } from "../provider-retry.js";
import {
  L1_NODE_CONFIG_FAILED,
  L1_NODE_CONFIG_UNREADABLE,
  type L1FollowerState,
} from "./l1-follower.readiness.js";
import { nativeLedgerNetworkMagic } from "./native-ledger.js";
import { STARTUP_L1_NODE_BUDGET } from "./startup-waiting.js";

type FollowerGlobals = Readonly<{ L1_FOLLOWER: Ref.Ref<L1FollowerState> }>;

/** An error's message, for a detail. */
export const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/** Records a follower the configuration does not allow (a `/readyz` reason). */
export const recordUnconfigured = (globals: FollowerGlobals, detail: string) =>
  Effect.logWarning(`L1 follower is not running: ${detail}`).pipe(
    Effect.zipRight(
      Ref.set(globals.L1_FOLLOWER, { kind: "unconfigured", detail }),
    ),
  );

/** Capped exponential backoff for reading the node's network magic. */
export const NETWORK_MAGIC_RETRY = Schedule.exponential(
  Duration.millis(500),
).pipe(Schedule.union(Schedule.spaced(Duration.seconds(30))));

/** Whether a failed read waits: the config files are not there yet (a node
 * still starting writes them), as `isL1NodeConfigPending` in `lucid.ts`. */
const pending = (error: UnknownException): boolean =>
  hasCauseCode(error.error, "ENOENT");

/** Records the read failed for good (`l1_node_config_failed`). */
const recordFailed = (
  globals: FollowerGlobals,
  error: UnknownException,
  why: string,
) => {
  const detail = `the local node's network magic is unreadable (${why}): ${message(error.error)}`;
  return Effect.logError(`L1 follower does not start: ${detail}`).pipe(
    Effect.zipRight(
      Ref.set(globals.L1_FOLLOWER, {
        kind: "failed",
        reason: L1_NODE_CONFIG_FAILED,
        detail,
      }),
    ),
  );
};

type MagicInput = Readonly<{
  globals: FollowerGlobals;
  nodeConfigPath: string;
  network: NativeLedgerNetwork;
}>;

/** One read; a pending failure sets `l1_node_config_unreadable`. */
const readNetworkMagic = (
  input: MagicInput,
): Effect.Effect<number, UnknownException> =>
  Effect.tryPromise(() =>
    nativeLedgerNetworkMagic(
      { nodeConfigPath: input.nodeConfigPath },
      input.network,
    ),
  ).pipe(
    Effect.tapError((error) => {
      if (!pending(error)) return Effect.void;
      const detail = `the local node's network magic is unreadable: ${message(error.error)}`;
      return Effect.logWarning(`L1 follower is waiting: ${detail}`).pipe(
        Effect.zipRight(
          Ref.set(input.globals.L1_FOLLOWER, {
            kind: "waiting",
            reason: L1_NODE_CONFIG_UNREADABLE,
            detail,
          }),
        ),
      );
    }),
  );

/**
 * The local node's network magic, read from its config files (never its
 * socket). While the files are not there yet (`pending`), the
 * follower state is `l1_node_config_unreadable` with the cause, and the read
 * is retried on `schedule` for at most `budget`; it resolves once the files
 * yield the magic. Any other failure, or one that outlives the budget, sets
 * `l1_node_config_failed` and fails.
 */
export const awaitNodeNetworkMagic = (
  input: MagicInput &
    Readonly<{
      schedule?: Schedule.Schedule<unknown, unknown>;
      budget?: Duration.DurationInput;
    }>,
): Effect.Effect<number, UnknownException> =>
  readNetworkMagic(input).pipe(
    Effect.retry({
      schedule: (input.schedule ?? NETWORK_MAGIC_RETRY).pipe(
        Schedule.upTo(input.budget ?? STARTUP_L1_NODE_BUDGET),
      ),
      while: pending,
    }),
    Effect.tapError((error) =>
      recordFailed(
        input.globals,
        error,
        pending(error)
          ? "the config files did not appear within the L1 node budget"
          : "a failure waiting does not fix",
      ),
    ),
  );

/**
 * Runs `start` with the node's network magic. When the first read finds the
 * config files not there yet, the node keeps starting: the state holds
 * `l1_node_config_unreadable` while a background fiber in the caller's scope
 * retries the read (within its budget) and then runs `start`; a read that
 * fails for good leaves `l1_node_config_failed`, and a later failure to start
 * is recorded as `unconfigured`.
 */
export const withNodeNetworkMagic = <A, E, R>(
  input: Readonly<{
    globals: FollowerGlobals;
    nodeConfigPath: string;
    network: NativeLedgerNetwork;
  }>,
  start: (networkMagic: number) => Effect.Effect<A, E, R>,
): Effect.Effect<A | undefined, E, R | Scope.Scope> =>
  Effect.gen(function* () {
    const first = yield* Effect.either(readNetworkMagic(input));
    if (Either.isRight(first)) return yield* start(first.right);
    if (!pending(first.left)) {
      yield* recordFailed(
        input.globals,
        first.left,
        "a failure waiting does not fix",
      );
      return undefined;
    }
    yield* Effect.forkScoped(
      awaitNodeNetworkMagic(input).pipe(
        Effect.matchCauseEffect({
          // The read's failure is already recorded.
          onFailure: () => Effect.void,
          onSuccess: (networkMagic) =>
            start(networkMagic).pipe(
              Effect.catchAllCause((cause) =>
                recordUnconfigured(
                  input.globals,
                  `the L1 follower failed to start: ${Cause.pretty(cause)}`,
                ),
              ),
            ),
        }),
      ),
    );
    return undefined;
  });
