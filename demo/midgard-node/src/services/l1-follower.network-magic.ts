/**
 * The local node's network magic for the node's follower (F1 finding 4): read
 * from the node's config and Shelley genesis files only, never its socket, so
 * a node that is still starting does not stop the follower from starting.
 * While the files do not yield the magic, the follower state is the named
 * transient reason `l1_node_config_unreadable` and the read is retried.
 */
import type { NativeLedgerNetwork } from "@al-ft/midgard-core/native-reward-account";
import { Cause, Duration, Effect, Ref, Schedule, type Scope } from "effect";
import type { UnknownException } from "effect/Cause";

import {
  L1_NODE_CONFIG_UNREADABLE,
  type L1FollowerState,
} from "./l1-follower.readiness.js";
import { nativeLedgerNetworkMagic } from "./native-ledger.js";

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

/**
 * The local node's network magic, read from its config files (never its
 * socket). While the read fails, the follower state is
 * `l1_node_config_unreadable` with the cause, and the read is retried on
 * `schedule`; it resolves once the files yield the magic.
 */
export const awaitNodeNetworkMagic = (
  input: Readonly<{
    globals: FollowerGlobals;
    nodeConfigPath: string;
    network: NativeLedgerNetwork;
    schedule?: Schedule.Schedule<unknown, unknown>;
  }>,
): Effect.Effect<number, UnknownException> =>
  Effect.tryPromise(() =>
    nativeLedgerNetworkMagic(
      { nodeConfigPath: input.nodeConfigPath },
      input.network,
    ),
  ).pipe(
    Effect.tapError((error) => {
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
    Effect.retry(input.schedule ?? NETWORK_MAGIC_RETRY),
  );

/**
 * Runs `start` with the node's network magic. When the first read fails, the
 * node keeps starting: the state holds `l1_node_config_unreadable` while a
 * background fiber in the caller's scope retries the read and then runs
 * `start`; a later failure to start is recorded as `unconfigured`.
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
    const first = yield* Effect.either(
      awaitNodeNetworkMagic({ ...input, schedule: Schedule.stop }),
    );
    if (first._tag === "Right") return yield* start(first.right);
    yield* Effect.forkScoped(
      awaitNodeNetworkMagic(input).pipe(
        Effect.flatMap(start),
        Effect.catchAllCause((cause) =>
          recordUnconfigured(
            input.globals,
            `the L1 follower failed to start: ${Cause.pretty(cause)}`,
          ),
        ),
      ),
    );
    return undefined;
  });
