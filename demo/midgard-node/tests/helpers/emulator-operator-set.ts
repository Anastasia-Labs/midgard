/**
 * The node's operator set (NC14) over an emulator, for the watchdog: the
 * follower at the emulator's tip (`syncEmulatorChain`: the operator lists,
 * the scheduler, the hub oracle and the state queue are its facts), then the
 * production mirror, hook and publish into `Globals.OPERATOR_SET`.
 *
 * Each call loads a fresh mirror at the follower's current view, so the
 * changed-rows path is exercised by `l1-operator-set.test.ts`, not here.
 */
import {
  currentViewIn,
  type FactStore,
  postgresDialect,
} from "@al-ft/midgard-l1-follower";
import type * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Ref, Runtime } from "effect";

import { followerSqlTx } from "../../src/database/follower-schema.js";
import {
  createOperatorSetMirror,
  memoryActivityRecord,
  operatorSetConfig,
  operatorSetHook,
  publishedOperatorSetOf,
  publishOperatorMembership,
  stateQueueTailOf,
} from "../../src/l1-operator-set/index.js";
import { Globals } from "../../src/services/globals.js";
import { readLandedStateQueue } from "../../src/services/landed-state-queue.js";
import { syncEmulatorChain } from "./emulator-l1-follower.js";

/** The fact store the hook reads, over the node's SQL connection. */
export const nodeFactStore = Effect.map(
  followerSqlTx,
  (tx): Pick<FactStore, "dialect" | "transaction"> => ({
    dialect: postgresDialect,
    transaction: (_mode, work) => work(tx),
  }),
);

export const publishEmulatorOperatorSet = (
  lucid: LucidEvolution,
  contracts: Pick<
    SDK.MidgardValidators,
    | "registeredOperators"
    | "activeOperators"
    | "retiredOperators"
    | "scheduler"
    | "hubOracle"
    | "stateQueue"
  >,
  ownKey: string,
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* syncEmulatorChain(lucid);
    const queue = yield* readLandedStateQueue(contracts.stateQueue);
    const stateQueueTail =
      queue.kind === "ok" ? stateQueueTailOf(queue.queue) : null;
    const store = yield* nodeFactStore;
    const config = operatorSetConfig(contracts);
    const run = Runtime.runPromise(yield* Effect.runtime<never>());
    const hook = operatorSetHook({
      store,
      mirror: createOperatorSetMirror({ config, ownKey }),
      depth: { confirmationDepth: 1, securityParameter: 2160 },
      activity: memoryActivityRecord(),
      publish: ({ set, membership }) =>
        run(
          Effect.zipRight(
            Ref.set(
              globals.OPERATOR_SET,
              publishedOperatorSetOf({
                set,
                membership,
                stateQueueTail,
                store,
                retired: config.retired,
              }),
            ),
            publishOperatorMembership(membership).pipe(
              Effect.provideService(Globals, globals),
            ),
          ),
        ),
    });
    return yield* Effect.promise(async () => {
      const view = await store.transaction("read", (tx) =>
        currentViewIn(tx, store.dialect),
      );
      if (view === null) throw new Error("the facts have no follower view");
      return hook({ kind: "unchanged", view });
    });
  });
