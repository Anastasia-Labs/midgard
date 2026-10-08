/**
 * The node's operator set (NC14) over an emulator, for the watchdog: the
 * emulator's live outputs at the operator lists, the scheduler, the hub
 * oracle and the state queue written as follower facts at the emulator's
 * slot (`writeAddressFacts`), then the production mirror, hook and publish
 * into `Globals.OPERATOR_SET`.
 *
 * Each call loads a fresh mirror: the facts are rewritten as seed rows, not
 * followed block by block, so the changed-rows path is exercised by the
 * follower-backed tests (`l1-operator-set.test.ts`), not here.
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
import {
  mirrorEmulatorStateQueue,
  writeAddressFacts,
} from "./landed-state-queue.js";

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
    const slot = lucid.currentSlot();
    for (const validator of [
      contracts.registeredOperators,
      contracts.activeOperators,
      contracts.retiredOperators,
      contracts.scheduler,
      contracts.hubOracle,
    ]) {
      const utxos = yield* Effect.promise(() =>
        lucid.utxosAt(validator.spendingScriptAddress),
      );
      yield* writeAddressFacts(validator.spendingScriptAddress, utxos, slot);
    }
    yield* mirrorEmulatorStateQueue(lucid, contracts.stateQueue);
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
