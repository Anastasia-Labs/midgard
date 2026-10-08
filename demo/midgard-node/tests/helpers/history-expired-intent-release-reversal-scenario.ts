import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { NativeMpfRootNotRetained } from "../../src/services/mpf-native-owner/protocol.js";
import {
  BASE_HEADER,
  BASE_OUT,
  hex,
  retainedBaseJournal,
} from "./history-expired-intent-release-before-ttl.js";
import {
  journal,
  queueNode,
  root,
  S_COMMIT,
  S_HEADER,
  W_COMMIT,
  W_HEADER,
  W_NODE_OUT,
  W_NODE_TX,
  wHoldsTheSlot,
} from "./history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  ledgerRoot,
  observerSees,
  onNode,
  ownerModel,
  plans,
  revival,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
} from "./history-expired-intent-release-preparation.js";

/* The displacement-reversal scenario: rollback past confirmation depth
 * switches a base slot's winner, and the root-moving displacement reconciles
 * its retained native/SQL obligations across each stop. `reversalOn` takes
 * the calling test file's hoisted L1 fixture. */

export const S_NODE_TX = hex("reversal:s-node-tx");
export const S_NODE_OUT = `${S_NODE_TX}#0`;

/** The queue after the deep rollback: D links to S, whose node is on it. */
export const sHoldsTheSlot = {
  root,
  nodes: [
    root,
    queueNode(
      BASE_HEADER.toString("hex"),
      root.headerHash,
      S_HEADER.toString("hex"),
      BASE_OUT,
    ),
    queueNode(
      S_HEADER.toString("hex"),
      BASE_HEADER.toString("hex"),
      undefined,
      S_NODE_OUT,
    ),
  ],
} as never;

/** `tx`'s node output 6 blocks deep at head height 10, past the depth 3. */
export const deep = (tx: string) => ({ head: 10, start: 1, txs: { [tx]: 5 } });

export const sql = (
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) => Effect.flatMap(SqlClient.SqlClient, statement);

/** The correction observer's cursor queue: the root, then `node`. */
export const observerNow = (node: { headerHash: string; outRef: string }) =>
  sql((sql) => sql`DELETE FROM state_queue_terminal_observer_states`).pipe(
    Effect.zipRight(observerSees([node])),
  );

export const outcome = Effect.gen(function* () {
  return {
    w: (yield* statusOf(W_HEADER))?.status,
    s: (yield* statusOf(S_HEADER))?.status,
    plans: (yield* plans).map(({ state }) => state),
    ledger: yield* ledgerRoot,
  };
});

/** W revived over S, W locally finalized, then the rollback deeper than the
 * confirmation depth lands S again, and the revival runs twice. */
export const reversalOn =
  (fixture: Fixture) =>
  (
    wChangedTheLedger: boolean,
    stopAfterCas = false,
    rollbackAfterCas = false,
    stopInverse = false,
    repeatCycle = false,
    refuseInverse = false,
  ) => {
    const owner = ownerModel(UTXOS_ROOT);
    return onNode(
      owner,
      (node) =>
        Effect.gen(function* () {
          yield* retainedBaseJournal;
          yield* journal(
            W_HEADER,
            Pending.Status.Abandoned,
            W_COMMIT,
            2_000_000,
            {
              abandonment: "replacement",
              empty: !wChangedTheLedger,
            },
          );
          yield* withNativeReplay(W_HEADER);
          yield* journal(
            S_HEADER,
            Pending.Status.LocallyApplied,
            S_COMMIT,
            3_000_000,
            {
              empty: true,
            },
          );
          yield* observerNow({
            headerHash: W_HEADER.toString("hex"),
            outRef: W_NODE_OUT,
          });
          fixture.queue = wHoldsTheSlot;
          fixture.coverage = deep(W_NODE_TX);
          const forward = yield* revival(node);
          const revived = yield* outcome;
          // W's local finalization completes.
          yield* sql(
            (sql) => sql`UPDATE pending_block_finalizations
            SET status = ${Pending.Status.LocallyApplied}
            WHERE header_hash = ${W_HEADER}`,
          );
          owner.durableRoot = wChangedTheLedger ? "00".repeat(32) : UTXOS_ROOT;
          yield* sql(
            (sql) =>
              sql`UPDATE mpf_engine_state SET root_hex = ${owner.durableRoot} WHERE store_name = 'ledger'`,
          );
          // The rollback deeper than the confirmation depth: S holds D's slot
          // again, deep, and W's commit is gone from the canonical history.
          yield* observerNow({
            headerHash: S_HEADER.toString("hex"),
            outRef: S_NODE_OUT,
          });
          fixture.queue = sHoldsTheSlot;
          fixture.coverage = deep(S_NODE_TX);
          if (stopAfterCas)
            owner.afterRestore = async () => {
              throw new Error("stop after displacement CAS");
            };
          const interrupted = stopAfterCas ? yield* revival(node) : undefined;
          const retained = yield* plans;
          owner.afterRestore = undefined;
          if (rollbackAfterCas) {
            yield* observerNow({
              headerHash: W_HEADER.toString("hex"),
              outRef: W_NODE_OUT,
            });
            fixture.queue = wHoldsTheSlot;
            fixture.coverage = deep(W_NODE_TX);
          }
          if (stopInverse)
            owner.afterRestore = async () => {
              throw new Error("stop after inverse CAS");
            };
          const inverseInterrupted = stopInverse
            ? yield* revival(node)
            : undefined;
          const inverseRetained = yield* plans;
          owner.afterRestore = undefined;
          // The native owner refuses the inverse restore as not retained.
          if (refuseInverse)
            owner.beforeRestore = async ({ targetRoot }) => {
              throw new NativeMpfRootNotRetained(targetRoot);
            };
          const inverseHeld = refuseInverse
            ? {
                attempts: [yield* revival(node), yield* revival(node)],
                plans: yield* plans,
                ledger: yield* ledgerRoot,
                native: owner.durableRoot,
                operations: [...owner.operations],
              }
            : undefined;
          owner.beforeRestore = undefined;
          const reversed = [yield* revival(node), yield* revival(node)];
          const cycleAttempts = [];
          if (repeatCycle) {
            yield* sql(
              (sql) =>
                sql`UPDATE pending_block_finalizations SET status = ${Pending.Status.LocallyApplied} WHERE header_hash = ${S_HEADER}`,
            );
            yield* observerNow({
              headerHash: W_HEADER.toString("hex"),
              outRef: W_NODE_OUT,
            });
            fixture.queue = wHoldsTheSlot;
            fixture.coverage = deep(W_NODE_TX);
            cycleAttempts.push(yield* revival(node));
            yield* sql(
              (sql) =>
                sql`UPDATE pending_block_finalizations SET status = ${Pending.Status.LocallyApplied} WHERE header_hash = ${W_HEADER}`,
            );
            owner.durableRoot = "00".repeat(32);
            yield* sql(
              (sql) =>
                sql`UPDATE mpf_engine_state SET root_hex = ${owner.durableRoot} WHERE store_name = 'ledger'`,
            );
            yield* observerNow({
              headerHash: S_HEADER.toString("hex"),
              outRef: S_NODE_OUT,
            });
            fixture.queue = sHoldsTheSlot;
            fixture.coverage = deep(S_NODE_TX);
            cycleAttempts.push(yield* revival(node));
          }

          return {
            cycleAttempts,
            cyclePlans: yield* plans,
            inverseInterrupted,
            inverseRetained,
            inverseHeld,
            operations: owner.operations,
            forward,
            revived,
            reversed,
            after: yield* outcome,
            durableRoot: owner.durableRoot,
            restores: owner.restores,
            interrupted,
            retained: retained.map(({ state }) => state),
          };
        }),
      UTXOS_ROOT,
    );
  };
