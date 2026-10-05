import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { Globals } from "../../src/services/globals.js";
import {
  BASE_HEADER,
  BASE_OUT,
  hex,
  insertJournal,
  signedCommit,
  TTL,
  UTXOS_ROOT,
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
  withNativeReplay,
} from "./history-expired-intent-release-preparation.js";

// Intentionally sorts before its ancestor, proving recovery uses ancestry.
export const CHILD = Buffer.alloc(28, 1);
export const PREFIX_ROOT = "11".repeat(32);
export const CHILD_ROOT = "22".repeat(32);
export const WINNER_ROOT = "33".repeat(32);
const S_TX = hex("completed-return:s");
const S_OUT = `${S_TX}#0`;
const CHILD_TX = hex("completed-return:child");
const childOut = `${CHILD_TX}#0`;
const sql = <A, E>(run: (sql: SqlClient.SqlClient) => Effect.Effect<A, E>) =>
  Effect.flatMap(SqlClient.SqlClient, run);
const observer = (nodes: readonly { headerHash: string; outRef: string }[]) =>
  sql((sql) => sql`DELETE FROM state_queue_terminal_observer_states`).pipe(
    Effect.zipRight(observerSees(nodes)),
  );
export type ChainOptions = {
  movingWinner?: boolean;
  stop?: "before CAS" | "after CAS" | "before SQL receipt";
  bad?: "queue link" | "native link" | "historical child";
  absentChild?: boolean;
};

/** Preparation, membership repair and recovery rows are production over real
 * SQL; source authentication/depth and native CAS/finalization are modeled. */
export const completedChainReturnScenario = (
  fixture: Fixture,
  options: ChainOptions = {},
) => {
  const owner = ownerModel(CHILD_ROOT);
  return onNode(
    owner,
    (node) =>
      Effect.gen(function* () {
        const childCommit = signedCommit(S_OUT, TTL + 5);
        yield* journal(
          W_HEADER,
          Pending.Status.Abandoned,
          W_COMMIT,
          2_000_000,
          { abandonment: "replacement" },
        );
        const winnerRoot = options.movingWinner ? WINNER_ROOT : UTXOS_ROOT;
        yield* sql(
          (sql) =>
            sql`UPDATE pending_block_finalizations SET expected_utxos_root=${winnerRoot} WHERE header_hash=${W_HEADER}`,
        );
        yield* withNativeReplay(W_HEADER);
        yield* journal(S_HEADER, Pending.Status.Finalized, S_COMMIT, 3_000_000);
        yield* sql(
          (sql) =>
            sql`UPDATE pending_block_finalizations SET expected_utxos_root=${PREFIX_ROOT} WHERE header_hash=${S_HEADER}`,
        );
        yield* withNativeReplay(S_HEADER);
        yield* insertJournal({
          header: CHILD,
          status: Pending.Status.Finalized,
          commit: childCommit,
          baseOut: S_OUT,
          baseHeader: S_HEADER,
          createdAt: new Date(3_500_000),
        });
        yield* sql(
          (sql) =>
            sql`UPDATE pending_block_finalizations SET base_utxos_root=${PREFIX_ROOT}, expected_utxos_root=${CHILD_ROOT}, block_end_time=block_start_time+INTERVAL '1 second' WHERE header_hash=${CHILD}`,
        );
        yield* withNativeReplay(CHILD);
        yield* observer([
          { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
        ]);
        fixture.queue = wHoldsTheSlot;
        fixture.coverage = { head: 10, start: 1, txs: { [W_NODE_TX]: 5 } };
        const initial = yield* revival(node);
        const original = yield* plans;
        // Normal local finalization is modeled, with matching native/SQL/journal
        // roots and retained replay. The preceding displacement repair was real.
        const globals = yield* Globals;
        const finalize = (header: Buffer, candidateRoot: string) =>
          Effect.gen(function* () {
            yield* sql(
              (sql) =>
                sql`UPDATE pending_block_finalizations SET status=${Pending.Status.Finalized} WHERE header_hash=${header}`,
            );
            owner.durableRoot = candidateRoot;
            yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
            yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
          });
        const snapshot = Effect.gen(function* () {
          return {
            w: yield* statusOf(W_HEADER),
            s: yield* statusOf(S_HEADER),
            child: yield* statusOf(CHILD),
            ledger: yield* ledgerRoot,
            native: owner.durableRoot,
            pending: yield* Ref.get(globals.LOCAL_FINALIZATION_PENDING),
            published: yield* Ref.get(
              globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            ),
            plans: yield* plans,
          };
        });
        yield* finalize(W_HEADER, winnerRoot);
        const afterCompleted = yield* snapshot;
        const hasChild =
          !options.absentChild && options.bad !== "historical child";
        const d = queueNode(
          BASE_HEADER.toString("hex"),
          root.headerHash,
          options.bad === "queue link"
            ? CHILD.toString("hex")
            : S_HEADER.toString("hex"),
          BASE_OUT,
        );
        const s = queueNode(
          S_HEADER.toString("hex"),
          BASE_HEADER.toString("hex"),
          hasChild ? CHILD.toString("hex") : undefined,
          S_OUT,
        );
        const child = queueNode(
          CHILD.toString("hex"),
          S_HEADER.toString("hex"),
          undefined,
          childOut,
        );
        yield* observer([
          { headerHash: S_HEADER.toString("hex"), outRef: S_OUT },
          ...(!options.absentChild
            ? [{ headerHash: CHILD.toString("hex"), outRef: childOut }]
            : []),
        ]);
        fixture.queue = {
          root,
          nodes: [root, d, s, ...(hasChild ? [child] : [])],
        } as never;
        fixture.coverage = {
          head: 10,
          start: 1,
          txs: {
            [S_TX]: 4,
            [S_COMMIT.hash]: 4,
            ...(!options.absentChild
              ? { [CHILD_TX]: 5, [childCommit.hash]: 5 }
              : {}),
          },
        };
        if (options.bad === "native link")
          yield* sql(
            (sql) =>
              sql`UPDATE pending_block_finalizations SET base_utxos_root=${WINNER_ROOT}, mpf_replay_base_root=${Buffer.from(WINNER_ROOT, "hex")} WHERE header_hash=${CHILD}`,
          );
        if (options.stop === "before SQL receipt") {
          yield* sql((sql) =>
            sql.unsafe(
              `CREATE OR REPLACE FUNCTION histchain_stop() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.state='applied' THEN RAISE EXCEPTION 'stop before SQL receipt'; END IF; RETURN NEW; END $$`,
            ),
          );
          yield* sql((sql) =>
            sql.unsafe(
              `CREATE TRIGGER histchain_stop BEFORE UPDATE ON event_history_recovery_plans FOR EACH ROW EXECUTE FUNCTION histchain_stop()`,
            ),
          );
        }
        if (options.stop === "before CAS")
          owner.beforeRestore = async () => {
            throw new Error("stop before CAS");
          };
        if (options.stop === "after CAS")
          owner.afterRestore = async () => {
            throw new Error("stop after CAS");
          };
        const interrupted = yield* revival(node);
        const intermediate = yield* snapshot;
        owner.beforeRestore = undefined;
        owner.afterRestore = undefined;
        if (options.stop === "before SQL receipt")
          yield* sql((sql) =>
            sql.unsafe(
              `DROP TRIGGER histchain_stop ON event_history_recovery_plans`,
            ),
          );
        const again = yield* revival(node);
        const parentPrepared = yield* snapshot;
        const activeRetry = yield* revival(node);
        const beforeParentFinalization = yield* snapshot;
        if (
          parentPrepared.s?.status === Pending.Status.ObservedWaitingStability
        ) {
          yield* finalize(S_HEADER, PREFIX_ROOT);
          const next = yield* revival(node);
          const childPrepared = yield* snapshot;
          if (
            childPrepared.child?.status ===
            Pending.Status.ObservedWaitingStability
          )
            yield* finalize(CHILD, CHILD_ROOT);
          return {
            initial,
            original,
            afterCompleted,
            interrupted,
            intermediate,
            again,
            parentPrepared,
            activeRetry,
            beforeParentFinalization,
            next,
            childPrepared,
            final: yield* snapshot,
            operations: owner.operations,
          };
        }
        return {
          initial,
          original,
          afterCompleted,
          interrupted,
          intermediate,
          again,
          parentPrepared,
          activeRetry,
          beforeParentFinalization,
          next: undefined,
          childPrepared: undefined,
          final: yield* snapshot,
          operations: owner.operations,
        };
      }),
    CHILD_ROOT,
  );
};
