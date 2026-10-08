import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { correctionRewindRemovedHeaders } from "../../src/database/eventHistoryRecoveryPlans.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { eventHistoryCanonicalJson } from "../../src/l1-event-history-source.js";
import { sha } from "../../src/services/history-expired-intent-release.table.js";
import {
  admitSuffixRemoval,
  suffixRemoval,
} from "./history-displacement-compensation-removal.js";
import {
  BASE_HEADER,
  bytes,
  hex,
  insertJournal,
  signedCommit,
  TTL,
} from "./history-expired-intent-release-before-ttl.js";
import { authority } from "./history-expired-intent-release-displaced-sibling.js";
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

export const CHILD = bytes("compensation:child", 28);
export const PREFIX_ROOT = "11".repeat(32);
export const CHILD_ROOT = "22".repeat(32);
export const S_TX = hex("reversal:s-node-tx");
export const S_OUT = `${S_TX}#0`;
const childOut = `${hex("compensation:child-node")}#0`;
const deep = (tx: string, extra: Record<string, number> = {}) => ({
  head: 10,
  start: 1,
  txs: { [tx]: 5, ...extra },
});
const sql = <A, E>(work: (sql: SqlClient.SqlClient) => Effect.Effect<A, E>) =>
  Effect.flatMap(SqlClient.SqlClient, work);
const observer = (nodes: readonly { headerHash: string; outRef: string }[]) =>
  sql((sql) => sql`DELETE FROM state_queue_terminal_observer_states`).pipe(
    Effect.zipRight(observerSees(nodes)),
  );
const returned = (out = S_OUT, withChild = false) => ({
  root,
  nodes: [
    root,
    queueNode(
      BASE_HEADER.toString("hex"),
      root.headerHash,
      S_HEADER.toString("hex"),
      `${hex("base-tx")}#0`,
    ),
    queueNode(
      S_HEADER.toString("hex"),
      BASE_HEADER.toString("hex"),
      withChild ? CHILD.toString("hex") : undefined,
      out,
    ),
    ...(withChild
      ? [
          queueNode(
            CHILD.toString("hex"),
            S_HEADER.toString("hex"),
            undefined,
            childOut,
          ),
        ]
      : []),
  ],
});
const noTtl = (spent: string) => {
  const [id, index] = spent.split("#");
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(id!), BigInt(index!)),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  const hash = CML.hash_transaction(body).to_hex();
  const tx = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const cbor = Buffer.from(tx.to_cbor_hex(), "hex");
  tx.free();
  return { hash, cbor };
};
export type ScenarioOptions = {
  stop?:
    | "before replacement"
    | "before compensation CAS"
    | "after compensation CAS"
    | "before SQL receipt";
  branch?: "original winner" | "full chain";
  bad?:
    | "future TTL"
    | "missing TTL"
    | "coverage"
    | "input"
    | "signed input"
    | "canonical suffix"
    | "original root"
    | "unknown root";
  beforeInitialCas?: boolean;
  removedSuffix?: boolean;
};

/** Real preparation/SQL/codec/receipts; only authenticated source transport and
 * native CAS are modelled. Fault triggers roll back actual SQL transactions. */
export const compensationScenario = (
  fixture: Fixture,
  options: ScenarioOptions = {},
) => {
  const owner = ownerModel(CHILD_ROOT);
  return onNode(
    owner,
    (node) =>
      Effect.gen(function* () {
        const commit =
          options.bad === "missing TTL"
            ? noTtl(S_OUT)
            : signedCommit(
                options.bad === "signed input"
                  ? `${hex("wrong-signed-input")}#0`
                  : S_OUT,
                options.bad === "future TTL" ? 2500 : TTL + 5,
              );
        yield* journal(
          W_HEADER,
          Pending.Status.Abandoned,
          W_COMMIT,
          2_000_000,
          { abandonment: "replacement" },
        );
        yield* journal(
          S_HEADER,
          Pending.Status.LocallyApplied,
          S_COMMIT,
          3_000_000,
        );
        yield* sql(
          (sql) =>
            sql`UPDATE pending_block_finalizations SET expected_utxos_root = ${PREFIX_ROOT} WHERE header_hash = ${S_HEADER}`,
        );
        yield* withNativeReplay(S_HEADER);
        yield* insertJournal({
          header: CHILD,
          status: Pending.Status.LocallyApplied,
          commit,
          baseOut: S_OUT,
          baseHeader: S_HEADER,
          createdAt: new Date(3_500_000),
        });
        yield* sql(
          (sql) =>
            sql`UPDATE pending_block_finalizations SET base_utxos_root = ${PREFIX_ROOT}, expected_utxos_root = ${CHILD_ROOT}, block_end_time = block_start_time + INTERVAL '1 second' WHERE header_hash = ${CHILD}`,
        );
        yield* withNativeReplay(CHILD);
        yield* observer([
          { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
        ]);
        fixture.queue = wHoldsTheSlot;
        fixture.coverage = deep(W_NODE_TX);
        if (options.beforeInitialCas)
          owner.beforeRestore = async () => {
            throw new Error("initial before CAS");
          };
        else
          owner.afterRestore = async () => {
            throw new Error("initial after CAS");
          };
        const initial = yield* revival(node);
        let original = yield* plans;
        if (options.bad === "original root") {
          const document = JSON.parse(original[0]!.intent) as Record<
            string,
            unknown
          >;
          document.expectedRoot = "ff".repeat(32);
          const identity = eventHistoryCanonicalJson(document);
          yield* sql(
            (sql) =>
              sql`UPDATE event_history_recovery_plans SET recovery_id = ${Buffer.from(sha(identity), "hex")}, intent = ${identity} WHERE state = 'prepared'`,
          );
          original = yield* plans;
        }
        owner.beforeRestore = undefined;
        owner.afterRestore = undefined;
        const currentOut =
          options.bad === "input" ? `${hex("changed-live-input")}#0` : S_OUT;
        yield* observer([
          { headerHash: S_HEADER.toString("hex"), outRef: currentOut },
        ]);
        fixture.queue = returned(currentOut) as never;
        fixture.coverage =
          options.bad === "coverage"
            ? "unavailable"
            : deep(
                S_TX,
                options.bad === "canonical suffix" ? { [commit.hash]: 7 } : {},
              );
        if (options.removedSuffix) {
          const removal = suffixRemoval(CHILD);
          yield* observer(
            removal.previousQueue.slice(1).map((entry) => ({
              headerHash: entry.headerHash!,
              outRef: entry.outRef,
            })),
          );
          yield* admitSuffixRemoval(removal.checkpoint);
          fixture.queue = returned(removal.nextQueue.at(-1)!.outRef) as never;
          fixture.coverage = deep(removal.tx, { [commit.hash]: 4 });
        }
        if (options.bad === "unknown root") owner.durableRoot = "ff".repeat(32);
        if (options.stop === "before replacement")
          yield* sql((sql) =>
            sql.unsafe(
              `CREATE OR REPLACE FUNCTION histprefix_stop() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'stop before replacement'; END $$`,
            ),
          ).pipe(
            Effect.zipRight(
              sql((sql) =>
                sql.unsafe(
                  `CREATE TRIGGER histprefix_stop BEFORE DELETE ON event_history_recovery_plans FOR EACH ROW EXECUTE FUNCTION histprefix_stop()`,
                ),
              ),
            ),
          );
        if (options.stop === "before SQL receipt")
          yield* sql((sql) =>
            sql.unsafe(
              `CREATE OR REPLACE FUNCTION histprefix_stop() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.state = 'applied' AND NEW.intent LIKE '%displacement-compensation%' THEN RAISE EXCEPTION 'stop before SQL receipt'; END IF; RETURN NEW; END $$`,
            ),
          ).pipe(
            Effect.zipRight(
              sql((sql) =>
                sql.unsafe(
                  `CREATE TRIGGER histprefix_stop BEFORE UPDATE ON event_history_recovery_plans FOR EACH ROW EXECUTE FUNCTION histprefix_stop()`,
                ),
              ),
            ),
          );
        const sqlService = yield* SqlClient.SqlClient;
        let nativeBoundaryPlans:
          | Awaited<Effect.Effect.Success<typeof plans>>
          | undefined;
        owner.beforeRestore = async () => {
          nativeBoundaryPlans = await Effect.runPromise(
            plans.pipe(Effect.provideService(SqlClient.SqlClient, sqlService)),
          );
          if (options.stop === "before compensation CAS")
            throw new Error("stop before compensation CAS");
        };
        if (options.stop === "after compensation CAS")
          owner.afterRestore = async () => {
            throw new Error("stop after compensation CAS");
          };
        const interrupted = yield* revival(node);
        const intermediate = yield* plans;
        const intermediateLedger = yield* ledgerRoot;
        const intermediateNative = owner.durableRoot;
        if (
          options.stop === "before replacement" ||
          options.stop === "before SQL receipt"
        )
          yield* sql((sql) =>
            sql.unsafe(
              `DROP TRIGGER histprefix_stop ON event_history_recovery_plans`,
            ),
          );
        owner.beforeRestore = undefined;
        owner.afterRestore = undefined;
        if (options.branch === "original winner") {
          yield* observer([
            { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
          ]);
          fixture.queue = wHoldsTheSlot;
          fixture.coverage = deep(W_NODE_TX);
        } else if (options.branch === "full chain") {
          yield* observer([
            { headerHash: S_HEADER.toString("hex"), outRef: S_OUT },
            { headerHash: CHILD.toString("hex"), outRef: childOut },
          ]);
          fixture.queue = returned(S_OUT, true) as never;
          fixture.coverage = deep(childOut.slice(0, 64), { [S_TX]: 4 });
        }
        const again = yield* revival(node);
        return {
          initial,
          original,
          interrupted,
          intermediate,
          intermediateLedger,
          intermediateNative,
          nativeBoundaryPlans,
          again,
          final: yield* plans,
          ledger: yield* ledgerRoot,
          native: owner.durableRoot,
          operations: owner.operations,
          replays: owner.recovers,
          w: yield* statusOf(W_HEADER),
          s: yield* statusOf(S_HEADER),
          child: yield* statusOf(CHILD),
          removed: yield* correctionRewindRemovedHeaders(authority.manifestId),
        };
      }),
    CHILD_ROOT,
  );
};
