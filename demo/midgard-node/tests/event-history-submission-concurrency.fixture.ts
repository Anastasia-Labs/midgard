import { SqlClient } from "@effect/sql";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { provideDatabaseLayers } from "./utils.js";

export const query = <A extends object>(
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<readonly A[], unknown>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(Effect.flatMap(SqlClient.SqlClient, statement)),
  );

export const until = async (
  condition: () => Promise<boolean>,
  what: string,
) => {
  const deadline = Date.now() + 120_000;
  while (!(await condition())) {
    if (Date.now() > deadline) throw new Error(`Timed out until ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
};

export const journal = async (submissionId: string) =>
  Effect.runPromise(provideDatabaseLayers(Journal.retrieve(submissionId)));

/** `lucid` whose reads of `wallet` run `before` and `after` around the read,
 * numbered from 1: the first is a fresh submission's nonce choice, each
 * later one a funding. Every read still returns the emulator's view. */
export const gateWalletReads = (
  lucid: LucidEvolution,
  wallet: string,
  gate: {
    readonly before?: (read: number) => Promise<void>;
    readonly after?: (read: number, view: readonly UTxO[]) => Promise<void>;
  },
): LucidEvolution => {
  let reads = 0;
  return {
    ...lucid,
    utxosAt: async (address: Parameters<LucidEvolution["utxosAt"]>[0]) => {
      if (address !== wallet) return lucid.utxosAt(address);
      const read = ++reads;
      await gate.before?.(read);
      const view = await lucid.utxosAt(address);
      await gate.after?.(read, view);
      return view;
    },
  };
};

/** Some session of the test database waits on an advisory lock. */
export const waitingOnLock = async () =>
  (
    await query<{ waiting: number }>(
      (sql) => sql`SELECT count(*)::int AS waiting FROM pg_locks
        WHERE locktype = 'advisory' AND NOT granted AND database =
          (SELECT oid FROM pg_database WHERE datname = current_database())`,
    )
  )[0]!.waiting > 0;
