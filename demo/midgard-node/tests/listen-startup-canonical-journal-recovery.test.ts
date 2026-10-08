import { SqlClient } from "@effect/sql";
import { Effect, Exit, Option } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { recoverCanonicalJournalsOnStartup } from "../src/commands/listen-startup.seed-latest-local-block-boundary-on-startup.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  type CanonicalCommittedHeader,
  withCanonicalHeaderJournals,
} from "../src/services/canonical-journal-recovery.js";
import {
  BASE_HEADER,
  BASE_OUT,
  bytes,
  E,
  E_HEADER,
  hex,
  insertJournal,
  run,
  seed,
  signedCommit,
  TTL,
  UTXOS_ROOT,
} from "./helpers/journal-recovery-sql-model.js";

/**
 * Startup's recovery over the journals of the canonical queue. It runs once,
 * before any fiber, so a refusal that escapes it exits the process on every
 * restart until the chain or the indexer moves.
 */

const C = Pending.Columns;
const NEW_E_HEADER = bytes("new-e-header", 28);
const CONTINUED_OUT = `${hex("attested-tx")}#0`;
const OTHER_HEADER = bytes("other-active-header", 28);
const TIP_END_MS = 1_500_000;

/** One deposit member, which makes the journal payload-bearing. */
const withDepositMember = (header: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO pending_block_finalization_deposits ${sql.insert({
      header_hash: header,
      member_id: bytes(`deposit:${header.toString("hex")}`),
      ordinal: 0,
      payload_cbor: Buffer.from("a0", "hex"),
      payload_sha256: bytes(`deposit-sha:${header.toString("hex")}`),
      source_table: "deposits_utxos",
      source_id: bytes(`deposit:${header.toString("hex")}`),
      source_time_stamp_tz: new Date(0),
    } as never)}`;
  });

/** The shared SQL model writes an empty block window; a loadable journal
 * needs a positive one. */
const positiveWindows = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) => sql`UPDATE pending_block_finalizations
    SET block_end_time = block_start_time + INTERVAL '1 second'`,
);

const readStatus = (header: Buffer) =>
  Effect.map(
    Pending.retrieveByHeaderHash(header),
    (journal) => Option.getOrUndefined(journal)?.[C.STATUS],
  );

const queueOf = (...headers: readonly Buffer[]) =>
  withCanonicalHeaderJournals(
    headers.map((headerHash) => ({ headerHash, endTimeMs: TIP_END_MS })),
  );

const recover = (
  canonicalHeaders: readonly CanonicalCommittedHeader[],
  latestHeaderHash: Option.Option<Buffer>,
) =>
  Effect.exit(
    recoverCanonicalJournalsOnStartup({
      canonicalHeaders,
      latestHeaderHash,
      latestEndTimeMs: TIP_END_MS,
    }),
  );

beforeEach(async () => {
  await run(seed);
});

describe("startup over a payload-bearing abandoned tip while another journal is active", () => {
  /** E is the canonical tip, abandoned with its abandonment unattributed;
   * another payload-bearing journal holds the single active slot. */
  const seedAbandonedTipBehindActive = Effect.gen(function* () {
    yield* insertJournal({
      header: E_HEADER,
      status: Pending.Status.Abandoned,
      commit: E,
      baseOut: BASE_OUT,
      baseHeader: BASE_HEADER,
      createdAt: new Date(2_000_000),
    });
    yield* insertJournal({
      header: OTHER_HEADER,
      status: Pending.Status.SubmittedUnconfirmed,
      commit: signedCommit(CONTINUED_OUT, TTL + 100),
      baseOut: CONTINUED_OUT,
      baseHeader: BASE_HEADER,
      createdAt: new Date(3_000_000),
    });
    yield* withDepositMember(E_HEADER);
    yield* withDepositMember(OTHER_HEADER);
    yield* positiveWindows;
  });

  it("leaves the tip abandoned behind the active journal and seeds the boundary from it", async () => {
    const exit = await run(
      Effect.gen(function* () {
        yield* seedAbandonedTipBehindActive;
        return yield* recover(yield* queueOf(E_HEADER), Option.some(E_HEADER));
      }),
    );
    // The tip journal's window ends at 2_000_000 + 1 s.
    expect(exit).toEqual(Exit.succeed(2_001_000));
    expect(await run(readStatus(E_HEADER))).toBe(Pending.Status.Abandoned);
    expect(await run(readStatus(OTHER_HEADER))).toBe(
      Pending.Status.SubmittedUnconfirmed,
    );
  });

  it("revives the tip through the guarded revival once nothing is active", async () => {
    const exit = await run(
      Effect.gen(function* () {
        yield* seedAbandonedTipBehindActive;
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE pending_block_finalizations SET status = ${Pending.Status.Abandoned}
          WHERE header_hash = ${OTHER_HEADER}`;
        return yield* recover(yield* queueOf(E_HEADER), Option.some(E_HEADER));
      }),
    );
    expect(exit).toEqual(Exit.succeed(2_001_000));
    expect(await run(readStatus(E_HEADER))).toBe(
      Pending.Status.ObservedWaitingStability,
    );
  });
});

describe("startup over a replaced block reported on the queue", () => {
  /** E was replaced by NEW_E on the other incarnation of the same base, and
   * NEW_E reached `sibling`. */
  const seedReplacedPair = (sibling: Pending.Status) =>
    Effect.gen(function* () {
      yield* insertJournal({
        header: E_HEADER,
        status: Pending.Status.Abandoned,
        commit: E,
        baseOut: BASE_OUT,
        baseHeader: BASE_HEADER,
        createdAt: new Date(2_000_000),
        abandonment: "replacement",
      });
      yield* insertJournal({
        header: NEW_E_HEADER,
        status: sibling,
        commit: signedCommit(CONTINUED_OUT, TTL + 100),
        baseOut: CONTINUED_OUT,
        baseHeader: BASE_HEADER,
        createdAt: new Date(3_000_000),
        ...(sibling === Pending.Status.Abandoned && {
          abandonment: "replacement" as const,
        }),
      });
      yield* withDepositMember(E_HEADER);
      yield* positiveWindows;
      const sql = yield* SqlClient.SqlClient;
      yield* sql`INSERT INTO mpf_engine_state (store_name, migration_version, root_hex)
        VALUES ('ledger', 0, ${UTXOS_ROOT})
        ON CONFLICT (store_name) DO UPDATE SET root_hex = EXCLUDED.root_hex`;
    });

  it.each([Pending.Status.LocallyApplied, Pending.Status.Abandoned])(
    "leaves it abandoned for the landed-block rebase, raising nothing, when its replacement is %s",
    async (sibling) => {
      const exit = await run(
        Effect.gen(function* () {
          yield* seedReplacedPair(sibling);
          return yield* recover(
            yield* queueOf(E_HEADER),
            Option.some(E_HEADER),
          );
        }),
      );
      expect(exit).toEqual(Exit.succeed(2_001_000));
      expect(await run(readStatus(E_HEADER))).toBe(Pending.Status.Abandoned);
      expect(await run(readStatus(NEW_E_HEADER))).toBe(sibling);
    },
  );

  it("still fails startup on a failure", async () => {
    const exit = await run(
      Effect.gen(function* () {
        yield* seedReplacedPair(Pending.Status.Abandoned);
        const canonicalHeaders = yield* queueOf(E_HEADER);
        // A transient: the tip journal's read is refused (the replaced
        // block itself is left to the landed-block rebase without a read).
        const sql = yield* SqlClient.SqlClient;
        yield* sql`ALTER TABLE pending_block_finalizations RENAME TO lf_job_hidden`;
        const outcome = yield* recover(canonicalHeaders, Option.some(E_HEADER));
        yield* sql`ALTER TABLE lf_job_hidden RENAME TO pending_block_finalizations`;
        return outcome;
      }),
    );
    expect(Exit.isFailure(exit)).toBe(true);
  });
});
