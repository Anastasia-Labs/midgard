/**
 * A pending journal is prepared and signed only under the state-queue
 * mutation lease it names, while that lease is the scope's active, unexpired
 * one (`holdMutationLease`). An unsigned journal a later lease holder finds
 * was therefore never signed and never will be: the next commit tick
 * discards it (`discardStoppedUnsignedJournals`), and its events stay
 * unassigned, since only a landing assigns them a header. A journal that
 * recorded its signed intent is never discarded there: its commit may be
 * on L1.
 */
import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Either, Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  DepositsDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "../src/database/index.js";
import { MUTATION_LEASE_NOT_HELD } from "../src/database/pendingBlockFinalizations.hold-mutation-lease.js";
import { discardStoppedUnsignedJournals } from "../src/fibers/block-commitment.discard-unsigned-journals.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { makeDepositEntry } from "./database.test/fixtures.events.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { insertDeposits } from "./helpers/event-rows.js";
import { simCommitAnchor } from "./helpers/landed-blocks-sim.own.js";
import {
  prepareUnderTestLease,
  withTestMutationLease,
} from "./helpers/mutation-lease.js";
import { header, journalFixture } from "./helpers/own-journal-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const Journals = PendingBlockFinalizationsDB;

/** A prepare whose journal is anchored on the follower's chain, so it is
 * live until it lands or dies. */
const anchored = { beforeJournalInsert: Effect.orDie(simCommitAnchor) };

/** The fixture journals' base is the frontier, so their blocks can land. */
const baseOnChain = Frontier.upsert({
  headerHash: header("base-tail").toString("hex"),
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
});

const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* effect.pipe(
          Effect.ensuring(Effect.orDie(resetApplicationTables)),
        );
      }),
    ) as Effect.Effect<A, unknown, never>,
  );

/** A signed transaction and its body hash. */
const signedTx = () => {
  const cbor = Buffer.from(makeCardanoSignedMapOutputTxBytes());
  const tx = CML.Transaction.from_cbor_bytes(cbor);
  const body = tx.body();
  const hash = CML.hash_transaction(body);
  const txHash = Buffer.from(hash.to_hex(), "hex");
  hash.free();
  body.free();
  tx.free();
  return { cbor, txHash };
};

/** A journal for `label` with one deposit member projected under it. */
const journalWithDeposit = (label: string, preparedTxHash?: Buffer) =>
  Effect.gen(function* () {
    const deposit = makeDepositEntry();
    const depositId = deposit[DepositsDB.Columns.ID];
    yield* baseOnChain;
    yield* insertDeposits([deposit]);
    const headerHash = header(label);
    yield* prepareUnderTestLease(
      {
        ...journalFixture(headerHash),
        ...(preparedTxHash === undefined ? {} : { preparedTxHash }),
        depositEventIds: [depositId],
        depositEntries: [deposit],
      },
      anchored,
    );
    yield* DepositsDB.markAwaitingAsProjected([depositId]);
    return { headerHash, depositId };
  });

const projectedHeaderOf = (depositId: Buffer) =>
  DepositsDB.retrieveByEventId(depositId).pipe(
    Effect.map(
      (entry) =>
        Option.getOrThrow(entry)[DepositsDB.Columns.PROJECTED_HEADER_HASH],
    ),
  );

/** Writes `token`'s lease row as `status`, its expiry `expiresInMs` away. */
const leaseRow = (
  token: string,
  status: "active" | "released",
  expiresInMs: number,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO state_queue_mutation_leases
      (token, scope, holder, status, expires_at, released_at)
      VALUES (${token}, 'state_queue', 'lease-test', ${status},
        NOW() + make_interval(secs => ${expiresInMs / 1000}),
        ${status === "released" ? sql`NOW()` : null})`;
  });

const endLease = (token: string, ending: "released" | "expired") =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* ending === "released"
      ? sql`UPDATE state_queue_mutation_leases
          SET status = 'released', released_at = NOW() WHERE token = ${token}`
      : sql`UPDATE state_queue_mutation_leases
          SET expires_at = NOW() - INTERVAL '1 second' WHERE token = ${token}`;
  });

const leaseRefusal = (outcome: Either.Either<unknown, unknown>) => {
  expect(Either.isLeft(outcome)).toBe(true);
  expect(Either.isLeft(outcome) ? outcome.left : undefined).toMatchObject({
    _tag: "DatabaseError",
    message: MUTATION_LEASE_NOT_HELD,
  });
};

describe("pending journal writes under the state-queue mutation lease", () => {
  it.each(["released", "expired"] as const)(
    "refuses to prepare under a %s lease, writing no journal",
    async (ending) => {
      const result = await run(
        Effect.gen(function* () {
          const journal = journalFixture(header(`prepare-${ending}`));
          const token = journal.metadata.stateQueueLeaseToken;
          yield* ending === "released"
            ? leaseRow(token, "released", 60_000)
            : leaseRow(token, "active", -1_000);
          return {
            outcome: yield* Effect.either(
              Journals.preparePendingSubmission(journal),
            ),
            journal: yield* Journals.retrieveByHeaderHash(journal.headerHash),
          };
        }),
      );
      leaseRefusal(result.outcome);
      expect(Option.isNone(result.journal)).toBe(true);
    },
  );

  it.each(["released", "expired"] as const)(
    "refuses to record a signed intent once the journal's lease is %s, and records it while held",
    async (ending) => {
      const result = await run(
        Effect.gen(function* () {
          const { cbor, txHash } = signedTx();
          const journal = {
            ...journalFixture(header(`sign-${ending}`)),
            preparedTxHash: txHash,
          };
          const token = journal.metadata.stateQueueLeaseToken;
          yield* leaseRow(token, "active", 3_600_000);
          yield* Journals.preparePendingSubmission(journal);
          yield* endLease(token, ending);
          const refused = yield* Effect.either(
            Journals.recordSignedIntent(journal.headerHash, txHash, cbor),
          );
          const afterRefusal = Option.getOrThrow(
            yield* Journals.retrieveByHeaderHash(journal.headerHash),
          )[Journals.Columns.INTENDED_TX_HASH];
          // The same write under a held lease goes through.
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE state_queue_mutation_leases
            SET status = 'active', released_at = NULL,
              expires_at = NOW() + INTERVAL '1 hour'
            WHERE token = ${token}`;
          yield* Journals.recordSignedIntent(journal.headerHash, txHash, cbor);
          const afterHeld = Option.getOrThrow(
            yield* Journals.retrieveByHeaderHash(journal.headerHash),
          )[Journals.Columns.INTENDED_TX_HASH];
          return { refused, afterRefusal, afterHeld, txHash };
        }),
      );
      leaseRefusal(result.refused);
      expect(result.afterRefusal).toBeNull();
      expect(result.afterHeld).toEqual(result.txHash);
    },
  );

  it("grants the scope's lease to exactly one of two concurrent acquirers", async () => {
    const result = await run(
      Effect.gen(function* () {
        const outcomes = yield* Effect.all(
          [
            StateQueueMutationLeasesDB.tryAcquire({ holder: "first" }),
            StateQueueMutationLeasesDB.tryAcquire({ holder: "second" }),
          ],
          { concurrency: "unbounded" },
        );
        const sql = yield* SqlClient.SqlClient;
        const [active] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
          FROM state_queue_mutation_leases WHERE status = 'active'`;
        return {
          tags: outcomes.map((outcome) => outcome._tag).sort(),
          active: active?.n,
        };
      }),
    );
    expect(result.tags).toEqual(["Acquired", "Busy"]);
    expect(result.active).toBe(1);
  });
});

describe("discarding the unsigned journals a later lease holder finds", () => {
  it("discards an unsigned journal, leaves its events unassigned, and lets the next header prepare", async () => {
    const next = journalFixture(header("next-header"));
    const result = await run(
      Effect.gen(function* () {
        const { headerHash, depositId } = yield* journalWithDeposit("unsigned");
        // While it is live, it refuses every other header's preparation.
        const refused = yield* Effect.either(
          prepareUnderTestLease(next, anchored),
        );
        const discarded = yield* discardStoppedUnsignedJournals;
        return {
          headerHex: headerHash.toString("hex"),
          refused,
          discarded,
          journal: yield* Journals.retrieveByHeaderHash(headerHash),
          projectedAfter: yield* projectedHeaderOf(depositId),
          prepared: yield* prepareUnderTestLease(next, anchored),
          live: Option.map(
            yield* Journals.retrieveLive(),
            (record) => record[Journals.Columns.HEADER_HASH],
          ),
        };
      }),
    );
    expect(Either.isLeft(result.refused)).toBe(true);
    expect(
      Either.isLeft(result.refused) ? result.refused.left : undefined,
    ).toMatchObject({ message: Journals.LIVE_JOURNAL_REFUSAL });
    expect(result.discarded).toEqual([result.headerHex]);
    expect(Option.isNone(result.journal)).toBe(true);
    expect(result.projectedAfter).toBeNull();
    expect(result.prepared).toEqual({ kind: "prepared" });
    expect(result.live).toEqual(Option.some(next.headerHash));
  });

  it("never discards a journal that recorded its signed intent", async () => {
    const result = await run(
      Effect.gen(function* () {
        const { cbor, txHash } = signedTx();
        const { headerHash } = yield* journalWithDeposit("signed", txHash);
        yield* Journals.recordSignedIntent(headerHash, txHash, cbor).pipe(
          withTestMutationLease(
            journalFixture(headerHash).metadata.stateQueueLeaseToken,
          ),
        );
        const statusBefore =
          yield* Journals.retrieveStatusByHeaderHash(headerHash);
        return {
          txHash,
          discarded: yield* discardStoppedUnsignedJournals,
          statusBefore,
          statusAfter: yield* Journals.retrieveStatusByHeaderHash(headerHash),
          journal: yield* Journals.retrieveByHeaderHash(headerHash),
        };
      }),
    );
    expect(result.discarded).toEqual([]);
    expect(result.statusBefore).toBe(Journals.OwnBlockStatus.Unlanded);
    expect(result.statusAfter).toBe(result.statusBefore);
    expect(
      Option.map(
        result.journal,
        (row) => row[Journals.Columns.INTENDED_TX_HASH],
      ),
    ).toEqual(Option.some(result.txHash));
  });
});
