/**
 * A pending journal is prepared and signed only under the state-queue
 * mutation lease it names, while that lease is the scope's active, unexpired
 * one (`holdMutationLease`). An unsigned active journal a later lease holder
 * finds was therefore never signed and never will be: the next commit tick
 * abandons it and clears its events' projected header
 * (`abandonUnsignedActiveJournal`). A journal holding a signed or submitted
 * transaction is never abandoned there: its commit may be on L1.
 */
import "./utils.js";

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
import { abandonUnsignedActiveJournal } from "../src/fibers/block-commitment.abandon-unsigned-journal.js";
import { makeDepositEntry } from "./database.test/fixtures.events.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { insertDeposits } from "./helpers/event-rows.js";
import {
  prepareUnderTestLease,
  withTestMutationLease,
} from "./helpers/mutation-lease.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const Journals = PendingBlockFinalizationsDB;

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
    yield* insertDeposits([deposit]);
    const headerHash = header(label);
    yield* prepareUnderTestLease({
      ...journalFixture(headerHash),
      ...(preparedTxHash === undefined ? {} : { preparedTxHash }),
      depositEventIds: [depositId],
      depositEntries: [deposit],
    });
    yield* DepositsDB.markAwaitingAsProjected([depositId]);
    yield* DepositsDB.markProjectedByEventIds([depositId], headerHash);
    return { headerHash, depositId };
  });

const projectedHeaderOf = (depositId: Buffer) =>
  DepositsDB.retrieveByEventId(depositId).pipe(
    Effect.map(
      (entry) =>
        Option.getOrThrow(entry)[DepositsDB.Columns.PROJECTED_HEADER_HASH],
    ),
  );

const journalStatus = (headerHash: Buffer) =>
  Journals.retrieveByHeaderHash(headerHash).pipe(
    Effect.map((record) =>
      Option.map(record, (value) => value[Journals.Columns.STATUS]),
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
            status: yield* journalStatus(journal.headerHash),
          };
        }),
      );
      leaseRefusal(result.outcome);
      expect(Option.isNone(result.status)).toBe(true);
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

describe("abandoning an unsigned active journal", () => {
  it("abandons it, clears its events' projected header, and lets the next header prepare", async () => {
    const result = await run(
      Effect.gen(function* () {
        const { headerHash, depositId } = yield* journalWithDeposit("unsigned");
        const projectedBefore = yield* projectedHeaderOf(depositId);
        const abandoned = yield* abandonUnsignedActiveJournal;
        const next = yield* Effect.either(
          prepareUnderTestLease(journalFixture(header("next-header"))),
        );
        return {
          headerHex: headerHash.toString("hex"),
          projectedBefore,
          abandoned,
          status: yield* journalStatus(headerHash),
          projectedAfter: yield* projectedHeaderOf(depositId),
          next: next._tag,
          active: Option.map(
            yield* Journals.retrieveActive(),
            (record) => record[Journals.Columns.HEADER_HASH],
          ),
        };
      }),
    );
    expect(result.projectedBefore?.toString("hex")).toBe(result.headerHex);
    expect(result.abandoned).toBe(result.headerHex);
    expect(result.status).toEqual(Option.some(Journals.Status.Abandoned));
    expect(result.projectedAfter).toBeNull();
    expect(result.next).toBe("Right");
    expect(result.active).toEqual(Option.some(header("next-header")));
  });

  it.each(["intended", "submitted"] as const)(
    "never abandons a journal whose %s transaction hash is set",
    async (signed) => {
      const result = await run(
        Effect.gen(function* () {
          const { cbor, txHash } = signedTx();
          const { headerHash, depositId } = yield* journalWithDeposit(
            `signed-${signed}`,
            txHash,
          );
          yield* signed === "intended"
            ? Journals.recordSignedIntent(headerHash, txHash, cbor).pipe(
                withTestMutationLease(
                  journalFixture(headerHash).metadata.stateQueueLeaseToken,
                ),
              )
            : Journals.markSubmitted(headerHash, txHash);
          const statusBefore = yield* journalStatus(headerHash);
          return {
            headerHex: headerHash.toString("hex"),
            abandoned: yield* abandonUnsignedActiveJournal,
            statusBefore,
            statusAfter: yield* journalStatus(headerHash),
            projectedAfter: yield* projectedHeaderOf(depositId),
          };
        }),
      );
      expect(result.abandoned).toBeUndefined();
      expect(result.statusAfter).toEqual(result.statusBefore);
      expect(result.statusAfter).not.toEqual(
        Option.some(Journals.Status.Abandoned),
      );
      expect(result.projectedAfter?.toString("hex")).toBe(result.headerHex);
    },
  );
});
