import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  type CanonicalCommittedHeader,
  REVIVAL_BLOCKING_SIBLING_STATUSES,
  reviveEarliestCanonicalPayloadJournal,
  reviveReplacedCanonicalJournal,
  SignedIntentReplacementIntegrityError,
} from "../src/services/canonical-journal-recovery.js";
import {
  BASE_HEADER,
  BASE_OUT,
  bytes,
  E,
  E_HEADER,
  hex,
  insertJournal,
  ROOT_HEADER,
  run,
  seed,
  signedCommit,
  TTL,
  UTXOS_ROOT,
} from "./helpers/history-expired-intent-release-before-ttl.js";

/**
 * The sibling guards of a replaced journal's revival match the other
 * incarnation of its base node too. E was built on D's output D#1 and
 * replaced; its replacement NEW_E was built on D#2, D's output after another
 * transaction (a DA attestation) continued it, so NEW_E shares E's non-root
 * base header and utxos root but not its base output reference. A guard that
 * matched base output references alone would never see NEW_E.
 */

const C = Pending.Columns;
const CONTINUED_OUT = `${hex("attested-tx")}#0`;
const NEW_E_HEADER = bytes("new-e-header", 28);

const seedReplacedPair = (
  sibling: Pending.Status,
  baseHeader: Buffer = BASE_HEADER,
) =>
  Effect.gen(function* () {
    yield* insertJournal({
      header: E_HEADER,
      status: Pending.Status.Abandoned,
      commit: E,
      baseOut: BASE_OUT,
      baseHeader,
      createdAt: new Date(2_000_000),
      abandonment: "replacement",
    });
    yield* insertJournal({
      header: NEW_E_HEADER,
      status: sibling,
      commit: signedCommit(CONTINUED_OUT, TTL + 100),
      baseOut: CONTINUED_OUT,
      baseHeader,
      createdAt: new Date(3_000_000),
      ...(sibling === Pending.Status.Abandoned && {
        abandonment: "replacement" as const,
      }),
    });
    const sql = yield* SqlClient.SqlClient;
    // The shared SQL model writes an empty block window; a loadable journal
    // needs a positive one.
    yield* sql`UPDATE pending_block_finalizations
      SET block_end_time = block_start_time + INTERVAL '1 second'`;
    // The replacement moved the ledger marker to the shared base.
    yield* sql`INSERT INTO mpf_engine_state (store_name, migration_version, root_hex)
      VALUES ('ledger', 0, ${UTXOS_ROOT})
      ON CONFLICT (store_name) DO UPDATE SET root_hex = EXCLUDED.root_hex`;
  });

/** The errors a failed exit carries, outermost first. The fixture's history
 * write gate wraps a refusal in a DatabaseError whose cause it is. */
const failureChain = (exit: Exit.Exit<unknown, unknown>) => {
  const chain: Error[] = [];
  let value: unknown = Exit.isFailure(exit)
    ? Option.getOrUndefined(Cause.failureOption(exit.cause))
    : undefined;
  for (; value instanceof Error; value = value.cause) chain.push(value);
  return chain;
};

const integrityDetail = (exit: Exit.Exit<unknown, unknown>) => {
  const failure = failureChain(exit).find(
    (error) => error instanceof SignedIntentReplacementIntegrityError,
  );
  expect(failure).toBeDefined();
  return failure!.message;
};

const readStatus = (header: Buffer) =>
  Effect.map(
    Pending.retrieveByHeaderHash(header),
    (journal) => Option.getOrUndefined(journal)?.[C.STATUS],
  );

const siblingNamed = (status: Pending.Status) =>
  `block ${NEW_E_HEADER.toString("hex")} built on the same base is already ${status}`;

beforeEach(async () => {
  await run(seed);
});

describe("reviving a replaced journal whose replacement is on the other incarnation of its base", () => {
  it.each(REVIVAL_BLOCKING_SIBLING_STATUSES)(
    "is the integrity failure when the replacement is %s",
    async (status) => {
      const exit = await run(
        seedReplacedPair(status).pipe(
          Effect.zipRight(
            Effect.exit(reviveReplacedCanonicalJournal(E_HEADER)),
          ),
        ),
      );
      expect(integrityDetail(exit)).toContain(siblingNamed(status));
      expect(await run(readStatus(E_HEADER))).toBe(Pending.Status.Abandoned);
    },
  );

  it("refuses while the replacement is still active and unlanded", async () => {
    const exit = await run(
      seedReplacedPair(Pending.Status.SubmittedLocalFinalizationPending).pipe(
        Effect.zipRight(Effect.exit(reviveReplacedCanonicalJournal(E_HEADER))),
      ),
    );
    const refusal = failureChain(exit).find(
      ({ message }) =>
        message ===
        "A replaced journal is revived only after every sibling on its base is abandoned",
    );
    expect(
      String((refusal as { cause?: unknown } | undefined)?.cause),
    ).toContain(`sibling=${NEW_E_HEADER.toString("hex")}`);
    expect(await run(readStatus(E_HEADER))).toBe(Pending.Status.Abandoned);
  });

  it("revives it once the replacement is abandoned", async () => {
    const revived = await run(
      seedReplacedPair(Pending.Status.Abandoned).pipe(
        Effect.zipRight(reviveReplacedCanonicalJournal(E_HEADER)),
      ),
    );
    expect(revived[C.HEADER_HASH]).toEqual(E_HEADER);
    expect(await run(readStatus(E_HEADER))).toBe(
      Pending.Status.ObservedWaitingStability,
    );
  });
});

/** E's journal as reported on the queue, with a payload member so the
 * read-only recovery pass considers it. */
const reportedOnQueue = Effect.gen(function* () {
  const journal = yield* Pending.retrieveByHeaderHash(E_HEADER);
  if (Option.isNone(journal)) throw new Error("E's journal is missing");
  return [
    {
      headerHash: E_HEADER,
      endTimeMs: 2_000_000,
      journal: Option.some({
        ...journal.value,
        depositEventIds: [bytes("deposit")],
      }),
    },
  ] satisfies CanonicalCommittedHeader[];
});

const recoverFromQueue = Effect.flatMap(reportedOnQueue, (canonicalHeaders) =>
  Effect.exit(
    reviveEarliestCanonicalPayloadJournal({
      canonicalHeaders,
      logPrefix: "test",
    }),
  ),
);

describe("a replaced block reported on the queue after its other-incarnation replacement", () => {
  it("is the integrity failure once the replacement is locally finalized", async () => {
    const exit = await run(
      seedReplacedPair(Pending.Status.LocallyApplied).pipe(
        Effect.zipRight(recoverFromQueue),
      ),
    );
    expect(integrityDetail(exit)).toContain(
      siblingNamed(Pending.Status.LocallyApplied),
    );
  });

  it.each([
    { case: "the replacement is abandoned", status: Pending.Status.Abandoned },
    {
      case: "the shared base header is the root, which names no incarnation",
      status: Pending.Status.LocallyApplied,
      baseHeader: ROOT_HEADER,
    },
  ])("only warns when $case", async ({ status, baseHeader }) => {
    const exit = await run(
      seedReplacedPair(status, baseHeader).pipe(
        Effect.zipRight(recoverFromQueue),
      ),
    );
    expect(exit).toEqual(Exit.succeed(Option.none()));
  });
});
