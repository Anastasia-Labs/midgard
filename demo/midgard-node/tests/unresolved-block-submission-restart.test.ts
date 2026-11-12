import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { hydratePendingBlockFinalizationOnStartup } from "../src/commands/listen-startup.js";
import { classifyCommitWorkerOutputAgainstLeaseJournal } from "../src/fibers/commit-worker-failure-classification.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { Database } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import {
  BASE_HEADER,
  BASE_OUT,
  E,
  E_HEADER,
  insertJournal,
  run,
  seed,
  UTXOS_ROOT,
} from "./helpers/journal-recovery-sql-model.js";
import { simCommitAnchor } from "./helpers/landed-blocks-sim.own.js";
import {
  insertCommitIntent,
  land,
  ownLandedRow,
} from "./helpers/own-block-facts.js";
import { readyz } from "./readiness-honest-degradation-route.readyz-fixture.js";
import { provideDatabaseLayers } from "./utils.js";

/**
 * The unresolved block submission (`UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH` and
 * its `_SINCE_MS`): a signed commit whose block has neither landed nor died.
 * A restart rebuilds it from the live journal: unresolved while a signed
 * commit may be on L1 unobserved, never for a block that landed. A failed
 * commit attempt sets it the same way from its lease's journal rows.
 */

/** Ten minutes before the test runs: older than any readiness bound. */
const UPDATED_AT = new Date(Date.now() - 600_000);
const TX_HASH = Buffer.from(E.hash, "hex");

/** What became of the journal's block. */
type Fate = "unlanded" | "landed" | "applied" | "dead";

/**
 * The submission refs a restart must hydrate for one durable journal: a
 * signed commit whose block neither landed nor died is unresolved.
 */
const JOURNALS: readonly {
  readonly fate: Fate;
  /** The journal recorded its signed commit. */
  readonly signed: boolean;
  /** A signed commit may be on L1 and is not yet observed. */
  readonly unresolved: boolean;
}[] = [
  // Prepared, never signed: nothing can be on L1.
  { fate: "unlanded", signed: false, unresolved: false },
  // Signed and recorded before its broadcast: it may be on L1 until its
  // block lands or dies.
  { fate: "unlanded", signed: true, unresolved: true },
  // Landed: the follower observed the commit; only its application, or its
  // fold, is outstanding.
  { fate: "landed", signed: true, unresolved: false },
  { fate: "applied", signed: true, unresolved: false },
  // Its signed commit's intent was pruned: its block can no longer land.
  { fate: "dead", signed: true, unresolved: false },
];

const LANDED = JOURNALS.find(({ fate }) => fate === "landed")!;

const writeJournal = (journal: (typeof JOURNALS)[number]) =>
  Effect.gen(function* () {
    // The journal's base is the frontier, so its block can land.
    yield* Frontier.upsert({
      headerHash: BASE_HEADER.toString("hex"),
      utxosRoot: UTXOS_ROOT,
    });
    yield* insertJournal({
      header: E_HEADER,
      commit: journal.signed ? E : undefined,
      anchor: yield* simCommitAnchor,
      baseOut: BASE_OUT,
      baseHeader: BASE_HEADER,
      createdAt: UPDATED_AT,
    });
    if (journal.signed && journal.fate !== "dead")
      yield* insertCommitIntent(E_HEADER, TX_HASH, E.cbor);
    if (journal.fate === "landed" || journal.fate === "applied")
      yield* land(
        ownLandedRow(E_HEADER.toString("hex"), BASE_HEADER.toString("hex"), {
          applied: journal.fate === "applied",
        }),
      );
    const sql = yield* SqlClient.SqlClient;
    // The shared SQL model writes an empty block window; a loadable journal
    // needs a positive one.
    yield* sql`UPDATE pending_block_finalizations SET
      block_end_time = block_start_time + INTERVAL '1 second',
      block_time = block_start_time + INTERVAL '1 second'
      WHERE header_hash = ${E_HEADER}`;
  });

// Hydration reads no contract state from the fixture journal.
const CONTRACTS = MidgardContracts.make({
  stateQueue: { policyId: "00".repeat(28) },
} as never);

/** Startup hydration into `globals`, as a restarted node runs it. */
const hydrate = (globals: Globals) =>
  hydratePendingBlockFinalizationOnStartup.pipe(
    Effect.provideService(Globals, globals),
    Effect.provideService(MidgardContracts, CONTRACTS),
    Effect.provide(Database.layer),
  );

const submissionOf = (globals: Globals) =>
  Effect.all({
    txHash: Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
    sinceMs: Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS),
  });

const NO_SUBMISSION = { txHash: "", sinceMs: 0 };

/** `program` over fresh node globals and the test database. */
const withGlobals = <A, E, R>(
  program: (globals: Globals) => Effect.Effect<A, E, R>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.flatMap(Globals, program).pipe(Effect.provide(Globals.Default)),
    ) as Effect.Effect<A, E, never>,
  );

beforeEach(async () => {
  await run(seed);
});

describe("startup hydration of the unresolved block submission", () => {
  it.each(JOURNALS)(
    "rebuilds the unresolved submission of a $fate journal (signed: $signed)",
    async (journal) => {
      const submission = await withGlobals((globals) =>
        Effect.gen(function* () {
          yield* writeJournal(journal);
          yield* hydrate(globals);
          return yield* submissionOf(globals);
        }),
      );
      expect(submission).toEqual(
        journal.unresolved
          ? { txHash: E.hash, sinceMs: UPDATED_AT.getTime() }
          : NO_SUBMISSION,
      );
    },
  );

  it("reports no unresolved_block_submission on /readyz after a restart while the landed block waits for its application", async () => {
    const response = await readyz({
      journal: writeJournal(LANDED),
      globalsSetup: hydrate,
    });
    expect(
      response.reasons.filter((reason) =>
        reason.startsWith("unresolved_block_submission:"),
      ),
    ).toEqual([]);
  });
});

describe("a failed commit attempt", () => {
  /** A worker failure after the journal retained the commit's signed intent. */
  const RETAINED_FAILURE = {
    type: "FailureOutput",
    error: "provider timed out after the signed intent was journaled",
  } as const;

  it.each(JOURNALS)(
    "sets the unresolved submission a restart would for a $fate journal (signed: $signed)",
    async (journal) => {
      const { classified, submission } = await withGlobals((globals) =>
        Effect.gen(function* () {
          yield* writeJournal(journal);
          const classified =
            yield* classifyCommitWorkerOutputAgainstLeaseJournal(
              globals,
              RETAINED_FAILURE,
              `before-ttl:${E_HEADER.toString("hex").slice(0, 8)}`,
            ).pipe(Effect.either, Effect.provide(Database.layer));
          return { classified, submission: yield* submissionOf(globals) };
        }),
      );
      // Only a journal that never recorded a signed commit releases the
      // lease: a signed one may be on L1 whatever became of its block.
      expect(classified._tag).toBe(journal.signed ? "Left" : "Right");
      expect(submission).toEqual(
        journal.unresolved
          ? { txHash: E.hash, sinceMs: UPDATED_AT.getTime() }
          : NO_SUBMISSION,
      );
    },
  );
});
