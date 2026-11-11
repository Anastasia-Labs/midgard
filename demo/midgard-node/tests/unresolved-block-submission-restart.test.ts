import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { hydratePendingBlockFinalizationOnStartup } from "../src/commands/listen-startup.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { buildBlockConfirmationAction } from "../src/fibers/block-confirmation.js";
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
} from "./helpers/journal-recovery-sql-model.js";
import { readyz } from "./readiness-honest-degradation-route.readyz-fixture.js";
import { provideDatabaseLayers } from "./utils.js";

/**
 * The unresolved block submission (`UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH` and
 * its `_SINCE_MS`): a submitted block neither confirmed nor resolved. A
 * restart rebuilds it from the active journal: unresolved while a signed
 * commit may be on L1 unobserved, never for a commit the confirmation has
 * already observed.
 */

/** Ten minutes before the test runs: older than any readiness bound. */
const UPDATED_AT = new Date(Date.now() - 600_000);
const TX_HASH = Buffer.from(E.hash, "hex");

/**
 * The submission refs a restart must hydrate for one durable journal: a
 * signed commit not yet observed on L1 is unresolved.
 */
const JOURNALS: readonly {
  readonly status: Pending.Status;
  /** The journal retains its signed commit. */
  readonly signed: boolean;
  /** L1 accepted the commit (`markSubmitted`). */
  readonly submitted: boolean;
  /** A signed commit may be on L1 and is not yet observed. */
  readonly unresolved: boolean;
}[] = [
  // Prepared, never signed: nothing can be on L1.
  {
    status: Pending.Status.PendingSubmission,
    signed: false,
    submitted: false,
    unresolved: false,
  },
  // Signed and recorded before its broadcast: it may be on L1 until the
  // confirmation observes it or the rebase disposes of it.
  {
    status: Pending.Status.PendingSubmission,
    signed: true,
    submitted: false,
    unresolved: true,
  },
  {
    status: Pending.Status.SubmittedLocalFinalizationPending,
    signed: true,
    submitted: true,
    unresolved: true,
  },
  {
    status: Pending.Status.SubmittedUnconfirmed,
    signed: true,
    submitted: true,
    unresolved: true,
  },
  // Observed on L1: the confirmation cleared the submission; only the local
  // finalization is outstanding.
  {
    status: Pending.Status.ObservedWaitingStability,
    signed: true,
    submitted: true,
    unresolved: false,
  },
  {
    status: Pending.Status.LocallyApplied,
    signed: true,
    submitted: true,
    unresolved: false,
  },
  {
    status: Pending.Status.Abandoned,
    signed: true,
    submitted: false,
    unresolved: false,
  },
];

const SUBMITTED = JOURNALS.find(
  ({ status }) => status === Pending.Status.SubmittedUnconfirmed,
)!;
const OBSERVED = JOURNALS.find(
  ({ status }) => status === Pending.Status.ObservedWaitingStability,
)!;

const writeJournal = (journal: (typeof JOURNALS)[number]) =>
  Effect.gen(function* () {
    yield* insertJournal({
      header: E_HEADER,
      status: journal.status,
      commit: E,
      baseOut: BASE_OUT,
      baseHeader: BASE_HEADER,
      createdAt: UPDATED_AT,
    });
    const sql = yield* SqlClient.SqlClient;
    // The shared SQL model writes an empty block window; a loadable journal
    // needs a positive one.
    yield* sql`UPDATE pending_block_finalizations SET
      block_end_time = block_start_time + INTERVAL '1 second',
      block_time = block_start_time + INTERVAL '1 second',
      intended_tx_hash = ${journal.signed ? TX_HASH : null},
      signed_tx_cbor = ${journal.signed ? E.cbor : null},
      submitted_tx_hash = ${journal.submitted ? TX_HASH : null}
      WHERE header_hash = ${E_HEADER}`;
  });

// Hydration reads an observed journal's node back from its signed commit;
// the fixture commit creates none, so the node stays unknown.
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
    "rebuilds the unresolved submission of a $status journal (signed: $signed)",
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

  it("keeps merge held by the local finalization of an observed journal", async () => {
    const localFinalizationPending = await withGlobals((globals) =>
      Effect.gen(function* () {
        yield* writeJournal(OBSERVED);
        yield* hydrate(globals);
        return yield* Ref.get(globals.LOCAL_FINALIZATION_PENDING);
      }),
    );
    expect(localFinalizationPending).toBe(true);
  });

  it("reports no unresolved_block_submission on /readyz after a restart while the block waits for stability", async () => {
    const response = await readyz({
      journal: writeJournal(OBSERVED),
      globalsSetup: hydrate,
    });
    expect(
      response.reasons.filter((reason) =>
        reason.startsWith("unresolved_block_submission:"),
      ),
    ).toEqual([]);
  });
});

describe("a confirmation tick", () => {
  const tick = (globals: Globals) =>
    buildBlockConfirmationAction(() =>
      Effect.succeed({ type: "NoTxForConfirmationOutput" as const }),
    ).pipe(Effect.provideService(Globals, globals));

  const submittedBefore = (globals: Globals) =>
    Effect.all([
      Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, E.hash),
      Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 1_000),
    ]);

  it("clears a submission no active journal holds", async () => {
    const submission = await withGlobals((globals) =>
      Effect.gen(function* () {
        yield* submittedBefore(globals);
        yield* tick(globals);
        return yield* submissionOf(globals);
      }),
    );
    expect(submission).toEqual(NO_SUBMISSION);
  });

  it("keeps the submission of the active journal", async () => {
    const submission = await withGlobals((globals) =>
      Effect.gen(function* () {
        yield* writeJournal(SUBMITTED);
        yield* submittedBefore(globals);
        yield* tick(globals);
        return yield* submissionOf(globals);
      }),
    );
    expect(submission).toEqual({ txHash: E.hash, sinceMs: 1_000 });
  });
});
