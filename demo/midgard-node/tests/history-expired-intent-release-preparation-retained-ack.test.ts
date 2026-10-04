import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  activeE,
  BASE_HEADER,
  BASE_OUT,
  E_HEADER,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  queueNode,
  root,
  SOURCE,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  onNode,
  type OwnerModel,
  ownerModel,
  plans,
  release,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";

/**
 * The retained release plan across the acknowledgement of its journal's
 * submission. The replacement of an expired signed intent E prepares its plan
 * (binding E's journal digest) and runs the native CAS; the node then stops
 * before the SQL repair. The submission's acknowledgement lands in between
 * (the submitted hash set to the signed hash), which changes E's journal
 * identity. The next attempt resumes the retained plan and replaces E
 * exactly once; a journal that really changed is held, never resumed.
 */

const fixture = vi.hoisted(
  (): Fixture => ({ queue: undefined, coverage: "unavailable" }),
);

vi.mock("../src/l1-event-history-source.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.ledgerSnapshot(original),
  ),
);
vi.mock(
  "../src/services/history-expired-intent-release.signed-commit-node.js",
  (original) =>
    import(
      "./helpers/history-expired-intent-release-preparation.mocks.js"
    ).then((mocks) => mocks.queueAuthentication(original, fixture)),
);
vi.mock("../src/database/eventHistoryCanonicalCoverage.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.canonicalCoverage(original, fixture),
  ),
);
vi.mock("../src/workers/utils/commit-block-header.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.nodeSerialization(original),
  ),
);

const INTEGRITY = "signed_intent_replacement_integrity";

/** E's base D is still the queue's tail at the checkpoint, past E's TTL, and
 * E's signed commit is not in the canonical history: E is replaced. */
const baseUnspent = {
  root,
  nodes: [
    root,
    queueNode(
      BASE_HEADER.toString("hex"),
      root.headerHash,
      undefined,
      BASE_OUT,
    ),
  ],
} as never;

const sql = (
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) => Effect.flatMap(SqlClient.SqlClient, statement);

/** What `markSubmitted` writes: the submitted hash is the signed hash. */
const acknowledge = sql(
  (sql) => sql`UPDATE pending_block_finalizations
    SET submitted_tx_hash = intended_tx_hash,
      status = ${Pending.Status.SubmittedLocalFinalizationPending}
    WHERE header_hash = ${E_HEADER}`,
);

/** A journal that really changed: its state-queue lease is another one. */
const releaseLease = sql(
  (sql) => sql`UPDATE pending_block_finalizations
    SET state_queue_lease_token = 'another-lease'
    WHERE header_hash = ${E_HEADER}`,
);

/** E journaled (promoted: the native root at its candidate), its plan's CAS
 * run, then the node stops; `between` runs before the next attempt. */
const crashAfterCas = (
  owner: OwnerModel,
  between: Effect.Effect<unknown, unknown, SqlClient.SqlClient>,
) =>
  onNode(
    owner,
    (node) =>
      Effect.gen(function* () {
        yield* activeE();
        yield* sql(
          (sql) => sql`UPDATE pending_block_finalizations
            SET block_end_time = block_start_time + INTERVAL '1 second'
            WHERE header_hash = ${E_HEADER}`,
        );
        yield* withNativeReplay(E_HEADER);
        fixture.queue = baseUnspent;
        fixture.coverage = { head: 10, start: 1, txs: {} };
        owner.afterRestore = async () => {
          throw new Error("modelled stop after the native CAS");
        };
        const crashed = yield* release(node);
        owner.afterRestore = undefined;
        const retained = (yield* plans).map(({ state }) => state);
        const restoredTo = owner.durableRoot;
        yield* between;
        const resumed = yield* release(node);
        const after = {
          e: (yield* statusOf(E_HEADER))?.status,
          plans: (yield* plans).map(({ state }) => state),
          restores: owner.restores,
        };
        const again = yield* release(node);
        return {
          crashed,
          retained,
          restoredTo,
          resumed,
          after,
          again,
          final: (yield* plans).map(({ state }) => state),
        };
      }),
    ZERO_ROOT,
  );

describe("the retained signed-intent release plan", () => {
  it("repairs the post-CAS acknowledgement race in the same attempt", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(
      owner,
      (node) =>
        Effect.gen(function* () {
          yield* activeE();
          yield* sql(
            (sql) => sql`UPDATE pending_block_finalizations
        SET block_end_time = block_start_time + INTERVAL '1 second' WHERE header_hash = ${E_HEADER}`,
          );
          yield* withNativeReplay(E_HEADER);
          fixture.queue = baseUnspent;
          fixture.coverage = { head: 10, start: 1, txs: {} };
          const client = yield* SqlClient.SqlClient;
          owner.afterRestore = () =>
            Effect.runPromise(
              sql(
                (sql) => sql`UPDATE pending_block_finalizations
        SET submitted_tx_hash = intended_tx_hash, status = ${Pending.Status.SubmittedLocalFinalizationPending}
        WHERE header_hash = ${E_HEADER}`,
              ).pipe(
                Effect.provideService(SqlClient.SqlClient, client),
                Effect.asVoid,
              ),
            );
          const resumed = yield* release(node);
          owner.afterRestore = undefined;
          return {
            resumed,
            status: (yield* statusOf(E_HEADER))?.status,
            plans: (yield* plans).map(({ state }) => state),
          };
        }),
      ZERO_ROOT,
    );
    expect(result.resumed.failure).toBeUndefined();
    expect(result.status).toBe(Pending.Status.Abandoned);
    expect(result.plans).toEqual(["applied"]);
    expect(owner.restores).toBe(1);
  });
  it("resumes across the acknowledgement of the journal's submission and replaces exactly once", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await crashAfterCas(owner, acknowledge);
    // The first attempt stopped after its CAS: the plan is retained and the
    // native root is at E's base.
    expect(result.crashed.failure).toContain(
      "modelled stop after the native CAS",
    );
    expect(result.retained).toEqual(["prepared"]);
    expect(result.restoredTo).toBe(UTXOS_ROOT);
    // The acknowledged journal resumes the same plan: one applied plan, E
    // abandoned, no hold raised.
    expect(result.resumed.failure).toBeUndefined();
    expect(result.resumed.raised.get(SOURCE)).toBeUndefined();
    expect(result.after).toEqual({
      e: Pending.Status.Abandoned,
      plans: ["applied"],
      restores: 2,
    });
    // Nothing is left to replace.
    expect(result.again.failure).toBeUndefined();
    expect(result.final).toEqual(["applied"]);
    expect(owner.restores).toBe(2);
  });

  it("holds, and never resumes, a retained plan whose journal really changed", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await crashAfterCas(owner, releaseLease);
    expect(result.retained).toEqual(["prepared"]);
    for (const attempt of [result.resumed, result.again]) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(SOURCE)).toBe(INTEGRITY);
      expect(attempt.reasons).toContain(INTEGRITY);
    }
    // Nothing written: E is still the active intent, the plan is still
    // retained, and the native CAS never ran again.
    expect(result.after).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: ["prepared"],
      restores: 1,
    });
    expect(result.final).toEqual(["prepared"]);
  });
});
