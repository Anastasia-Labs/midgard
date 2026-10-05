import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { Globals } from "../src/services/globals.js";
import { replacedBlockRevivalDisposition } from "../src/services/history-expired-intent-release.prepare-replaced-block-revival.js";
import { stateQueueCorrectionRewindDisposition } from "../src/services/state-queue-correction-rewind.js";
import { loadObligation } from "../src/services/state-queue-correction-rewind.prove-unlanded.js";
import {
  A_HEADER,
  A_OUT,
  admitCorrection,
  after as correctedQueue,
  before,
} from "./helpers/history-displacement-correction.js";
import {
  BASE_HEADER,
  BASE_OUT,
  hex,
  insertJournal,
  signedCommit,
  TTL,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  authority,
  journal,
  S_COMMIT,
  S_HEADER,
  W_COMMIT,
  W_HEADER,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  inputs,
  ledgerRoot,
  observerSees,
  onNode,
  ownerModel,
  plans,
  revival,
  rewind,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
} from "./helpers/history-expired-intent-release-preparation.js";

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

const S_NODE_TX = hex("reversal:s-node-tx");
const S_NODE_OUT = `${S_NODE_TX}#0`;
const deep = (tx: string) => ({ head: 10, start: 1, txs: { [tx]: 5 } });
const sql = (
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) => Effect.flatMap(SqlClient.SqlClient, statement);
const observerNow = (node: { headerHash: string; outRef: string }) =>
  sql((sql) => sql`DELETE FROM state_queue_terminal_observer_states`).pipe(
    Effect.zipRight(observerSees([node])),
  );
const outcome = Effect.gen(function* () {
  return {
    w: (yield* statusOf(W_HEADER))?.status,
    s: (yield* statusOf(S_HEADER))?.status,
    plans: (yield* plans).map(({ state }) => state),
    ledger: yield* ledgerRoot,
  };
});

describe("retained displacement followed by an ancestor correction", () => {
  it.each(["none", "before repair", "after repair"])(
    "resolves the original displacement and correction across %s interruption without reviving the corrected descendant",
    async (stop) => {
      const owner = ownerModel("00".repeat(32));
      const result = await onNode(
        owner,
        (node) =>
          Effect.gen(function* () {
            yield* insertJournal({
              header: A_HEADER,
              status: Pending.Status.Finalized,
              commit: signedCommit(`${hex("root-tx")}#0`, TTL),
              baseOut: `${hex("root-tx")}#0`,
              baseHeader: Buffer.alloc(28),
              createdAt: new Date(500_000),
            });
            yield* sql(
              (sql) =>
                sql`UPDATE pending_block_finalizations SET base_utxos_root = ${"00".repeat(32)}, block_end_time = block_start_time + INTERVAL '1 second' WHERE header_hash = ${A_HEADER}`,
            );
            yield* insertJournal({
              header: BASE_HEADER,
              status: Pending.Status.Finalized,
              commit: signedCommit(A_OUT, TTL),
              baseOut: A_OUT,
              baseHeader: A_HEADER,
              createdAt: new Date(1_000_000),
            });
            yield* sql(
              (sql) =>
                sql`UPDATE pending_block_finalizations SET base_utxos_root = ${"00".repeat(32)}, expected_utxos_root = ${UTXOS_ROOT}, block_end_time = block_start_time + INTERVAL '1 second' WHERE header_hash = ${BASE_HEADER}`,
            );
            yield* withNativeReplay(BASE_HEADER);
            yield* journal(
              W_HEADER,
              Pending.Status.Finalized,
              W_COMMIT,
              2_000_000,
            );
            yield* withNativeReplay(W_HEADER);
            yield* journal(
              S_HEADER,
              Pending.Status.Abandoned,
              S_COMMIT,
              3_000_000,
              { abandonment: "replacement", empty: true },
            );
            yield* observerNow({
              headerHash: S_HEADER.toString("hex"),
              outRef: S_NODE_OUT,
            });
            // Bootstrap to the full pre-correction topology, including retained A,D.
            yield* sql(
              (sql) => sql`DELETE FROM state_queue_terminal_observer_states`,
            );
            yield* observerSees([
              { headerHash: A_HEADER.toString("hex"), outRef: A_OUT },
              { headerHash: BASE_HEADER.toString("hex"), outRef: BASE_OUT },
              { headerHash: S_HEADER.toString("hex"), outRef: S_NODE_OUT },
            ]);
            fixture.queue = before as never;
            fixture.coverage = deep(S_NODE_TX);
            owner.afterRestore = async () => {
              throw new Error("stop after displacement CAS");
            };
            const stopped = yield* revival(node);
            const original = yield* plans;
            owner.afterRestore = undefined;
            const admitted = yield* admitCorrection;
            fixture.queue = correctedQueue as never;
            const owedBefore = yield* loadObligation(authority);
            let owedAtCorrectionCas: Awaited<
              Effect.Effect.Success<
                ReturnType<typeof stateQueueCorrectionRewindDisposition>
              >
            >;
            let publicationAtCorrectionCas: boolean | undefined;
            let operations = 0;
            const sqlService = yield* SqlClient.SqlClient;
            const globals = yield* Globals;
            owner.afterRestore = async () => {
              operations += 1;
              if (operations === 2) {
                owedAtCorrectionCas = await Effect.runPromise(
                  stateQueueCorrectionRewindDisposition(authority).pipe(
                    Effect.provideService(SqlClient.SqlClient, sqlService),
                  ),
                );
                publicationAtCorrectionCas = await Effect.runPromise(
                  Ref.get(globals.LOCAL_FINALIZATION_PENDING),
                );
              }
              if (
                (stop === "before repair" && operations === 1) ||
                (stop === "after repair" && operations === 2)
              )
                throw new Error(`stop ${stop}`);
            };
            const interrupted = yield* revival(node);
            const intermediate = yield* plans;
            owner.afterRestore = undefined;
            const resumed = [
              yield* rewind(node),
              yield* revival(node),
              yield* rewind(node),
              yield* revival(node),
            ];
            const input = inputs(node);
            const gate = yield* replacedBlockRevivalDisposition({
              change: { kind: "seed", after: input.checkpoint } as never,
              deferral: node.deferral,
              rewindAuthority: authority,
            });
            return {
              stopped,
              original,
              admitted,
              owedBefore,
              resumed,
              interrupted,
              intermediate,
              gate,
              operations: owner.operations,
              owedAtCorrectionCas,
              publicationAtCorrectionCas,
              after: yield* outcome,
              d: yield* statusOf(BASE_HEADER),
              owedAfter: yield* loadObligation(authority),
              native: owner.durableRoot,
              replayed: owner.recovers,
            };
          }),
        "00".repeat(32),
      );
      expect(result.stopped.failure).toContain("stop after displacement CAS");
      expect(result.original.map(({ state }) => state)).toEqual(["prepared"]);
      expect(result.admitted.admittedTransactionHashes).toEqual([
        hex("correction:fraud-link"),
      ]);
      expect(result.owedBefore.kind).toBe("blocked");
      if (stop !== "none")
        expect(result.interrupted.failure).toContain(`stop ${stop}`);
      const originalId = result.original[0]!.recovery_id;
      expect(
        result.operations.filter((id) => id === originalId).length,
      ).toBeGreaterThanOrEqual(2);
      if (stop === "before repair")
        expect(result.intermediate[0]?.recovery_id).toBe(originalId);
      if (stop === "after repair") {
        expect(
          result.intermediate.find(
            ({ recovery_id }) => recovery_id === originalId,
          )?.state,
        ).toBe("applied");
        expect(result.owedAtCorrectionCas?.status).toBe("pending");
        expect(result.publicationAtCorrectionCas).toBe(false);
      }
      expect(result.gate?.status).toBe("pending");
      expect(result.after.plans).not.toContain("prepared");
      expect(result.d?.status).toBe(Pending.Status.Abandoned);
      expect(result.after.w).toBe(Pending.Status.Abandoned);
      expect(result.after.s).toBe(Pending.Status.Abandoned);
      expect(result.owedAfter.kind).toBe("none");
      expect(result.native).toBe(result.after.ledger);
      expect(result.native).toBe("00".repeat(32));
      expect(result.replayed).toBe(0);
      for (const attempt of result.resumed)
        expect(attempt.failure).toBeUndefined();
    },
  );
});
