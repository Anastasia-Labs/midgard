import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option, Ref } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  expiredIntentReleaseClearingIncident,
  type ReleaseIncidentJournal,
} from "../src/services/event-history-runtime.js";
import { Globals } from "../src/services/globals.js";
import { decide } from "../src/services/history-expired-intent-release.decide.js";
import type {
  QueueView,
  ReleaseEvidence,
} from "../src/services/history-expired-intent-release.signed-commit-node.js";
import {
  deferralKey,
  makeSignedIntentDeferral,
} from "../src/services/history-expired-intent-release.table.js";
import { HISTORY_SIGNED_INTENT_RELEASE_SOURCE } from "../src/services/liveness-halt.js";
import { SIGNED_INTENT_UNDECIDED } from "../src/services/signed-intent-undecided.js";
import {
  BASE_HEADER,
  BASE_OUT,
  binding,
  bytes,
  change,
  hex,
  insertJournal,
  ROOT_HEADER,
  run,
  seed,
  signedCommit,
  TTL,
} from "./helpers/history-expired-intent-release-before-ttl.js";

/**
 * The undecided-release incident `decide` raises must not outlive its
 * condition: `decide` runs only while a release is in question, so the
 * history runtime clears it once none is (no active signed intent, or one
 * that can still land), and when the active journal changes. While the
 * release is still in question for the same journal, it stays raised.
 */

const W_HEADER = bytes("w-header", 28);
const R_HEADER = bytes("r-header", 28);
const T_HEADER = bytes("t-header", 28);
const R_COMMIT = signedCommit(BASE_OUT, TTL + 1);
const R_KEY = deferralKey({
  headerHash: R_HEADER,
  intendedTxHash: Buffer.from(R_COMMIT.hash, "hex"),
});

const queueNode = (header: string, prev: string, next: string | undefined) => {
  const [txHash, index] = `${hex(`out:${header}`)}#0`.split("#");
  return {
    headerHash: header,
    prevHeaderHash: prev,
    node: {
      utxo: { txHash, outputIndex: Number(index) },
      datum: { next: next === undefined ? "Empty" : { Key: { key: next } } },
    },
  };
};

const root = queueNode(ROOT_HEADER.toString("hex"), hex("pre-root"), undefined);

/** This node's replaced block W holds the base's slot while a third block T
 * on the same base is already finalized: `decide` cannot release R. */
const undecidedEvidence: ReleaseEvidence = {
  queue: {
    root,
    nodes: [
      root,
      {
        ...queueNode(
          BASE_HEADER.toString("hex"),
          root.headerHash,
          W_HEADER.toString("hex"),
        ),
        node: {
          utxo: { txHash: BASE_OUT.split("#")[0], outputIndex: 0 },
          datum: { next: { Key: { key: W_HEADER.toString("hex") } } },
        },
      },
    ],
  } as unknown as QueueView,
  canonicalHistory: new Set(),
  contracts: {} as never,
  rewindAuthority: {
    manifestId: hex("manifest"),
    stateQueuePolicyId: hex("policy").slice(0, 56),
    requiredFinalityDepth: 1n,
  },
};

const journal = (
  header: Buffer,
  status: Pending.Status,
  ttl: number,
  abandonment?: "replacement",
) =>
  insertJournal({
    header,
    status,
    commit: signedCommit(BASE_OUT, ttl),
    baseOut: BASE_OUT,
    baseHeader: BASE_HEADER,
    createdAt: new Date(2_000_000 + ttl),
    ...(abandonment !== undefined && { abandonment }),
  }).pipe(
    Effect.zipRight(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
          SET block_end_time = block_start_time + INTERVAL '1 second'
          WHERE header_hash = ${header}`,
      ),
    ),
  );

/** On one node's globals: `decide` raises the incident for R, then
 * `between` runs, then the runtime's release disposition sees `observed` with
 * `seen` as the journal it saw before. Returns the incident before and after,
 * and the disposition. */
const raiseThenReconcile = (input: {
  readonly observed: ReturnType<typeof change>;
  readonly seen: string | undefined;
  readonly between?: Effect.Effect<void, unknown, SqlClient.SqlClient>;
}) =>
  run(
    Effect.gen(function* () {
      const globals = yield* Globals;
      const raised = () =>
        Effect.map(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
          reasons.get(HISTORY_SIGNED_INTENT_RELEASE_SOURCE),
        );
      const record = Option.getOrThrow(
        yield* Pending.retrieveByHeaderHash(R_HEADER),
      );
      const decision = yield* Effect.exit(decide(record, undecidedEvidence));
      if (!Exit.isSuccess(decision))
        throw new Error(`decide failed: ${Cause.pretty(decision.cause)}`);
      expect(decision.value.kind).toBe("wait");
      const before = yield* raised();
      yield* input.between ?? Effect.void;
      const seen: ReleaseIncidentJournal = { current: input.seen };
      const release = yield* expiredIntentReleaseClearingIncident(
        {
          binding,
          change: input.observed,
          deferral: makeSignedIntentDeferral(),
          rewindAuthority: undefined as never,
        },
        seen,
      );
      return { before, after: yield* raised(), release, seen: seen.current };
    }).pipe(Effect.provide(Globals.Default)),
  );

const beforeTtl = change("forward", 5);
const pastTtl = change("forward", 200, { slot: 2_000 });

beforeEach(async () => {
  await run(seed);
  await run(journal(W_HEADER, Pending.Status.Abandoned, TTL, "replacement"));
  await run(journal(R_HEADER, Pending.Status.PendingSubmission, TTL + 1));
  await run(journal(T_HEADER, Pending.Status.Finalized, TTL + 2));
});

describe("the undecided signed-intent release incident", () => {
  it("stays raised while the same journal's release is still in question", async () => {
    const result = await raiseThenReconcile({ observed: pastTtl, seen: R_KEY });
    expect(result.before).toBe(SIGNED_INTENT_UNDECIDED);
    expect(result.release?.status).toBe("pending");
    expect(result.after).toBe(SIGNED_INTENT_UNDECIDED);
  });

  it("clears once the active intent can still land (a rollback below its TTL)", async () => {
    const result = await raiseThenReconcile({
      observed: change("rollback", 5),
      seen: R_KEY,
    });
    expect(result.before).toBe(SIGNED_INTENT_UNDECIDED);
    expect(result.release).toBeUndefined();
    expect(result.after).toBeUndefined();
  });

  it("clears once no signed intent is active", async () => {
    const result = await raiseThenReconcile({
      observed: beforeTtl,
      seen: undefined,
      between: Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
          SET status = ${Pending.Status.Abandoned}
          WHERE header_hash = ${R_HEADER}`,
      ),
    });
    expect(result.before).toBe(SIGNED_INTENT_UNDECIDED);
    expect(result.release).toBeUndefined();
    expect(result.after).toBeUndefined();
  });

  it("clears when the active journal changed, though the release is in question", async () => {
    const result = await raiseThenReconcile({
      observed: pastTtl,
      seen: "an-earlier-journal",
    });
    expect(result.before).toBe(SIGNED_INTENT_UNDECIDED);
    expect(result.release?.status).toBe("pending");
    expect(result.after).toBeUndefined();
    expect(result.seen).toBe(R_KEY);
  });
});
