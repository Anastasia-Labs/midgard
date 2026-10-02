import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option, Ref } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { SignedIntentReplacementIntegrityError } from "../src/services/canonical-journal-recovery.js";
import { Globals } from "../src/services/globals.js";
import { decide } from "../src/services/history-expired-intent-release.decide.js";
import type {
  QueueView,
  ReleaseEvidence,
} from "../src/services/history-expired-intent-release.signed-commit-node.js";
import { SIGNED_INTENT_UNDECIDED } from "../src/services/signed-intent-undecided.js";
import {
  makeState,
  STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
} from "../src/services/state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import {
  BASE_HEADER,
  BASE_OUT,
  bytes,
  hex,
  insertJournal,
  ROOT_HEADER,
  run,
  seed,
  signedCommit,
  TTL,
} from "./helpers/history-expired-intent-release-before-ttl.js";

/**
 * The release decision of an expired signed intent R when this node's replaced
 * block W holds its base's slot but a third sibling on the same base already
 * went further (locally finalized): undecided, never a process exit (R itself
 * locally finalized is reversed by the repair, so W is revived). The intent stays, the decision is "wait" (the history gate
 * stays closed, which holds block production), `signed_intent_undecided` is
 * raised, and the next evaluation decides again. Two landed siblings of one
 * base stay an integrity failure.
 */

const SOURCE = "history_signed_intent_release";
const W_HEADER = bytes("w-header", 28);
const R_HEADER = bytes("r-header", 28);
const T_HEADER = bytes("t-header", 28);
const MANIFEST = hex("manifest");
const POLICY = hex("policy").slice(0, 56);
const ROOT_OUT = `${hex("root-tx")}#0`;

const authority = {
  manifestId: MANIFEST,
  stateQueuePolicyId: POLICY,
  requiredFinalityDepth: 1n,
};

const queueNode = (
  header: string,
  prev: string,
  next: string | undefined,
  out = `${hex(`out:${header}`)}#0`,
) => {
  const [txHash, index] = out.split("#");
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

/** The base node D, whose successor is `winner`, plus `extra` nodes. */
const queueWithBaseLinkingTo = (winner: string, extra: unknown[] = []) =>
  ({
    root,
    nodes: [
      root,
      queueNode(BASE_HEADER.toString("hex"), root.headerHash, winner, BASE_OUT),
      ...extra,
    ],
  }) as unknown as QueueView;

const evidence = (
  queue: QueueView,
  canonicalHistory: ReadonlySet<string> = new Set(),
): ReleaseEvidence => ({
  queue,
  canonicalHistory,
  contracts: {} as never,
  rewindAuthority: authority,
});

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
    // The shared SQL model writes an empty block window; a loadable journal
    // needs a positive one.
    Effect.zipRight(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
          SET block_end_time = block_start_time + INTERVAL '1 second'
          WHERE header_hash = ${header}`,
      ),
    ),
  );

/** Runs `decide` for R on fresh node globals, returning its exit and the
 * reason it left raised. `raisedBefore` seeds a previously raised reason. */
const decideFor = (
  release: ReleaseEvidence,
  raisedBefore?: string,
): Promise<{
  exit: Exit.Exit<unknown, unknown>;
  raised: string | undefined;
}> =>
  run(
    Effect.gen(function* () {
      const globals = yield* Globals;
      if (raisedBefore !== undefined)
        yield* Ref.set(
          globals.LIVENESS_REASONS,
          new Map([[SOURCE, raisedBefore]]),
        );
      const record = Option.getOrThrow(
        yield* Pending.retrieveByHeaderHash(R_HEADER),
      );
      const exit = yield* Effect.exit(decide(record, release));
      const raised = (yield* Ref.get(globals.LIVENESS_REASONS)).get(SOURCE);
      return { exit, raised };
    }).pipe(Effect.provide(Globals.Default)),
  );

const decisionOf = (exit: Exit.Exit<unknown, unknown>) => {
  if (!Exit.isSuccess(exit))
    throw new Error(`decide failed: ${Cause.pretty(exit.cause)}`);
  return exit.value as { kind: string; reason?: string };
};

const readStatus = (header: Buffer) =>
  run(
    Effect.map(
      Pending.retrieveByHeaderHash(header),
      (found) => Option.getOrUndefined(found)?.[Pending.Columns.STATUS],
    ),
  );

beforeEach(async () => {
  await run(seed);
});

describe("an expired signed intent whose replaced sibling holds the slot", () => {
  it("revives the winner, clearing the reason, while the active block on the same base is already locally finalized", async () => {
    await run(journal(W_HEADER, Pending.Status.Abandoned, TTL, "replacement"));
    await run(journal(R_HEADER, Pending.Status.SubmittedUnconfirmed, TTL + 1));
    const winner = W_HEADER.toString("hex");
    const { exit, raised } = await decideFor(
      evidence(
        queueWithBaseLinkingTo(winner, [
          queueNode(winner, BASE_HEADER.toString("hex"), undefined),
        ]),
      ),
      SIGNED_INTENT_UNDECIDED,
    );
    // The repair reverses R's local finalization like any unlanded block's.
    expect(decisionOf(exit).kind).toBe("revive");
    expect(raised).toBeUndefined();
    // Nothing persisted: both journals keep their status.
    expect(await readStatus(W_HEADER)).toBe(Pending.Status.Abandoned);
    expect(await readStatus(R_HEADER)).toBe(
      Pending.Status.SubmittedUnconfirmed,
    );
  });

  it("is undecided, not fatal, while a third block on the same base is already finalized", async () => {
    await run(journal(W_HEADER, Pending.Status.Abandoned, TTL, "replacement"));
    await run(journal(R_HEADER, Pending.Status.PendingSubmission, TTL + 1));
    await run(journal(T_HEADER, Pending.Status.Finalized, TTL + 2));
    const { exit, raised } = await decideFor(
      evidence(queueWithBaseLinkingTo(W_HEADER.toString("hex"))),
    );
    const decision = decisionOf(exit);
    expect(decision.kind).toBe("wait");
    expect(decision.reason).toContain(
      `block ${T_HEADER.toString("hex")} built on the same base is already ${Pending.Status.Finalized}`,
    );
    expect(raised).toBe(SIGNED_INTENT_UNDECIDED);
    expect(await readStatus(T_HEADER)).toBe(Pending.Status.Finalized);
  });

  it("decides the revival and clears the reason once nothing on the base went further", async () => {
    await run(journal(W_HEADER, Pending.Status.Abandoned, TTL, "replacement"));
    await run(journal(R_HEADER, Pending.Status.PendingSubmission, TTL + 1));
    const winner = W_HEADER.toString("hex");
    const { exit, raised } = await decideFor(
      evidence(
        queueWithBaseLinkingTo(winner, [
          queueNode(winner, BASE_HEADER.toString("hex"), undefined),
        ]),
      ),
      SIGNED_INTENT_UNDECIDED,
    );
    expect(decisionOf(exit).kind).toBe("revive");
    expect(raised).toBeUndefined();
  });

  it("stays an integrity failure when two replaced siblings of one base both landed", async () => {
    const first = signedCommit(BASE_OUT, TTL);
    const second = signedCommit(BASE_OUT, TTL + 2);
    await run(journal(W_HEADER, Pending.Status.Abandoned, TTL, "replacement"));
    await run(
      journal(T_HEADER, Pending.Status.Abandoned, TTL + 2, "replacement"),
    );
    await run(journal(R_HEADER, Pending.Status.PendingSubmission, TTL + 1));
    // The observer has a view; neither D nor R is on the queue.
    const state = makeState({
      schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
      deploymentIdentityDigest: MANIFEST,
      stateQueuePolicyId: POLICY,
      cursorQueue: [{ headerHash: null, outRef: ROOT_OUT }],
      pending: [],
      admitted: [],
      retractedTransactionHashes: [],
      postFinalityRollbackIncidents: [],
    });
    await run(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`INSERT INTO state_queue_terminal_observer_states (
            deployment_identity_digest, state_queue_policy_id, state_digest,
            state_record, updated_at
          ) VALUES (${Buffer.from(MANIFEST, "hex")}, ${Buffer.from(POLICY, "hex")},
            ${Buffer.from(state.stateDigest, "hex")}, ${JSON.stringify(state)}, NOW())`,
      ),
    );
    const { exit } = await decideFor(
      evidence(
        { root, nodes: [root] } as unknown as QueueView,
        new Set([first.hash, second.hash]),
      ),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    const failure = Exit.isFailure(exit)
      ? Option.getOrUndefined(Cause.failureOption(exit.cause))
      : undefined;
    expect(failure).toBeInstanceOf(SignedIntentReplacementIntegrityError);
    const landed = [W_HEADER, T_HEADER]
      .map((header) => header.toString("hex"))
      .sort();
    expect((failure as Error).message).toContain(
      `blocks ${landed.join(", ")} of this node`,
    );
  });
});
