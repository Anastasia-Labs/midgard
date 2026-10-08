import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option, Ref } from "effect";
import { expect } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { SignedIntentReplacementIntegrityError } from "../../src/services/canonical-journal-recovery.js";
import { Globals } from "../../src/services/globals.js";
import { decide } from "../../src/services/history-expired-intent-release.decide.js";
import type {
  QueueView,
  ReleaseEvidence,
} from "../../src/services/history-expired-intent-release.signed-commit-node.js";
import type { CanonicalDepth } from "../../src/services/history-expired-intent-release.table.js";
import {
  BASE_HEADER,
  BASE_OUT,
  bytes,
  fixtureHeader,
  hex,
  insertJournal,
  ROOT_HEADER,
  run,
  signedCommit,
  TTL,
} from "./history-expired-intent-release-before-ttl.js";

/**
 * A locally finalized sibling that an L1 rollback displaced. Base D; this
 * node's block W was signed and journaled, then replaced by S, which landed
 * and finalized locally. A rollback removed S, and W's own signed commit then
 * took D's slot. S changed no ledger state (its expected root is its base
 * root), so after the rollback the node could journal a new block X on D;
 * X's signed commit then expired. The release of X reads W holding D's slot
 * with S still Finalized beside it.
 */

export const SOURCE = "history_signed_intent_release";
export const W_HEADER = fixtureHeader("displaced:w-header", BASE_HEADER);
export const S_HEADER = bytes("displaced:s-header", 28);
export const X_HEADER = fixtureHeader("displaced:x-header", BASE_HEADER);
export const W_COMMIT = signedCommit(BASE_OUT, TTL);
export const S_COMMIT = signedCommit(BASE_OUT, TTL + 1);
export const X_COMMIT = signedCommit(BASE_OUT, TTL + 2);
export const W_NODE_OUT = `${hex("displaced:w-node-tx")}#0`;

export const authority = {
  manifestId: hex("manifest"),
  stateQueuePolicyId: hex("policy").slice(0, 56),
  requiredFinalityDepth: 3n,
};

export const queueNode = (
  header: string,
  prev: string,
  next: string | undefined,
  out: string,
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

export const root = queueNode(
  ROOT_HEADER.toString("hex"),
  hex("pre-root"),
  BASE_HEADER.toString("hex"),
  `${hex("root-tx")}#0`,
);

/** The rolled-forward queue: D links to W, whose node is on the queue. */
export const wHoldsTheSlot = {
  root,
  nodes: [
    root,
    queueNode(
      BASE_HEADER.toString("hex"),
      root.headerHash,
      W_HEADER.toString("hex"),
      BASE_OUT,
    ),
    queueNode(
      W_HEADER.toString("hex"),
      BASE_HEADER.toString("hex"),
      undefined,
      W_NODE_OUT,
    ),
  ],
} as unknown as QueueView;

export const evidence = (
  extra: Partial<ReleaseEvidence> = {},
): ReleaseEvidence => ({
  queue: wHoldsTheSlot,
  canonicalHistory: new Set(),
  contracts: {} as never,
  rewindAuthority: authority,
  ...extra,
});

export const journal = (
  header: Buffer,
  status: Pending.Status,
  commit: { readonly hash: string; readonly cbor: Buffer },
  createdAtMs: number,
  options: { abandonment?: "replacement"; empty?: boolean } = {},
) =>
  insertJournal({
    header,
    status,
    commit,
    baseOut: BASE_OUT,
    baseHeader: BASE_HEADER,
    createdAt: new Date(createdAtMs),
    ...(options.abandonment !== undefined && {
      abandonment: options.abandonment,
    }),
  }).pipe(
    Effect.zipRight(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
          SET block_end_time = block_start_time + INTERVAL '1 second'
            ${options.empty === true ? sql`, expected_utxos_root = base_utxos_root` : sql``}
          WHERE header_hash = ${header}`,
      ),
    ),
  );

/** W abandoned for replacement, S at `sibling` and changing no ledger
 * state, X the active signed intent on D at `active`. */
export const seedDisplaced = (
  sibling: Pending.Status,
  active: Pending.Status = Pending.Status.PendingSubmission,
) =>
  Effect.gen(function* () {
    yield* journal(W_HEADER, Pending.Status.Abandoned, W_COMMIT, 2_000_000, {
      abandonment: "replacement",
    });
    yield* journal(S_HEADER, sibling, S_COMMIT, 3_000_000, { empty: true });
    yield* journal(X_HEADER, active, X_COMMIT, 4_000_000);
  });

export const readStatus = (header: Buffer) =>
  run(
    Effect.map(
      Pending.retrieveByHeaderHash(header),
      (found) => Option.getOrUndefined(found)?.[Pending.Columns.STATUS],
    ),
  );

export type Round = {
  kind: string;
  reason?: string;
  displaced?: string[];
  raised?: string;
};

/** Runs `decide` for X once per evidence, in order, on one set of node
 * globals, returning each decision and the reason left raised after it. */
export const decideInTurn = (releases: readonly ReleaseEvidence[]) =>
  run(
    Effect.gen(function* () {
      const globals = yield* Globals;
      const rounds: Round[] = [];
      for (const release of releases) {
        const record = Option.getOrThrow(
          yield* Pending.retrieveByHeaderHash(X_HEADER),
        );
        const exit = yield* Effect.exit(decide(record, release));
        if (!Exit.isSuccess(exit))
          throw new Error(`decide failed: ${Cause.pretty(exit.cause)}`);
        const decision = exit.value as {
          kind: string;
          reason?: string;
          displaced?: Pending.Record[];
        };
        rounds.push({
          kind: decision.kind,
          ...(decision.reason !== undefined && { reason: decision.reason }),
          ...(decision.displaced !== undefined && {
            displaced: decision.displaced.map((record) =>
              record[Pending.Columns.HEADER_HASH].toString("hex"),
            ),
          }),
          raised: (yield* Ref.get(globals.LIVENESS_REASONS)).get(SOURCE),
        });
      }
      return rounds;
    }).pipe(Effect.provide(Globals.Default)),
  );

export const failureMessage = (exit: Exit.Exit<unknown, unknown>) => {
  const messages: string[] = [];
  let value: unknown = Exit.isFailure(exit)
    ? Option.getOrUndefined(Cause.failureOption(exit.cause))
    : undefined;
  for (; value instanceof Error; value = value.cause)
    messages.push(value.message);
  return messages.join(" <- ");
};

export const W_NODE_TX = W_NODE_OUT.split("#")[0]!;

/** Depth evidence: `depths` by transaction, a chain of `retained` blocks. */
export const depthOf = (
  depths: Readonly<Record<string, bigint>>,
  retained = 100n,
): CanonicalDepth => ({ of: (txHash) => depths[txHash], retained });

export const atDepth = (held: bigint, extra: Record<string, bigint> = {}) =>
  evidence({ canonicalDepth: depthOf({ [W_NODE_TX]: held, ...extra }) });

export const decideOnce = (release: ReleaseEvidence) =>
  run(
    Effect.gen(function* () {
      const record = Option.getOrThrow(
        yield* Pending.retrieveByHeaderHash(X_HEADER),
      );
      return yield* Effect.exit(decide(record, release));
    }).pipe(Effect.provide(Globals.Default)),
  );

export const integrityFailure = (exit: Exit.Exit<unknown, unknown>) => {
  const failure = Exit.isFailure(exit)
    ? Option.getOrUndefined(Cause.failureOption(exit.cause))
    : undefined;
  expect(failure).toBeInstanceOf(SignedIntentReplacementIntegrityError);
  return (failure as Error).message;
};
