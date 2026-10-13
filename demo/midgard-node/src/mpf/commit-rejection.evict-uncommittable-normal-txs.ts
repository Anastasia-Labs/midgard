/**
 * A normal transaction that a commit stage cannot process leaves the block
 * with a rejection instead of holding block production, but only for a
 * failure that is the transaction's own: one that happens again on a second
 * run and is not a fault of the node (a store, the MPF, memory or the wasm
 * runtime). Any other failure holds the build and raises the alarm, as do
 * failures of forced transactions, deposits, withdrawals and the stores, and
 * more rejections at one stage than one bad transaction per few can explain.
 */

import {
  assertMidgardCekProgramMaterialBundle,
  mergeMidgardCekProgramMaterialSidecars,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { collectMidgardEventProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import type { PhaseAValidatedTx } from "@al-ft/midgard-validation";
import { Cause, Effect, Exit, Option } from "effect";

import * as MempoolDB from "../database/mempool.js";
import * as TxRejectionsDB from "../database/txRejections.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  COMMIT_REJECT_CODE_PROGRAM_COVERAGE,
  COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
} from "./commit-rejection.js";

const MAX_DETAIL_CAUSE_LENGTH = 512;

export const commitStageFailureDetail = (stage: string, cause: unknown) => {
  const text = formatUnknownError(cause);
  return `${stage} could not process the transaction: ${
    text.length > MAX_DETAIL_CAUSE_LENGTH
      ? `${text.slice(0, MAX_DETAIL_CAUSE_LENGTH)}...`
      : text
  }`;
};

export const traceUnavailableRejection = (
  txId: Uint8Array,
  stage: string,
  cause: unknown,
): TxRejectionsDB.EntryNoTimestamp => ({
  [TxRejectionsDB.Columns.TX_ID]: Buffer.from(txId),
  [TxRejectionsDB.Columns.REJECT_CODE]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
  [TxRejectionsDB.Columns.REJECT_DETAIL]: commitStageFailureDetail(
    stage,
    cause,
  ),
});

/**
 * Runs `effect`, capturing a failure or defect; an interruption stays an
 * interruption.
 */
export const attempt = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<
  | { readonly ok: true; readonly value: A }
  | { readonly ok: false; readonly cause: Cause.Cause<E> },
  never,
  R
> =>
  Effect.flatMap(
    Effect.exit(effect),
    (
      exit,
    ): Effect.Effect<
      | { readonly ok: true; readonly value: A }
      | { readonly ok: false; readonly cause: Cause.Cause<E> }
    > =>
      Exit.isSuccess(exit)
        ? Effect.succeed({ ok: true, value: exit.value })
        : // An interruption-only cause carries no failure.
          Cause.isInterruptedOnly(exit.cause)
          ? Effect.failCause(exit.cause as Cause.Cause<never>)
          : Effect.succeed({ ok: false, cause: exit.cause }),
  );

/** The failure a captured cause carries, or its defect. */
export const causeError = (cause: Cause.Cause<unknown>): unknown =>
  Option.getOrElse(Cause.failureOption(cause), () => Cause.squash(cause));

const NODE_FAULT_TAGS = new Set(["DatabaseError", "MpfError", "SqlError"]);
const RESOURCE_EXHAUSTION =
  /out of memory|allocation failed|heap limit|ENOMEM|cannot allocate|maximum memory/iu;
const MAX_ERROR_NESTING = 5;

const isNodeFaultError = (error: unknown, depth: number): boolean => {
  if (depth > MAX_ERROR_NESTING || error === null || error === undefined)
    return false;
  if (typeof error === "string") return RESOURCE_EXHAUSTION.test(error);
  if (typeof error !== "object") return false;
  if (
    error instanceof RangeError ||
    (typeof WebAssembly !== "undefined" &&
      error instanceof WebAssembly.RuntimeError)
  )
    return true;
  const { _tag, message, cause } = error as {
    readonly _tag?: unknown;
    readonly message?: unknown;
    readonly cause?: unknown;
  };
  if (typeof _tag === "string" && NODE_FAULT_TAGS.has(_tag)) return true;
  if (typeof message === "string" && RESOURCE_EXHAUSTION.test(message))
    return true;
  return (
    isNodeFaultError(cause, depth + 1) ||
    isNodeFaultError((error as { readonly error?: unknown }).error, depth + 1)
  );
};

/**
 * Whether a failure is the node's rather than any transaction's: a store or
 * MPF failure, a `RangeError`, a wasm runtime error or memory exhaustion,
 * anywhere in the cause or the errors it wraps.
 */
export const isNodeFault = (cause: Cause.Cause<unknown>): boolean =>
  [...Cause.failures(cause), ...Cause.defects(cause)].some((error) =>
    isNodeFaultError(error, 0),
  );

/** Holds the build: the stage's failure is not one transaction's own. */
export const holdBuild = (
  stage: string,
  cause: Cause.Cause<unknown>,
): Effect.Effect<never, DatabaseError> =>
  Effect.fail(
    new DatabaseError({
      table: MempoolDB.tableName,
      message: `Holding the block build: ${stage} failed and the failure is not a normal transaction's own`,
      cause: causeError(cause),
    }),
  );

/**
 * Decides whether `stage` failing on one transaction is that transaction's
 * own. A node fault, a failure that does not happen again when `rerun` runs
 * the same work, or a node fault on the rerun holds the build. Returns the
 * reproduced cause.
 */
export const confirmTransactionFailure = <E, E2, R>(
  stage: string,
  cause: Cause.Cause<E>,
  rerun: Effect.Effect<unknown, E2, R>,
): Effect.Effect<Cause.Cause<E2>, DatabaseError, R> =>
  Effect.gen(function* () {
    if (isNodeFault(cause)) return yield* holdBuild(stage, cause);
    const again = yield* attempt(rerun);
    if (again.ok) {
      yield* Effect.logWarning(
        `${stage} failed once on a normal transaction and passed when run again; holding the build.`,
      );
      return yield* holdBuild(stage, cause);
    }
    if (isNodeFault(again.cause)) return yield* holdBuild(stage, again.cause);
    return again.cause;
  });

/** At most this many normal transactions are rejected at one commit stage
 * of one build; past it, the stage's failures are the node's. */
export const COMMIT_STAGE_REJECTION_LIMIT = 4;

/** Counts one build's rejections per commit stage, and holds the build once
 * one stage rejects more than {@link COMMIT_STAGE_REJECTION_LIMIT}. */
export const makeCommitStageRejectionBreaker = () => {
  const counts = new Map<string, number>();
  return (stage: string): Effect.Effect<void, DatabaseError> =>
    Effect.suspend(() => {
      const count = (counts.get(stage) ?? 0) + 1;
      counts.set(stage, count);
      return count > COMMIT_STAGE_REJECTION_LIMIT
        ? Effect.fail(
            new DatabaseError({
              table: TxRejectionsDB.tableName,
              message: `Holding the block build: ${stage} would reject more normal transactions than one build may`,
              cause: `limit=${COMMIT_STAGE_REJECTION_LIMIT.toString()}`,
            }),
          )
        : Effect.void;
    });
};

export type CommitStageRejectionBreaker = ReturnType<
  typeof makeCommitStageRejectionBreaker
>;

/**
 * The first prefix length in `1..count` at which `holds` stops holding, given
 * that it holds for the empty prefix and not for `count`: the transaction at
 * index `result - 1` turns a passing prefix into a failing one.
 */
export const firstFailingPrefix = <E, R>(
  count: number,
  holds: (prefixLength: number) => Effect.Effect<boolean, E, R>,
): Effect.Effect<number, E, R> =>
  Effect.gen(function* () {
    let passing = 0;
    let failing = count;
    while (failing - passing > 1) {
      const middle = Math.floor((passing + failing) / 2);
      if (yield* holds(middle)) passing = middle;
      else failing = middle;
    }
    return failing;
  });

type EvaluatedCandidates<C> = {
  readonly accepted: readonly C[];
  readonly rejected: readonly {
    readonly txId: Uint8Array;
    readonly code: string;
    readonly detail: string | null;
  }[];
};

type CommitCandidate = Pick<PhaseAValidatedTx, "ledgerTx" | "graph">;

/** `deferred` and every candidate that spends or references an output of
 * one already taken, transitively. */
export const withInBlockDescendants = <C extends CommitCandidate>(
  deferred: C,
  candidates: readonly C[],
): ReadonlySet<C> => {
  const taken = new Set([deferred]);
  const produced = new Set(
    deferred.graph.produced.map((row) =>
      Buffer.from(row[Ledger.Columns.OUTREF]).toString("hex"),
    ),
  );
  for (let grew = true; grew; ) {
    grew = false;
    for (const candidate of candidates) {
      if (
        taken.has(candidate) ||
        ![
          ...candidate.graph.spentOutRefHexes,
          ...candidate.graph.referenceOutRefHexes,
        ].some((outRef) => produced.has(outRef))
      )
        continue;
      taken.add(candidate);
      for (const row of candidate.graph.produced)
        produced.add(Buffer.from(row[Ledger.Columns.OUTREF]).toString("hex"));
      grew = true;
    }
  }
  return taken;
};

/** A transaction that breaks the block-wide check: rejected when it fails
 * the check on its own, otherwise left pending with its in-block
 * descendants. */
export type BlockCheckBreak =
  | {
      readonly kind: "reject";
      readonly index: number;
      readonly code: string;
      readonly detail: string;
    }
  | { readonly kind: "defer"; readonly index: number; readonly detail: string };

/**
 * Evaluates `candidates`, taking out every transaction the evaluation or the
 * block-wide check cannot process, one at a time, until the rest evaluates
 * and passes.
 *
 * A failing evaluation is narrowed to the shortest failing prefix of the
 * candidates. Its last transaction is rejected only when that prefix fails
 * again, the prefix without it passes, and no run failed on a node fault;
 * otherwise the build holds. A transaction that breaks the block-wide check
 * is rejected or deferred as {@link BlockCheckBreak} says; a deferred one is
 * neither accepted nor rejected and stays pending. The rejected transactions
 * are returned with the evaluation's own rejections.
 */
export const evaluateEvictingUncommittable = <C extends CommitCandidate, E, R>({
  candidates,
  evaluate,
  stage,
  breaker,
  blockCheck,
}: {
  readonly candidates: readonly C[];
  readonly evaluate: (
    candidates: readonly C[],
  ) => Effect.Effect<EvaluatedCandidates<C>, E, R>;
  readonly stage: string;
  readonly breaker: CommitStageRejectionBreaker;
  readonly blockCheck?: (
    accepted: readonly C[],
  ) => (BlockCheckBreak & { readonly stage: string }) | undefined;
}): Effect.Effect<EvaluatedCandidates<C>, DatabaseError, R> =>
  Effect.gen(function* () {
    let remaining = candidates;
    const evicted: EvaluatedCandidates<C>["rejected"][number][] = [];
    const passes = (prefix: readonly C[]) =>
      attempt(evaluate(prefix)).pipe(
        Effect.flatMap((result) =>
          result.ok
            ? Effect.succeed(true)
            : isNodeFault(result.cause)
              ? holdBuild(stage, result.cause)
              : Effect.succeed(false),
        ),
      );
    for (;;) {
      const evaluated = yield* attempt(evaluate(remaining));
      if (!evaluated.ok) {
        // With no candidate left, the failure is not any transaction's.
        if (remaining.length === 0 || isNodeFault(evaluated.cause))
          return yield* holdBuild(stage, evaluated.cause);
        const failing = yield* firstFailingPrefix(remaining.length, (length) =>
          passes(remaining.slice(0, length)),
        );
        // The search saw every shorter prefix it relied on pass except the
        // empty one.
        if (failing === 1 && !(yield* passes([])))
          return yield* holdBuild(stage, evaluated.cause);
        const reproduced = yield* confirmTransactionFailure(
          stage,
          evaluated.cause,
          evaluate(remaining.slice(0, failing)),
        );
        yield* breaker(stage);
        const culprit = remaining[failing - 1]!;
        evicted.push({
          txId: culprit.ledgerTx.txId,
          code: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
          detail: commitStageFailureDetail(stage, causeError(reproduced)),
        });
        remaining = remaining.filter((candidate) => candidate !== culprit);
        continue;
      }
      const broken = blockCheck?.(evaluated.value.accepted);
      if (broken !== undefined) {
        const culprit = evaluated.value.accepted[broken.index]!;
        const culpritId = Buffer.from(culprit.ledgerTx.txId);
        const own = remaining.find((candidate) =>
          culpritId.equals(candidate.ledgerTx.txId),
        )!;
        if (broken.kind === "reject") {
          yield* breaker(broken.stage);
          evicted.push({
            txId: culprit.ledgerTx.txId,
            code: broken.code,
            detail: broken.detail,
          });
          remaining = remaining.filter((candidate) => candidate !== own);
          continue;
        }
        const deferred = withInBlockDescendants(own, remaining);
        yield* Effect.logWarning(
          `Leaving ${deferred.size.toString()} transaction(s) from tx ${culpritId.toString("hex")} pending for a later block: ${broken.detail}`,
        );
        remaining = remaining.filter((candidate) => !deferred.has(candidate));
        continue;
      }
      return {
        accepted: evaluated.value.accepted,
        rejected: [...evaluated.value.rejected, ...evicted],
      };
    }
  });

export const PROGRAM_COVERAGE_STAGE = "Program material coverage";

/**
 * The block-wide CEK program-material check the DA committee runs on a
 * payload, over the normal transactions in commit order: every transaction's
 * programs are collected against the state immediately before it, and the
 * merged material must cover exactly their union within the bundle limits.
 * Finds the transaction whose addition makes the check fail. It is rejected
 * when its own programs fail the check against its own material; otherwise
 * the failure is the combination's, and it is deferred.
 */
export const normalProgramCoverageBreak = (
  accepted: readonly PhaseAValidatedTx[],
  preState: ReadonlyMap<string, Buffer>,
): (BlockCheckBreak & { readonly stage: string }) | undefined => {
  // A node fault is no transaction's: it fails the build.
  const rethrowNodeFault = (cause: unknown) => {
    if (isNodeFault(Cause.die(cause))) throw cause;
    return cause;
  };
  const state = new Map(preState);
  const envelopes: (
    | {
        readonly ok: true;
        readonly value: readonly MidgardCekProgramEnvelope[];
      }
    | { readonly ok: false; readonly cause: unknown }
  )[] = [];
  for (const candidate of accepted) {
    try {
      envelopes.push({
        ok: true,
        value: collectMidgardEventProgramEnvelopes(
          decodeMidgardNativeTxFullFromCanonicalCbor(
            candidate.submission.txCbor,
          ),
          (outRefHex) => state.get(outRefHex),
        ),
      });
    } catch (cause) {
      envelopes.push({ ok: false, cause: rethrowNodeFault(cause) });
    }
    for (const spent of candidate.graph.spentOutRefHexes) state.delete(spent);
    for (const produced of candidate.graph.produced)
      state.set(
        produced[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(produced[Ledger.Columns.OUTPUT]),
      );
  }
  const sidecars = (members: readonly PhaseAValidatedTx[]) =>
    members.flatMap(({ submission }) =>
      submission.programMaterialSidecarCbor === null
        ? []
        : [submission.programMaterialSidecarCbor],
    );
  const failure = (
    collected: typeof envelopes,
    members: readonly PhaseAValidatedTx[],
  ): { readonly cause: unknown } | undefined => {
    const uncollected = collected.find((entry) => !entry.ok);
    if (uncollected !== undefined && !uncollected.ok)
      return { cause: uncollected.cause };
    try {
      assertMidgardCekProgramMaterialBundle(
        collected.flatMap((entry) => (entry.ok ? entry.value : [])),
        mergeMidgardCekProgramMaterialSidecars(sidecars(members)),
      );
      return undefined;
    } catch (cause) {
      return { cause: rethrowNodeFault(cause) };
    }
  };
  const prefixFailure = (length: number) =>
    failure(envelopes.slice(0, length), accepted.slice(0, length));
  const blockFailure = prefixFailure(accepted.length);
  if (blockFailure === undefined) return undefined;
  const index =
    Effect.runSync(
      firstFailingPrefix(accepted.length, (length) =>
        Effect.sync(() => prefixFailure(length) === undefined),
      ),
    ) - 1;
  const alone = failure([envelopes[index]!], [accepted[index]!]);
  return alone === undefined
    ? {
        kind: "defer",
        index,
        stage: PROGRAM_COVERAGE_STAGE,
        detail: commitStageFailureDetail(
          "Block-wide program material coverage",
          blockFailure.cause,
        ),
      }
    : {
        kind: "reject",
        index,
        stage: PROGRAM_COVERAGE_STAGE,
        code: COMMIT_REJECT_CODE_PROGRAM_COVERAGE,
        detail: commitStageFailureDetail(PROGRAM_COVERAGE_STAGE, alone.cause),
      };
};
