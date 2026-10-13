/**
 * A normal transaction that a commit stage cannot process leaves the block
 * with a rejection instead of holding block production: every stage whose
 * failure is attributable to one normal transaction is guarded here. Failures
 * of forced transactions, deposits, withdrawals and the stores are not
 * attributable to a normal transaction and still stop the build.
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

import * as TxRejectionsDB from "../database/txRejections.js";
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

/**
 * Evaluates `candidates`, taking out every transaction the evaluation or the
 * block-wide check cannot process, one at a time, until the rest evaluates
 * and passes. A failing evaluation is narrowed to the shortest failing prefix
 * of the candidates; its last transaction is the one rejected. The rejected
 * transactions are returned with the evaluation's own rejections.
 */
export const evaluateEvictingUncommittable = <
  C extends { readonly ledgerTx: { readonly txId: Uint8Array } },
  E,
  R,
>({
  candidates,
  evaluate,
  stage,
  blockCheck,
}: {
  readonly candidates: readonly C[];
  readonly evaluate: (
    candidates: readonly C[],
  ) => Effect.Effect<EvaluatedCandidates<C>, E, R>;
  readonly stage: string;
  /** Index into `accepted` of the transaction that breaks the block-wide
   * check, or `undefined` when the accepted set passes it. */
  readonly blockCheck?: (accepted: readonly C[]) => {
    readonly index: number;
    readonly code: string;
    readonly detail: string;
  } | void;
}): Effect.Effect<EvaluatedCandidates<C>, E, R> =>
  Effect.gen(function* () {
    let remaining = candidates;
    const evicted: EvaluatedCandidates<C>["rejected"][number][] = [];
    for (;;) {
      const evaluated = yield* attempt(evaluate(remaining));
      if (!evaluated.ok) {
        // With no candidate left, the failure is not any transaction's.
        if (remaining.length === 0)
          return yield* Effect.failCause(evaluated.cause);
        const failing = yield* firstFailingPrefix(remaining.length, (length) =>
          attempt(evaluate(remaining.slice(0, length))).pipe(
            Effect.map((result) => result.ok),
          ),
        );
        const culprit = remaining[failing - 1]!;
        evicted.push({
          txId: culprit.ledgerTx.txId,
          code: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
          detail: commitStageFailureDetail(stage, causeError(evaluated.cause)),
        });
        remaining = remaining.filter((candidate) => candidate !== culprit);
        continue;
      }
      const broken = blockCheck?.(evaluated.value.accepted);
      if (broken !== undefined) {
        const culprit = evaluated.value.accepted[broken.index]!;
        evicted.push({
          txId: culprit.ledgerTx.txId,
          code: broken.code,
          detail: broken.detail,
        });
        remaining = remaining.filter(
          (candidate) =>
            !Buffer.from(candidate.ledgerTx.txId).equals(culprit.ledgerTx.txId),
        );
        continue;
      }
      return {
        accepted: evaluated.value.accepted,
        rejected: [...evaluated.value.rejected, ...evicted],
      };
    }
  });

/**
 * The block-wide CEK program-material check the DA committee runs on a
 * payload, over the normal transactions in commit order: every transaction's
 * programs are collected against the state immediately before it, and the
 * merged material must cover exactly their union within the bundle limits.
 * Each transaction's own material already covers its own programs (Phase B),
 * so the union can only fail on what the transactions add up to. Returns the
 * index of the transaction whose addition makes the check fail.
 */
export const normalProgramCoverageBreak = (
  accepted: readonly PhaseAValidatedTx[],
  preState: ReadonlyMap<string, Buffer>,
): { index: number; code: string; detail: string } | undefined => {
  const state = new Map(preState);
  const envelopes: (readonly MidgardCekProgramEnvelope[] | undefined)[] = [];
  for (const candidate of accepted) {
    try {
      envelopes.push(
        collectMidgardEventProgramEnvelopes(
          decodeMidgardNativeTxFullFromCanonicalCbor(
            candidate.submission.txCbor,
          ),
          (outRefHex) => state.get(outRefHex),
        ),
      );
    } catch {
      envelopes.push(undefined);
    }
    for (const spent of candidate.graph.spentOutRefHexes) state.delete(spent);
    for (const produced of candidate.graph.produced)
      state.set(
        produced[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(produced[Ledger.Columns.OUTPUT]),
      );
  }
  let failure: unknown;
  const holds = (length: number): boolean => {
    const prefix = envelopes.slice(0, length);
    if (prefix.some((entry) => entry === undefined)) {
      failure = "program envelopes could not be collected";
      return false;
    }
    try {
      assertMidgardCekProgramMaterialBundle(
        prefix.flatMap((entry) => entry ?? []),
        mergeMidgardCekProgramMaterialSidecars(
          accepted
            .slice(0, length)
            .flatMap(({ submission }) =>
              submission.programMaterialSidecarCbor === null
                ? []
                : [submission.programMaterialSidecarCbor],
            ),
        ),
      );
      return true;
    } catch (cause) {
      failure = cause;
      return false;
    }
  };
  if (holds(accepted.length)) return undefined;
  const fullFailure = failure;
  const failing = Effect.runSync(
    firstFailingPrefix(accepted.length, (length) =>
      Effect.sync(() => holds(length)),
    ),
  );
  return {
    index: failing - 1,
    code: COMMIT_REJECT_CODE_PROGRAM_COVERAGE,
    detail: commitStageFailureDetail(
      "Block-wide program material coverage",
      fullFailure,
    ),
  };
};
