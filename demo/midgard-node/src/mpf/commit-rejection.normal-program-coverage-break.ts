/**
 * The block-wide CEK program-material coverage check over a build's normal
 * transactions, as a block check of `evaluateEvictingUncommittable`.
 */

import {
  assertMidgardCekProgramMaterialBundle,
  mergeMidgardCekProgramMaterialSidecars,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { collectMidgardEventProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import type { PhaseAValidatedTx } from "@al-ft/midgard-validation";
import { Cause, Effect } from "effect";

import * as Ledger from "../database/utils/ledger.js";
import {
  type BlockCheckBreak,
  commitStageFailureDetail,
  firstFailingPrefix,
  isNodeFault,
} from "./commit-rejection.evict-uncommittable-normal-txs.js";
import { COMMIT_REJECT_CODE_PROGRAM_COVERAGE } from "./commit-rejection.js";

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
