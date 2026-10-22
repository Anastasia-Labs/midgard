/**
 * Checks one entry: rebuilds its transaction's script runs, and their
 * script contexts (the import builds none), from the stored material
 * (`ImportCheckMaterial`) with the same validation pipeline the import
 * replayed it with, and evaluates each run through `evaluate`
 * instead of the committed verdict. Only a script verdict comes out of a
 * rebuild that differs from the import's: any other rejection, or a
 * rebuild that fails, is a fault.
 */
import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  decodeMidgardLedgerTxFromCanonicalCbor,
  type MidgardCekVerdictRequest,
  midgardOutRefToCborHex,
  RejectCodes,
  replayValidationVerdictEvent,
} from "@al-ft/midgard-validation";
import { Effect, Either } from "effect";

import { type ImportCheckMaterial } from "../database/importChecks.js";
import { type ImportCheckOutcome } from "./evaluator.js";

/**
 * - `accepted`: every run accepted within its declared budget;
 * - `rejected`: a run rejected or over its declared budget;
 * - `fault`: a run faulted (with its script's sha256), or the rebuild
 *   failed or disagreed with the import;
 * - `timeout`: a run was killed at its kill, so no verdict was reached;
 * - `cancelled`: a run was not started in time.
 */
export type ImportCheckVerdict =
  | Readonly<{ kind: "accepted" }>
  | Readonly<{ kind: "rejected"; detail: string }>
  | Readonly<{ kind: "fault"; detail: string; scriptHash?: Buffer }>
  | Readonly<{ kind: "timeout"; detail: string }>
  | Readonly<{ kind: "cancelled" }>;

/** The out-refs (CBOR) a transaction spends or references; none when it
 * does not decode (its rebuild then fails). */
export const importCheckOutRefs = (transactionCbor: Buffer): Buffer[] => {
  try {
    const transaction = decodeMidgardLedgerTxFromCanonicalCbor(transactionCbor);
    return [...transaction.spendInputs, ...transaction.referenceInputs].map(
      (outRef) => Buffer.from(midgardOutRefToCborHex(outRef), "hex"),
    );
  } catch {
    return [];
  }
};

/** Rebuilds `material`'s runs and evaluates them through `evaluate`. */
export const rebuildAndEvaluate = (
  transactionId: Buffer,
  material: ImportCheckMaterial,
  evaluate: (
    run: MidgardCekVerdictRequest,
  ) => Effect.Effect<ImportCheckOutcome>,
): Effect.Effect<ImportCheckVerdict> =>
  Effect.gen(function* () {
    let stopped: ImportCheckVerdict | undefined;
    const replay = yield* replayValidationVerdictEvent({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      canonicalTransactionCbor: material.transactionCbor,
      ledgerWitnessEntries: material.outputs,
      ...material.context,
      sourceKind: "normal",
      // Each run's context is built on this thread: yield between runs.
      evaluateScriptRun: (run) =>
        Effect.zipRight(Effect.yieldNow(), evaluate(run)).pipe(
          Effect.flatMap((outcome) => {
            if (outcome.kind === "evaluated")
              return Effect.succeed(outcome.result);
            stopped =
              outcome.kind !== "fault"
                ? outcome
                : {
                    kind: "fault",
                    detail: outcome.detail,
                    scriptHash: createHash("sha256")
                      .update(run.scriptBytes)
                      .digest(),
                  };
            return Effect.fail(new Error("import check run stopped"));
          }),
        ),
    }).pipe(
      Effect.catchAllDefect((defect) =>
        Effect.fail(new Error(formatUnknownError(defect))),
      ),
      Effect.either,
    );
    if (stopped !== undefined) return stopped;
    if (Either.isLeft(replay))
      return {
        kind: "fault",
        detail: `rebuild failed: ${replay.left.message}`,
      };
    if (!replay.right.transactionId.equals(transactionId))
      return { kind: "fault", detail: "rebuild differs from the import" };
    const rejection = replay.right.rejection;
    if (rejection === undefined) return { kind: "accepted" };
    if (rejection.code === RejectCodes.PlutusScriptInvalid)
      return { kind: "rejected", detail: rejection.detail ?? rejection.code };
    return {
      kind: "fault",
      detail: `rebuild differs from the import: ${rejection.code}`,
    };
  });
