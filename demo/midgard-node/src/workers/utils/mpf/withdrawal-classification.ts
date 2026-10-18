import { findLedgerSignatureVerifierUnavailable } from "@al-ft/midgard-core";
import { LOVELACE_UNIT, normalizeAssets } from "@al-ft/midgard-core/assets";
import {
  attributeWithdrawalClassificationFault,
  isReplayLocalFault,
} from "@al-ft/midgard-fault-proofs/block-replay";
import { classifyWithdrawalFromLedger } from "@al-ft/midgard-sdk";
import { awaitLedgerEd25519SignatureReady } from "@al-ft/midgard-validation";
import { Data, Effect, Option } from "effect";

import { DatabaseError } from "../../../database/utils/common.js";
import * as Ledger from "../../../database/utils/ledger.js";
import * as WithdrawalsDB from "../../../database/withdrawals.js";
import { heldBuildError } from "../../../mpf/commit-rejection.evict-uncommittable-normal-txs.js";
import type { WithdrawalUnclassifiableCase } from "../commit-block-header.js";

/** The commit stage a withdrawal classification's local fault holds. */
export const WITHDRAWAL_CLASSIFICATION_STAGE = "Withdrawal classification";

/**
 * A due withdrawal row this node cannot classify, named by its event id
 * (hex). Its `kind` says why: `undecodable`, its event info, l2_value or
 * l2_outref, or the ledger output it names, does not decode; or
 * `classifier_failed`, it decodes and the classifier failed on this node.
 * Carried as the cause of the classification's `DatabaseError`, it marks the
 * commit worker's failure as a local fault on that row, which the parent
 * bounds (`boundCommitWorkerLocalFaults`).
 */
export class WithdrawalUnclassifiableError extends Data.TaggedError(
  "WithdrawalUnclassifiableError",
)<{
  readonly eventId: string;
  readonly kind: WithdrawalUnclassifiableCase;
  readonly message: string;
  readonly cause: unknown;
}> {}

/** The `WithdrawalUnclassifiableError` `error` is or has as a cause, if any. */
export const findWithdrawalUnclassifiable = (
  error: unknown,
): WithdrawalUnclassifiableError | undefined => {
  let current = error;
  for (let depth = 0; depth < 8; depth += 1) {
    if (current instanceof WithdrawalUnclassifiableError) return current;
    if (typeof current !== "object" || current === null) return undefined;
    current = (current as { cause?: unknown }).cause;
  }
  return undefined;
};

/** `error` with its cause as the row's `WithdrawalUnclassifiableError`. */
const unclassifiable = (
  entry: WithdrawalsDB.Entry,
  error: { readonly message: string; readonly cause?: unknown },
): DatabaseError =>
  new DatabaseError({
    table: WithdrawalsDB.tableName,
    message: error.message,
    cause: new WithdrawalUnclassifiableError({
      eventId: entry[WithdrawalsDB.Columns.ID].toString("hex"),
      kind: "undecodable",
      message: error.message,
      cause: error.cause,
    }),
  });

export type ClassifiedWithdrawal = {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly validity: WithdrawalsDB.Validity;
  readonly validityDetail: unknown;
  readonly settlementEventInfo: Buffer;
  readonly shouldDeleteLedgerUtxo: boolean;
};

export { LOVELACE_UNIT, normalizeAssets };

export const indexSelectedLedgerOutputs = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<ReadonlyMap<string, Buffer>, DatabaseError, never> =>
  Effect.try({
    try: () => {
      const outputs = new Map<string, Buffer>();
      for (const entry of entries) {
        const outRef = entry[Ledger.Columns.OUTREF].toString("hex");
        if (outputs.has(outRef)) {
          throw new Error(
            `selected ledger snapshot contains duplicate outref ${outRef}`,
          );
        }
        outputs.set(outRef, Buffer.from(entry[Ledger.Columns.OUTPUT]));
      }
      return outputs;
    },
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message:
          "Failed to index selected ledger snapshot for withdrawal classification",
        cause,
      }),
  });
export const classifyWithdrawal = ({
  entry,
  ledgerOutRef,
  ledgerOutput,
}: {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly ledgerOutput: Option.Option<Buffer>;
}): Effect.Effect<ClassifiedWithdrawal, DatabaseError, never> => {
  const input = {
    l2Owner: entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex"),
    l2ValueCbor: entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
    eventInfoCbor: entry[WithdrawalsDB.Columns.RAW_EVENT_INFO].toString("hex"),
    ledgerOutRef,
    ledgerOutput: Option.getOrNull(ledgerOutput),
  };
  return attributeWithdrawalClassificationFault(
    input,
    classifyWithdrawalFromLedger(input),
  ).pipe(
    Effect.map((classification) => ({
      entry,
      ledgerOutRef,
      ...classification,
    })),
    // A verifier fault stays the cause as it is, so the commit path can tell
    // this process's fault from a failure of the withdrawal's bytes, which
    // names the row.
    Effect.mapError((error) => {
      const verifier = findLedgerSignatureVerifierUnavailable(error);
      return verifier === undefined
        ? unclassifiable(entry, error)
        : new DatabaseError({
            table: WithdrawalsDB.tableName,
            message: error.message,
            cause: verifier,
          });
    }),
    // A runtime fault of the classifier is this node's: the withdrawal is
    // not classified, and the build holds (nothing is committed or
    // projected) until a later build classifies it. The hold names the row,
    // so the commit worker's failure is a local fault on it.
    Effect.catchSomeDefect((defect) =>
      isReplayLocalFault(defect)
        ? Option.some(
            Effect.logError("WithdrawalClassificationLocalFault").pipe(
              Effect.annotateLogs({
                alarm: "withdrawal_classification_local_fault",
                eventId: entry[WithdrawalsDB.Columns.ID].toString("hex"),
                detail: defect.message,
              }),
              Effect.zipRight(
                Effect.fail(
                  heldBuildError(WITHDRAWAL_CLASSIFICATION_STAGE, {
                    table: WithdrawalsDB.tableName,
                    message: `Holding the block build: withdrawal classification failed locally: ${defect.message}`,
                    cause: new WithdrawalUnclassifiableError({
                      eventId: entry[WithdrawalsDB.Columns.ID].toString("hex"),
                      kind: "classifier_failed",
                      message: defect.message,
                      cause: defect,
                    }),
                  }),
                ),
              ),
            ),
          )
        : Option.none(),
    ),
  );
};

/**
 * Classifies a block's withdrawals in trace-step order. Each one is judged
 * against the ledger the earlier payable withdrawals leave: a payable
 * withdrawal deletes its outref, a non-payable one changes nothing, and the
 * withdrawal phase runs before any other phase. A later withdrawal of an
 * outref an earlier one drained finds no output, so it is
 * `NonExistentWithdrawalUtxo`, which is what foreign replay derives.
 */
export const classifyWithdrawalsInOrder = ({
  entries,
  baseOutputs,
}: {
  readonly entries: readonly WithdrawalsDB.Entry[];
  readonly baseOutputs: ReadonlyMap<string, Buffer>;
}): Effect.Effect<readonly ClassifiedWithdrawal[], DatabaseError, never> =>
  Effect.gen(function* () {
    // Signature checks are libsodium's, which answer only once initialised.
    // A verifier that cannot answer fails as its typed fault, kept as the
    // cause exactly as `classifyWithdrawal` keeps it.
    yield* awaitLedgerEd25519SignatureReady.pipe(
      Effect.mapError(
        (fault) =>
          new DatabaseError({
            table: WithdrawalsDB.tableName,
            message: fault.message,
            cause: fault,
          }),
      ),
    );
    const drained = new Set<string>();
    const classified: ClassifiedWithdrawal[] = [];
    for (const entry of entries) {
      const ledgerOutRef = yield* WithdrawalsDB.toLedgerOutRef(entry).pipe(
        Effect.mapError((error) => unclassifiable(entry, error)),
      );
      const ledgerOutRefHex = ledgerOutRef.toString("hex");
      const output = drained.has(ledgerOutRefHex)
        ? undefined
        : baseOutputs.get(ledgerOutRefHex);
      const withdrawal = yield* classifyWithdrawal({
        entry,
        ledgerOutRef,
        ledgerOutput:
          output === undefined
            ? Option.none<Buffer>()
            : Option.some(Buffer.from(output)),
      });
      if (withdrawal.shouldDeleteLedgerUtxo) drained.add(ledgerOutRefHex);
      classified.push(withdrawal);
    }
    return classified;
  });
