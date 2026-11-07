import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
} from "@al-ft/midgard-core/da-payload-sizing";
import { Effect, Logger } from "effect";

import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../src/database/utils/tx.js";
import { measureCommitDaPayloadUpperBound } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import {
  type CommitDaFrameMeasurement,
  type CommitTxCandidateSelection,
  selectCommitTxCandidates,
  stepDownCommitSelectionToDaFrame,
} from "../src/workers/utils/commit-block-planner.js";
import {
  blockContentFor,
  type BlockShape,
  mkCandidate,
} from "./helpers/commit-da-frame-fixtures.js";

export const quiet = <A, E>(effect: Effect.Effect<A, E, unknown>) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

export const txIdsOf = (txs: readonly EntryWithTimeStamp[]) =>
  txs.map((entry) => entry[TxColumns.TX_ID].toString("hex"));

/**
 * Runs the step-down over a fake block build whose DA content has `shape`,
 * measured by the commit program's real upper-bound measurement.
 */
export const stepDown = async (
  candidateSelection: CommitTxCandidateSelection,
  mode: DaPayloadEmissionMode,
  shape: BlockShape,
) => {
  const processed: (readonly EntryWithTimeStamp[])[] = [];
  const measured: (CommitDaFrameMeasurement | undefined)[] = [];
  let rebases = 0;
  const result = await quiet(
    stepDownCommitSelectionToDaFrame({
      candidateSelection,
      maxInnerBytes: maxDaPayloadInnerBytes(mode),
      process: (selection) =>
        Effect.sync(() => {
          processed.push(selection.candidateTxs);
          return blockContentFor(selection.candidateTxs, shape);
        }),
      measure: (built) =>
        measureCommitDaPayloadUpperBound({
          ...built,
          rejectedTxIds: [],
        }).pipe(Effect.tap((measurement) => measured.push(measurement))),
      rebase: Effect.sync(() => {
        rebases += 1;
      }),
    }),
  );
  return { result, processed, measured, rebases };
};

export const threeTransfers = () =>
  selectCommitTxCandidates({
    mempoolTxs: Array.from({ length: 3 }, (_, index) => mkCandidate(index + 1)),
    processedMempoolTxs: [],
  });

// A transfer carrying about 336 KB of DA weight the planner's estimate does
// not see (see SYNTHETIC_TX_DA_BYTES).
export const HEAVY_TRANSFER: BlockShape = { descriptorBytes: 336_000 };
