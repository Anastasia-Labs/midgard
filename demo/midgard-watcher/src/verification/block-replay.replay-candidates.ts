import { makeReturn } from "@al-ft/midgard-sdk";
import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";
import { LedgerColumns } from "@al-ft/midgard-validation/ledger";
import { runPhaseBValidationWithPatch } from "@al-ft/midgard-validation/phase-b";
import type { PhaseAValidatedTx } from "@al-ft/midgard-validation/types";

import { type EvaluateWatcherBlockReplayCandidatesInput } from "./block-replay.validate-event-authority.js";
import {
  type WatcherBlockReplayEventAuthorityRecord,
  watcherBlockReplayPriorState,
} from "./block-replay.watcher-block-replay-prior-state.js";
import { REACHABLE_SET } from "./block-replay.watcher-block-replay-reason-codes.js";
import {
  normalizeRootHex,
  watcherBlockReplayRejectionProjection,
} from "./block-replay.watcher-block-replay-rejection-projection.js";
import {
  fail,
  type WatcherBlockReplayEventRoot,
  type WatcherBlockReplayForcedValidationFact,
  type WatcherBlockReplayIntermediateRoot,
  type WatcherBlockReplayReasonCode,
  type WatcherBlockReplayRejection,
  type WatcherBlockReplayStageMismatch,
  type WatcherBlockReplayTransactionRoot,
} from "./block-replay.watcher-block-replay-result.js";

export type ReplayCore = {
  readonly eventAuthorityRecords?: readonly WatcherBlockReplayEventAuthorityRecord[];
  readonly reasonCodes: Set<WatcherBlockReplayReasonCode>;
  readonly stageMismatches: WatcherBlockReplayStageMismatch[];
  readonly rejections: WatcherBlockReplayRejection[];
  readonly acceptedTxIds: string[];
  readonly intermediateRoots: WatcherBlockReplayIntermediateRoot[];
  readonly transactionRoots: WatcherBlockReplayTransactionRoot[];
  readonly eventRoots: WatcherBlockReplayEventRoot[];
  readonly forcedValidationFacts: WatcherBlockReplayForcedValidationFact[];
  readonly priorStateRoot: string;
  readonly postStateRoot: string;
  readonly authorityManifestDigest: string | null;
  readonly sourceManifestDigest: string | null;
  readonly effectManifestDigest: string | null;
};

export const buildLedgerOperations = (
  accepted: readonly PhaseAValidatedTx[],
): readonly {
  readonly txId: string;
  readonly operations: readonly ValidationMachineLedgerOp[];
}[] =>
  accepted.map((candidate) => ({
    txId: candidate.ledgerTx.txId.toString("hex"),
    operations: [
      ...candidate.graph.spentOutRefHexes.map(
        (outRefHex) =>
          ({
            type: "delete",
            key: Buffer.from(outRefHex, "hex"),
          }) satisfies ValidationMachineLedgerOp,
      ),
      ...candidate.graph.produced.map((produced) =>
        buildValidationMachineLedgerInsertOp({
          key: produced[LedgerColumns.OUTREF],
          outputCbor: produced[LedgerColumns.OUTPUT],
        }),
      ),
    ],
  }));

export const replayCandidates = async (
  input: EvaluateWatcherBlockReplayCandidatesInput,
): Promise<ReplayCore> => {
  const reasonCodes = new Set<WatcherBlockReplayReasonCode>();
  const stageMismatches: WatcherBlockReplayStageMismatch[] = [];

  const prior = await watcherBlockReplayPriorState(input.priorState);
  if (prior.root !== input.expectedPriorStateRoot) {
    // Fail before the replay: a prior state that is not the committed one
    // cannot produce meaningful intermediate roots, and continuing would
    // manufacture an attributed fault against the operator out of the
    // watcher's own bad input.
    reasonCodes.add("prior_state_root_mismatch");
    stageMismatches.push({
      stage: "prior_state",
      reasonCode: "prior_state_root_mismatch",
      field: "$.header.prevUtxosRoot",
      expected: input.expectedPriorStateRoot,
      actual: prior.root,
    });
    return {
      reasonCodes,
      stageMismatches,
      rejections: [],
      acceptedTxIds: [],
      intermediateRoots: [],
      transactionRoots: [],
      eventRoots: [],
      forcedValidationFacts: [],
      priorStateRoot: prior.root,
      postStateRoot: prior.root,
      authorityManifestDigest: null,
      sourceManifestDigest: null,
      effectManifestDigest: null,
    };
  }

  const indexByTxId = new Map<string, number>();
  for (const [index, candidate] of input.candidates.entries()) {
    indexByTxId.set(candidate.ledgerTx.txId.toString("hex"), index);
  }

  const preState = new Map<string, Buffer>(
    prior.ledgerEntries.map((entry) => [
      entry.outRef.toString("hex"),
      entry.output,
    ]),
  );

  let phaseB;
  try {
    phaseB = await makeReturn(
      runPhaseBValidationWithPatch(input.candidates, preState, input.config),
    ).unsafeRun();
  } catch {
    return fail("canonical_validation_threw", "$.phaseB");
  }

  const rejections = phaseB.rejected
    .map((rejected) =>
      watcherBlockReplayRejectionProjection({ rejected, indexByTxId }),
    )
    .sort((left, right) => left.index - right.index);
  for (const rejection of rejections) {
    if (!REACHABLE_SET.has(rejection.code)) {
      reasonCodes.add("undeclared_reachable_code");
    }
    reasonCodes.add("phase_b_rejection");
  }

  const perTx = buildLedgerOperations(phaseB.accepted);
  let mutationSteps;
  try {
    mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: prior.ledgerEntries,
      operations: perTx.flatMap((entry) => entry.operations),
    });
  } catch {
    return fail("canonical_replay_threw", "$.intermediateRoots");
  }

  const intermediateRoots: WatcherBlockReplayIntermediateRoot[] = [];
  const transactionRoots: WatcherBlockReplayTransactionRoot[] = [];
  let cursor = 0;
  let postStateRoot = prior.root;
  for (const [txIndex, entry] of perTx.entries()) {
    const first = cursor;
    for (
      let remaining = entry.operations.length;
      remaining > 0;
      remaining -= 1
    ) {
      const step = mutationSteps[cursor];
      const operation = step.operation;
      intermediateRoots.push(
        Object.freeze({
          sequence: cursor,
          txIndex,
          txId: entry.txId,
          stepIndex: null,
          phase: null,
          operation: operation.type,
          outRef: operation.key.toString("hex"),
          preRoot: normalizeRootHex(step.preRoot.toString("hex")),
          postRoot: normalizeRootHex(step.postRoot.toString("hex")),
        }),
      );
      cursor += 1;
    }
    const preRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[first].preRoot.toString("hex"));
    postStateRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[cursor - 1].postRoot.toString("hex"));
    transactionRoots.push(
      Object.freeze({
        txIndex,
        txId: entry.txId,
        preRoot,
        postRoot: postStateRoot,
        mutationCount: cursor - first,
        committedStepIndex: null,
        committedPreRoot: null,
        committedPostRoot: null,
      }),
    );
  }

  return {
    reasonCodes,
    stageMismatches,
    rejections,
    acceptedTxIds: phaseB.accepted.map((candidate) =>
      candidate.ledgerTx.txId.toString("hex"),
    ),
    intermediateRoots,
    transactionRoots,
    eventRoots: [],
    forcedValidationFacts: [],
    priorStateRoot: prior.root,
    postStateRoot,
    authorityManifestDigest: null,
    sourceManifestDigest: null,
    effectManifestDigest: null,
  };
};
