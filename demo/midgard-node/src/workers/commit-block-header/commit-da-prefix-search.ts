import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";

import type { processMpfs } from "../../mpf/process.process-mpfs.js";
import { CommitWorkerInvariantError } from "../commit-block-header.pending-user-event-counts-up-to.js";
import type { PlannedCommitBatchSelection } from "../utils/commit-block-planner.js";
import { measureCommitDaPayloadUpperBound } from "./submission.assert-pre-submit-da-payload-size.js";

export const measureBuiltCommitDaPrefixes = (
  built: Effect.Effect.Success<ReturnType<typeof processMpfs>>,
  baseRoot: string,
  consensusProfile: MidgardConsensusProfile,
  config: {
    readonly NETWORK: string;
    readonly MIN_FEE_A: bigint;
    readonly MIN_FEE_B: bigint;
  },
) =>
  measureCommitDaPayloadUpperBound({
    ...built,
    rejectedTxIds: built.rejectedMempoolTxHashes,
    identityContext: Buffer.from(
      JSON.stringify([
        baseRoot,
        built.effectiveBlockEndTime?.getTime(),
        consensusProfile,
        config.NETWORK,
        config.MIN_FEE_A.toString(),
        config.MIN_FEE_B.toString(),
      ]),
    ),
  });

export const assertCompleteDaPrefixSearch = (outcome: string) =>
  outcome === "incomplete" || outcome === "unmeasured"
    ? Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Complete DA prefix search could not revalidate the selected source material",
        }),
      )
    : Effect.void;

export const logDaPrefixPreselection = (
  plan: PlannedCommitBatchSelection,
  baseEntryCount: number,
) =>
  plan.prunedTxCount === 0
    ? Effect.void
    : Effect.logInfo(
        `🔹 Commit batch planner trimmed the selection to the base ledger's DA frame tx_count=${plan.plan.selectedTxCount.toString()}, estimated_da_payload_bytes=${plan.plan.estimatedDaPayloadBytes.toString()}, base_utxo_entry_count=${baseEntryCount.toString()}, pruned_tx_count=${plan.prunedTxCount.toString()}.`,
      );

export const logMpfProcessingFinished = (startedAtMs: number) =>
  Effect.sync(() => Date.now()).pipe(
    Effect.tap((finishedAtMs) =>
      Effect.logInfo(
        `pipeline_trace phase=mpf_processing_finished at_ms=${finishedAtMs.toString()} duration_ms=${Math.max(0, finishedAtMs - startedAtMs).toString()}`,
      ),
    ),
  );
