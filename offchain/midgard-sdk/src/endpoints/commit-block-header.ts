import { Config as FetchConfig } from "@/endpoints/state-queue/fetch-latest-block.js";
import { makeReturn } from "../core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { StateQueue, ActiveOperators } from "../tx-builder/index.js";
import { Effect } from "effect";
import { errorToString } from "@/utils/common.js";

export const commitBlockHeaderProgram = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig,
  sqCommitParams: StateQueue.CommitBlockParams,
  aoUpdateParams: ActiveOperators.UpdateCommitmentTimeParams
): Effect.Effect<TxSignBuilder, string> =>
  Effect.gen(function* () {
    const commitTx = yield* StateQueue.commitTxBuilder(
      lucid,
      fetchConfig,
      sqCommitParams
    );
    const completedTx = yield* Effect.tryPromise({
      try: () =>
        commitTx
          .compose(
            ActiveOperators.updateCommitmentTimeTxBuilder(lucid, aoUpdateParams)
          )
          .complete(),
      catch: (e) => errorToString(e),
    });
    return completedTx;
  });

/**
 * Builds completed tx for submitting a new block using the provided
 * `LucidEvolution` instance, fetch config, and required parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param sqCommitParams - Parameters required for committing to state queue.
 * @param aoUpdateParams - Parameters required for updating the active operator's commitment time.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const commitBlockHeader = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig,
  sqCommitParams: StateQueue.CommitBlockParams,
  aoUpdateParams: ActiveOperators.UpdateCommitmentTimeParams
): Promise<TxSignBuilder> =>
  makeReturn(
    commitBlockHeaderProgram(lucid, fetchConfig, sqCommitParams, aoUpdateParams)
  ).unsafeRun();
