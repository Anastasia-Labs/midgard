import { LucidEvolution } from "@lucid-evolution/lucid";
import { StateQueue } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { utxosAtByNFTPolicyId } from "@/utils/common.js";
import { makeReturn } from "@/core.js";
import {
  sortStateQueueUTxOs,
  utxosToStateQueueUTxOs,
} from "@/utils/state-queue.js";
import { StateQueueUTxO } from "@/tx-builder/state-queue/types.js";

export const fetchSortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
): Effect.Effect<StateQueueUTxO[], Error> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    const unsorted = yield* utxosToStateQueueUTxOs(
      allUTxOs,
      config.stateQueuePolicyId,
    );
    return yield* sortStateQueueUTxOs(unsorted);
  });

export const fetchUnsortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
): Effect.Effect<StateQueueUTxO[], Error> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    return yield* utxosToStateQueueUTxOs(
      allUTxOs,
      config.stateQueuePolicyId,
    );
  });

/**
 * Attempts fetching the whole state queue linked list.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO[]} - All the authentic node UTxOs.
 */
export const fetchSortedStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
) => makeReturn(fetchSortedStateQueueUTxOsProgram(lucid, config)).unsafeRun();

export const fetchUnsortedStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
) => makeReturn(fetchUnsortedStateQueueUTxOsProgram(lucid, config)).unsafeRun();
