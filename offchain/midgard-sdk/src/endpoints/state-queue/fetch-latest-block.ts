import { Effect, Either } from "effect";
import { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { utxosAtByNFTPolicyId } from "@/utils/common.js";
import { makeReturn } from "@/core.js";
import { getLinkFromBlockUTxO } from "@/utils/state-queue.js";
import { FetchConfig } from "@/types/state-queue.js";

export const fetchLatestCommitedBlockProgram = (
  lucid: LucidEvolution,
  config: FetchConfig
): Effect.Effect<UTxO, string> =>
  Effect.gen(function* () {
    const allBlocks = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId
    );
    const filtered = allBlocks.filter((u: UTxO) => {
      const eithNodeKey = getLinkFromBlockUTxO(u);
      return Either.isRight(eithNodeKey) && eithNodeKey.right === "Empty";
    });
    if (filtered.length === 1) {
      return filtered[0];
    } else {
      return yield* Effect.fail("Latest block not found");
    }
  });

/**
 * Attempts fetching the commited block at the very end of the state queue
 * linked list.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic UTxO which links to no other nodes.
 */
export const fetchLatestCommitedBlock = (
  lucid: LucidEvolution,
  config: FetchConfig
) => makeReturn(fetchLatestCommitedBlockProgram(lucid, config)).unsafeRun();
