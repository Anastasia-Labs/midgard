import { Effect } from "effect";
import { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import {
  utxosAtByNFTPolicyId,
  getLinkFromBlockUTxO,
} from "../../utils/index.js";
import { makeReturn } from "../../core.js";
import { StateQueue } from "../../tx-builder/index.js";

export const fetchLatestCommitedBlockProgram = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
): Effect.Effect<UTxO, Error> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("Fetching latest commited block...");
    const allBlocks = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    yield* Effect.logInfo("allBlocks", allBlocks.length);
    const filtered = yield* Effect.allSuccesses(
      allBlocks.map((u: UTxO) => {
        const nodeKeyEffect = getLinkFromBlockUTxO(u);
        return Effect.andThen(nodeKeyEffect, (nodeKey) =>
          nodeKey === "Empty"
            ? Effect.succeed(u)
            : Effect.fail(new Error("Not a tail node")),
        );
      }),
    );
    if (filtered.length === 1) {
      return filtered[0];
    } else {
      return yield* Effect.fail(new Error("Latest block not found"));
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
  config: StateQueue.FetchConfig,
) => makeReturn(fetchLatestCommitedBlockProgram(lucid, config)).unsafeRun();
