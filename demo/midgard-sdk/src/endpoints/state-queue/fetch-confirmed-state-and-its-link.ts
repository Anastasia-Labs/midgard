import { Effect } from "effect";
import { LucidEvolution, UTxO, fromText, toUnit } from "@lucid-evolution/lucid";
import { makeReturn } from "../../core.js";
import { getConfirmedStateFromUTxO } from "../../utils/state-queue.js";
import { getNodeDatumFromUTxO } from "../../utils/linked-list.js";
import { LedgerState, LinkedList, StateQueue } from "../../tx-builder/index.js";
import { utxosAtByNFTPolicyId } from "@/utils/common.js";
export const fetchConfirmedStateAndItsLinkProgram = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
): Effect.Effect<{ confirmed: UTxO; link?: UTxO }, Error> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
      // toUnit(config.stateQueuePolicyId, fromText("Node"))
    );
    let confirmedStateResult:
      | { data: LedgerState.ConfirmedState; link: LinkedList.NodeKey }
      | undefined;
    const filteredForConfirmedState = yield* Effect.allSuccesses(
      allUTxOs.map((u: UTxO) => {
        const confirmedStateEffect = getConfirmedStateFromUTxO(u);
        return Effect.map(confirmedStateEffect, (confirmedState) => {
          confirmedStateResult = confirmedState;
          return u;
        });
      }),
    );
    // console.log("allUTxOs :>> ", allUTxOs);
    if (filteredForConfirmedState.length === 1 && confirmedStateResult) {
      const confirmedStateUTxO = filteredForConfirmedState[0];
      if (confirmedStateResult.link !== "Empty") {
        const firstLink = confirmedStateResult.link.Key;
        console.log("firstLink :>> ", firstLink);
        const filteredForLink = yield* Effect.allSuccesses(
          allUTxOs.map((u: UTxO) => {
            const nodeDatumEffect = getNodeDatumFromUTxO(u);
            return Effect.andThen(nodeDatumEffect, (nodeDatum) => {
              console.log("nodeDatum :>> ", nodeDatum);
              if (
                nodeDatum.key !== "Empty" &&
                nodeDatum.key.Key.key === firstLink.key
              ) {
                return Effect.succeed(u);
              } else {
                return Effect.fail(
                  new Error(
                    "Link is either a root, or its key doesn't match with what the root is pointing to",
                  ),
                );
              }
            });
          }),
        );
        if (filteredForLink.length === 1) {
          return { confirmed: confirmedStateUTxO, link: filteredForLink[0] };
        } else {
          return yield* Effect.fail(
            new Error("Confirmed state's link not found"),
          );
        }
      } else {
        return { confirmed: confirmedStateUTxO };
      }
    } else {
      return yield* Effect.fail(new Error("Confirmed state not found"));
    }
  });

/**
 * Attempts fetching the confirmed state, i.e. the root node of the state queue
 * linked list, along with its link (i.e. first non-root node in the list).
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic UTxO which is the root node.
 */
export const fetchConfirmedStateAndItsLink = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
) =>
  makeReturn(fetchConfirmedStateAndItsLinkProgram(lucid, config)).unsafeRun();
