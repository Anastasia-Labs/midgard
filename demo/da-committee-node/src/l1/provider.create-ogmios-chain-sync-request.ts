import { type OgmiosChainSyncRequest } from "./provider.local-node-chain-authority.js";
import {
  assertNetworkMagic,
  OgmiosRpcSession,
  parseOgmiosPoint,
  parseOgmiosPointOrOrigin,
} from "./provider.ogmios-rpc-session.js";
import {
  type CanonicalChainPoint,
  CHAIN_SYNC_INTERSECTION_POINTS,
  type ChainSyncEventBatch,
  getRecord,
  safeSlot,
  sameCanonicalPoint,
} from "./provider.parse-persisted-chain-sync-state.js";
import { L1SourceIntegrityError } from "./source-integrity.js";

export const createOgmiosChainSyncRequest = (): OgmiosChainSyncRequest => {
  let session: OgmiosRpcSession | undefined;
  let sessionUrl: string | undefined;
  let intersection: CanonicalChainPoint | undefined;
  // The caller position this session is synchronized with: the cursor it
  // intersected from, then each event point it hands back. A caller that did
  // not record a delivered event (a failed durable append, say) passes an older
  // cursor; continuing the session would then skip the events it lost.
  let delivered: CanonicalChainPoint | undefined;
  let pendingRollback: ChainSyncEventBatch | undefined;
  let suppressHandshakeRollback = false;
  // The node tip the session last reported. Once the session has delivered it,
  // the caller is at the tip, and Ogmios answers a nextBlock there only when
  // the node adopts another block.
  let reportedTip: CanonicalChainPoint | undefined;

  const disconnect = (): void => {
    session?.close();
    session = undefined;
    sessionUrl = undefined;
    intersection = undefined;
    delivered = undefined;
    pendingRollback = undefined;
    suppressHandshakeRollback = false;
    reportedTip = undefined;
  };

  const deliver = (batch: ChainSyncEventBatch): ChainSyncEventBatch => {
    reportedTip = batch.tip;
    if (batch.event !== undefined) {
      delivered = batch.event.point;
    }
    return batch;
  };

  return async (
    ogmiosUrl,
    cursor,
    intersectionCandidates,
    network,
    authorityNodeId,
    networkMagic,
  ) => {
    const source = `chain-sync:${authorityNodeId}`;
    for (let attempt = 0; attempt < 2; attempt += 1) {
      try {
        if (
          session === undefined ||
          sessionUrl !== ogmiosUrl ||
          cursor === undefined ||
          delivered === undefined ||
          !sameCanonicalPoint(cursor, delivered)
        ) {
          // Only a caller at the session's delivered point may continue it;
          // any other cursor re-intersects so no event is skipped or repeated.
          disconnect();
          session = await OgmiosRpcSession.open(ogmiosUrl);
          sessionUrl = ogmiosUrl;
          const genesis = getRecord(
            await session.request("queryNetwork/genesisConfiguration", {
              era: "shelley",
            }),
            "Ogmios genesis configuration",
          );
          assertNetworkMagic(
            network,
            safeSlot(
              genesis.networkMagic ?? genesis.network_magic,
              "Ogmios network magic",
            ),
            "Ogmios",
            networkMagic,
          );
          const bootstrapTip =
            cursor === undefined
              ? parseOgmiosPoint(
                  await session.request("queryNetwork/tip", {}),
                  network,
                  source,
                  "Ogmios bootstrap tip",
                )
              : undefined;
          const durableCandidates =
            cursor === undefined
              ? []
              : [
                  cursor,
                  ...(intersectionCandidates ?? []).filter(
                    (point) => !sameCanonicalPoint(point, cursor),
                  ),
                ].slice(0, CHAIN_SYNC_INTERSECTION_POINTS);
          const found = getRecord(
            await session.request("findIntersection", {
              points:
                cursor === undefined
                  ? [
                      {
                        slot: bootstrapTip!.slot,
                        id: bootstrapTip!.blockHash,
                      },
                      "origin",
                    ]
                  : [
                      ...durableCandidates.map((point) => ({
                        slot: point.slot,
                        id: point.blockHash,
                      })),
                      "origin",
                    ],
            }),
            "findIntersection result",
          );
          const tip = parseOgmiosPoint(
            found.tip,
            network,
            source,
            "findIntersection tip",
          );
          intersection = parseOgmiosPointOrOrigin(
            found.intersection,
            network,
            source,
            "findIntersection intersection",
          );
          delivered = cursor;
          suppressHandshakeRollback = true;
          if (cursor === undefined) {
            if (
              bootstrapTip === undefined ||
              intersection === undefined ||
              !sameCanonicalPoint(intersection, bootstrapTip)
            ) {
              throw new Error(
                "Ogmios bootstrap tip left the canonical chain before intersection; retrying from a fresh node-derived tip",
              );
            }
            return deliver({
              event: { direction: "roll_forward", point: bootstrapTip },
              tip,
            });
          }
          if (cursor !== undefined && intersection === undefined) {
            throw new L1SourceIntegrityError(
              "Ogmios rolled the durable chain-sync cursor back to origin; explicit state reset is required",
            );
          }
          if (
            cursor !== undefined &&
            intersection !== undefined &&
            !sameCanonicalPoint(intersection, cursor)
          ) {
            pendingRollback = {
              event: { direction: "roll_backward", point: intersection },
              tip,
            };
          }
          if (pendingRollback !== undefined) {
            const result = pendingRollback;
            pendingRollback = undefined;
            return deliver(result);
          }
          if (cursor !== undefined && sameCanonicalPoint(cursor, tip)) {
            return deliver({ tip });
          }
        } else if (
          reportedTip !== undefined &&
          sameCanonicalPoint(cursor, reportedTip)
        ) {
          // At the tip a nextBlock waits for the node's next block, up to the
          // request timeout, and the timeout then drops the session. Asking
          // for the tip first answers at once: while it is still the caller's
          // point there is nothing to deliver. Anything the node did since,
          // rollbacks included, stays queued on the session in order for the
          // nextBlock below or a later call.
          const tip = parseOgmiosPoint(
            await session.request("queryNetwork/tip", {}),
            network,
            source,
            "Ogmios tip",
          );
          if (sameCanonicalPoint(cursor, tip)) {
            return deliver({ tip });
          }
        }

        // Ogmios may echo the negotiated intersection as the first backward
        // response. It is a handshake acknowledgement, not a second rollback.
        for (
          let handshakeResponses = 0;
          handshakeResponses < 2;
          handshakeResponses += 1
        ) {
          const nextResult = getRecord(
            await session.request("nextBlock", {}),
            "nextBlock result",
          );
          const direction = nextResult.direction;
          const tip = parseOgmiosPoint(
            nextResult.tip,
            network,
            source,
            "nextBlock tip",
          );
          if (direction === "forward") {
            suppressHandshakeRollback = false;
            const block = getRecord(nextResult.block, "nextBlock block");
            return deliver({
              event: {
                direction: "roll_forward",
                point: parseOgmiosPoint(
                  block,
                  network,
                  source,
                  "roll-forward block",
                ),
              },
              tip,
            });
          }
          if (direction === "backward") {
            const point = parseOgmiosPointOrOrigin(
              nextResult.point,
              network,
              source,
              "roll-backward point",
            );
            if (
              suppressHandshakeRollback &&
              point === undefined &&
              intersection === undefined &&
              cursor === undefined
            ) {
              suppressHandshakeRollback = false;
              continue;
            }
            if (point === undefined) {
              throw new L1SourceIntegrityError(
                "Ogmios rolled chain sync back to origin; explicit state reset is required",
              );
            }
            if (
              suppressHandshakeRollback &&
              intersection !== undefined &&
              sameCanonicalPoint(point, intersection)
            ) {
              suppressHandshakeRollback = false;
              if (cursor !== undefined && sameCanonicalPoint(cursor, tip)) {
                return deliver({ tip });
              }
              continue;
            }
            suppressHandshakeRollback = false;
            return deliver({
              event: { direction: "roll_backward", point },
              tip,
            });
          }
          throw new Error("Ogmios nextBlock returned an unsupported direction");
        }
        throw new Error(
          "Ogmios repeated its chain-sync handshake rollback response",
        );
      } catch (error) {
        disconnect();
        if (attempt === 1) {
          throw error;
        }
      }
    }
    throw new Error("Ogmios chain-sync reconnect exhausted");
  };
};
