import { OgmiosJsonRpcError } from "@al-ft/midgard-core/ogmios-json-rpc-error";

import type { PromiseCapacityPoint } from "../availability/promise-capacity-evidence.js";
import type { PromiseCanonicalPointReader } from "../availability/promise-cutoff-source.js";
import {
  HEX_32,
  openRpc,
  type StateQueueReplayWebSocket,
  type StateQueueReplayWebSocketFactory,
} from "./state-queue-replay-provider.open-rpc.js";

const selectedTip = (value: unknown) => {
  if (value === null || typeof value !== "object")
    throw new Error("Capacity proof has no selected-chain tip");
  const tip = value as Record<string, unknown>;
  if (
    typeof tip.id !== "string" ||
    !HEX_32.test(tip.id) ||
    typeof tip.slot !== "number" ||
    !Number.isSafeInteger(tip.slot) ||
    tip.slot < 0 ||
    typeof tip.height !== "number" ||
    !Number.isSafeInteger(tip.height) ||
    tip.height < 0
  )
    throw new Error("Capacity proof selected-chain tip is malformed");
  return { slot: tip.slot, blockHash: tip.id, blockNo: tip.height };
};
/** Ogmios6.9 exact intersection and raw immediate successor bind retained height. */
export const promiseCanonicalPointReader =
  (
    ogmiosUrl: string,
    factory: StateQueueReplayWebSocketFactory = (url) =>
      new WebSocket(url) as unknown as StateQueueReplayWebSocket,
    expectedTip?: PromiseCapacityPoint,
  ): PromiseCanonicalPointReader =>
  async (point) => {
    let socket: StateQueueReplayWebSocket | undefined;
    const finish = <T>(result: T): T => {
      socket?.close();
      socket = undefined;
      return result;
    };
    try {
      const rpc = await openRpc(ogmiosUrl, (url) => {
        socket = factory(url);
        return socket;
      });
      let found: { intersection?: unknown; tip?: unknown };
      try {
        found = (await rpc.request("findIntersection", {
          points: [{ slot: point.slot, id: point.blockHash }],
        })) as typeof found;
      } catch (error) {
        if (error instanceof OgmiosJsonRpcError && error.answer.code === 1000) {
          const absenceTip = selectedTip(
            (error.answer.data as { tip?: unknown } | undefined)?.tip,
          );
          if (
            expectedTip === undefined ||
            absenceTip.blockHash !== expectedTip.blockHash ||
            absenceTip.slot !== expectedTip.slot ||
            absenceTip.blockNo !== expectedTip.blockNo
          )
            throw new Error(
              "Capacity absence proof does not match the current selected tip",
            );
          return finish(null);
        }
        throw error;
      }
      const intersection = found.intersection as
        | { slot?: unknown; id?: unknown }
        | undefined;
      if (
        intersection?.slot !== point.slot ||
        intersection.id !== point.blockHash
      )
        throw new Error(
          "Capacity proof intersection differs from exact recorded point",
        );
      const tip = selectedTip(found.tip);
      if (tip.blockNo < point.blockNo || tip.slot < point.slot)
        throw new Error("Capacity proof height precedes its recorded point");
      if (tip.blockHash === point.blockHash) {
        if (tip.slot !== point.slot || tip.blockNo !== point.blockNo)
          throw new Error(
            "Current cutoff height is not bound to its selected tip",
          );
        return finish({ point: { ...point, blockNo: tip.blockNo }, tip });
      }
      if (tip.slot <= point.slot || tip.blockNo <= point.blockNo)
        throw new Error("Capacity proof successor tip is incoherent");
      let next = (await rpc.request("nextBlock", {})) as {
        direction?: unknown;
        block?: unknown;
        tip?: unknown;
        point?: unknown;
      };
      // Chain-sync can first announce its exact intersection before advancing.
      // A different point or another backward response is a real rollback.
      if (next.direction === "backward") {
        const handshake = next.point as
          | { id?: unknown; slot?: unknown }
          | undefined;
        const handshakeTip = selectedTip(next.tip);
        if (
          handshake?.id !== point.blockHash ||
          handshake.slot !== point.slot ||
          handshakeTip.blockHash !== tip.blockHash ||
          handshakeTip.slot !== tip.slot ||
          handshakeTip.blockNo !== tip.blockNo
        )
          throw new Error("Canonical chain rolled back during capacity proof");
        next = (await rpc.request("nextBlock", {})) as typeof next;
      }
      if (next.direction !== "forward")
        throw new Error("Canonical chain rolled back during capacity proof");
      const block = next.block as
        | { ancestor?: unknown; id?: unknown; slot?: unknown; height?: unknown }
        | undefined;
      const nextTip = selectedTip(next.tip);
      if (
        block?.ancestor !== point.blockHash ||
        typeof block.id !== "string" ||
        !HEX_32.test(block.id) ||
        typeof block.slot !== "number" ||
        !Number.isSafeInteger(block.slot) ||
        block.slot <= point.slot ||
        block.slot > tip.slot ||
        typeof block.height !== "number" ||
        !Number.isSafeInteger(block.height) ||
        block.height !== point.blockNo + 1 ||
        block.height > tip.blockNo ||
        nextTip.blockHash !== tip.blockHash ||
        nextTip.slot !== tip.slot ||
        nextTip.blockNo !== tip.blockNo
      )
        throw new Error(
          "Capacity proof lacks an exact raw successor and coherent selected tip",
        );
      return finish({
        point: { ...point, blockNo: block.height - 1 },
        tip: nextTip,
      });
    } catch (error) {
      try {
        socket?.close();
      } catch {
        // Cleanup failure must not replace the original proof/opening error.
      }
      throw error;
    }
  };
