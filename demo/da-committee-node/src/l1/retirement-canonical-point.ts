import { OgmiosJsonRpcError } from "@al-ft/midgard-core/ogmios-json-rpc-error";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type { PromiseCapacityPoint } from "../availability/promise-capacity-evidence.js";
import {
  committeeScopedOgmiosRpc,
  type CommitteeSourceReadLimits,
} from "../availability/scoped-transports.js";
import {
  parseRetirementPoint,
  sameRetirementPoint,
} from "../store/retirement-model.js";
import type {
  RetirementPointProof,
  RetirementPointRequest,
} from "../store/retirement-source.js";
import type { StateQueueReplayWebSocketFactory } from "./state-queue-replay-provider.open-rpc.js";
const selectedTip = (value: unknown): PromiseCapacityPoint => {
  if (value === null || typeof value !== "object")
    throw new Error("Retirement proof selected tip is unavailable");
  const v = value as Record<string, unknown>;
  return parseRetirementPoint({
    slot: v.slot,
    blockHash: v.id,
    blockNo: v.height,
  });
};
/** Ogmios6.9 intersection+raw immediate successor proves unknown old heights,
 * including only its one exact initial backward handshake. The passed factory
 * owns the physical socket under the caller's existing SDK read scope. */
export const readCommitteeRetirementPoint = async (
  args: Readonly<{
    ogmiosUrl: string;
    point: RetirementPointRequest;
    boundary: PromiseCapacityPoint;
    scope: DaAvailabilityReadScope;
    limits: CommitteeSourceReadLimits;
    webSocketFactory: StateQueueReplayWebSocketFactory;
  }>,
): Promise<RetirementPointProof | null> => {
  const rpc = await committeeScopedOgmiosRpc(
    args.ogmiosUrl,
    args.scope,
    args.limits,
    args.webSocketFactory,
  );
  try {
    let found: { intersection?: unknown; tip?: unknown };
    try {
      found = (await rpc.request("findIntersection", {
        points: [{ slot: args.point.slot, id: args.point.blockHash }],
      })) as typeof found;
    } catch (error) {
      if (error instanceof OgmiosJsonRpcError && error.answer.code === 1000) {
        const tip = selectedTip(
          (error.answer.data as { tip?: unknown } | undefined)?.tip,
        );
        if (!sameRetirementPoint(tip, args.boundary))
          throw new Error("Retirement absence proof selected tip changed");
        args.scope.assertCurrent();
        return null;
      }
      throw error;
    }
    const intersection = found.intersection as
      | { slot?: unknown; id?: unknown }
      | undefined;
    if (
      intersection?.slot !== args.point.slot ||
      intersection.id !== args.point.blockHash
    )
      throw new Error("Retirement exact intersection changed");
    const tip = selectedTip(found.tip);
    if (!sameRetirementPoint(tip, args.boundary))
      throw new Error("Retirement selected tip changed");
    if (tip.blockHash === args.point.blockHash) {
      if (
        tip.slot !== args.point.slot ||
        (args.point.blockNo !== undefined && args.point.blockNo !== tip.blockNo)
      )
        throw new Error("Retirement current point height changed");
      return { point: tip, tip };
    }
    type Next = {
      direction?: unknown;
      point?: unknown;
      block?: unknown;
      tip?: unknown;
    };
    let next = (await rpc.request("nextBlock", {})) as Next;
    if (next.direction === "backward") {
      const p = next.point as { slot?: unknown; id?: unknown } | undefined;
      if (
        p?.slot !== args.point.slot ||
        p.id !== args.point.blockHash ||
        !sameRetirementPoint(selectedTip(next.tip), tip)
      )
        throw new Error("Retirement proof rolled back");
      next = (await rpc.request("nextBlock", {})) as Next;
    }
    if (next.direction !== "forward")
      throw new Error("Retirement proof rolled back");
    const b = next.block as
      | { ancestor?: unknown; id?: unknown; slot?: unknown; height?: unknown }
      | undefined;
    if (
      !b ||
      b.ancestor !== args.point.blockHash ||
      typeof b.id !== "string" ||
      !/^[0-9a-f]{64}$/u.test(b.id) ||
      typeof b.slot !== "number" ||
      !Number.isSafeInteger(b.slot) ||
      b.slot <= args.point.slot ||
      b.slot > tip.slot ||
      typeof b.height !== "number" ||
      !Number.isSafeInteger(b.height) ||
      b.height < 1 ||
      b.height > tip.blockNo ||
      !sameRetirementPoint(selectedTip(next.tip), tip)
    )
      throw new Error("Retirement raw successor is incoherent");
    if (
      (b.height === tip.blockNo) !== (b.id === tip.blockHash) ||
      (b.slot === tip.slot) !== (b.height === tip.blockNo) ||
      b.id === args.point.blockHash
    )
      throw new Error("Retirement raw successor disagrees with selected C");
    const point = parseRetirementPoint({
      slot: args.point.slot,
      blockHash: args.point.blockHash,
      blockNo: b.height - 1,
    });
    if (
      point.blockNo >= tip.blockNo ||
      (args.point.blockNo !== undefined && args.point.blockNo !== point.blockNo)
    )
      throw new Error("Retirement raw checkpoint height changed");
    args.scope.assertCurrent();
    return { point, tip };
  } finally {
    rpc.close();
  }
};
