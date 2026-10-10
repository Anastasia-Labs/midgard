/**
 * The Blockfrost adapter of the L1-access port (`../l1-access.ts`; option
 * E): tools only (remote developers and SDK users with no local node). Role
 * processes never load this module: it is reached only by a dynamic import
 * from the tool adapter layer, and the role boundary test pins that.
 *
 * - Reads, submit and confirmation: provider-native.
 * - Tip (the clock), view point and synchronized view point: the latest
 *   block (`/blocks/latest`).
 * - Submit slot: that block against wall time on the network's slot
 *   mapping, refused when it is further behind than the bound.
 *
 * Named networks only: a Custom network has no public slot mapping here.
 */
import * as LE from "@lucid-evolution/lucid";

import {
  type L1Access,
  type L1AccessAdapter,
  type L1ViewPoint,
  openL1Access,
} from "../l1-access.js";

export type BlockfrostAccessInput = Readonly<{
  network: Exclude<LE.Network, "Custom">;
  /** The Blockfrost API base URL, e.g. `https://cardano-preprod.blockfrost.io/api/v0`. */
  url: string;
  projectId: string;
  /** A provider the caller built; `new Blockfrost(url, projectId)` otherwise. */
  provider?: LE.Provider;
  /** How far the latest block may trail wall time for a submit slot (ms). */
  behindMaxMs?: number;
  requestTimeoutMs?: number;
  fetchImpl?: typeof fetch;
  nowMs?: () => number;
}>;

export type BlockfrostAccessAdapter = L1AccessAdapter &
  Readonly<{
    kind: "blockfrost";
    /** The latest block now. */
    ledgerTip: () => Promise<L1ViewPoint>;
  }>;

export type BlockfrostAccess = L1Access<BlockfrostAccessAdapter>;

const DEFAULT_BEHIND_MAX_MS = 600_000;
const DEFAULT_REQUEST_TIMEOUT_MS = 10_000;

/** The latest block's point from a `/blocks/latest` answer. */
export const blockfrostTipPointOf = (payload: unknown): L1ViewPoint => {
  const block = payload as { slot?: unknown; hash?: unknown };
  if (
    typeof block?.slot !== "number" ||
    !Number.isSafeInteger(block.slot) ||
    block.slot < 0 ||
    typeof block.hash !== "string" ||
    !/^[0-9a-f]{64}$/u.test(block.hash)
  )
    throw new Error(
      `Blockfrost /blocks/latest is not a block with a slot and hash: ${JSON.stringify(payload)}`,
    );
  return { slot: block.slot, id: block.hash };
};

/** Opens the Blockfrost adapter; reads nothing until a read asks. */
export const openBlockfrostAccess = (
  input: BlockfrostAccessInput,
): BlockfrostAccess => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const url = input.url.trim().replace(/\/+$/u, "");
  const timeoutMs = input.requestTimeoutMs ?? DEFAULT_REQUEST_TIMEOUT_MS;
  const behindMaxMs = input.behindMaxMs ?? DEFAULT_BEHIND_MAX_MS;
  const nowMs = input.nowMs ?? Date.now;
  const slotConfig: LE.SlotConfig = {
    ...LE.SLOT_CONFIG_NETWORK[input.network],
  };
  const provider = input.provider ?? new LE.Blockfrost(url, input.projectId);
  const ledgerTip = async (): Promise<L1ViewPoint> => {
    const response = await fetchImpl(`${url}/blocks/latest`, {
      headers: { project_id: input.projectId },
      signal: AbortSignal.timeout(timeoutMs),
    });
    if (!response.ok)
      throw new Error(
        `Blockfrost /blocks/latest answered HTTP ${response.status.toString()}`,
      );
    return blockfrostTipPointOf(await response.json());
  };
  return openL1Access({
    kind: "blockfrost",
    provider,
    ledgerTip,
    endpoint: url,
    slotConfig: async () => slotConfig,
    tipSlot: async () => (await ledgerTip()).slot,
    viewPoint: ledgerTip,
    synchronizedViewPoint: ledgerTip,
    submitSlotSnapshot: async () => {
      const tip = await ledgerTip();
      const now = nowMs();
      const wallSlot =
        slotConfig.zeroSlot +
        Math.floor((now - slotConfig.zeroTime) / slotConfig.slotLength);
      const lagMs = Math.max(0, (wallSlot - tip.slot) * slotConfig.slotLength);
      if (lagMs > behindMaxMs)
        throw new Error(
          `Blockfrost's latest block ${tip.slot.toString()} is ${lagMs.toString()} ms behind wall slot ${wallSlot.toString()} (bound ${behindMaxMs.toString()} ms)`,
        );
      return {
        source: "provider_tip",
        currentSlot: Math.max(wallSlot, tip.slot),
        ledgerTipSlot: tip.slot,
        observedAtMs: now,
        slotLengthMs: slotConfig.slotLength,
      };
    },
    close: async () => undefined,
  });
};
