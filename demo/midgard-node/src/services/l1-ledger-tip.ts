/**
 * The local node's ledger tip as a submit slot: shared by the follower
 * adapter (roles) and the node-ledger adapter (tools). `currentSlot` is the
 * later of the ledger tip and wall time on the ledger's slot configuration;
 * a ledger tip further behind wall time than `L1_NODE_BEHIND_MAX_MS` is the
 * retryable `l1_node_behind`, the follower's own reason for it.
 */
import { decodeCbor } from "@al-ft/l1-node-transport";
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import type { SlotConfig } from "@lucid-evolution/lucid";

import type { L1ViewPoint } from "../l1-access.js";

/** The local node's ledger tip is further behind wall time than the bound. */
export class L1LedgerBehindError extends Error {
  override readonly name = "L1LedgerBehindError";
  readonly reason = "l1_node_behind";
  readonly retryable = true;
  constructor(
    readonly ledgerTipSlot: number,
    readonly wallSlot: number,
    readonly lagMs: number,
    readonly boundMs: number,
  ) {
    super(
      `l1_node_behind: the local node's ledger tip ${ledgerTipSlot.toString()} is ${lagMs.toString()} ms behind wall slot ${wallSlot.toString()} (bound ${boundMs.toString()} ms)`,
    );
  }
}

/** The ledger tip from a `chain_point` answer: `[]` (origin) or `[slot, hash]`. */
export const ledgerTipPointOf = (answer: Uint8Array): L1ViewPoint => {
  const value = decodeCbor(answer);
  if (!Array.isArray(value))
    throw new Error("the ledger's chain point is not a CBOR array");
  if (value.length === 0)
    throw new L1ProviderTransientError("transport", "ledger_at_origin");
  const [slot, hash] = value;
  if (
    value.length !== 2 ||
    !(
      (typeof slot === "number" && Number.isSafeInteger(slot) && slot >= 0) ||
      (typeof slot === "bigint" &&
        slot >= 0n &&
        slot <= BigInt(Number.MAX_SAFE_INTEGER))
    ) ||
    !(hash instanceof Uint8Array) ||
    hash.length !== 32
  )
    throw new Error("the ledger's chain point is not [slot, hash32]");
  return { slot: Number(slot), id: Buffer.from(hash).toString("hex") };
};

/** The slot `nowMs` falls in on `slotConfig`. */
export const wallSlotAt = (slotConfig: SlotConfig, nowMs: number): number =>
  slotConfig.zeroSlot +
  Math.floor((nowMs - slotConfig.zeroTime) / slotConfig.slotLength);

/**
 * A submit-slot snapshot from a ledger tip and wall time: `currentSlot` is
 * the later of the two; a tip more than `boundMs` behind wall time is refused.
 */
export const ledgerSubmitSlotSnapshot = (
  input: Readonly<{
    slotConfig: SlotConfig;
    ledgerTipSlot: number;
    nowMs: number;
    boundMs: number;
  }>,
): SubmitSlotSnapshot => {
  const wallSlot = wallSlotAt(input.slotConfig, input.nowMs);
  const lagMs = Math.max(
    0,
    (wallSlot - input.ledgerTipSlot) * input.slotConfig.slotLength,
  );
  if (lagMs > input.boundMs)
    throw new L1LedgerBehindError(
      input.ledgerTipSlot,
      wallSlot,
      lagMs,
      input.boundMs,
    );
  return {
    source: "l1_node_tip",
    currentSlot: Math.max(wallSlot, input.ledgerTipSlot),
    ledgerTipSlot: input.ledgerTipSlot,
    observedAtMs: input.nowMs,
    slotLengthMs: input.slotConfig.slotLength,
  };
};
