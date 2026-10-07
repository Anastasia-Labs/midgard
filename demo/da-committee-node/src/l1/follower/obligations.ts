import {
  type ChainLevel,
  depth,
  type DepthParameters,
  heightAtDepth,
  isFinal,
  levelAtDepth,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";

import { COMMITTEE_QUEUE_TABLE } from "./queue-table.js";

/**
 * A header this member signed an availability decision for (class B: the
 * decision and its payload are the member's own material, never rewound).
 * `endTimeMs` comes from the signed header itself, so it stays known after
 * every output carrying the header has been rolled back.
 */
export type SignedHeader = Readonly<{ headerHash: string; endTimeMs: bigint }>;

/** Where a signed header stands on the current chain (from the facts). */
export type HeaderPresence = Readonly<{
  headerHash: string;
  /** Height of the earliest queue output carrying the header (its commit). */
  firstCreatedHeight: number;
  /** The DA status of its live node output, or null when none is live. */
  liveStatus: string | null;
}>;

/**
 * `landed`: committed on the current chain, not yet final.
 * `final`: committed more than k deep; no legal rollback can undo it.
 * `pending`: not on the current chain, and it could still land.
 * `cannot_land`: not on the current chain, and a final block is later than
 * the header's end time. A commit lands only in a block no later than the
 * header's end time (the state-queue validator pins the header end time to
 * the commit tx's inclusive validity upper bound,
 * onchain/aiken/lib/midgard/state-queue.ak), so the header either landed
 * before that final block and was pruned after it was merged final, or it
 * can never land.
 */
export type ObligationState = "pending" | "landed" | "final" | "cannot_land";

export type Obligation = Readonly<{
  headerHash: string;
  state: ObligationState;
  /** Depth of the commit on the current chain; null when not on it. */
  depth: number | null;
  level: ChainLevel | null;
  /**
   * The live node's DA status: the promise duty while it is Attested or
   * Challenged, none once it merged or was removed.
   */
  liveStatus: string | null;
  /** Keep the signed payload (C4: until final or provably unable to land). */
  retainPayload: boolean;
  /**
   * The signed decision may be deleted. Never before the header is final or
   * provably unable to land: a decision for a header that disappeared is
   * kept, and never re-signed with different content.
   */
  decisionDeletable: boolean;
}>;

export type ObligationInputs = Readonly<{
  signed: readonly SignedHeader[];
  presence: readonly HeaderPresence[];
  tipHeight: number;
  /**
   * The POSIX time (ms) of the latest final block (depth > k), or null when
   * the follower holds no final block yet.
   */
  finalBlockTimeMs: number | null;
  parameters: DepthParameters;
}>;

/**
 * Promise and retention obligations of every signed header. A pure function
 * of the signed set (class B), the queue facts and the tip.
 */
export const obligations = (inputs: ObligationInputs): Obligation[] => {
  const byHash = new Map(inputs.presence.map((p) => [p.headerHash, p]));
  return [...inputs.signed]
    .sort((a, b) =>
      a.headerHash < b.headerHash ? -1 : a.headerHash > b.headerHash ? 1 : 0,
    )
    .map((signed): Obligation => {
      const present = byHash.get(signed.headerHash);
      if (present !== undefined) {
        const atDepth = depth(inputs.tipHeight, present.firstCreatedHeight);
        const final = isFinal(atDepth, inputs.parameters);
        return {
          headerHash: signed.headerHash,
          state: final ? "final" : "landed",
          depth: atDepth,
          level: levelAtDepth(atDepth, inputs.parameters),
          liveStatus: present.liveStatus,
          retainPayload: !final,
          decisionDeletable: final,
        };
      }
      const cannotLand =
        inputs.finalBlockTimeMs !== null &&
        BigInt(inputs.finalBlockTimeMs) > signed.endTimeMs;
      return {
        headerHash: signed.headerHash,
        state: cannotLand ? "cannot_land" : "pending",
        depth: null,
        level: null,
        liveStatus: null,
        retainPayload: !cannotLand,
        decisionDeletable: cannotLand,
      };
    });
};

/** The slot-to-time map of the current era (Lucid's `SlotConfig` shape). */
export type SlotTime = Readonly<{
  zeroTime: number;
  zeroSlot: number;
  slotLength: number;
}>;

/** A block's POSIX time (ms): the start of its slot. */
export const slotTimeMs = (slot: number, config: SlotTime): number =>
  config.zeroTime + (slot - config.zeroSlot) * config.slotLength;

/**
 * Reads the facts the obligations need, in one read transaction at the
 * cursor: each signed header's earliest queue output and live status, and
 * the latest final block's slot. The rows of a header merged and pruned are
 * gone, and so is the evidence of its commit: such a header reads as absent,
 * which `obligations` resolves as `cannot_land` once a final block is past
 * its end time (it is final either way).
 */
export const readObligationFacts = async (
  tx: SqlTx,
  signed: readonly SignedHeader[],
  tipHeight: number,
  parameters: DepthParameters,
): Promise<
  Readonly<{ presence: HeaderPresence[]; finalBlockSlot: number | null }>
> => {
  const wanted = new Set(signed.map((header) => header.headerHash));
  const rows =
    wanted.size === 0
      ? []
      : await tx.query(
          `SELECT header_hash, MIN(created_height) AS first_height, MAX(CASE WHEN spent_slot IS NULL THEN da_status END) AS live_status FROM ${COMMITTEE_QUEUE_TABLE} WHERE kind = 'node' GROUP BY header_hash`,
        );
  const presence = rows
    .filter((row) => wanted.has(String(row.header_hash)))
    .map((row) => ({
      headerHash: String(row.header_hash),
      firstCreatedHeight: Number(row.first_height as string | number),
      liveStatus: typeof row.live_status === "string" ? row.live_status : null,
    }));
  const boundary = heightAtDepth(tipHeight, parameters.securityParameter + 1);
  const final = await tx.query(
    "SELECT slot FROM l1_blocks WHERE height <= ? ORDER BY height DESC LIMIT 1",
    [boundary],
  );
  const slot = final[0]?.slot;
  return {
    presence,
    finalBlockSlot:
      slot === undefined || slot === null
        ? null
        : Number(slot as string | number),
  };
};
