import {
  type ChainLevel,
  depth,
  type DepthParameters,
  heightAtDepth,
  isFinal,
  levelAtDepth,
  type SqlRow,
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
 * `cannot_land`: never committed on the current chain, and a final block is
 * later than the header's end time. A commit lands only in a block no later
 * than the header's end time (the state-queue validator pins the header end
 * time to the commit tx's inclusive validity upper bound,
 * onchain/aiken/lib/midgard/state-queue.ak), so it can never land. Read only
 * while the store has pruned nothing: the facts then hold every commit.
 * `beyond_retention`: not on the current chain, a final block is later than
 * the header's end time, and the store has pruned facts, so its commit
 * history is past retention. Terminal; nothing reads it as `cannot_land`.
 */
export type ObligationState =
  | "pending"
  | "landed"
  | "final"
  | "cannot_land"
  | "beyond_retention";

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
  /**
   * Keep this commit obligation record: until the commit is final or the
   * header is terminal. It governs the record only. The DA payload follows
   * its own retention (plan §11, DA payloads: the retention window and a
   * retirement proof deeper than k), never commit finality.
   */
  retainCommitRecord: boolean;
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
  /** The store has pruned facts above its origin (`pruned_through_slot`). */
  pruned: boolean;
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
          retainCommitRecord: !final,
          decisionDeletable: final,
        };
      }
      const terminal =
        inputs.finalBlockTimeMs !== null &&
        BigInt(inputs.finalBlockTimeMs) > signed.endTimeMs;
      return {
        headerHash: signed.headerHash,
        state: !terminal
          ? "pending"
          : inputs.pruned
            ? "beyond_retention"
            : "cannot_land",
        depth: null,
        level: null,
        liveStatus: null,
        retainCommitRecord: !terminal,
        decisionDeletable: terminal,
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

/** Hashes per presence query: well under every backend's parameter cap. */
const PRESENCE_BATCH = 500;

/**
 * Reads the facts the obligations need, inside the caller's read
 * transaction: each signed header's earliest queue output and live status
 * (only the asked hashes are grouped), and the latest final block's slot.
 * Rows the store pruned are gone; `obligations` reads an absent header past
 * a final block as `beyond_retention` once the store has pruned.
 */
export const readObligationFacts = async (
  tx: SqlTx,
  signed: readonly SignedHeader[],
  tipHeight: number,
  parameters: DepthParameters,
): Promise<
  Readonly<{ presence: HeaderPresence[]; finalBlockSlot: number | null }>
> => {
  const wanted = [...new Set(signed.map((header) => header.headerHash))];
  const rows: SqlRow[] = [];
  for (let at = 0; at < wanted.length; at += PRESENCE_BATCH) {
    const batch = wanted.slice(at, at + PRESENCE_BATCH);
    rows.push(
      ...(await tx.query(
        `SELECT header_hash, MIN(created_height) AS first_height, MAX(CASE WHEN spent_slot IS NULL THEN da_status END) AS live_status FROM ${COMMITTEE_QUEUE_TABLE} WHERE kind = 'node' AND header_hash IN (${batch.map(() => "?").join(", ")}) GROUP BY header_hash`,
        batch,
      )),
    );
  }
  const presence = rows.map((row) => ({
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
