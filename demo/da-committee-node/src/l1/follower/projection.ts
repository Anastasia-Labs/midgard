import type {
  DepthParameters,
  FactStore,
  FollowerProjection,
  TrackedSet,
} from "@al-ft/midgard-l1-follower";

import {
  type AwaitingHeader,
  headersAwaitingAttestation,
  type LandedQueue,
  readLiveQueueRows,
  walkLandedQueue,
} from "./landed-queue.js";
import {
  type Obligation,
  obligations,
  readObligationFacts,
  type SignedHeader,
  type SlotTime,
  slotTimeMs,
} from "./obligations.js";
import {
  committeeQueueDerivation,
  type CommitteeQueueParameters,
} from "./queue-derivation.js";
import {
  COMMITTEE_QUEUE_TABLE_SPEC,
  committeeMigrations,
} from "./queue-table.js";

/**
 * The committee's tracked set (plan §5.2, committee column), as far as the
 * C1 projections read it: the state-queue address and policy.
 */
export const committeeTrackedSet = (
  parameters: CommitteeQueueParameters,
): TrackedSet => ({
  addresses: new Set([parameters.stateQueueAddress.toString("hex")]),
  paymentCredentials: new Set(),
  policies: new Set([parameters.stateQueuePolicyId]),
});

/** The committee's projections, as the follower store and F8 plug them in. */
export const committeeProjection = (
  parameters: CommitteeQueueParameters,
): FollowerProjection => ({
  name: "committee",
  trackedSet: committeeTrackedSet(parameters),
  temporalTables: [COMMITTEE_QUEUE_TABLE_SPEC],
  migrations: committeeMigrations,
  derivations: [committeeQueueDerivation(parameters)],
});

/** Everything one committee tick reads from the follower, at one cursor. */
export type CommitteeView = Readonly<{
  at: Readonly<{ slot: number; height: number; generation: number }>;
  queue: LandedQueue;
  awaiting: readonly AwaitingHeader[];
  obligations: readonly Obligation[];
}>;

export type CommitteeViewOptions = Readonly<{
  parameters: DepthParameters;
  slotTime: SlotTime;
  /** The member's signed headers (class B), read by the caller. */
  signed?: readonly SignedHeader[];
}>;

/**
 * One committee tick's read: the landed queue, the headers awaiting
 * attestation and the obligations, all in one read transaction (a
 * snapshot) at the store's cursor. Null before the store is initialized.
 */
export const readCommitteeView = async (
  store: FactStore,
  options: CommitteeViewOptions,
): Promise<CommitteeView | null> => {
  const signed = options.signed ?? [];
  const read = await store.transaction("read", async (tx) => {
    const cursor = (
      await tx.query("SELECT slot, height, generation FROM l1_follower_cursor")
    )[0];
    if (cursor === undefined) return null;
    const at = {
      slot: Number(cursor.slot as string | number),
      height: Number(cursor.height as string | number),
      generation: Number(cursor.generation as string | number),
    };
    const rows = await readLiveQueueRows(tx);
    const facts = await readObligationFacts(
      tx,
      signed,
      at.height,
      options.parameters,
    );
    return { at, rows, facts };
  });
  if (read === null) return null;
  const tipHeight = read.at.height;
  const queue = walkLandedQueue(read.rows);
  return {
    at: read.at,
    queue,
    awaiting: headersAwaitingAttestation(queue, tipHeight, options.parameters),
    obligations: obligations({
      signed,
      presence: read.facts.presence,
      tipHeight,
      finalBlockTimeMs:
        read.facts.finalBlockSlot === null
          ? null
          : slotTimeMs(read.facts.finalBlockSlot, options.slotTime),
      parameters: options.parameters,
    }),
  };
};
