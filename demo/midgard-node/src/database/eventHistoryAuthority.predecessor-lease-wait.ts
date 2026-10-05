import { Context } from "effect";

/** Node startup only: wait out another token's live lease instead of refusing
 * it at once. A predecessor that was killed never releases, so its lease runs
 * to the end. A lease seen renewed under the same generation has an owner that
 * is alive, so acquire refuses it at once, as it would without this; otherwise
 * the wait is bounded by one lease duration plus marginMs from the first
 * refusal. */
export const PredecessorLeaseWait = Context.GenericTag<
  Readonly<{ marginMs: number; pollIntervalMs: number }>
>("midgard/HistoryAuthorityPredecessorLeaseWait");

/** Another token's live lease, as the claim transaction saw it under the row
 * lock. `remainingMs` is on the database clock. */
export type HeldLease = Readonly<{
  heldBy: string;
  generation: string;
  leaseUntil: Date;
  remainingMs: number;
}>;

/** The startup wait's next step after `held` refused a claim at `now`: refuse,
 * or sleep until just past the lease end. `previous` is the lease the previous
 * refusal saw, and `boundMs` counts from the first refusal. */
export const nextPredecessorLeaseWaitStep = (input: {
  readonly held: HeldLease;
  readonly previous: HeldLease | undefined;
  readonly waitStartedAt: number;
  readonly now: number;
  readonly boundMs: number;
  readonly pollIntervalMs: number;
}): { readonly refuse: string } | { readonly sleepMs: number } => {
  const { held, previous } = input;
  // Only a live owner renews its lease: waiting it out cannot help.
  if (
    previous?.heldBy === held.heldBy &&
    previous.generation === held.generation &&
    held.leaseUntil.getTime() > previous.leaseUntil.getTime()
  )
    return {
      refuse: `History authority still has a live owner: ${held.heldBy} (generation ${held.generation}) renewed its lease while startup waited`,
    };
  const left = input.waitStartedAt + input.boundMs - input.now;
  if (left <= 0)
    return {
      refuse: `History authority still has a live owner after waiting ${(input.now - input.waitStartedAt).toString()} ms for its lease to expire`,
    };
  // The lease end is on the database clock; poll just past it.
  return {
    sleepMs: Math.min(input.pollIntervalMs, held.remainingMs + 25, left),
  };
};
