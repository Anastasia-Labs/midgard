/**
 * The intent simulation's I5 traffic (§8.1): some plans are recorded one to
 * eight checks after they were planned, under the view they were planned
 * at, so rewinds can fall between plan and record; the record is
 * `stale_at_write` exactly when that view is no longer valid (the
 * generation moved and its point is off the model chain). Some recorded
 * plans take the role's send decision (`decideSubmitIn`) one to eight
 * checks later, so rewinds can fall between record and submit: it sends
 * exactly when the record was not stale, S6 has not abandoned it, and the
 * view is still valid.
 */
import {
  type BlockSummary,
  decideSubmitIn,
  type FactStore,
  type OutRef,
  readIntentEventsIn,
  recordIntentIn,
  type RecordIntentResult,
  type View,
} from "../../src/index.js";
import {
  encodeSimTx,
  SIM_ORIGIN,
  type SimTx,
} from "../../src/testing/index.js";
import type { Journal } from "./intent-sim.text.js";

export type Planned = Readonly<{ tx: SimTx; hash: Buffer; family: string }>;

export type LateStats = {
  recorded: number;
  /** Children refused because their parent was pruned dead. */
  refusedPrunedParent: number;
  /** Plans recorded late whose view a rewind made stale. */
  staleAtWrite: number;
  /** Plans recorded late after a rewind that left their point canonical. */
  recordedAcrossRewind: number;
  /** Plans refused late: a rewind removed an input's creator. */
  refusedAfterRewind: number;
  /** Role send decisions held for a stale view (recorded stale, or rewound since). */
  submitHeld: number;
  /** Role send decisions taken after a rewind that left the point canonical. */
  sentAcrossRewind: number;
};

/** What the late steps of one check see. */
export type LateCheck = Readonly<{
  store: FactStore;
  journal: Journal;
  blocks: readonly BlockSummary[];
  generation: number;
  prunedThrough: number;
}>;

const hex = (bytes: Buffer): string => bytes.toString("hex");

export const isOwn = (p: Planned) => (output: { address: Buffer }) =>
  output.address.equals(p.tx.outputs[0]?.address ?? Buffer.alloc(0));

export const recordPlanned = (
  store: FactStore,
  p: Planned,
  builtAt: View,
): Promise<RecordIntentResult> =>
  store.transaction("write", (tx) =>
    recordIntentIn(tx, store.dialect, {
      family: p.family,
      workflowKey: `${p.family}:${hex(p.hash)}`,
      txCbor: encodeSimTx(p.tx),
      isOwnOutput: isOwn(p),
      builtAt,
    }),
  );

/**
 * A child of a parent pruned dead (terminal k deep), or of such a refused
 * child, is refused: its input is neither a fact nor a journaled intent's
 * output.
 */
export const refusedForPrunedParent = (
  result: RecordIntentResult,
  journal: Journal,
  refused: ReadonlySet<string>,
  known: ReadonlyMap<string, unknown>,
): boolean =>
  result.kind === "input_untracked" &&
  result.untracked.every(
    (o: OutRef) =>
      refused.has(hex(o.txHash)) ||
      (known.has(hex(o.txHash)) &&
        !journal.intents.some((i) => i.txHash.equals(o.txHash))),
  );

/** Whether `view` is still valid on the model chain at `generation`. */
const validOnModel = (
  blocks: readonly BlockSummary[],
  view: View,
  generation: number,
): boolean =>
  generation === view.generation ||
  view.point.hash.equals(SIM_ORIGIN.point.hash) ||
  blocks.some(
    (block) =>
      block.point.slot === view.point.slot &&
      block.point.hash.equals(view.point.hash),
  );

export const lateIntents = (
  stats: LateStats,
  refused: Set<string>,
  known: ReadonlyMap<string, unknown>,
) => {
  /** Checks run so far: late records and sends fall due by it. */
  let checks = 0;
  /** Plans to record at check `due`, under the view they were planned at. */
  let records: {
    p: Planned;
    view: View;
    due: number;
    prunedThrough: number;
  }[] = [];
  /** Recorded plans whose send decision comes at check `due`. */
  let sends: { p: Planned; built: View; due: number }[] = [];
  /** One to eight checks from now, per intent. */
  const lateBy = (p: Planned, salt: number): number =>
    checks + 1 + ((p.hash.readUInt8(1) ^ salt) % 8);

  const recordDue = async (check: LateCheck): Promise<string | null> => {
    const { store, journal, generation, prunedThrough } = check;
    const due = records.filter((late) => late.due <= checks);
    records = records.filter((late) => late.due > checks);
    for (const { p, view, prunedThrough: prunedAtPlan } of due) {
      const expectStale = !validOnModel(check.blocks, view, generation);
      const result = await recordPlanned(store, p, view);
      if (
        result.kind === "input_untracked" &&
        (generation !== view.generation || prunedThrough > prunedAtPlan)
      ) {
        // A rewind removed the creator of an input since it was planned,
        // or a prune removed the input's spent row.
        refused.add(hex(p.hash));
        stats.refusedAfterRewind += 1;
        continue;
      }
      if (refusedForPrunedParent(result, journal, refused, known)) {
        refused.add(hex(p.hash));
        stats.refusedPrunedParent += 1;
        continue;
      }
      if (result.kind !== "recorded")
        return `late record ${hex(p.hash)}: ${result.kind}`;
      if (result.stale !== expectStale)
        return `late record ${hex(p.hash)} planned at generation ${view.generation} slot ${view.point.slot}, recorded at generation ${generation}: stale ${result.stale}, expected ${expectStale}`;
      const events = await store.transaction("read", (tx) =>
        readIntentEventsIn(tx, p.hash),
      );
      if (
        events.some((e) => e.kind === "stale_at_write") !== expectStale ||
        events.length !== (expectStale ? 2 : 1)
      )
        return `late record ${hex(p.hash)}: events ${events.map((e) => e.kind).join(",")}`;
      if (expectStale) stats.staleAtWrite += 1;
      else if (generation !== view.generation) stats.recordedAcrossRewind += 1;
      stats.recorded += 1;
    }
    return null;
  };

  const sendDue = async (check: LateCheck): Promise<string | null> => {
    const { store, generation } = check;
    const due = sends.filter((late) => late.due <= checks);
    sends = sends.filter((late) => late.due > checks);
    for (const { p, built } of due) {
      const before = await store.transaction("read", (tx) =>
        readIntentEventsIn(tx, p.hash),
      );
      if (before.length === 0) continue; // pruned since
      const expectSend =
        !before.some(
          (e) => e.kind === "stale_at_write" || e.kind === "abandoned",
        ) && validOnModel(check.blocks, built, generation);
      const decision = await store.transaction("write", (tx) =>
        decideSubmitIn(tx, store.dialect, p.hash),
      );
      if ((decision.kind === "send") !== expectSend)
        return `send decision for ${hex(p.hash)} built at generation ${built.generation} slot ${built.point.slot}, now generation ${generation}: ${decision.kind}${decision.kind === "hold" ? ` (${decision.reason})` : ""}, expected ${expectSend ? "send" : "hold"}`;
      if (decision.kind === "send") {
        if (!decision.intent.txCbor.equals(encodeSimTx(p.tx)))
          return `send decision for ${hex(p.hash)} returned other bytes`;
        if (generation !== built.generation) stats.sentAcrossRewind += 1;
      } else stats.submitHeld += 1;
    }
    return null;
  };

  return {
    /** One check's late records, then its late send decisions. */
    run: async (check: LateCheck): Promise<string | null> => {
      checks += 1;
      return (await recordDue(check)) ?? (await sendDue(check));
    },
    /**
     * The check at which `p` is recorded late, or null to record it now: a
     * child of a plan still waiting to be recorded waits with it.
     */
    dueFor: (p: Planned, recordNow: boolean): number | null => {
      const parent = records.find((late) =>
        p.tx.inputs.some((input) => input.txHash.equals(late.p.hash)),
      );
      if (parent !== undefined) return parent.due;
      return recordNow ? null : lateBy(p, 3);
    },
    deferRecord: (
      p: Planned,
      view: View,
      due: number,
      prunedThrough: number,
    ): void => {
      records.push({ p, view, due, prunedThrough });
    },
    deferSend: (p: Planned, built: View): void => {
      sends.push({ p, built, due: lateBy(p, 4) });
    },
  };
};
