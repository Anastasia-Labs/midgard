import {
  type FactStore,
  type FollowerProjection,
  isSafe,
} from "@al-ft/midgard-l1-follower";
import { type ForkScenario } from "@al-ft/midgard-l1-follower/testing";

import {
  type SignedHeader,
  slotTimeMs,
} from "../../src/l1/follower/obligations.js";
import {
  committeeProjection,
  type CommitteeView,
  readCommitteeView,
} from "../../src/l1/follower/projection.js";
import { compareWithFreshReplay } from "./queue-sim-fresh.js";
import { type ExpectedQueue, QueueOracle } from "./queue-sim-oracle.js";
import {
  commitInvalidAfter,
  isQueueOutput,
  queueTraffic,
  type QueueTrafficOptions,
  SIM_DEPTHS,
  SIM_K,
  SIM_QUEUE,
  SIM_SLOT_TIME,
} from "./queue-sim-traffic.js";

export * from "./queue-sim-oracle.js";
export * from "./queue-sim-traffic.js";

/** What the committee fork checks counted over one run. */
export type CommitteeSimStats = {
  steps: number;
  signed: number;
  /** Signed headers sharing a parent: siblings signed on two forks. */
  siblingPairs: number;
  /** Signed headers a rollback removed from the chain at least once. */
  disappeared: number;
  cannotLand: number;
  final: number;
  /** Reads of an absent signed header past retention (store pruned). */
  beyondRetention: number;
  /**
   * Reads of an absent signed header whose latest final block sits at the
   * last slot its commit could use or the first it could not: the exact
   * boundary of `cannot_land`.
   */
  boundary: number;
  /** Removed commits that landed again. */
  relanded: number;
  /** Steps compared with a fresh replay over a pruned store. */
  prunedViews: number;
  /** Rollbacks after which the queue was shorter, and still healthy. */
  tailRemovedHealthy: number;
  unhealthy: number;
};

export const zeroStats = (): CommitteeSimStats => ({
  steps: 0,
  signed: 0,
  siblingPairs: 0,
  disappeared: 0,
  cannotLand: 0,
  final: 0,
  beyondRetention: 0,
  boundary: 0,
  relanded: 0,
  prunedViews: 0,
  tailRemovedHealthy: 0,
  unhealthy: 0,
});

const sameList = (a: readonly string[], b: readonly string[]): boolean =>
  a.length === b.length && a.every((value, index) => value === b[index]);

const committeeViewOf = (
  store: FactStore,
  signed: ReadonlyMap<string, SignedHeader>,
): Promise<CommitteeView | null> =>
  readCommitteeView(store, {
    parameters: SIM_DEPTHS,
    slotTime: SIM_SLOT_TIME,
    signed: [...signed.values()],
  });

/**
 * The committee projection with its simulator traffic and §5.5 checks (plan
 * CP and P1 rows). A member signs every header the projection reports
 * signable, keeping each decision forever (class B, never rewound). After
 * every event it checks, against the oracle:
 *
 * - the landed queue (order, health) and every awaiting header's depth;
 * - signing: a signable header is signed even when a sibling was signed on
 *   another fork, and no signed header is ever reported unsignable for the
 *   sibling reason (there is none);
 * - obligations, for safety: `final` only for a commit more than k deep;
 *   `cannot_land` or `beyond_retention` for an absent header only once no
 *   block its commit could use can still be added (the latest final block
 *   is at or past the commit's last valid slot), and `pending` only until a
 *   final block is later than its end time; the payload is released (and the
 *   decision made deletable) only in the terminal states;
 * - monotonicity: a header once read terminal while absent never lands;
 * - a decision, once signed, is never re-signed with other content;
 * - the committee view equals the view over a fresh, never-pruned replay of
 *   the same chain, up to the differences pruning is allowed to make (see
 *   `prunedPairs`).
 */
export const committeeSimProjection = (
  stats: CommitteeSimStats,
  options: QueueTrafficOptions & { expectHealthy?: boolean } = {},
): FollowerProjection => {
  const base = committeeProjection(SIM_QUEUE);
  const oracle = new QueueOracle();
  const signed = new Map<string, SignedHeader>();
  const everSignedAbsent = new Set<string>();
  const everCommitted = new Set<string>();
  let committed = new Set<string>();
  /** Headers read terminal while absent: none may land again. */
  const terminalAbsent = new Map<string, string>();
  let previousLength = 0;
  return {
    ...base,
    traffic: queueTraffic(options),
    protects: isQueueOutput,
    check: async ({ store, reference, step }) => {
      oracle.observe(step);
      stats.steps += 1;
      const view = await committeeViewOf(store, signed);
      const fresh = await committeeViewOf(reference, signed);
      if (view === null || fresh === null) return "no committee view";
      const expected = oracle.expected();
      for (const [headerHash, state] of terminalAbsent)
        if (expected.commitHeight.has(headerHash))
          return `header ${headerHash} read ${state} while absent, then landed`;
      for (const headerHash of expected.commitHeight.keys())
        if (!committed.has(headerHash) && everCommitted.has(headerHash))
          stats.relanded += 1;
      committed = new Set(expected.commitHeight.keys());
      for (const headerHash of committed) everCommitted.add(headerHash);
      const pruned = view.prunedThroughSlot > fresh.prunedThroughSlot;
      const failure =
        checkView(view, expected, oracle, signed, stats, pruned) ??
        compareWithFreshReplay(view, fresh, pruned, expected);
      if (failure !== null) return failure;
      if (pruned) stats.prunedViews += 1;
      for (const obligation of view.obligations)
        if (
          !expected.commitHeight.has(obligation.headerHash) &&
          (obligation.state === "cannot_land" ||
            obligation.state === "beyond_retention")
        )
          terminalAbsent.set(obligation.headerHash, obligation.state);
      if (!view.queue.healthy) {
        stats.unhealthy += 1;
        if (options.expectHealthy === true)
          return `queue unhealthy: ${view.queue.reason ?? ""} ${view.queue.detail ?? ""}`;
      }
      const length = view.queue.nodes.length;
      if (
        step.event.kind === "roll_backward" &&
        length < previousLength &&
        view.queue.healthy
      )
        stats.tailRemovedHealthy += 1;
      previousLength = length;
      for (const header of view.awaiting) {
        if (!header.signable) continue;
        const prior = signed.get(header.headerHash);
        if (prior !== undefined && prior.endTimeMs !== header.endTimeMs)
          return `header ${header.headerHash} re-signed with other content`;
        if (prior !== undefined) continue;
        const parent = oracle.prevOf.get(header.headerHash);
        stats.siblingPairs += [...signed.keys()].filter(
          (other) =>
            other !== header.headerHash && oracle.prevOf.get(other) === parent,
        ).length;
        signed.set(header.headerHash, {
          headerHash: header.headerHash,
          endTimeMs: header.endTimeMs,
        });
        stats.signed += 1;
      }
      for (const obligation of view.obligations)
        if (obligation.state !== "landed" && obligation.state !== "final")
          if (!everSignedAbsent.has(obligation.headerHash)) {
            everSignedAbsent.add(obligation.headerHash);
            stats.disappeared += 1;
          }
      return null;
    },
  };
};

/**
 * Obligation states allowed for a header committed on the current chain.
 * `permanent`: its commit is final, or at or below the store's
 * `prunedThroughSlot`, below which the follower refuses every rollback.
 * Over a pruned store such a commit's outputs may all be gone: it then
 * reads as not committed, which keeps the decision (`pending`) or is
 * terminal (`beyond_retention`), never `cannot_land`.
 */
const allowedPresent = (
  oracleFinal: boolean,
  permanent: boolean,
  pruned: boolean,
) =>
  new Set<string>([
    ...(oracleFinal ? ["final"] : []),
    ...(!oracleFinal || pruned ? ["landed"] : []),
    ...(pruned && permanent ? ["beyond_retention", "pending"] : []),
  ]);

/** Obligation states allowed for a header absent from the current chain. */
const allowedAbsent = (unlandable: boolean, late: boolean, pruned: boolean) =>
  new Set<string>([
    ...(unlandable ? [pruned ? "beyond_retention" : "cannot_land"] : []),
    ...(!late || pruned ? ["pending"] : []),
  ]);

const checkView = (
  view: CommitteeView,
  expected: ExpectedQueue,
  oracle: QueueOracle,
  signed: ReadonlyMap<string, SignedHeader>,
  stats: CommitteeSimStats,
  pruned: boolean,
): string | null => {
  const projected = [
    ...(view.queue.root === null ? [] : [view.queue.root.outRef]),
    ...view.queue.nodes.map((node) => node.outRef),
  ];
  if (view.queue.healthy !== expected.healthy)
    return `queue health ${String(view.queue.healthy)}, oracle ${String(expected.healthy)} (${view.queue.reason ?? ""})`;
  if (expected.healthy && !sameList(projected, expected.outRefs))
    return `queue order ${projected.join(",")} vs oracle ${expected.outRefs.join(",")}`;
  const tip = view.at.height;
  for (const header of view.awaiting) {
    const created = expected.createdHeight.get(header.outRef);
    if (created === undefined) return `awaiting ${header.outRef} not live`;
    if (header.depth !== tip - created + 1)
      return `awaiting ${header.outRef} depth ${header.depth.toString()} vs ${(tip - created + 1).toString()}`;
    const safe = isSafe(header.depth, SIM_DEPTHS);
    if (header.signable !== (expected.healthy && safe))
      return `awaiting ${header.headerHash} signable ${String(header.signable)} at depth ${header.depth.toString()}`;
  }
  const boundaryHeight = tip - SIM_K;
  const finalSlot = [...oracle.blocks]
    .reverse()
    .find((block) => block.height <= boundaryHeight)?.point.slot;
  for (const obligation of view.obligations) {
    const header = signed.get(obligation.headerHash);
    if (header === undefined)
      return `obligation for unsigned ${obligation.headerHash}`;
    const commit = expected.commitHeight.get(obligation.headerHash);
    let allowed: ReadonlySet<string>;
    if (commit !== undefined) {
      const oracleFinal = tip - commit + 1 > SIM_K;
      const commitSlot = oracle.slotAtHeight(commit);
      allowed = allowedPresent(
        oracleFinal,
        oracleFinal ||
          (commitSlot !== null && commitSlot <= view.prunedThroughSlot),
        pruned,
      );
    } else {
      // A commit lands only in a block before its `invalidAfter` slot; once
      // the latest final block is at or past the last such slot, every block
      // still to come is later, and the header can never land.
      const invalidAfter = commitInvalidAfter(Number(header.endTimeMs));
      const unlandable =
        finalSlot !== undefined && finalSlot >= invalidAfter - 1;
      const late =
        finalSlot !== undefined &&
        slotTimeMs(finalSlot, SIM_SLOT_TIME) > Number(header.endTimeMs);
      if (
        finalSlot !== undefined &&
        (finalSlot === invalidAfter - 1 || finalSlot === invalidAfter)
      )
        stats.boundary += 1;
      allowed = allowedAbsent(unlandable, late, pruned);
    }
    if (!allowed.has(obligation.state))
      return `obligation ${obligation.headerHash} ${obligation.state}, oracle allows ${[...allowed].join("|") || "nothing"} (${commit === undefined ? "absent" : "committed"}, pruned ${String(pruned)})`;
    const released =
      obligation.state === "final" ||
      obligation.state === "cannot_land" ||
      obligation.state === "beyond_retention";
    if (
      obligation.retainCommitRecord === released ||
      obligation.decisionDeletable !== released
    )
      return `obligation ${obligation.headerHash} ${obligation.state}: retain ${String(obligation.retainCommitRecord)}, deletable ${String(obligation.decisionDeletable)}`;
    if (obligation.state === "cannot_land") stats.cannotLand += 1;
    if (obligation.state === "beyond_retention") stats.beyondRetention += 1;
    if (obligation.state === "final") stats.final += 1;
  }
  if (view.obligations.length !== signed.size)
    return `${view.obligations.length.toString()} obligations for ${signed.size.toString()} signed headers`;
  return null;
};

/**
 * The committee's fork cases (plan CP, P1; C1 acceptance from L1): a signed
 * header rolled back by 1, cd, cd + 1 and k blocks, each followed by a long
 * enough new branch that the old header becomes provably unable to land and
 * its sibling becomes final.
 */
export const committeeForkCorpus = (): readonly Readonly<{
  name: string;
  scenario: ForkScenario;
}>[] => {
  const depths = [
    1,
    SIM_DEPTHS.confirmationDepth,
    SIM_DEPTHS.confirmationDepth + 1,
    SIM_K,
  ];
  return depths.flatMap((rollback, index) =>
    [0, 1].map((variant) => ({
      name: `committee rollback ${rollback.toString()} across a signed header, variant ${variant.toString()}`,
      scenario: {
        seed: 0xc1_0000 + index * 2 + variant,
        episodes: [
          {
            shape: "new_fork_only" as const,
            depth: rollback,
            extra: SIM_K + 4,
            landAt: 0,
            variant,
            lead: 4,
          },
          {
            shape: "never_reland" as const,
            depth: rollback,
            extra: SIM_K + 4,
            landAt: 0,
            variant,
            lead: 2,
          },
        ],
      },
    })),
  );
};
