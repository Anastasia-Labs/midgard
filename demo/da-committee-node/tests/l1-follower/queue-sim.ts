import {
  type BlockSummary,
  decodeBlock,
  isSafe,
  type OutRef,
  outRefKey,
} from "@al-ft/midgard-l1-follower";
import type { FollowerProjection } from "@al-ft/midgard-l1-follower/shadow";
import {
  type ForkScenario,
  type ForkStep,
  type SimOutput,
} from "@al-ft/midgard-l1-follower/testing";

import {
  type SignedHeader,
  slotTimeMs,
} from "../../src/l1/follower/obligations.js";
import {
  committeeProjection,
  type CommitteeView,
  readCommitteeView,
} from "../../src/l1/follower/projection.js";
import {
  decodeElement,
  isQueueOutput,
  queueTraffic,
  type QueueTrafficOptions,
  SIM_DEPTHS,
  SIM_K,
  SIM_QUEUE,
  SIM_SLOT_TIME,
} from "./queue-sim-traffic.js";

export * from "./queue-sim-traffic.js";

/** The expected landed queue, from the canonical blocks alone. */
export type ExpectedQueue = Readonly<{
  healthy: boolean;
  outRefs: readonly string[];
  /** Commit height of every header ever seen on the current chain. */
  commitHeight: ReadonlyMap<string, number>;
  /** Creation height of every live queue output. */
  createdHeight: ReadonlyMap<string, number>;
}>;

const label = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

/**
 * An oracle independent of the store: it keeps the canonical blocks from
 * the event stream and replays the queue outputs over them.
 */
export class QueueOracle {
  readonly blocks: BlockSummary[] = [];
  /** prevHeaderHash of every header ever seen, on any branch. */
  readonly prevOf = new Map<string, string>();

  observe(step: ForkStep): void {
    const event = step.event;
    if (event.kind === "roll_forward") {
      this.blocks.push(decodeBlock(event.block));
      return;
    }
    if (event.point.kind !== "point") throw new Error("rollback to genesis");
    const hash = Buffer.from(event.point.hash, "hex");
    while (
      this.blocks.length > 0 &&
      !this.blocks[this.blocks.length - 1]!.point.hash.equals(hash)
    )
      this.blocks.pop();
  }

  tipHeight(origin: number): number {
    return this.blocks.at(-1)?.height ?? origin;
  }

  slotAtHeight(height: number): number | null {
    return (
      this.blocks.find((block) => block.height === height)?.point.slot ?? null
    );
  }

  expected(): ExpectedQueue {
    const live = new Map<
      string,
      { outRef: OutRef; output: SimOutput; height: number }
    >();
    const commitHeight = new Map<string, number>();
    for (const block of this.blocks)
      for (const tx of block.txs) {
        if (!tx.isValid) continue;
        for (const input of tx.inputs) live.delete(outRefKey(input));
        tx.outputs.forEach((output, index) => {
          if (!output.address.equals(SIM_QUEUE.stateQueueAddress)) return;
          const names = output.assets.get(SIM_QUEUE.stateQueuePolicyId);
          if (names === undefined) return;
          const simOutput: SimOutput = {
            address: output.address,
            lovelace: output.lovelace,
            assets: output.assets,
            ...(output.datum === null ? {} : { datum: output.datum }),
          };
          const outRef = { txHash: tx.hash, index };
          live.set(outRefKey(outRef), {
            outRef,
            output: simOutput,
            height: block.height,
          });
          const element = decodeElement(outRef, simOutput);
          if (element.header !== null) {
            this.prevOf.set(element.headerHash, element.header.prevHeaderHash);
            if (!commitHeight.has(element.headerHash))
              commitHeight.set(element.headerHash, block.height);
          }
        });
      }
    const elements = [...live.values()].map((entry) => ({
      ...decodeElement(entry.outRef, entry.output),
      height: entry.height,
    }));
    const roots = elements.filter((element) => element.header === null);
    const nodes = elements.filter((element) => element.header !== null);
    const byKey = new Map(nodes.map((node) => [node.headerHash, node]));
    const outRefs: string[] = [];
    let healthy = roots.length === 1;
    if (healthy) {
      const root = roots[0]!;
      outRefs.push(label(root.outRef));
      let key = root.datum.link;
      while (key !== null) {
        const next = byKey.get(key);
        if (next === undefined) {
          healthy = false;
          break;
        }
        outRefs.push(label(next.outRef));
        key = next.datum.link;
      }
      if (outRefs.length !== nodes.length + 1) healthy = false;
    }
    return {
      healthy,
      outRefs,
      commitHeight,
      createdHeight: new Map(
        elements.map((element) => [label(element.outRef), element.height]),
      ),
    };
  }
}

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
  tailRemovedHealthy: 0,
  unhealthy: 0,
});

const sameList = (a: readonly string[], b: readonly string[]): boolean =>
  a.length === b.length && a.every((value, index) => value === b[index]);

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
 * - obligations: a signed header is `final` exactly when its commit is more
 *   than k deep, `cannot_land` exactly when it is absent and a final block is
 *   past its end time, and its payload is released (and its decision made
 *   deletable) in those two states only;
 * - a decision, once signed, is never re-signed with other content.
 */
export const committeeSimProjection = (
  stats: CommitteeSimStats,
  options: QueueTrafficOptions & { expectHealthy?: boolean } = {},
): FollowerProjection => {
  const base = committeeProjection(SIM_QUEUE);
  const oracle = new QueueOracle();
  const signed = new Map<string, SignedHeader>();
  const everSignedAbsent = new Set<string>();
  let previousLength = 0;
  return {
    ...base,
    traffic: queueTraffic(options),
    protects: isQueueOutput,
    check: async ({ store, step }) => {
      oracle.observe(step);
      stats.steps += 1;
      const view = await readCommitteeView(store, {
        parameters: SIM_DEPTHS,
        slotTime: SIM_SLOT_TIME,
        signed: [...signed.values()],
      });
      if (view === null) return "no committee view";
      const expected = oracle.expected();
      const failure = checkView(view, expected, oracle, signed, stats);
      if (failure !== null) return failure;
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
        if (
          obligation.state === "pending" ||
          obligation.state === "cannot_land"
        )
          if (!everSignedAbsent.has(obligation.headerHash)) {
            everSignedAbsent.add(obligation.headerHash);
            stats.disappeared += 1;
          }
      return null;
    },
  };
};

const checkView = (
  view: CommitteeView,
  expected: ExpectedQueue,
  oracle: QueueOracle,
  signed: ReadonlyMap<string, SignedHeader>,
  stats: CommitteeSimStats,
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
  const boundarySlot = [...oracle.blocks]
    .reverse()
    .find((block) => block.height <= boundaryHeight)?.point.slot;
  for (const obligation of view.obligations) {
    const header = signed.get(obligation.headerHash);
    if (header === undefined)
      return `obligation for unsigned ${obligation.headerHash}`;
    const commit = expected.commitHeight.get(obligation.headerHash);
    let state: string;
    if (commit !== undefined) {
      state = tip - commit + 1 > SIM_K ? "final" : "landed";
    } else {
      const late =
        boundarySlot !== undefined &&
        slotTimeMs(boundarySlot, SIM_SLOT_TIME) > Number(header.endTimeMs);
      state = late ? "cannot_land" : "pending";
    }
    if (obligation.state !== state)
      return `obligation ${obligation.headerHash} ${obligation.state}, oracle ${state}`;
    const released = state === "final" || state === "cannot_land";
    if (
      obligation.retainPayload === released ||
      obligation.decisionDeletable !== released
    )
      return `obligation ${obligation.headerHash} ${state}: retain ${String(obligation.retainPayload)}, deletable ${String(obligation.decisionDeletable)}`;
    if (state === "cannot_land") stats.cannotLand += 1;
    if (state === "final") stats.final += 1;
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
