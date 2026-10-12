import {
  decodeBlock,
  type FactStore,
  type FollowerProjection,
  type View,
} from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";

import {
  type CommitteeAvailabilityReads,
  committeeAvailabilityReads,
  type FollowerBoundary,
} from "../../src/l1/follower/availability-reads.js";
import { SIM_K } from "./queue-sim-traffic.js";

/** What the availability-read checks counted over one run. */
export type AvailabilityReadStats = {
  steps: number;
  /** Points read canonical on both stores. */
  canonical: number;
  /** Points of an abandoned branch, read off the chain on both stores. */
  offChain: number;
  /** Valid transactions whose submission block both stores named. */
  submitted: number;
  /** Spends both stores resolved, with the spending transaction read back. */
  spends: number;
  /** Views a rollback invalidated. */
  invalidatedViews: number;
  /** Reads at or below the store's pruned slot, once it was pruned. */
  prunedReads: number;
};

export const zeroAvailabilityReadStats = (): AvailabilityReadStats => ({
  steps: 0,
  canonical: 0,
  offChain: 0,
  submitted: 0,
  spends: 0,
  invalidatedViews: 0,
  prunedReads: 0,
});

type Outcome = Readonly<{ kind: "value"; json: string } | { kind: "error" }>;

const outcomeOf = async (read: () => Promise<unknown>): Promise<Outcome> => {
  try {
    const value = await read();
    return { kind: "value", json: JSON.stringify(value ?? null) };
  } catch {
    return { kind: "error" };
  }
};

const isAbsent = (outcome: Outcome): boolean =>
  outcome.kind === "error" || outcome.json === "null";

const readsOver = (store: FactStore): CommitteeAvailabilityReads =>
  committeeAvailabilityReads({ store, readiness: () => [] });

type SeenTx = Readonly<{ txHash: string; slot: number; blockHash: string }>;

/** A block of the model chain the events describe. */
type ModelBlock = Readonly<{
  slot: number;
  blockHash: string;
  height: number;
  /** Valid transactions by hash. */
  valid: ReadonlySet<string>;
  /** Each transaction's consumed outputs: inputs if valid, else collateral. */
  consumes: ReadonlyMap<string, ReadonlySet<string>>;
}>;

const refKey = (ref: Readonly<{ txHash: string; outputIndex: number }>) =>
  `${ref.txHash}#${ref.outputIndex.toString()}`;

/**
 * The committee's availability, promise and retirement reads
 * (`committeeAvailabilityReads`) over the store under test against the same
 * reads over a fresh, never-pruned replay of the current chain, after every
 * event: the boundary, the canonicity of every block ever seen (promise
 * capacity and retirement canonical points), the submission block of every
 * transaction ever seen (retirement submission points), and every spend of
 * an output ever seen with its spending transaction and ancestor (the
 * availability responder's foreign-spend reads). Rolled-back blocks and
 * transactions stay in the sets, so the reads of an abandoned branch are
 * checked too. Once the store is pruned it may fail to answer a read at or
 * below its pruned slot, but never answers it differently.
 *
 * Equal reads alone would pass a read both stores get wrong alike, so each
 * read is also held to a model of the chain built from the events: a point
 * reads canonical exactly when it is on the model chain, at its height; a
 * named submission block is the model's block of that valid transaction; a
 * named spend is a model-chain transaction that consumes the output. And
 * `viewValid` (§8.1): a view the store calls valid has its point on the
 * chain, and a rollback invalidates the view read just before it.
 *
 * Which reads each event can change: a forward only those of its own
 * block, its transactions and the outputs they spend or create (every other
 * fact is unchanged, which the runner's own comparison with the fresh
 * replay checks); a rollback or a prune any of them. And a read whose block
 * is more than k deep (or off the chain, below that depth) cannot change
 * again, as the follower refuses any deeper rollback: once read there it is
 * settled, and read again only after a prune.
 */
export const availabilityReadsSimProjection = (
  stats: AvailabilityReadStats,
): FollowerProjection => {
  const blocks = new Map<
    string,
    Readonly<{ slot: number; blockHash: string }>
  >();
  const txs = new Map<string, SeenTx>();
  const outRefs = new Map<
    string,
    Readonly<{ txHash: string; outputIndex: number }>
  >();
  const views: View[] = [];
  let previous: View | null = null;
  const chain: ModelBlock[] = [];
  const onChain = (blockHash: string): ModelBlock | undefined =>
    chain.find((block) => block.blockHash === blockHash);
  const settled = new Set<string>();
  /** The reads the current forward event can change. */
  const touched = new Set<string>();
  return {
    name: "committee-availability-reads",
    check: async ({ store, reference, step }) => {
      stats.steps += 1;
      touched.clear();
      if (step.event.kind === "roll_forward") {
        const block = decodeBlock(step.event.block);
        const blockHash = block.point.hash.toString("hex");
        blocks.set(blockHash, { slot: block.point.slot, blockHash });
        touched.add(`block:${blockHash}`);
        chain.push({
          slot: block.point.slot,
          blockHash,
          height: block.height,
          valid: new Set(
            block.txs
              .filter((tx) => tx.isValid)
              .map((tx) => tx.hash.toString("hex")),
          ),
          consumes: new Map(
            block.txs.map((tx) => [
              tx.hash.toString("hex"),
              new Set(
                (tx.isValid ? tx.inputs : tx.collaterals).map((input) =>
                  refKey({
                    txHash: input.txHash.toString("hex"),
                    outputIndex: input.index,
                  }),
                ),
              ),
            ]),
          ),
        });
        for (const tx of block.txs) {
          const txHash = tx.hash.toString("hex");
          txs.set(txHash, { txHash, slot: block.point.slot, blockHash });
          touched.add(`tx:${txHash}`);
          for (const input of [...tx.inputs, ...tx.collaterals]) {
            const ref = {
              txHash: input.txHash.toString("hex"),
              outputIndex: input.index,
            };
            const key = refKey(ref);
            outRefs.set(key, ref);
            // A new block may spend an output already settled unspent.
            settled.delete(`spend:${key}`);
            touched.add(`spend:${key}`);
          }
          tx.outputs.forEach((_, outputIndex) => {
            const ref = { txHash, outputIndex };
            outRefs.set(refKey(ref), ref);
            touched.add(`spend:${refKey(ref)}`);
          });
        }
      }
      if (step.event.kind === "roll_backward") {
        const target = step.event.point;
        while (
          chain.length > 0 &&
          (target.kind === "origin" ||
            (chain[chain.length - 1] as ModelBlock).blockHash !== target.hash)
        )
          chain.pop();
      }
      const reads = readsOver(store);
      const fresh = readsOver(reference);
      let at: FollowerBoundary;
      let freshAt: FollowerBoundary;
      try {
        at = await reads.readBoundary();
        freshAt = await fresh.readBoundary();
      } catch (error) {
        return `boundary: ${error instanceof Error ? error.message : String(error)}`;
      }
      if (at.pointId !== freshAt.pointId || at.blockNo !== freshAt.blockNo)
        return `boundary ${at.pointId}@${at.blockNo.toString()} vs fresh ${freshAt.pointId}@${freshAt.blockNo.toString()}`;
      if (!(await reads.viewValid(at.view)))
        return "the store's own view reads invalid";
      if (previous !== null) {
        const valid = await reads.viewValid(previous);
        if (valid !== (step.event.kind === "roll_forward"))
          return `the view before a ${step.event.kind} reads ${valid ? "valid" : "invalid"}`;
        if (!valid) stats.invalidatedViews += 1;
      }
      const finalSlot = chain[chain.length - 1 - SIM_K]?.slot ?? -1;
      const recheck = step.prune === true;
      const every = recheck || step.event.kind === "roll_backward";
      /** Whether this event can change the read `key`. */
      const changeable = (key: string): boolean =>
        (every || touched.has(key)) && (recheck || !settled.has(key));
      /** Whether the read `key` at `slot` needs reading now, settling it. */
      const due = (key: string, slot: number): boolean => {
        if (!changeable(key)) return false;
        if (slot <= finalSlot) settled.add(key);
        return true;
      };
      for (const view of views)
        if (
          every &&
          (recheck || view.point.slot > finalSlot) &&
          (await reads.viewValid(view))
        ) {
          const onChain = await fresh.canonicalPoint(
            {
              slot: view.point.slot,
              blockHash: view.point.hash.toString("hex"),
            },
            freshAt,
          );
          if (
            onChain === null &&
            !view.point.hash.equals(SIM_ORIGIN.point.hash)
          )
            return `view ${view.point.slot.toString()} reads valid off the chain`;
        }
      views.push(at.view);
      previous = at.view;
      const prunedThrough = (await store.cursor())?.prunedThroughSlot ?? 0;
      /** Equal outcomes, or one the pruned store could not answer at `slot`. */
      const compare = (
        what: string,
        slot: number,
        actual: Outcome,
        expected: Outcome,
      ): string | null => {
        if (slot <= prunedThrough) stats.prunedReads += 1;
        if (
          actual.kind === expected.kind &&
          (actual.kind === "error" ||
            actual.json === (expected as { json: string }).json)
        )
          return null;
        if (slot <= prunedThrough && isAbsent(actual)) return null;
        return `${what}: ${JSON.stringify(actual)} vs fresh ${JSON.stringify(expected)}`;
      };
      for (const point of blocks.values()) {
        if (!due(`block:${point.blockHash}`, point.slot)) continue;
        const actual = await outcomeOf(() => reads.canonicalPoint(point, at));
        const expected = await outcomeOf(() =>
          fresh.canonicalPoint(point, freshAt),
        );
        const failure = compare(
          `canonicalPoint ${point.slot.toString()}`,
          point.slot,
          actual,
          expected,
        );
        if (failure !== null) return failure;
        const model = onChain(point.blockHash);
        const modelled =
          model === undefined
            ? "null"
            : JSON.stringify({
                point: { ...point, blockNo: model.height },
                tip: {
                  slot: at.slot,
                  blockHash: at.blockHash,
                  blockNo: at.blockNo,
                },
              });
        if (actual.kind === "value" && actual.json !== modelled)
          return `canonicalPoint ${point.slot.toString()}: ${actual.json}, the chain has ${modelled}`;
        if (actual.kind === "value" && expected.kind === "value")
          if (actual.json === "null") stats.offChain += 1;
          else stats.canonical += 1;
      }
      for (const tx of txs.values()) {
        if (!due(`tx:${tx.txHash}`, tx.slot)) continue;
        const actual = await outcomeOf(() =>
          reads.submissionPoint(tx.txHash, at),
        );
        const expected = await outcomeOf(() =>
          fresh.submissionPoint(tx.txHash, freshAt),
        );
        const failure = compare(
          `submissionPoint ${tx.txHash}`,
          tx.slot,
          actual,
          expected,
        );
        if (failure !== null) return failure;
        if (actual.kind === "value" && actual.json !== "null") {
          const named = (
            JSON.parse(actual.json) as { point: { blockHash: string } }
          ).point.blockHash;
          if (onChain(named)?.valid.has(tx.txHash) !== true)
            return `submissionPoint ${tx.txHash}: block ${named} is not the chain's block of the valid transaction`;
        }
        if (
          expected.kind === "value" &&
          expected.json !== "null" &&
          actual.kind === "value"
        )
          stats.submitted += 1;
      }
      for (const ref of outRefs.values()) {
        const what = refKey(ref);
        if (!changeable(`spend:${what}`)) continue;
        const spend = await fresh.foreignSpend
          .fetchSpend(ref)
          .catch(() => undefined);
        // An unspent output's facts are those of its creation; one created
        // outside the simulated chain is never pruned.
        const slot =
          spend?.point.slot ??
          txs.get(ref.txHash)?.slot ??
          Number.MAX_SAFE_INTEGER;
        if (!due(`spend:${what}`, slot)) continue;
        const failure = compare(
          `fetchSpend ${what}`,
          slot,
          await outcomeOf(() => reads.foreignSpend.fetchSpend(ref)),
          await outcomeOf(() => fresh.foreignSpend.fetchSpend(ref)),
        );
        if (failure !== null) return failure;
        const named = await reads.foreignSpend
          .fetchSpend(ref)
          .catch(() => undefined);
        if (
          named !== undefined &&
          onChain(named.point.blockHash)
            ?.consumes.get(named.transactionId)
            ?.has(refKey(ref)) !== true
        )
          return `fetchSpend ${what}: ${named.transactionId} at ${named.point.blockHash} does not consume it on the chain`;
        if (spend === undefined) continue;
        // The ancestor only has to be a block of the chain strictly before
        // the spend (the SDK's contract): the block right before it, or an
        // older one where the store pruned that block.
        const ancestor = await reads.foreignSpend
          .fetchAncestor(slot)
          .catch(() => undefined);
        const freshAncestor = await fresh.foreignSpend.fetchAncestor(slot);
        if (ancestor === undefined || ancestor.slot >= slot)
          return `fetchAncestor ${slot.toString()}: ${JSON.stringify(ancestor ?? null)}`;
        if (
          freshAncestor.slot > prunedThrough &&
          JSON.stringify(ancestor) !== JSON.stringify(freshAncestor)
        )
          return `fetchAncestor ${slot.toString()}: ${JSON.stringify(ancestor)} vs fresh ${JSON.stringify(freshAncestor)}`;
        if (
          ancestor.blockHash !== SIM_ORIGIN.point.hash.toString("hex") &&
          (await fresh.canonicalPoint(ancestor, freshAt)) === null
        )
          return `fetchAncestor ${slot.toString()}: ${ancestor.blockHash} is off the chain`;
        const transaction = compare(
          `readTransaction ${spend.transactionId}`,
          slot,
          await outcomeOf(() =>
            reads.foreignSpend.readTransaction({
              ancestor,
              point: spend.point,
              txHash: spend.transactionId,
            }),
          ),
          await outcomeOf(() =>
            fresh.foreignSpend.readTransaction({
              ancestor: freshAncestor,
              point: spend.point,
              txHash: spend.transactionId,
            }),
          ),
        );
        if (transaction !== null) return transaction;
        stats.spends += 1;
      }
      return null;
    },
  };
};
