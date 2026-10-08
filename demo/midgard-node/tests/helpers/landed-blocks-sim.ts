/**
 * Landed-block processing (N3) in the fork simulator. After every chain-sync
 * event the check runs the node's landed-block hook at the follower's view
 * and the working-ledger rebase it asks for (the native MPF move, then the
 * SQL recompute), until nothing is left to do, the way the driver and the
 * history owner's reconcile do; then it compares the node with the model
 * (`landed-blocks-sim.model.ts`), a fresh derivation from the canonical
 * queue.
 *
 * - Replays are honest and exactly-once-checked: replaying a header that
 *   already has a row fails the check. A late-DA block is missing once; a
 *   transient fault fails one replay in eleven.
 * - Restarts: the hook is built per check, the native owner is reopened
 *   every fourth check, and every third rebase stops after the native move
 *   (a crash) and resumes after a reopen. A pending rebase is deferred to
 *   the next event every fifth check, and every other rollback's for three
 *   events, so removed rows can reland before it runs.
 * - The node is down for stretches of events (and for the first ones of
 *   some scenarios; one long scenario's traffic merges whenever it can),
 *   so one run meets appends and merges together, starts past genesis,
 *   and holds on a late payload its block's merge already passed. The
 *   frontier folds to the root only once the root's lineage is processed.
 * - The mempool model admits pending chains (one spending the processed
 *   tip's `X`, one spending that) and single spends of `Y` and the pool,
 *   some as one accepted batch, so rebases reject directly, transitively
 *   and by batch, on rollbacks and on double spends (blocks spend the pool
 *   outputs too); foreign blocks include pending transactions.
 * - The model's settlement record is what every block on the processed
 *   chain includes, folded blocks too; a rollback takes a block's out. A
 *   member it names is settled for the batch closure, its row stays marked
 *   until the block folds, and the node's recorded receipt settlements and
 *   marks must equal it. So a batch can be rejected around a member a
 *   folded block settled.
 * - The node commits its own blocks on the processed tip
 *   (`landed-blocks-sim.node.ts`): the traffic's own candidate on it, which
 *   lands later (or never), or a fresh block that never lands. It abandons
 *   a journal whose base left the tip and revives one whose block lands
 *   anyway; a candidate that lands with no journal is a foreign block.
 * - While `confirmed_ledger` is behind (only where the model says a merge a
 *   rollback undid left the frontier off the root's lineage), the rows,
 *   ledgers, mempool and deposits must not move.
 */
import {
  type BlockSummary,
  decodeBlock,
  type FactStore,
  type FollowerProjection,
} from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Runtime } from "effect";

import { stateQueueProjection } from "../../src/l1-state-queue/index.js";
import {
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_INVALID,
} from "../../src/landed-blocks/holds.js";
import { ledgerMap } from "../../src/landed-blocks/ledger.js";
import {
  landedFrontierNeeds,
  landedFrontierPruneFloor,
} from "../../src/landed-blocks/prune-floor.js";
import { Frontier, retrieveRows } from "../../src/landed-blocks/store.js";
import type { Database } from "../../src/services/database.js";
import {
  settleMempool,
  type SimIncluded,
} from "./landed-blocks-sim.mempool.js";
import {
  expectedState,
  modelProcessing,
  type ModelQueueHeaders,
  readActual,
  rootLineage,
  stateDifference,
} from "./landed-blocks-sim.model.js";
import { simNode } from "./landed-blocks-sim.node.js";
import {
  canonicalQueue,
  type Faults,
  type LandedSimEnv,
} from "./landed-blocks-sim.ports.js";
import {
  holdNames,
  type Settled,
  simSettler,
} from "./landed-blocks-sim.settle.js";
import { landedBlocksTraffic } from "./landed-blocks-sim.traffic.js";
import { H_MAX, hasDeposit } from "./landed-blocks-sim.universe.js";
import { SIM_QUEUE_CONFIG } from "./state-queue-sim.fixtures.js";
import { isQueueOutput } from "./state-queue-sim.model.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export const landedBlocksSimProjection = (
  env: LandedSimEnv,
): FollowerProjection => {
  const canonical: BlockSummary[] = [];
  const served = new Set<string>();
  let behind = false;
  let lastFrontier: string | undefined;
  /** The frontier the model derived on the last check it compared. */
  let modelFrontier: string | undefined;
  let lastRows: readonly string[] = [];
  /** The node's state at the end of the last check it was up for. */
  let snapshot: string | undefined;
  let seen = 0;
  /** The foreign blocks whose included transactions `foreignIncluded` counted. */
  const countedIncludes = new Set<string>();
  const run = <A, E>(effect: Effect.Effect<A, E, Database>) =>
    Runtime.runPromise(env.runtime)(effect);
  const { stats, mempool, book } = env;
  const node = simNode(env, run);
  const servedNow = (header: string) => {
    if (!env.registry.get(header)!.longLate) return true;
    const until = env.lateUntil.get(header);
    return until !== undefined && stats.checks >= until;
  };
  const state = async () => {
    const rows = (await run(retrieveRows))
      .map((row) => `${row.headerHash}:${row.kind}:${row.state}:${row.applied}`)
      .sort();
    return JSON.stringify({ rows, ...(await run(readActual(env.registry))) });
  };

  const settler = simSettler(env, node, run, served);

  /** The transactions a block includes (an own block's journal, a foreign block's replay). */
  const includesOf = (header: string): readonly Buffer[] =>
    env.registry.get(header)!.own
      ? book.blocks.get(header)!.txIds
      : (env.includes.get(header) ?? []);

  /**
   * The model's settlement record at `model`: the block on the processed
   * chain that includes each transaction, and the kind of the folded block
   * (at or below the frontier) for those a folded block includes.
   */
  const recordAt = (model: Readonly<{ frontier: string; tip: string }>) => {
    const chain = rootLineage(env.registry, model.tip);
    const foldedThrough = chain.indexOf(model.frontier);
    const settledBy = new Map<string, string>();
    const folded = new Map<string, "own" | "foreign">();
    chain.forEach((header, at) => {
      for (const id of includesOf(header)) {
        settledBy.set(hex(id), header);
        if (at <= foldedThrough)
          folded.set(
            hex(id),
            env.registry.get(header)!.own ? "own" : "foreign",
          );
      }
    });
    return { settledBy, folded };
  };

  type Record = ReturnType<typeof recordAt>;

  /** The record and the live own block's members, as the batch closure reads them. */
  const includedBy = (
    record: Record,
    live: Readonly<{ txIds: readonly Buffer[] }> | undefined,
  ): SimIncluded => ({
    settled: new Set([
      ...record.settledBy.keys(),
      ...(live?.txIds ?? []).map(hex),
    ]),
    folded: record.folded,
  });

  /** The node equals the model at `top` (the live own block or the processed tip). */
  const compareState = async (
    model: Readonly<{ frontier: string; tip: string }>,
    top: string,
    ledger: ReadonlyMap<string, Buffer>,
    record: Record,
  ) => {
    const expected = expectedState(
      env.universe,
      env.registry,
      model,
      ledger,
      mempool,
      record.settledBy,
    );
    const difference = stateDifference(
      await run(readActual(env.registry)),
      expected,
    );
    if (difference !== null) return difference;
    const info = env.registry.get(top)!;
    const { durableRoot } = await env.owner.current.diagnostics();
    const root = env.universe.root(info.h, info.b);
    return durableRoot === root
      ? null
      : `native root ${durableRoot}, the model's ${root} (at ${top})`;
  };

  const compare = async (
    store: FactStore,
    rollback: boolean,
    queue: ModelQueueHeaders,
    settled: Exclude<Settled, { error: string }>,
    rebuilt: boolean,
    rowsBefore: ReadonlySet<string>,
  ): Promise<string | null> => {
    const { hold } = settled;
    const model = modelProcessing(
      env.registry,
      queue,
      modelFrontier,
      servedNow,
    );
    const frontier = await run(Frontier.retrieve);
    if (frontier?.headerHash !== lastFrontier && lastFrontier !== undefined)
      stats.folds += 1;
    lastFrontier = frontier?.headerHash;
    if (model.kind === "behind") {
      if (frontier?.headerHash !== model.frontier)
        return `confirmed_ledger is at ${frontier?.headerHash}, the model's (behind) at ${model.frontier}`;
      if (!holdNames(hold, CONFIRMED_LEDGER_BEHIND))
        return `a frontier off the root's lineage held ${JSON.stringify(hold)}`;
      stats.behindHeld += 1;
      behind = true;
      if (!rebuilt && snapshot !== undefined) {
        const now = await state();
        if (now !== snapshot)
          return `behind, the node moved: ${snapshot.slice(0, 600)} → ${now.slice(0, 600)}`;
        stats.behindCompared += 1;
      }
      return null;
    }
    if (model.from === undefined && queue.root !== SDK.GENESIS_HEADER_HASH)
      stats.bootstrapsPastGenesis += 1;
    const rooted = rootLineage(env.registry, queue.root);
    if (settled.deferred === true) {
      // Processing ran up to the rebase it asked for: the frontier is
      // between where the model left it and where it is due.
      const at = rooted.indexOf(frontier?.headerHash ?? "");
      if (
        frontier === undefined ||
        (frontier.headerHash !== model.from &&
          (at < Math.max(rooted.indexOf(model.from ?? ""), 0) ||
            at > rooted.indexOf(model.frontier)))
      )
        return `a deferred rebase left confirmed_ledger at ${frontier?.headerHash}, outside ${model.from}..${model.frontier}`;
      modelFrontier = frontier.headerHash;
      return null;
    }
    if (frontier?.headerHash !== model.frontier)
      return `confirmed_ledger is at ${frontier?.headerHash}, the model's at ${model.frontier}`;
    if (behind) stats.behindHealed += 1;
    behind = false;
    stats.comparedChecks += 1;
    const { stop } = model;
    if (stop === undefined) {
      if (hold !== undefined) return `unexpected hold ${JSON.stringify(hold)}`;
    } else {
      const reason =
        stop.kind === "invalid"
          ? LANDED_BLOCK_INVALID
          : LANDED_BLOCK_AWAITING_DA;
      if (hold?.reason !== reason || !hold.detail.includes(stop.header))
        return `${stop.kind} block ${stop.header} held ${JSON.stringify(hold)}`;
      if (stop.kind === "invalid") stats.invalidHeld += 1;
      if (stop.merged) stats.heldPastMerge += 1;
    }
    const rowSummary = (await run(retrieveRows))
      .map((row) => `${row.headerHash}:${row.kind}:${row.state}:${row.applied}`)
      .sort();
    const expectedRows = model.rows
      .map(
        (header) =>
          `${header}:${env.registry.get(header)!.own ? "own" : "foreign"}:processed:true`,
      )
      .sort();
    if (JSON.stringify(rowSummary) !== JSON.stringify(expectedRows))
      return `rows ${JSON.stringify(rowSummary)} vs model ${JSON.stringify(expectedRows)}`;
    const live =
      book.active !== undefined && !model.rows.includes(book.active)
        ? book.blocks.get(book.active)!
        : undefined;
    if (live !== undefined && live.parentHash !== model.tip)
      return `active own block ${live.headerHash} is built on ${live.parentHash}, the processed tip is ${model.tip}`;
    const top = live?.headerHash ?? model.tip;
    const topInfo = env.registry.get(top)!;
    const record = recordAt(model);
    const rebuild = settleMempool(
      mempool,
      ledgerMap(env.universe.ledger(topInfo.h, topInfo.b)),
      includedBy(record, live),
    );
    if (typeof rebuild === "string") return rebuild;
    const difference = await compareState(model, top, rebuild.ledger, record);
    if (difference !== null) return difference;
    // Case counters (the node agreed with the model).
    const merged = new Set(rooted);
    if (
      model.processed.some(
        (header) => merged.has(header) && !rowsBefore.has(header),
      )
    )
      stats.coalescedMerges += 1;
    const removed = lastRows.filter(
      (header) => !model.processed.includes(header),
    );
    if (rollback && removed.length > 0) {
      stats.rollbacksRemovingProcessed += 1;
      if (rebuild.newly.length > 0) stats.rejectionsOnRollback += 1;
      if (rebuild.newly.some(([, reason]) => reason === "dependent"))
        stats.latentHoleClosed += 1;
    }
    for (const [, reason] of rebuild.newly)
      if (reason === "direct") stats.directRejections += 1;
      else if (reason === "dependent") stats.dependentRejections += 1;
      else stats.batchRejections += 1;
    for (const header of model.processed)
      if (!env.registry.get(header)!.own && !countedIncludes.has(header)) {
        countedIncludes.add(header);
        stats.foreignIncluded += includesOf(header).length;
      }
    if (rebuild.batchSettled) stats.batchSettled += 1;
    if (rebuild.foldThenReject.has("own")) stats.foldThenRejectOwn += 1;
    if (rebuild.foldThenReject.has("foreign")) stats.foldThenRejectForeign += 1;
    if (model.rows.some((header) => env.registry.get(header)!.own))
      stats.ownProcessed += 1;
    modelFrontier = model.frontier;
    lastRows = model.rows;
    if (
      ((await store.cursor())?.prunedThroughSlot ?? SIM_ORIGIN.point.slot) >
      SIM_ORIGIN.point.slot
    )
      stats.prunedChecks += 1;
    // The node's own moves: admissions, then now and then its own block on
    // the processed tail.
    const pending = () =>
      mempool.survivors.filter((tx) => !record.settledBy.has(hex(tx.id)));
    // Pool spends stay pending for good: they do not count.
    const poolKeys = new Set(
      env.universe.pool.map((entry) => hex(entry.outref)),
    );
    if (
      stats.checks % 2 === 0 &&
      pending().filter((tx) => !tx.spent.some((key) => poolKeys.has(hex(key))))
        .length < 6
    )
      await node.admit(topInfo, rebuild.ledger);
    const tipInfo = env.registry.get(model.tip)!;
    const candidate =
      stop === undefined &&
      book.active === undefined &&
      model.tip === (queue.nodes.at(-1) ?? queue.root)
        ? await node.candidateOn(model.tip)
        : undefined;
    if (
      candidate !== undefined ||
      (stop === undefined &&
        book.active === undefined &&
        model.tip === (queue.nodes.at(-1) ?? queue.root) &&
        stats.checks % 5 === 1 &&
        tipInfo.h + 1 <= H_MAX &&
        !hasDeposit(tipInfo.h + 1))
    ) {
      const block = await node.commit(model.tip, candidate, pending());
      if (typeof block === "string") return block;
      const info = env.registry.get(block.headerHash)!;
      const after = settleMempool(
        mempool,
        ledgerMap(env.universe.ledger(info.h, info.b)),
        includedBy(record, block),
      );
      if (typeof after === "string") return after;
      const committed = await compareState(
        model,
        block.headerHash,
        after.ledger,
        record,
      );
      if (committed !== null) return `after an own commit: ${committed}`;
    }
    return null;
  };

  const check: NonNullable<FollowerProjection["check"]> = async ({
    store,
    step,
  }) => {
    stats.checks += 1;
    seen += 1;
    const { event } = step;
    if (event.kind === "roll_forward") canonical.push(decodeBlock(event.block));
    else {
      const target =
        event.point.kind === "point" ? event.point.hash.toLowerCase() : null;
      while (
        canonical.length > 0 &&
        canonical[canonical.length - 1]!.point.hash.toString("hex") !== target
      )
        canonical.pop();
    }
    // The node is down: the follower moves on without it.
    if (seen <= env.offlineFor || seen % 17 >= 14) {
      stats.offlineChecks += 1;
      return null;
    }
    if (stats.checks % 4 === 0) {
      await env.owner.reopen();
      stats.ownerRestarts += 1;
    }
    const queue = canonicalQueue(canonical);
    if (queue !== undefined)
      await node.finalizeMerged(new Set(rootLineage(env.registry, queue.root)));
    const rowsBefore = new Set(
      (await run(retrieveRows)).map((row) => row.headerHash),
    );
    const faults: Faults = { missing: [], transient: 0 };
    const before = settler.rebuilds();
    const settled = await settler.settle(
      store,
      faults,
      event.kind === "roll_backward",
      queue,
    );
    if ("error" in settled) return settled.error;
    if (queue !== undefined) {
      const failed = await compare(
        store,
        event.kind === "roll_backward",
        queue,
        settled,
        settler.rebuilds() !== before,
        rowsBefore,
      );
      if (failed !== null) return failed;
    }
    snapshot = await state();
    return null;
  };

  return {
    ...stateQueueProjection(SIM_QUEUE_CONFIG),
    name: "landed-blocks",
    pruneFloor: landedFrontierPruneFloor({
      config: SIM_QUEUE_CONFIG,
      needs: () => run(landedFrontierNeeds),
    }),
    traffic: landedBlocksTraffic(
      env.universe,
      env.registry,
      stats,
      env.mergeHeavy,
    ),
    check: async (context) => {
      try {
        return await check(context);
      } catch (error) {
        return `check threw: ${error instanceof Error ? (error.stack ?? error.message) : String(error)}`;
      }
    },
    protects: (output) => isQueueOutput(output),
  };
};
