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
 * - The mempool model admits pending chains (one spending the processed
 *   tip's `X`, one spending that) and single spends of `Y` and the pool,
 *   so rebases reject directly and transitively, on rollbacks and on
 *   double spends.
 */
import {
  type BlockSummary,
  decodeBlock,
  type FactStore,
  type FollowerProjection,
} from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { Effect, Runtime } from "effect";

import type { DriverHold } from "../../src/l1-events/driver.js";
import { stateQueueProjection } from "../../src/l1-state-queue/index.js";
import {
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
} from "../../src/landed-blocks/holds.js";
import { landedBlockHook } from "../../src/landed-blocks/hook.js";
import { ledgerMap } from "../../src/landed-blocks/ledger.js";
import { moveNativeRoot, rebaseSql } from "../../src/landed-blocks/rebase.js";
import {
  rebasePlan,
  walkTarget,
} from "../../src/landed-blocks/rebase-target.js";
import {
  Basis,
  Frontier,
  retrieveRows,
} from "../../src/landed-blocks/store.js";
import type { Database } from "../../src/services/database.js";
import { withHistoryWrite } from "../../src/services/event-history-producer.js";
import { makeOutRefCbor } from "../midgard-output-helpers.js";
import {
  admitPending,
  expectedState,
  processedPrefix,
  readActual,
  settleMempool,
  stateDifference,
} from "./landed-blocks-sim.model.js";
import {
  canonicalQueue,
  type Faults,
  type LandedSimEnv,
  simPorts,
} from "./landed-blocks-sim.ports.js";
import { landedBlocksTraffic } from "./landed-blocks-sim.traffic.js";
import { simDigest, simOutput } from "./landed-blocks-sim.universe.js";
import { SIM_QUEUE_CONFIG } from "./state-queue-sim.fixtures.js";
import { isQueueOutput } from "./state-queue-sim.model.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

const holdNames = (
  hold: { reason: string; detail: string } | undefined,
  reason: string,
) =>
  hold !== undefined &&
  (hold.reason === reason || hold.detail.includes(`also ${reason}:`));

type Settled =
  | Readonly<{ error: string }>
  | Readonly<{ hold: DriverHold | undefined; deferred?: true }>;

export const landedBlocksSimProjection = (
  env: LandedSimEnv,
): FollowerProjection => {
  const canonical: BlockSummary[] = [];
  const served = new Set<string>();
  let rolledBack = false;
  let behind = false;
  let lastFrontier: string | undefined;
  let lastPrefix: string[] = [];
  const run = <A, E>(effect: Effect.Effect<A, E, Database>) =>
    Runtime.runPromise(env.runtime)(effect);
  const { stats, mempool } = env;

  // A rollback's rebase is held back for the next few events now and then,
  // so the blocks it removed can reland before it runs.
  let deferUntil = 0;
  let rollbacks = 0;
  const settle = async (
    store: FactStore,
    faults: Faults,
    rollback: boolean,
  ): Promise<Settled> => {
    const view = (await store.currentView())!;
    const requested = { value: false };
    const hook = landedBlockHook({
      store,
      config: SIM_QUEUE_CONFIG,
      ports: simPorts(env, store, faults, served, requested),
      run,
    });
    const removedBefore = (await run(retrieveRows))
      .filter((row) => row.state === "removed")
      .map((row) => row.headerHash);
    for (let round = 0; round < 40; round++) {
      faults.missing = [];
      faults.transient = 0;
      requested.value = false;
      const hold = await hook({ kind: "unchanged", view });
      if (faults.violation !== undefined) return { error: faults.violation };
      if (round === 0) {
        const after = new Map(
          (await run(retrieveRows)).map((row) => [row.headerHash, row.state]),
        );
        stats.relands += removedBefore.filter(
          (hash) => after.get(hash) === "processed",
        ).length;
      }
      if (faults.missing.length > 0) {
        if (hold?.reason !== LANDED_BLOCK_AWAITING_DA)
          return { error: `missing DA held ${JSON.stringify(hold)}` };
        stats.awaitingDaHeld += 1;
        continue;
      }
      if (faults.transient > 0) {
        if (!holdNames(hold, LANDED_BLOCK_REPLAY_FAILED))
          return { error: `a replay fault held ${JSON.stringify(hold)}` };
        stats.transientHeld += 1;
        continue;
      }
      const plan = await run(rebasePlan);
      if (plan.kind === "blocked")
        return { error: `rebase blocked: ${plan.detail}` };
      if (plan.kind === "none") return { hold };
      const offRoot = hold?.reason === CONFIRMED_LEDGER_BEHIND;
      if (!offRoot && !holdNames(hold, LANDED_BLOCK_REBASE_PENDING))
        return { error: `a due rebase held ${JSON.stringify(hold)}` };
      if (!offRoot && !requested.value)
        return { error: "a due rebase was not requested" };
      if (round === 0 && rollback && (rollbacks += 1) % 2 === 0)
        deferUntil = stats.checks + 3;
      if (
        round === 0 &&
        (stats.checks % 5 === 4 || stats.checks <= deferUntil)
      ) {
        stats.deferredRebases += 1;
        return { deferred: true, hold };
      }
      stats.rebases += 1;
      const { durableRoot } = await env.owner.current.diagnostics();
      if (!walkTarget(plan.target).roots.includes(durableRoot))
        stats.restoredRoots += 1;
      const preparation = { assertCurrent: Effect.void };
      await run(moveNativeRoot(env.owner.current, plan.target, preparation));
      if (stats.rebases % 3 === 0) {
        await env.owner.reopen();
        stats.crashResumes += 1;
        continue;
      }
      await run(withHistoryWrite(rebaseSql(plan.target)));
    }
    return { error: "landed-block processing did not settle" };
  };

  const admit = async (tip: string, ledger: ReadonlyMap<string, Buffer>) => {
    const info = env.registry.get(tip)!;
    const spentBySurvivor = new Set(
      mempool.survivors.flatMap((tx) => tx.spent.map(hex)),
    );
    const next = (spent: Buffer[]) => {
      mempool.admitted += 1;
      const id = simDigest(`tx:${env.label}:${mempool.admitted}`);
      return {
        id,
        spent,
        produced: [
          {
            outref: makeOutRefCbor(id, 0),
            output: simOutput(9_000_000n + BigInt(mempool.admitted)),
          },
        ],
        at: new Date(
          Date.parse("2026-10-01T00:00:00.000Z") + mempool.admitted * 1_000,
        ),
      };
    };
    const free = (outRef: Buffer) =>
      ledger.has(hex(outRef)) && !spentBySurvivor.has(hex(outRef));
    const txs = [];
    if (info.h >= 1) {
      const x = env.universe.x(info.h, info.b).outref;
      if (free(x)) {
        const first = next([x]);
        txs.push(first, next([first.produced[0]!.outref]));
      }
      const y = env.universe.y(info.h).outref;
      if (free(y)) txs.push(next([y]));
    }
    const pool = env.universe.pool[mempool.poolUsed];
    if (pool !== undefined && free(pool.outref)) {
      mempool.poolUsed += 1;
      txs.push(next([pool.outref]));
    }
    if (txs.length === 0) return;
    await run(admitPending(txs));
    mempool.survivors.push(...txs);
    stats.admitted += txs.length;
  };

  const check: NonNullable<FollowerProjection["check"]> = async ({
    store,
    step,
  }) => {
    stats.checks += 1;
    const { event } = step;
    if (event.kind === "roll_forward") canonical.push(decodeBlock(event.block));
    else {
      rolledBack = true;
      const target =
        event.point.kind === "point" ? event.point.hash.toLowerCase() : null;
      while (
        canonical.length > 0 &&
        canonical[canonical.length - 1]!.point.hash.toString("hex") !== target
      )
        canonical.pop();
    }
    if (stats.checks % 4 === 0) {
      await env.owner.reopen();
      stats.ownerRestarts += 1;
    }
    const queue = canonicalQueue(canonical);
    const faults: Faults = { missing: [], transient: 0 };
    const settled = await settle(store, faults, event.kind === "roll_backward");
    if ("error" in settled) return settled.error;
    if (queue === undefined) return null;
    if (settled.deferred === true) return null;
    const { hold } = settled;
    const frontier = await run(Frontier.retrieve);
    if (frontier === undefined) return `no frontier at ${queue.root}`;
    if (frontier.headerHash !== lastFrontier && lastFrontier !== undefined)
      stats.folds += 1;
    lastFrontier = frontier.headerHash;
    if (frontier.headerHash !== queue.root) {
      if (!rolledBack)
        return `confirmed_ledger is at ${frontier.headerHash}, the queue root ${queue.root}, with no rollback yet`;
      if (hold?.reason !== CONFIRMED_LEDGER_BEHIND)
        return `a frontier off the queue root held ${JSON.stringify(hold)}`;
      stats.behindHeld += 1;
      behind = true;
      return null;
    }
    if (behind) stats.behindHealed += 1;
    behind = false;
    stats.comparedChecks += 1;
    const { prefix, bad } = processedPrefix(env.registry, queue);
    if (bad === undefined) {
      if (hold !== undefined) return `unexpected hold ${JSON.stringify(hold)}`;
    } else {
      if (hold?.reason !== LANDED_BLOCK_INVALID || !hold.detail.includes(bad))
        return `bad block ${bad} held ${JSON.stringify(hold)}`;
      stats.invalidHeld += 1;
    }
    const rows = await run(retrieveRows);
    const rowSummary = rows
      .map((row) => `${row.headerHash}:${row.kind}:${row.state}:${row.applied}`)
      .sort();
    const expectedRows = prefix
      .map((hash) => `${hash}:foreign:processed:true`)
      .sort();
    if (JSON.stringify(rowSummary) !== JSON.stringify(expectedRows))
      return `rows ${JSON.stringify(rowSummary)} vs model ${JSON.stringify(expectedRows)}`;
    const tip = prefix.at(-1) ?? queue.root;
    const tipInfo = env.registry.get(tip)!;
    const base = ledgerMap(env.universe.ledger(tipInfo.h, tipInfo.b));
    const { ledger, newly } = settleMempool(mempool, base);
    const expected = expectedState(
      env.universe,
      env.registry,
      queue,
      ledger,
      mempool,
    );
    const actual = await run(readActual(env.registry));
    const difference = stateDifference(actual, expected.actual);
    if (difference !== null) return difference;
    const { durableRoot } = await env.owner.current.diagnostics();
    if (durableRoot !== expected.tipRoot)
      return `native root ${durableRoot}, the processed tip's ${expected.tipRoot}`;
    const basis = (await run(Basis.retrieve)) ?? frontier;
    if (basis.headerHash !== tip)
      return `working-ledger basis ${basis.headerHash}, the processed tip ${tip}`;
    // Case counters (the node agreed with the model).
    const removed = lastPrefix.filter((hash) => !prefix.includes(hash));
    if (event.kind === "roll_backward" && removed.length > 0) {
      stats.rollbacksRemovingProcessed += 1;
      if (newly.length > 0) stats.rejectionsOnRollback += 1;
      if (newly.some(([, reason]) => reason === "dependent"))
        stats.latentHoleClosed += 1;
    }
    for (const [, reason] of newly)
      if (reason === "direct") stats.directRejections += 1;
      else stats.dependentRejections += 1;
    lastPrefix = prefix;
    if (
      ((await store.cursor())?.prunedThroughSlot ?? SIM_ORIGIN.point.slot) >
      SIM_ORIGIN.point.slot
    )
      stats.prunedChecks += 1;
    if (stats.checks % 2 === 0 && mempool.survivors.length < 6)
      await admit(tip, ledger);
    return null;
  };

  return {
    ...stateQueueProjection(SIM_QUEUE_CONFIG),
    name: "landed-blocks",
    traffic: landedBlocksTraffic(env.universe, env.registry, stats),
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
