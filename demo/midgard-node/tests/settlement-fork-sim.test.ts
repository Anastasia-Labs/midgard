/**
 * Settlement confirmation (plan §15 N6 and L7, D-N4) under the fork
 * simulator, on the node's Postgres follower store with prune on, with the
 * node's settlement and journal projections. The own wallet plans
 * settlement bodies; each is journaled, with its settlement attempt, just
 * before the block it was planned for, then lands there, lands later or
 * never. After every event the production `settleAttempts` runs, and:
 *
 * - every pending attempt's derived level equals the model chain's: landed
 *   at depth > k final, at depth >= cd safe, else open; so a rollback deeper
 *   than cd reverts a confirmation, and a prune never loses one (an attempt
 *   whose journal entry was pruned reads open, unlike the chain);
 * - a stored `final` is landed on the model chain, at every later step;
 * - each job's phase is `complete` exactly while its attempt is settled.
 *
 * The suite proves it saw confirmations reverted by rollbacks and attempts
 * stored final by the prune step.
 */
import "./utils.js";

import { randomUUID } from "node:crypto";

import {
  type BlockSummary,
  currentViewIn,
  decodeBlock,
  type FactStore,
  type FollowerProjection,
  intentJournalProjection,
  levelAtDepth,
  openPostgresFactStore,
  recordIntentIn,
} from "@al-ft/midgard-l1-follower";
import {
  encodeSimTx,
  forkCorpus,
  type ForkRunOptions,
  forkScenarioArbitrary,
  runForkScenario,
  SIM_ORIGIN,
  type SimTx,
  simTxHash,
} from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, ManagedRuntime, Redacted } from "effect";
import fc from "fast-check";
import { afterAll, describe, expect, it } from "vitest";

import * as MigrationRunner from "../src/database/migrations/runner.js";
import * as Journal from "../src/database/settlement.js";
import { readIntentStatus } from "../src/services/intent-journal.js";
import { settlementProjection } from "../src/services/settlement.final-hook.js";
import {
  settleAttempts,
  type SettlementLevel,
  settlementLevel,
} from "../src/services/settlement.status.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const SIM_K = 6;
const CD = 2;
const depths = { confirmationDepth: CD, securityParameter: SIM_K };
const RUNS = Number(process.env.SETTLEMENT_FORK_SIM_RUNS ?? "6");
const deploymentId = "a8".repeat(32);
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

type Stats = {
  recorded: number;
  /** Safe or final before a rollback, open right after it. */
  reverted: number;
  /** Attempts the prune step stored final. */
  storedFinal: number;
  levels: Record<SettlementLevel, number>;
};

type Run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient>,
) => Promise<A>;

const hex = (bytes: Buffer): string => bytes.toString("hex");

const settlementSimulation = (stats: Stats, node: () => Run) => {
  type Planned = Readonly<{ tx: SimTx; hash: string; eventId: string }>;
  /** Build side: bodies to journal after the n-th roll-forward. */
  const plans = new Map<number, Planned[]>();
  const planned: Planned[] = [];
  let built = 0;

  const traffic: FollowerProjection["traffic"] = ({ chain, rng, claim }) => {
    built += 1;
    const block: SimTx[] = [];
    const slot = chain.nextSlot();
    const includable = (p: Planned): boolean =>
      (p.tx.invalidAfter === undefined || slot < p.tx.invalidAfter) &&
      p.tx.inputs.every((outRef) => chain.isLive(outRef));
    for (const p of planned)
      if (rng.chance(0.4) && includable(p) && p.tx.inputs.every(claim))
        block.push(p.tx);
    if (built === 1 || !rng.chance(0.6)) return block;
    const own = chain
      .live()
      .filter((utxo) =>
        utxo.output.address.equals(chain.universe.trackedAddress),
      );
    let input = null;
    for (
      let tries = 0;
      tries < 4 && own.length > 0 && input === null;
      tries++
    ) {
      const utxo = rng.pick(own);
      if (claim(utxo.outRef)) input = utxo.outRef;
    }
    if (input === null) return block;
    const tx: SimTx = {
      inputs: [input],
      outputs: [
        {
          address: chain.universe.trackedAddress,
          lovelace: BigInt(rng.range(2, 9)) * 1_000_000n,
        },
      ],
      ...(rng.chance(0.3) ? { invalidAfter: slot + rng.range(1, 8) } : {}),
      nonce: chain.nonce(),
    };
    const p: Planned = {
      tx,
      hash: hex(simTxHash(tx)),
      eventId: planned.length.toString(16).padStart(4, "0"),
    };
    planned.push(p);
    const list = plans.get(built - 1) ?? [];
    list.push(p);
    plans.set(built - 1, list);
    if (rng.chance(0.5) && includable(p)) block.push(tx);
    return block;
  };

  /** Replay side: the model chain above the origin. */
  const blocks: BlockSummary[] = [];
  let forwards = 0;
  const lastLevel = new Map<string, SettlementLevel>();
  const finals = new Set<string>();
  const owner: Journal.SettlementOwner = {
    deploymentId,
    walletAddress: "settlement-wallet",
    token: randomUUID(),
  };
  let owned = false;

  /** The model's level of `hash`, and whether it is landed at all. */
  const modelLevel = (hash: string) => {
    const tipHeight = blocks.at(-1)?.height ?? SIM_ORIGIN.height;
    for (const block of blocks)
      if (block.txs.some((tx) => tx.isValid && hex(tx.hash) === hash)) {
        const depth = tipHeight - block.height + 1;
        const level = levelAtDepth(depth, depths);
        return {
          landed: true,
          level: (level === "final"
            ? "final"
            : level === "safe"
              ? "safe"
              : "open") as SettlementLevel,
        };
      }
    return { landed: false, level: "open" as SettlementLevel };
  };

  const check: FollowerProjection["check"] = async ({ store, step }) => {
    const run = node();
    const backward = step.event.kind === "roll_backward";
    if (step.event.kind === "roll_forward") {
      forwards += 1;
      blocks.push(decodeBlock(step.event.block));
    } else {
      const target = step.event.point;
      while (
        blocks.length > 0 &&
        (target.kind === "origin" ||
          hex(blocks[blocks.length - 1]!.point.hash) !== target.hash)
      )
        blocks.pop();
    }
    if (!owned) {
      await run(openFollowerWriteGate);
      owned = true;
    }
    await run(Journal.renew(owner));
    // Journal what was planned for the next block, with its attempt.
    if (!backward)
      for (const p of plans.get(forwards) ?? []) {
        const result = await store.transaction("write", async (tx) =>
          recordIntentIn(tx, store.dialect, {
            family: "settlement",
            workflowKey: `settlement:${p.hash}`,
            txCbor: encodeSimTx(p.tx),
            isOwnOutput: (output) =>
              output.address.equals(p.tx.outputs[0]!.address),
            builtAt: (await currentViewIn(tx, store.dialect))!,
          }),
        );
        // A body whose input another body or the filler spent first is
        // refused by the journal; S6 never sends it, so no attempt.
        if (result.kind !== "recorded") continue;
        await run(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
              VALUES (${deploymentId}, 'deposit', ${p.eventId}, 'absorb')`;
            yield* Journal.saveAttempt(
              owner,
              {
                deployment_id: deploymentId,
                kind: "deposit",
                event_id: p.eventId,
                phase: "absorb",
                tx_hash: p.hash,
                signed_cbor: hex(encodeSimTx(p.tx)),
                required_outputs: [0],
                fee_inputs: [
                  `${hex(p.tx.inputs[0]!.txHash)}#${p.tx.inputs[0]!.index}`,
                ],
                status: "pending",
              },
              Effect.void,
            );
          }),
        );
        stats.recorded += 1;
      }
    if (!backward) plans.delete(forwards);
    await run(settleAttempts(owner, depths));
    const rows = await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{
          tx_hash: string;
          status: string;
          job_phase: string;
        }>`SELECT a.tx_hash, a.status, j.phase AS job_phase
          FROM settlement_attempts a JOIN settlement_jobs j USING (deployment_id, kind, event_id)
          WHERE a.deployment_id = ${deploymentId}`;
      }),
    );
    for (const row of rows) {
      const model = modelLevel(row.tx_hash);
      let level: SettlementLevel;
      if (row.status === "final") {
        if (!model.landed)
          return `attempt ${row.tx_hash} stored final but not landed on the chain`;
        if (!finals.has(row.tx_hash)) {
          finals.add(row.tx_hash);
          stats.storedFinal += 1;
        }
        level = "final";
      } else {
        if (finals.has(row.tx_hash))
          return `attempt ${row.tx_hash} was final, now ${row.status}`;
        level = settlementLevel(
          await run(readIntentStatus(row.tx_hash)),
          depths,
        );
        if (level !== model.level)
          return `attempt ${row.tx_hash}: derived ${level}, the chain says ${model.level}`;
      }
      const settled = level !== "open";
      if ((row.job_phase === "complete") !== settled)
        return `attempt ${row.tx_hash} ${level}, its job ${row.job_phase}`;
      stats.levels[level] += 1;
      const previous = lastLevel.get(row.tx_hash);
      if (backward && previous !== undefined && previous !== "open" && !settled)
        stats.reverted += 1;
      lastLevel.set(row.tx_hash, level);
    }
    return null;
  };

  const projection: FollowerProjection = {
    ...intentJournalProjection,
    name: "settlement-sim",
    traffic,
    check,
  };
  return projection;
};

describe("settlement confirmation in the fork simulator (Postgres, node database)", () => {
  it("derives every attempt's level from the chain over the corpus and random scenarios", async () => {
    const stats: Stats = {
      recorded: 0,
      reverted: 0,
      storedFinal: 0,
      levels: { open: 0, safe: 0, final: 0 },
    };
    let current: { run: Run; close: () => Promise<void> } | null = null;
    const node = (): Run => {
      if (current === null) throw new Error("no node database open");
      return current.run;
    };
    const open: ForkRunOptions["open"] = async (optionsFor) => {
      const connectionString = await databases.create();
      const runtime = ManagedRuntime.make(
        PgClient.layer({ url: Redacted.make(connectionString) }),
      );
      const run: Run = (effect) => runtime.runPromise(effect);
      await run(
        MigrationRunner.migrate({ appVersion: "test", actor: "n6b-fork-sim" }),
      );
      current = { run, close: () => runtime.dispose() };
      const store: FactStore = openPostgresFactStore({
        ...optionsFor("postgres"),
        connection: { connectionString },
      });
      return store;
    };
    const scenario = async (
      name: string,
      value: Parameters<typeof runForkScenario>[0],
    ) => {
      try {
        const outcome = await runForkScenario(value, {
          open,
          k: SIM_K,
          projections: [
            settlementProjection,
            settlementSimulation(stats, node),
          ],
        });
        if (!outcome.ok)
          throw new Error(`${name}, step ${outcome.step}: ${outcome.reason}`);
      } finally {
        await current?.close();
        current = null;
      }
    };
    for (const { name, scenario: value } of forkCorpus(SIM_K))
      await scenario(name, value);
    await fc.assert(
      fc.asyncProperty(forkScenarioArbitrary(SIM_K), (value) =>
        scenario("random", value),
      ),
      { numRuns: RUNS, seed: 0x06_0b07 },
    );
    expect(stats.recorded).toBeGreaterThan(0);
    expect(stats.levels.safe).toBeGreaterThan(0);
    expect(stats.reverted).toBeGreaterThan(0);
    expect(stats.storedFinal).toBeGreaterThan(0);
  }, 900_000);
});
