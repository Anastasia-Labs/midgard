/**
 * The landed-block rebase's working ledger equals a from-scratch oracle
 * (`helpers/working-ledger-oracle.ts`) over generated cases: after every
 * rebase, `mempool_ledger`'s rows (output, ledger ids, address), the
 * rejections in the order the rebase returns them with their causes, the
 * deposit statuses, and the stamp of every row the rebase kept.
 *
 * Each case runs rounds on one database: transactions admitted (spending
 * the working ledger, chained); deposits projected outside any block; at
 * the round's start, a rollback of the processed tip; a processed foreign
 * block on the tip that spends outputs pending transactions spend, includes
 * pending transactions (one whose output another transaction of the block
 * spends), and may carry a deposit (one a transaction of the block spends);
 * then an own journal on the new tip that carries pending transactions or
 * only a deposit, and lives, dies, or dies and is pruned before the rebase
 * (a dead deposit-only journal with no rebase due is the case
 * `landed-blocks-rebase-pruned-journal.test.ts` pins). The corpus must
 * exercise every one of these.
 */
import { expect, it } from "vitest";

import { REBASE_REJECTIONS } from "../src/landed-blocks/rebase.js";
import type { LandedBlockRow } from "../src/landed-blocks/store.js";
import type { SimPendingTx } from "./helpers/landed-blocks-sim.mempool.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import {
  type OracleDelta,
  oracleRebuild,
  type OracleTx,
} from "./helpers/working-ledger-oracle.js";
import {
  freshNative,
  hex,
  processOf,
  root,
} from "./landed-blocks-rebase.fixture.js";
import {
  admit,
  AT,
  type CaseDeposit,
  type CaseJournal,
  commitJournal,
  depositOf,
  killJournal,
  landBlock,
  projectDeposit,
  pruneJournals,
  rebaseOnce,
  rollBack,
  seedCase,
  shownCase,
} from "./landed-blocks-rebase-equivalence.fixture.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

const CASES = 10;
const ROUNDS = 3;

/** A seeded generator in [0, 1). */
const random = (seed: number) => {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4_294_967_296;
  };
};

const STATS = [
  "direct",
  "dependent",
  "spentInBlock",
  "included",
  "rolledBack",
  "liveJournal",
  "deadJournal",
  "prunedJournal",
  "depositJournal",
  "outsideDeposit",
  "blockDeposit",
  "depositSpentInBlock",
  "releasedDeposit",
  "consumedDeposit",
] as const;
const stats = Object.fromEntries(STATS.map((name) => [name, 0])) as Record<
  (typeof STATS)[number],
  number
>;

const netDelta = (txs: readonly SimPendingTx[]): OracleDelta => {
  const produced = new Map(
    txs.flatMap((tx) => tx.produced.map((entry) => [hex(entry.outref), entry])),
  );
  const spent: Buffer[] = [];
  for (const outRef of txs.flatMap((tx) => tx.spent))
    if (!produced.delete(hex(outRef))) spent.push(outRef);
  return { spent, produced: [...produced.values()] };
};

/** The transactions of `pending` a block on `ledger` may carry, each with odds `odds`. */
const includable = (
  pending: readonly SimPendingTx[],
  ledger: ReadonlySet<string>,
  next: () => number,
  odds: number,
) => {
  const available = new Set(ledger);
  const chosen: SimPendingTx[] = [];
  for (const tx of pending)
    if (next() < odds && tx.spent.every((key) => available.has(hex(key)))) {
      for (const key of tx.spent) available.delete(hex(key));
      for (const entry of tx.produced) available.add(hex(entry.outref));
      chosen.push(tx);
    }
  return chosen;
};

const runCase = async (seed: number) => {
  const next = random(seed);
  const pick = <A>(values: readonly A[]) =>
    values[Math.floor(next() * values.length)]!;
  const label = (name: string) => `equivalence:${seed}:${name}`;
  const header = (name: string) =>
    simDigest(label(name)).subarray(0, 28).toString("hex");
  const output = (name: string, lovelace: bigint) => ({
    outref: makeOutRefCbor(simDigest(label(name)), 0),
    output: simOutput(lovelace),
  });
  const confirmed = [0, 1, 2, 3, 4, 5].map((n) =>
    output(`c${n}`, 2_000_000n + BigInt(n)),
  );
  const frontier = { headerHash: header("frontier"), utxosRoot: root(0x20) };
  const globals = await processOf(freshNative());
  await seedCase(globals, frontier, confirmed);

  const chain: LandedBlockRow[] = [];
  const marked = new Set<string>();
  let pending: SimPendingTx[] = [];
  const working = new Set(confirmed.map((entry) => hex(entry.outref)));
  const outside = new Map<string, CaseDeposit>();
  const held = new Map<string, CaseDeposit>();
  const released = new Set<string>();
  let live: string | undefined;
  let txCount = 0;
  let rootCount = 0x21;
  const tipLedger = () => {
    const ledger = new Set(confirmed.map((entry) => hex(entry.outref)));
    for (const row of chain) {
      for (const key of row.spent) ledger.delete(hex(key));
      for (const entry of row.produced) ledger.add(hex(entry.outref));
    }
    return ledger;
  };
  const tip = () =>
    chain.at(-1) === undefined
      ? frontier
      : {
          headerHash: chain.at(-1)!.headerHash,
          utxosRoot: chain.at(-1)!.utxosRoot,
        };

  for (let round = 0; round < ROUNDS; round++) {
    if (live !== undefined) {
      await killJournal(globals, live);
      live = undefined;
    }
    if (chain.length > 0 && next() < 0.3) {
      const row = chain.pop()!;
      await rollBack(globals, row);
      for (const id of row.txIds) marked.delete(hex(id));
      for (const id of row.depositIds) {
        held.delete(hex(id));
        released.add(hex(id));
        stats.releasedDeposit += 1;
      }
      stats.rolledBack += 1;
    }
    // Admissions: each spends one or two outputs of the working ledger.
    const admitted: SimPendingTx[] = [];
    for (let n = 1 + Math.floor(next() * 4); n > 0; n--) {
      const keys = [...working];
      if (keys.length === 0) break;
      const spent = new Set([pick(keys)]);
      if (next() < 0.4) spent.add(pick(keys));
      const id = simDigest(label(`tx${txCount}`));
      const produced = [0, 1].slice(0, 1 + Math.floor(next() * 2)).map((i) => ({
        outref: makeOutRefCbor(id, i),
        output: simOutput(5_000_000n + BigInt(txCount * 10 + i)),
      }));
      const tx = {
        id,
        spent: [...spent].map((key) => Buffer.from(key, "hex")),
        produced,
        at: new Date(AT + txCount * 1_000),
      };
      txCount += 1;
      for (const key of spent) working.delete(key);
      for (const entry of produced) working.add(hex(entry.outref));
      admitted.push(tx);
    }
    await admit(globals, admitted);
    pending.push(...admitted);
    if (next() < 0.5) {
      const deposit = depositOf(
        `${seed}:d${round}`,
        7_000_000n + BigInt(round),
      );
      await projectDeposit(globals, deposit);
      outside.set(hex(deposit.eventId), deposit);
      working.add(hex(deposit.outref));
      stats.outsideDeposit += 1;
    }

    // A processed foreign block on the tip.
    const base = tipLedger();
    const spentByPending = pending
      .filter((tx) => !marked.has(hex(tx.id)))
      .flatMap((tx) => tx.spent.map(hex))
      .filter((key) => base.has(key));
    const direct = new Set<string>();
    if (spentByPending.length > 0 && next() < 0.7)
      direct.add(pick(spentByPending));
    if (next() < 0.3) direct.add(pick([...base]));
    const carried = includable(
      pending.filter((tx) => !marked.has(hex(tx.id))),
      new Set([...base].filter((key) => !direct.has(key))),
      next,
      0.4,
    );
    const delta = netDelta(carried);
    const produced = [...delta.produced];
    const txIds = carried.map((tx) => tx.id);
    if (produced.length > 0 && next() < 0.6) {
      // A transaction the node never admitted spends a carried output.
      produced.splice(Math.floor(next() * produced.length), 1);
      const foreignTx = simDigest(label(`foreign-tx${round}`));
      txIds.push(foreignTx);
      produced.push({
        outref: makeOutRefCbor(foreignTx, 0),
        output: simOutput(9_000_000n),
      });
      stats.spentInBlock += 1;
    }
    produced.push(output(`block${round}`, 8_000_000n + BigInt(round)));
    const depositIds: Buffer[] = [];
    if (outside.size > 0 && next() < 0.4) {
      const deposit = pick([...outside.values()]);
      outside.delete(hex(deposit.eventId));
      held.set(hex(deposit.eventId), deposit);
      depositIds.push(deposit.eventId);
      if (next() < 0.5) {
        // A transaction of the block spends the deposit's output: the
        // block's net delta names neither.
        const spender = simDigest(label(`deposit-spender${round}`));
        txIds.push(spender);
        produced.push({
          outref: makeOutRefCbor(spender, 0),
          output: simOutput(9_500_000n),
        });
        stats.depositSpentInBlock += 1;
      } else produced.push({ outref: deposit.outref, output: deposit.output });
      stats.blockDeposit += 1;
    }
    const row: LandedBlockRow = {
      headerHash: header(`block${round}`),
      parentHeaderHash: tip().headerHash,
      parentUtxosRoot: tip().utxosRoot,
      utxosRoot: root(rootCount++),
      kind: "foreign",
      state: "processed",
      applied: false,
      spent: [
        ...[...direct].map((key) => Buffer.from(key, "hex")),
        ...delta.spent,
      ],
      produced,
      depositIds,
      withdrawals: [],
      forcedIds: [],
      txIds,
    };
    await landBlock(globals, row);
    chain.push(row);
    for (const id of txIds) marked.add(hex(id));
    stats.included += carried.length;

    // An own journal on the new tip.
    let liveDelta: OracleDelta | undefined;
    let liveTxIds = new Set<string>();
    if (next() < 0.7) {
      const deposit =
        outside.size > 0 && next() < 0.4
          ? pick([...outside.values()])
          : undefined;
      const txs =
        deposit === undefined
          ? includable(
              pending.filter((tx) => !marked.has(hex(tx.id))),
              tipLedger(),
              next,
              0.5,
            )
          : [];
      const journalDelta =
        deposit === undefined
          ? netDelta(txs)
          : {
              spent: [],
              produced: [{ outref: deposit.outref, output: deposit.output }],
            };
      const journal: CaseJournal = {
        headerHash: header(`own${round}`),
        baseHeaderHash: tip().headerHash,
        baseUtxosRoot: tip().utxosRoot,
        expectedUtxosRoot: root(rootCount++),
        ...journalDelta,
        txIds: txs.map((tx) => tx.id),
        deposits: deposit === undefined ? [] : [deposit],
      };
      await commitJournal(globals, journal);
      if (deposit !== undefined) stats.depositJournal += 1;
      const fate = next();
      if (fate < 0.4) {
        live = journal.headerHash;
        liveDelta = journalDelta;
        liveTxIds = new Set(txs.map((tx) => hex(tx.id)));
        stats.liveJournal += 1;
      } else {
        await killJournal(globals, journal.headerHash);
        stats.deadJournal += 1;
        if (fate < 0.7) {
          expect(await pruneJournals(globals)).toBeGreaterThan(0);
          stats.prunedJournal += 1;
        }
      }
    }

    // The rebase, against the oracle.
    const before = await shownCase(globals);
    const replayed: OracleTx[] = pending.filter(
      (tx) => !marked.has(hex(tx.id)) && !liveTxIds.has(hex(tx.id)),
    );
    const expected = oracleRebuild({
      confirmed,
      steps: [...chain, ...(liveDelta === undefined ? [] : [liveDelta])],
      outside: [...outside.values()],
      held: [...held.values()],
      pending: replayed,
    });
    const order = await rebaseOnce(globals);
    const after = await shownCase(globals);
    expect(after.rows).toEqual(expected.rows);
    expect(order).toEqual(expected.rejected.map((rejection) => rejection.id));
    for (const rejection of expected.rejected) {
      expect(after.rejections.get(rejection.id)).toBe(
        REBASE_REJECTIONS[rejection.reason].code,
      );
      stats[rejection.reason] += 1;
    }
    expect(after.causes).toEqual(
      [
        ...before.causes,
        ...expected.rejected.flatMap((rejection) =>
          rejection.causes.map((cause) => `${rejection.id}<-${cause}`),
        ),
      ].sort(),
    );
    for (const [outRef, stamp] of before.stamps)
      if (after.stamps.has(outRef))
        expect(after.stamps.get(outRef)).toBe(stamp);
    for (const [id, deposit] of [...outside, ...held]) {
      const present = expected.rows.has(hex(deposit.outref));
      expect(after.deposits.get(id)).toBe(present ? "projected" : "consumed");
      if (!present) stats.consumedDeposit += 1;
    }
    for (const id of released) expect(after.deposits.get(id)).toBe("awaiting");
    for (const [index, row] of chain.entries())
      chain[index] = { ...row, applied: true };
    const rejected = new Set(
      expected.rejected.map((rejection) => rejection.id),
    );
    pending = pending.filter((tx) => !rejected.has(hex(tx.id)));
    working.clear();
    for (const key of expected.rows.keys()) working.add(key);
    released.clear();
  }
};

it("leaves the working ledger, rejections, deposit statuses and kept stamps a from-scratch replay derives, over generated rebases", async () => {
  for (let seed = 1; seed <= CASES; seed++) await runCase(seed);
  for (const name of STATS) expect([name, stats[name]]).not.toEqual([name, 0]);
}, 600_000);
