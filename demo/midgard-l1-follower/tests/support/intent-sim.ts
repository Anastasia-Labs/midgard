import {
  type BlockSummary,
  createIntentReconciler,
  decodeBlock,
  deriveIntentStatusesIn,
  type FactStore,
  type FollowerProjection,
  insertIntentIn,
  type IntentHead,
  intentJournalProjection,
  type IntentState,
  type OutRef,
  recordIntentIn,
} from "../../src/index.js";
import {
  encodeSimTx,
  SIM_ORIGIN,
  type SimOutput,
  type SimTx,
  simTxHash,
} from "../../src/testing/index.js";
import type { SimChain } from "../../src/testing/sim-chain.js";
import { SIM_K } from "./fork-sim.js";
import { expectedText, modelStatuses, observedOf } from "./intent-sim.model.js";
import {
  type Journal,
  journalText,
  readJournal,
  statusText,
} from "./intent-sim.text.js";

/**
 * The intent journal in the fork simulator. Its traffic is an own wallet
 * (the universe's tracked address) planning transactions: each is journaled
 * just before the block it was planned for, then lands there, lands later,
 * never lands, or is beaten to an input by the filler. Some expire, some
 * fail phase 2, some spend a still-unlanded parent's output (a dependency).
 *
 * After every simulator event the check:
 * - compares every derived status with the same journal over a fresh
 *   forward-only replay (the reference is never pruned); a pruned intent
 *   must be terminal for k blocks there, and a retained one must not be
 *   right after a prune;
 * - on a rollback, before anything else runs, finds the journal unchanged
 *   (only a prune step may delete rows): a landed intent the rollback
 *   un-lands is live again with no write;
 * - runs S6 with a submit spy: only the exact journaled bytes of a live
 *   intent are ever sent, at most once per tip, and every live, wanted
 *   intent outside the mempool with its inputs live is sent.
 */
export type IntentSimStats = {
  recorded: number;
  resubmitted: number;
  abandoned: number;
  /** Landed before a rollback, live right after it. */
  unlandedToLive: number;
  /** Status kinds seen in checks, by kind. */
  seen: Record<string, number>;
  /** Intents pruned from the store. */
  pruned: number;
  /** Superseded conflicts (an own intent spent the input). */
  superseded: number;
  /** Children refused because their parent was pruned dead. */
  refusedPrunedParent: number;
};

type Planned = Readonly<{ tx: SimTx; hash: Buffer; family: string }>;

const hex = (bytes: Buffer): string => bytes.toString("hex");

/** A stable pseudo-random bit per (intent, step): the mempool and the predicate. */
const coin = (hash: Buffer, step: number, salt: number, percent: number) =>
  ((hash.readUInt32BE(0) ^ Math.imul(step + 1, 0x9e3779b1) ^ salt) >>> 0) %
    100 <
  percent;

export const intentSimulation = (): {
  projection: FollowerProjection;
  stats: IntentSimStats;
} => {
  const stats: IntentSimStats = {
    recorded: 0,
    resubmitted: 0,
    abandoned: 0,
    unlandedToLive: 0,
    seen: {},
    pruned: 0,
    superseded: 0,
    refusedPrunedParent: 0,
  };
  /** Build side: intents to journal after the n-th roll-forward. */
  const plans = new Map<number, Planned[]>();
  const planned: Planned[] = [];
  let built = 0;

  const ownAddress = (chain: SimChain): Buffer => chain.universe.trackedAddress;

  const traffic: FollowerProjection["traffic"] = ({ chain, rng, claim }) => {
    built += 1;
    const block: SimTx[] = [];
    const slot = chain.nextSlot();
    const includable = (p: Planned): boolean =>
      (p.tx.invalidAfter === undefined || slot < p.tx.invalidAfter) &&
      [...p.tx.inputs, ...(p.tx.collaterals ?? [])].every((outRef) =>
        chain.isLive(outRef),
      );
    // Earlier plans may land now (again, after a rollback un-landed them).
    for (const p of planned)
      if (rng.chance(0.4) && includable(p)) {
        const spends = [...p.tx.inputs, ...(p.tx.collaterals ?? [])];
        if (spends.every((outRef) => claim(outRef))) block.push(p.tx);
      }
    if (built === 1 || !rng.chance(0.6)) return block;
    const own = chain
      .live()
      .filter((utxo) => utxo.output.address.equals(ownAddress(chain)));
    const pick = (): OutRef | null => {
      for (let tries = 0; tries < 4 && own.length > 0; tries += 1) {
        const utxo = rng.pick(own);
        if (claim(utxo.outRef)) return utxo.outRef;
      }
      return null;
    };
    const parents = planned.filter(
      (p) => !chain.isLive({ txHash: p.hash, index: 0 }),
    );
    const child = parents.length > 0 && rng.chance(0.3);
    const parent = child ? rng.pick(parents) : null;
    // A failed parent's child may spend its collateral return (§8.2: that
    // output exists on a phase-2 failure).
    const input: OutRef | null =
      parent === null
        ? pick()
        : {
            txHash: parent.hash,
            index:
              parent.tx.isValid === false &&
              parent.tx.collateralReturn !== undefined &&
              rng.chance(0.5)
                ? parent.tx.outputs.length
                : 0,
          };
    if (input === null) return block;
    const failed = rng.chance(0.1);
    const collateral = failed ? pick() : null;
    if (failed && collateral === null) return block;
    const outputs: SimOutput[] = [
      {
        address: ownAddress(chain),
        lovelace: BigInt(rng.range(2, 9)) * 1_000_000n,
      },
      ...(rng.chance(0.5)
        ? [{ address: chain.universe.untrackedAddress, lovelace: 1_000_000n }]
        : []),
    ];
    const tx: SimTx = {
      inputs: [input],
      outputs,
      ...(rng.chance(0.3) ? { invalidAfter: slot + rng.range(1, 8) } : {}),
      ...(failed && collateral !== null
        ? {
            isValid: false,
            collaterals: [collateral],
            collateralReturn: {
              address: ownAddress(chain),
              lovelace: 1_000_000n,
            },
          }
        : {}),
      nonce: chain.nonce(),
    };
    const p: Planned = {
      tx,
      hash: simTxHash(tx),
      family: rng.pick(["commit", "merge", "settlement"]),
    };
    planned.push(p);
    const list = plans.get(built - 1) ?? [];
    list.push(p);
    plans.set(built - 1, list);
    if (!child && rng.chance(0.5) && includable(p)) block.push(tx);
    return block;
  };

  /** Replay side. */
  let forwards = 0;
  let after: string[] | null = null;
  let lastStates = new Map<string, IntentState>();
  /** Every journaled intent with its latest events (pruned ones too). */
  const known = new Map<string, Journal>();
  const sent: { hash: string; tip: string; bytes: Buffer }[] = [];
  let tip = "";
  let liveNow = new Set<string>();
  /** The model's current chain above the origin. */
  const blocks: BlockSummary[] = [];
  const failures: string[] = [];
  /** Planned children refused because their parent was pruned dead. */
  const refused = new Set<string>();

  const loadReference = async (reference: FactStore): Promise<void> => {
    await reference.transaction("write", async (tx) => {
      await tx.query("DELETE FROM l1_intent_events");
      await tx.query("DELETE FROM l1_intents");
      const intents = [...known.values()].flatMap((j) => j.intents);
      // Parents before children: insertion order of `known` is record order.
      for (const intent of intents)
        await insertIntentIn(tx, reference.dialect, intent);
      for (const event of [...known.values()].flatMap((j) => j.events))
        await tx.query(
          "INSERT INTO l1_intent_events (tx_hash, seq, kind, detail, tip_slot) VALUES (?, ?, ?, ?, ?)",
          [
            event.txHash,
            event.seq,
            event.kind,
            event.detail === null
              ? null
              : reference.dialect.json(JSON.stringify(event.detail)),
            event.tipSlot,
          ],
        );
    });
  };

  const snapshotKnown = (journal: Journal): void => {
    for (const intent of journal.intents)
      known.set(hex(intent.txHash), {
        intents: [intent],
        events: journal.events.filter((e) => e.txHash.equals(intent.txHash)),
      });
  };

  const projection: FollowerProjection = {
    ...intentJournalProjection,
    name: "intent-journal-sim",
    traffic,
    check: async ({ store, reference, step }) => {
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
      const tipSlot =
        blocks[blocks.length - 1]?.point.slot ?? SIM_ORIGIN.point.slot;
      const journal = await readJournal(store);
      // A rollback writes nothing to the journal; a prune only deletes.
      if (after !== null) {
        const now = new Set(journalText(journal));
        const before = new Set(after);
        for (const row of now)
          if (!before.has(row))
            return `journal row written by the event: ${row}`;
        if (step.prune !== true && now.size !== before.size)
          return "journal rows deleted outside a prune";
      }
      const cursor = await store.cursor();
      const prunedThrough = cursor?.prunedThroughSlot ?? 0;
      // The prune step runs its hooks only when the block k below the
      // cursor is still a row: right after a rewind of exactly k blocks it
      // was pruned, and an intent abandoned at that tip waits for the next
      // step whose boundary block exists.
      const pruneRanHooks =
        cursor !== null &&
        (await store.transaction(
          "read",
          async (tx) =>
            (
              await tx.query(
                "SELECT 1 AS one FROM l1_blocks WHERE height = ?",
                [cursor.height - SIM_K],
              )
            ).length > 0,
        ));
      const retained = new Set(journal.intents.map((i) => hex(i.txHash)));
      // Derived statuses against the fresh replay with the whole journal.
      await loadReference(reference);
      const [mine, theirs] = await Promise.all([
        store.transaction("read", (tx) =>
          deriveIntentStatusesIn(tx, store.dialect),
        ),
        reference.transaction("read", (tx) =>
          deriveIntentStatusesIn(tx, reference.dialect),
        ),
      ]);
      const expected = new Map(
        theirs.states.map((s) => [hex(s.intent.txHash), s]),
      );
      for (const state of mine.states) {
        const key = hex(state.intent.txHash);
        const other = expected.get(key);
        if (other === undefined)
          return `intent ${key} missing from the reference`;
        if (statusText(state) !== statusText(other))
          return `intent ${key}: ${statusText(state)} vs fresh ${statusText(other)}`;
        stats.seen[state.status.kind] =
          (stats.seen[state.status.kind] ?? 0) + 1;
        if (state.status.kind === "conflicted" && state.status.ownSpender)
          stats.superseded += 1;
        const previous = lastStates.get(key);
        if (
          backward &&
          previous?.status.kind === "landed" &&
          state.status.kind === "live"
        )
          stats.unlandedToLive += 1;
      }
      for (const [key, state] of expected) {
        const isRetained = retained.has(key);
        const terminalForK =
          state.terminalSlot !== null && state.terminalSlot <= prunedThrough;
        if (!isRetained && !terminalForK)
          return `intent ${key} pruned while not terminal for k (${statusText(state)}, pruned through ${prunedThrough})`;
        if (isRetained && terminalForK && step.prune === true && pruneRanHooks)
          return `intent ${key} terminal for k but kept by the prune (${statusText(state)}, pruned through ${prunedThrough})`;
      }
      // Every status against the naive oracle over the whole model chain.
      const abandonedNow = new Set(
        journal.events
          .filter((e) => e.kind === "abandoned")
          .map((e) => hex(e.txHash)),
      );
      const model = modelStatuses(
        blocks,
        tipSlot,
        [...known.values()].flatMap((j) => j.intents),
        abandonedNow,
      );
      for (const state of mine.states) {
        const key = hex(state.intent.txHash);
        const want = model.get(key);
        if (want === undefined) return `intent ${key} unknown to the model`;
        if (expectedText(observedOf(state.status)) !== expectedText(want))
          return `intent ${key}: derived ${expectedText(observedOf(state.status))}, the chain says ${expectedText(want)}`;
      }
      for (const [key, want] of model)
        if (!retained.has(key) && want.kind === "live")
          return `intent ${key} pruned while live on the chain`;
      stats.pruned = [...known.keys()].filter((k) => !retained.has(k)).length;
      lastStates = new Map(mine.states.map((s) => [hex(s.intent.txHash), s]));
      // Journal what was planned for the next block.
      for (const p of plans.get(forwards) ?? []) {
        if (backward) break;
        const result = await store.transaction("write", (tx) =>
          recordIntentIn(tx, store.dialect, {
            family: p.family,
            workflowKey: `${p.family}:${hex(p.hash)}`,
            txCbor: encodeSimTx(p.tx),
            isOwnOutput: (output) =>
              output.address.equals(
                p.tx.outputs[0]?.address ?? Buffer.alloc(0),
              ),
          }),
        );
        // A child of a parent pruned dead (terminal k deep), or of such a
        // refused child, is refused: its input is neither a fact nor a
        // journaled intent's output.
        if (
          result.kind === "input_untracked" &&
          result.untracked.every(
            (o) =>
              refused.has(hex(o.txHash)) ||
              (known.has(hex(o.txHash)) &&
                !journal.intents.some((i) => i.txHash.equals(o.txHash))),
          )
        ) {
          refused.add(hex(p.hash));
          stats.refusedPrunedParent += 1;
          continue;
        }
        if (result.kind !== "recorded")
          return `record ${hex(p.hash)}: ${result.kind}${
            result.kind === "input_untracked"
              ? ` ${result.untracked
                  .map((o) => {
                    const by = planned.findIndex((q) =>
                      q.hash.equals(o.txHash),
                    );
                    return `${hex(o.txHash)}#${o.index}${by >= 0 ? ` (planned #${by}, ${known.has(hex(o.txHash)) ? "recorded" : "unrecorded"})` : ""}`;
                  })
                  .join(", ")} at forward ${forwards}`
              : ""
          }`;
        stats.recorded += 1;
      }
      if (!backward) plans.delete(forwards);
      // S6 with a spy.
      tip = `${cursor?.generation ?? 0}:${hex(cursor?.point.hash ?? Buffer.alloc(0))}`;
      const fresh = await store.transaction("read", (tx) =>
        deriveIntentStatusesIn(tx, store.dialect),
      );
      const modelAfter = modelStatuses(
        blocks,
        tipSlot,
        [
          ...[...known.values()].flatMap((j): IntentHead[] => j.intents),
          ...fresh.states.map((s) => s.intent),
        ],
        new Set(
          (await readJournal(store)).events
            .filter((e) => e.kind === "abandoned")
            .map((e) => hex(e.txHash)),
        ),
      );
      liveNow = new Set(
        [...modelAfter]
          .filter(([, e]) => e.kind === "live")
          .map(([key]) => key),
      );
      const step_ = forwards;
      const reconciler = reconcilerFor(store);
      const report = await reconciler.reconcile();
      if (failures.length > 0) return failures.join("; ");
      for (const entry of report.intents) {
        if (entry.error !== undefined) return `reconcile error: ${entry.error}`;
        const key = hex(entry.intent.txHash);
        const want = modelAfter.get(key);
        if (want === undefined)
          return `reconciled ${key}, unknown to the model`;
        if (want.kind !== "live" && entry.action === "resubmit")
          return `${want.kind} intent ${key} resubmitted`;
        if (want.kind === "live" && want.inputsAvailable === true) {
          const inMempool = coin(entry.intent.txHash, step_, 1, 30);
          const wanted = coin(entry.intent.txHash, step_, 2, 97);
          const shouldSend = !inMempool && wanted;
          const didSend = sent.some((s) => s.hash === key && s.tip === tip);
          if (shouldSend !== didSend)
            return `live intent ${key}: sent ${didSend}, expected ${shouldSend}`;
        }
        if (
          want.kind === "live" &&
          want.inputsAvailable === false &&
          entry.action !== "wait_inputs" &&
          entry.action !== "wait_in_mempool"
        )
          return `live intent ${key} with an input not live: ${entry.action}, expected a wait`;
        if (entry.action === "abandon") stats.abandoned += 1;
      }
      after = journalText(await readJournal(store));
      snapshotKnown(await readJournal(store));
      return null;
    },
  };

  let reconciler: ReturnType<typeof createIntentReconciler> | null = null;
  let reconcilerStore: FactStore | null = null;
  const reconcilerFor = (store: FactStore) => {
    if (reconciler !== null && reconcilerStore === store) return reconciler;
    reconcilerStore = store;
    reconciler = createIntentReconciler({
      dialect: store.dialect,
      transaction: (mode, run) => store.transaction(mode, run),
      securityParameter: 6,
      inMempool: (intent) =>
        Promise.resolve(coin(intent.txHash, forwards, 1, 30)),
      wanted: (state) =>
        Promise.resolve(coin(state.intent.txHash, forwards, 2, 97)),
      submit: (intent) => {
        const key = hex(intent.txHash);
        if (!liveNow.has(key)) failures.push(`sent ${key}, which is not live`);
        if (sent.some((s) => s.hash === key && s.tip === tip))
          failures.push(`sent ${key} twice at one tip`);
        const original = known.get(key)?.intents[0]?.txCbor;
        if (original !== undefined && !original.equals(intent.txCbor))
          failures.push(`sent ${key} with bytes other than the journal's`);
        if (!intent.txCbor.equals(encodeSimTx(plannedTx(key))))
          failures.push(`sent ${key} with bytes other than the signed ones`);
        sent.push({ hash: key, tip, bytes: intent.txCbor });
        stats.resubmitted += 1;
        return Promise.resolve({ kind: "accepted" });
      },
    });
    return reconciler;
  };
  const plannedTx = (key: string): SimTx => {
    const p = planned.find((candidate) => hex(candidate.hash) === key);
    if (p === undefined) throw new Error(`unplanned intent ${key}`);
    return p.tx;
  };

  return { projection, stats };
};
