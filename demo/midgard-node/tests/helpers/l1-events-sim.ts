/**
 * The node event projection in the fork simulator (plan §15 F8, N1): its
 * traffic (admissions inline and external, continuations, retirements,
 * resubmitted keys, the same key under the other kind) and its check, which
 * compares the projection at every step with an independent in-memory model
 * of the canonical chain (P2 event and key sets, P4 spendability).
 */
import {
  type BlockSummary,
  decodeBlock,
  type FactStore,
  type FollowerProjection,
  type OutRef,
  type TxSummary,
} from "@al-ft/midgard-l1-follower";
import {
  outRefHex,
  type ScenarioTraffic,
  SIM_ORIGIN,
  type SimChain,
  type SimTx,
  simTxHash,
  type SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  EVENT_KINDS,
  type EventKind,
  eventProjection,
  eventsAt,
  type SlotTime,
  slotToPosixMs,
  spendableDepositsAt,
} from "../../src/l1-events/index.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
  retirementTx,
} from "./l1-events-chain.js";

export const SIM_SLOT_TIME: SlotTime = {
  zeroTime: 0,
  zeroSlot: 0,
  slotLength: 1_000,
};

const REASONS = ["absorbed", "payout_initialized", "refunded"] as const;

const other = (kind: EventKind): EventKind =>
  kind === "deposit" ? "withdrawal" : "deposit";

/** What the corpus exercised, so a suite can prove each case happened. */
export type EventSimStats = {
  admissions: number;
  externals: number;
  continuations: number;
  retirements: number;
  /** A key whose admission a rollback orphaned, admitted again later. */
  readmissions: number;
  /** A resubmitted key refused because it is in the key set. */
  retiredKeyRefusals: number;
  /** A key admitted while the other kind's list already holds it. */
  sameKeyOtherKind: number;
  /** Deposits checked unspendable at the tip because they are not yet due. */
  notYetDue: number;
  checks: number;
};

export const zeroEventSimStats = (): EventSimStats => ({
  admissions: 0,
  externals: 0,
  continuations: 0,
  retirements: 0,
  readmissions: 0,
  retiredKeyRefusals: 0,
  sameKeyOtherKind: 0,
  notYetDue: 0,
  checks: 0,
});

const holdsKey = (utxo: SimUtxo, kind: EventKind, key: string): boolean =>
  utxo.output.assets?.get(listOf(kind).policyId)?.has(key) === true;

type ModelEvent = {
  kind: EventKind;
  key: string;
  admissionTx: string;
  admissionBlock: string;
  inclusionTime: bigint;
  holder: string;
  retiredBy: string | null;
};

type Model = {
  events: Map<string, ModelEvent>;
  refusals: Set<string>;
};

const orderFacts = (datum: Buffer | null): SDK.EventHistoryFacts | null => {
  if (datum === null) return null;
  try {
    const node = Data.from(datum.toString("hex"), SDK.EventHistoryNode);
    return node.payload !== "RootContent" && "Order" in node.payload
      ? node.payload.Order.facts
      : null;
  } catch {
    return null;
  }
};

const applyTx = (model: Model, tx: TxSummary, blockHash: string): void => {
  for (const kind of EVENT_KINDS) {
    const list = listOf(kind);
    for (const [name, quantity] of tx.mint.get(list.policyId) ?? []) {
      const event = model.events.get(`${kind}:${name}`);
      if (quantity === -1n && event !== undefined && event.retiredBy === null)
        event.retiredBy = tx.hash.toString("hex");
    }
    tx.outputs.forEach((output, index) => {
      if (output.address.toString("hex") !== list.listAddress) return;
      for (const [name] of output.assets.get(list.policyId) ?? []) {
        if (name.length !== 64) continue;
        const id = `${kind}:${name}`;
        const holder = outRefHex({ txHash: tx.hash, index });
        const known = model.events.get(id);
        if (known !== undefined && known.retiredBy === null) {
          known.holder = holder;
          continue;
        }
        const facts = orderFacts(output.datum);
        if (facts === null) continue;
        if (known !== undefined) {
          model.refusals.add(`${id}:${holder}`);
          continue;
        }
        model.events.set(id, {
          kind,
          key: name,
          admissionTx: tx.hash.toString("hex"),
          admissionBlock: blockHash,
          inclusionTime: facts.inclusion_time,
          holder,
          retiredBy: null,
        });
      }
    });
  }
};

/** The model of a chain: every block from the origin, in order. */
const modelOf = (blocks: readonly BlockSummary[]): Model => {
  const model: Model = { events: new Map(), refusals: new Set() };
  for (const block of blocks)
    for (const tx of block.txs)
      if (tx.isValid) applyTx(model, tx, block.point.hash.toString("hex"));
  return model;
};

/**
 * The Orders of live events on the chain being built (a refused
 * resubmission's output holds a list token too, but no event: on L1 the
 * policy never mints a used key again, so traffic never spends one).
 */
const liveOrders = (chain: SimChain, kind: EventKind): SimUtxo[] => {
  const model = modelOf(chain.rawBlocks().map((raw) => decodeBlock(raw)));
  const holders = new Set(
    [...model.events.values()]
      .filter((event) => event.kind === kind && event.retiredBy === null)
      .map((event) => event.holder),
  );
  return chain.live().filter((utxo) => holders.has(outRefHex(utxo.outRef)));
};

const sorted = <T>(items: T[], key: (item: T) => string): T[] =>
  [...items].sort((a, b) => (key(a) < key(b) ? -1 : key(a) > key(b) ? 1 : 0));

const projectedRefusals = async (store: FactStore): Promise<Set<string>> =>
  new Set(
    (
      await store.transaction("read", (tx) =>
        tx.query(
          "SELECT kind, event_key, tx_hash, output_index, reason, detail FROM node_l1_event_refusals",
        ),
      )
    ).map(
      (row) =>
        `${String(row.kind)}:${Buffer.from(row.event_key as Uint8Array).toString("hex")}:${outRefHex(
          {
            txHash: Buffer.from(row.tx_hash as Uint8Array),
            index: Number(row.output_index),
          },
        )}${row.reason === "retired_key" ? "" : `:${String(row.reason)}:${String(row.detail)}`}`,
    ),
  );

const firstDifference = (a: unknown, b: unknown): string | null => {
  const left = JSON.stringify(a, (_, v: unknown) =>
    typeof v === "bigint" ? v.toString() : v,
  );
  const right = JSON.stringify(b, (_, v: unknown) =>
    typeof v === "bigint" ? v.toString() : v,
  );
  return left === right
    ? null
    : `projection ${left.slice(0, 400)} vs model ${right.slice(0, 400)}`;
};

/**
 * The projection with traffic and the model check. One per scenario run:
 * the traffic remembers the ids it used, and the stats accumulate.
 */
export const eventSimProjection = (
  stats: EventSimStats,
): FollowerProjection => {
  const used: { kind: EventKind; nonce: OutRef }[] = [];
  const admittedIn = new Map<string, string>();
  const refusalsSeen = new Set<string>();

  const traffic: ScenarioTraffic = ({ chain, rng, claim }) => {
    const txs: SimTx[] = [];
    const tipSlot = chain.tip.point.slot;
    const keyLive = (kind: EventKind, key: string): boolean =>
      chain.live().some((utxo) => holdsKey(utxo, kind, key));
    const admit = (kind: EventKind, nonce: OutRef): void => {
      const order = eventOrder(kind, nonce, {
        external: rng.chance(0.25),
        inclusionTime: BigInt(
          slotToPosixMs(SIM_SLOT_TIME, tipSlot + rng.range(-10, 20)),
        ),
      });
      if (keyLive(kind, order.key)) return;
      if (order.retained !== null) {
        const store: SimTx = {
          inputs: [chain.outsideInput()],
          outputs: [order.retained],
          nonce: chain.nonce(),
        };
        txs.push(store);
        stats.externals += 1;
        // The retention output is created earlier in the same block.
        txs.push(
          admissionTx(order, chain.nonce(), {
            txHash: simTxHash(store),
            index: 0,
          }),
        );
      } else txs.push(admissionTx(order, chain.nonce()));
      used.push({ kind, nonce });
    };
    for (const kind of EVENT_KINDS) {
      const orders = liveOrders(chain, kind);
      if (orders.length > 0 && rng.chance(0.35)) {
        const target = rng.pick(orders);
        const [key] = [
          ...target.output.assets!.get(listOf(kind).policyId)!.keys(),
        ];
        if (claim(target.outRef))
          txs.push(
            retirementTx(
              { kind, key: key! },
              target.outRef,
              rng.pick(REASONS),
              chain.nonce(),
            ),
          );
      } else if (orders.length > 0 && rng.chance(0.25)) {
        const target = rng.pick(orders);
        if (claim(target.outRef)) {
          txs.push({
            inputs: [target.outRef],
            outputs: [target.output],
            nonce: chain.nonce(),
          });
          stats.continuations += 1;
        }
      }
    }
    // A resubmitted id first (from earlier blocks only), then a fresh one.
    if (used.length > 0 && rng.chance(0.3)) {
      const old = rng.pick(used);
      admit(rng.chance(0.75) ? old.kind : other(old.kind), old.nonce);
    }
    if (rng.chance(0.5))
      admit(rng.chance(0.5) ? "deposit" : "withdrawal", chain.outsideInput());
    return txs;
  };

  // The check runs after each event; the chain it is handed is the final
  // one, so it keeps the canonical blocks itself, from the events.
  const canonical: BlockSummary[] = [];
  const check: FollowerProjection["check"] = async ({ store, step }) => {
    stats.checks += 1;
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
    const model = modelOf(canonical);
    const tip = canonical[canonical.length - 1]?.point ?? SIM_ORIGIN.point;
    for (const kind of EVENT_KINDS) {
      const read = await eventsAt(store, listOf(kind), tip);
      if (read.kind !== "ok") return `eventsAt ${kind}: ${read.kind}`;
      const projected = sorted(
        read.value.map((event) => ({
          key: event.key,
          admissionTx: event.admission.txHash,
          admissionBlock: event.admission.blockHash,
          inclusionTime: event.inclusionTime,
          holder: outRefHex(event.location),
          retiredBy: event.retirement?.txHash ?? null,
          reasonKnown:
            event.retirement === null || event.retirement.reason !== null,
        })),
        (e) => e.key,
      );
      const expected = sorted(
        [...model.events.values()]
          .filter((event) => event.kind === kind)
          .map((event) => ({
            key: event.key,
            admissionTx: event.admissionTx,
            admissionBlock: event.admissionBlock,
            inclusionTime: event.inclusionTime,
            holder: event.holder,
            retiredBy: event.retiredBy,
            reasonKnown: true,
          })),
        (e) => e.key,
      );
      const difference = firstDifference(projected, expected);
      if (difference !== null) return `${kind} events: ${difference}`;
    }
    const refusals = await projectedRefusals(store);
    const difference = firstDifference(
      [...refusals].sort(),
      [...model.refusals].sort(),
    );
    if (difference !== null) return `retired-key refusals: ${difference}`;
    // P4 at the tip: due by slotNow (here the tip slot) and admitted.
    const cutoffMs = BigInt(slotToPosixMs(SIM_SLOT_TIME, tip.slot));
    const spendable = await spendableDepositsAt(store, listOf("deposit"), tip, {
      slot: tip.slot,
      slotTime: SIM_SLOT_TIME,
    });
    if (spendable.kind !== "ok") return `spendable: ${spendable.kind}`;
    const deposits = [...model.events.values()].filter(
      (e) => e.kind === "deposit",
    );
    const spendableDifference = firstDifference(
      spendable.value.map((d) => d.key).sort(),
      deposits
        .filter((e) => e.inclusionTime <= cutoffMs)
        .map((e) => e.key)
        .sort(),
    );
    if (spendableDifference !== null)
      return `spendable deposits: ${spendableDifference}`;
    stats.notYetDue += deposits.filter(
      (e) => e.inclusionTime > cutoffMs,
    ).length;
    // Case counters (the model agreed, so these describe the projection).
    for (const event of model.events.values()) {
      const id = `${event.kind}:${event.key}`;
      const before = admittedIn.get(id);
      if (before === undefined) {
        stats.admissions += 1;
        if (model.events.has(`${other(event.kind)}:${event.key}`))
          stats.sameKeyOtherKind += 1;
      } else if (before !== event.admissionTx) stats.readmissions += 1;
      admittedIn.set(id, event.admissionTx);
      if (event.retiredBy !== null && !refusalsSeen.has(`retired:${id}`)) {
        refusalsSeen.add(`retired:${id}`);
        stats.retirements += 1;
      }
    }
    for (const refusal of model.refusals)
      if (!refusalsSeen.has(refusal)) {
        refusalsSeen.add(refusal);
        stats.retiredKeyRefusals += 1;
      }
    return null;
  };

  // List and retention outputs are script-locked on L1: only this traffic spends them.
  const scriptAddresses = new Set(
    EVENTS_CONFIG.lists.flatMap((list) => [
      list.listAddress,
      list.retentionAddress,
    ]),
  );
  return {
    ...eventProjection(EVENTS_CONFIG),
    traffic,
    check,
    protects: (output) => scriptAddresses.has(output.address.toString("hex")),
  };
};
