/**
 * An independent model of the canonical chain for the watcher's fork-
 * simulator tests (ticket W1): it follows the chain-sync events itself and
 * replays the decoded blocks into a UTxO set, unit histories and the
 * state-queue linked list, without the watcher projection's code.
 */
import {
  type BlockSummary,
  decodeBlock,
  type OutputSummary,
  type TxSummary,
} from "@al-ft/midgard-l1-follower";
import type { ForkStep } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { WatcherProjectionDeployment } from "../../src/l1-follower/projection.js";

export type OracleEntry = Readonly<{ tx: TxSummary; block: BlockSummary }>;

export type ChainModel = Readonly<{
  blocks: readonly BlockSummary[];
  /** Every output the canonical chain created, by `txHash#index`. */
  created: ReadonlyMap<string, OracleEntry & { output: OutputSummary }>;
  /** Who spent each spent output. */
  spentBy: ReadonlyMap<string, OracleEntry>;
  /** Unspent outputs at the tip. */
  live: ReadonlySet<string>;
  /** Per asset unit, every valid tx that created or spent an output holding it. */
  unitHistory: ReadonlyMap<string, readonly OracleEntry[]>;
  /** Every stored-or-not tx of the chain by hash. */
  txs: ReadonlyMap<string, OracleEntry>;
}>;

export const label = (txHash: Buffer | string, index: number): string =>
  `${typeof txHash === "string" ? txHash : txHash.toString("hex")}#${index.toString()}`;

const unitsOf = (output: OutputSummary): string[] =>
  [...output.assets].flatMap(([policy, names]) =>
    [...names.keys()].map((name) => `${policy}${name}`),
  );

/** Replays canonical blocks into the model. */
export const modelOf = (blocks: readonly BlockSummary[]): ChainModel => {
  const created = new Map<string, OracleEntry & { output: OutputSummary }>();
  const spentBy = new Map<string, OracleEntry>();
  const live = new Set<string>();
  const unitHistory = new Map<string, OracleEntry[]>();
  const txs = new Map<string, OracleEntry>();
  const note = (unit: string, entry: OracleEntry): void => {
    const list = unitHistory.get(unit) ?? [];
    if (list.at(-1)?.tx !== entry.tx) list.push(entry);
    unitHistory.set(unit, list);
  };
  for (const block of blocks)
    for (const tx of block.txs) {
      const entry = { tx, block };
      txs.set(tx.hash.toString("hex"), entry);
      const spends = tx.isValid ? tx.inputs : tx.collaterals;
      for (const outRef of spends) {
        const key = label(outRef.txHash, outRef.index);
        live.delete(key);
        spentBy.set(key, entry);
        const output = created.get(key)?.output;
        if (tx.isValid && output !== undefined)
          for (const unit of unitsOf(output)) note(unit, entry);
      }
      const outputs = tx.isValid
        ? tx.outputs.map((output, index) => ({ output, index }))
        : tx.collateralReturn === null
          ? []
          : [{ output: tx.collateralReturn, index: tx.outputs.length }];
      for (const { output, index } of outputs) {
        const key = label(tx.hash, index);
        created.set(key, { ...entry, output });
        live.add(key);
        if (tx.isValid) for (const unit of unitsOf(output)) note(unit, entry);
      }
    }
  return { blocks, created, spentBy, live, unitHistory, txs };
};

/** Follows a run's chain-sync events into the canonical decoded blocks. */
export const createChainFollower = () => {
  const blocks: BlockSummary[] = [];
  let seen = -1n;
  return {
    /** Applies `step` once (a check may run more than once per step). */
    follow: (step: ForkStep): readonly BlockSummary[] => {
      const event = step.event;
      if (event.seq === seen) return blocks;
      seen = event.seq;
      if (event.kind === "roll_forward")
        blocks.push(decodeBlock(Buffer.from(event.block)));
      else if (event.point.kind === "point") {
        const hash = event.point.hash;
        while (
          blocks.length > 0 &&
          blocks.at(-1)!.point.hash.toString("hex") !== hash
        )
          blocks.pop();
      } else blocks.length = 0;
      return blocks;
    },
  };
};

export type ModelQueue = Readonly<{
  /** Root (null header) then nodes, as `{ headerHash, outRef }`. */
  queue: readonly Readonly<{ headerHash: string | null; outRef: string }>[];
  lock: string | null;
}>;

/** The state-queue linked list and CorrectionLock live in the model. */
export const modelQueue = (
  model: ChainModel,
  deployment: WatcherProjectionDeployment,
): ModelQueue => {
  const queueAddress = `70${deployment.stateQueueSpend}`;
  const nodes = new Map<
    string | null,
    Readonly<{ outRef: string; link: string | null }>
  >();
  let lock: string | null = null;
  for (const key of model.live) {
    const output = (model.created.get(key) as { output: OutputSummary }).output;
    const hub = output.assets.get(deployment.hubOracleMint);
    if (
      hub?.has(SDK.CORRECTION_LOCK_ASSET_NAME) === true &&
      output.address.toString("hex") === `70${deployment.correctionLockSpend}`
    )
      lock = key;
    const names = output.assets.get(deployment.stateQueueMint);
    if (names === undefined || output.address.toString("hex") !== queueAddress)
      continue;
    const name = [...names.keys()][0] as string;
    const header =
      name === SDK.STATE_QUEUE_ROOT_ASSET_NAME
        ? null
        : name.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length);
    const datum = Data.from(
      (output.datum as Buffer).toString("hex"),
      SDK.LinkedListDatum,
    );
    nodes.set(header, { outRef: key, link: datum.link });
  }
  const queue: { headerHash: string | null; outRef: string }[] = [];
  for (
    let at: string | null | undefined = nodes.has(null) ? null : undefined;
    at !== undefined;

  ) {
    const node = nodes.get(at);
    if (node === undefined) throw new Error("the model queue is broken");
    queue.push({ headerHash: at, outRef: node.outRef });
    at = node.link ?? undefined;
  }
  return { queue, lock };
};
