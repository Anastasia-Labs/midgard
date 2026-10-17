import {
  type BlockSummary,
  decodeBlock,
  type OutRef,
  outRefKey,
} from "@al-ft/midgard-l1-follower";
import {
  type ForkStep,
  type SimOutput,
} from "@al-ft/midgard-l1-follower/testing";

import { decodeElement, SIM_QUEUE } from "./queue-sim-traffic.js";

/** The expected landed queue, from the canonical blocks alone. */
export type ExpectedQueue = Readonly<{
  healthy: boolean;
  outRefs: readonly string[];
  /** Commit height of every header ever seen on the current chain. */
  commitHeight: ReadonlyMap<string, number>;
  /** Creation height of every live queue output. */
  createdHeight: ReadonlyMap<string, number>;
  /** Every queue output on the current chain, live or spent. */
  rows: readonly QueueRowLife[];
}>;

/** A queue output's life on the current chain. */
export type QueueRowLife = Readonly<{
  /** Root: its confirmed header hash; node: its header hash. */
  headerHash: string;
  createdSlot: number;
  spentSlot: number | null;
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
    const rows = new Map<string, { life: QueueRowLife }>();
    for (const block of this.blocks)
      for (const tx of block.txs) {
        if (!tx.isValid) continue;
        for (const input of tx.inputs) {
          const key = outRefKey(input);
          live.delete(key);
          const row = rows.get(key);
          if (row !== undefined)
            row.life = { ...row.life, spentSlot: block.point.slot };
        }
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
          rows.set(outRefKey(outRef), {
            life: {
              headerHash: element.headerHash,
              createdSlot: block.point.slot,
              spentSlot: null,
            },
          });
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
      rows: [...rows.values()].map((row) => row.life),
    };
  }
}
