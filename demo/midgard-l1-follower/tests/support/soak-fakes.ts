import {
  type ChainPoint,
  type ChainSyncEvent,
  IntersectNotFoundError,
  pointKey,
  samePoint,
} from "@al-ft/l1-node-transport";

import {
  decodeBlock,
  type OutputSummary,
  outRefKey,
  transportPoint,
} from "../../src/index.js";
import type { LedgerStateReader, SoakStream } from "../../src/shadow/index.js";
import {
  encodeUtxoAnswer,
  type ForkStep,
  SIM_ORIGIN,
  type SimOutput,
  type SimUtxo,
} from "../../src/testing/index.js";

/**
 * A node that serves `steps` once, in order, across stream reopenings (as a
 * node that remembers what it served), and refuses an intersection that is
 * not where the consumer must be.
 */
export const fakeNode = (steps: readonly ForkStep[]) => {
  let served = 0;
  const expectedAt = (): ChainPoint =>
    served === 0
      ? transportPoint(SIM_ORIGIN.point)
      : (steps[served - 1] as ForkStep).event.point;
  return {
    get served(): number {
      return served;
    },
    /** Serves the next event outside any stream (a consumer that crashed). */
    take(): ChainSyncEvent {
      const event = steps[served]?.event;
      if (event === undefined) throw new Error("no event left");
      served += 1;
      return event;
    },
    openChainSync: ({
      points,
    }: Readonly<{ points: readonly ChainPoint[] }>): SoakStream => {
      const at = expectedAt();
      const found = points.some((point) => samePoint(point, at));
      let closed = false;
      return {
        next: () => {
          if (!found)
            return Promise.reject(
              new IntersectNotFoundError({ point: at, blockNo: 0n }, false),
            );
          const event = closed ? undefined : steps[served]?.event;
          if (event !== undefined) served += 1;
          return Promise.resolve(event);
        },
        ack: () => undefined,
        close: () => {
          closed = true;
          return Promise.resolve();
        },
      };
    },
  };
};

const simOutput = (output: OutputSummary): SimOutput => ({
  address: output.address,
  lovelace: output.lovelace,
  assets: output.assets,
  ...(output.datum === null ? {} : { datum: output.datum }),
});

/**
 * The node's UTxO set at every point the steps reach, folded from the raw
 * blocks with plain ledger rules (independent of the store's SQL), and a
 * ledger-state reader over it. `tamper` edits an answer (red checks).
 */
export const fakeLedger = (
  steps: readonly ForkStep[],
  tamper?: (height: number, utxos: SimUtxo[]) => SimUtxo[],
): LedgerStateReader => {
  const states = new Map<string, { height: number; utxos: SimUtxo[] }>();
  const canonical: { point: ChainPoint; utxos: Map<string, SimUtxo> }[] = [];
  const originKey = pointKey(transportPoint(SIM_ORIGIN.point));
  states.set(originKey, { height: SIM_ORIGIN.height, utxos: [] });
  for (const { event } of steps) {
    if (event.kind === "roll_backward") {
      while (
        canonical.length > 0 &&
        !samePoint(canonical[canonical.length - 1]!.point, event.point)
      )
        canonical.pop();
      continue;
    }
    const utxos = new Map(canonical[canonical.length - 1]?.utxos ?? []);
    for (const tx of decodeBlock(event.block).txs) {
      for (const outRef of tx.isValid ? tx.inputs : tx.collaterals)
        utxos.delete(outRefKey(outRef));
      const created = tx.isValid
        ? tx.outputs.map((output, index) => ({ index, output }))
        : tx.collateralReturn === null
          ? []
          : [{ index: tx.outputs.length, output: tx.collateralReturn }];
      for (const { index, output } of created)
        utxos.set(outRefKey({ txHash: tx.hash, index }), {
          outRef: { txHash: tx.hash, index },
          output: simOutput(output),
        });
    }
    canonical.push({ point: event.point, utxos });
    states.set(pointKey(event.point), {
      height: Number(event.blockNo),
      utxos: [...utxos.values()],
    });
  }
  return {
    withLedgerState: async (at, use) => {
      if (at === "tip") throw new Error("the fake ledger answers at points");
      const state = states.get(pointKey(at));
      if (state === undefined) throw new Error("acquire_failed: point unknown");
      return await use({
        query: (query) => {
          if (query.query !== "utxo_by_address")
            return Promise.reject(new Error(`unexpected ${query.query}`));
          const wanted = new Set(
            query.addresses.map((a) => Buffer.from(a).toString("hex")),
          );
          const matching = state.utxos.filter((u) =>
            wanted.has(u.output.address.toString("hex")),
          );
          return Promise.resolve(
            encodeUtxoAnswer(tamper?.(state.height, matching) ?? matching),
          );
        },
      });
    },
  };
};
