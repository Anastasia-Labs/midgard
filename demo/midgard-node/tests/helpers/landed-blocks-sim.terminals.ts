/**
 * The queue-terminal projection (N4, plan §5.5 P8) under the fork
 * simulator: its rows must equal a fresh derivation from the canonical
 * chain after every event, so a correction that lands is a `removed` row at
 * once, a merge a `merged` one, and a rollback past either deletes the row
 * with the facts (the header is live again).
 *
 * The model walks the canonical blocks only: each valid tx's spent
 * state-queue nodes (looked up among the outputs earlier canonical txs
 * created) that it does not put back as nodes are terminal, `merged` when a
 * root it creates confirms the header.
 */
import type {
  BlockSummary,
  FactStore,
  OutputSummary,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import { QUEUE_TERMINALS_TABLE } from "../../src/l1-queue-terminals/index.js";
import { SIM_QUEUE_CONFIG } from "./state-queue-sim.fixtures.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

const headersOf = (
  outputs: readonly OutputSummary[],
  kind: "root" | "node",
): Set<string> => {
  const headers = new Set<string>();
  for (const output of outputs) {
    if (hex(output.address) !== SIM_QUEUE_CONFIG.address) continue;
    const decoded = SDK.decodeStateQueueOutput(
      output,
      SIM_QUEUE_CONFIG.policyId,
    );
    if (decoded?.kind === kind && decoded.headerHash !== null)
      headers.add(decoded.headerHash);
  }
  return headers;
};

/** `header:tx:outcome:slot` for every terminal on the canonical chain, sorted. */
export const canonicalTerminals = (
  blocks: readonly BlockSummary[],
): string[] => {
  const outputs = new Map<string, OutputSummary>();
  const rows: string[] = [];
  for (const block of blocks)
    for (const tx of block.txs) {
      if (!tx.isValid) continue;
      const spent = tx.inputs.flatMap((input) => {
        const output = outputs.get(`${hex(input.txHash)}#${input.index}`);
        return output === undefined ? [] : [output];
      });
      tx.outputs.forEach((output, index) =>
        outputs.set(`${hex(tx.hash)}#${index}`, output),
      );
      const kept = headersOf(tx.outputs, "node");
      const confirmed = headersOf(tx.outputs, "root");
      for (const header of headersOf(spent, "node"))
        if (!kept.has(header))
          rows.push(
            `${header}:${hex(tx.hash)}:${confirmed.has(header) ? "merged" : "removed"}:${block.point.slot.toString()}`,
          );
    }
  return rows.sort();
};

/** The projection's rows in `store`, in the model's form. */
export const storedTerminals = async (store: FactStore): Promise<string[]> =>
  (
    await store.transaction("read", (tx) =>
      tx.query(
        `SELECT header_hash, transaction_hash, terminal_outcome, slot FROM ${QUEUE_TERMINALS_TABLE}`,
      ),
    )
  )
    .map(
      (row) =>
        `${hex(row.header_hash as Uint8Array)}:${hex(row.transaction_hash as Uint8Array)}:${String(row.terminal_outcome)}:${String(row.slot)}`,
    )
    .sort();

/**
 * Null when the stored rows equal the model's; counts each `removed` row
 * (an admitted correction) the first time it is seen.
 */
export const compareTerminals = async (
  store: FactStore,
  canonical: readonly BlockSummary[],
  seen: Set<string>,
  stats: { terminalRemovals: number },
): Promise<string | null> => {
  const expected = canonicalTerminals(canonical);
  const actual = await storedTerminals(store);
  if (JSON.stringify(actual) !== JSON.stringify(expected))
    return `queue terminals ${JSON.stringify(actual)} vs model ${JSON.stringify(expected)}`;
  for (const row of actual)
    if (row.includes(":removed:") && !seen.has(row)) {
      seen.add(row);
      stats.terminalRemovals += 1;
    }
  return null;
};
