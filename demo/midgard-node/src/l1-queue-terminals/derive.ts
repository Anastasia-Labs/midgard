/**
 * The queue-terminal projection's S3 derivation (plan §5.5 P8, §7.4, N4).
 *
 * Per valid qualifying tx, in block order: every state-queue node it spends
 * whose header it does not put back as a node is terminal. The header is
 * `merged` when a root the tx creates confirms it, and `removed` otherwise.
 * A commit or a correction that relinks a node puts its header back, so it
 * is not terminal.
 *
 * Admission is the landed tx, so a correction is admitted in the block that
 * lands it; acting on it may wait for `safe` (liveness). A rollback removes
 * the row with the facts, and every reader recomputes from what remains: no
 * observer state, no revocation and no halt.
 *
 * A pure function of the block, the facts and the config: no clock, no
 * network.
 */
import {
  changedUtxosIn,
  type DerivationHook,
  type OutputSummary,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import type { StateQueueProjectionConfig } from "../l1-state-queue/config.js";
import { QUEUE_TERMINALS_TABLE } from "./schema.js";

const HEADER_HASH = /^[0-9a-f]{56}$/u;

/** The header hashes of the queue roots or nodes among `outputs`. */
const headersOf = (
  outputs: readonly OutputSummary[],
  config: StateQueueProjectionConfig,
  kind: "root" | "node",
): Set<string> => {
  const address = Buffer.from(config.address, "hex");
  const headers = new Set<string>();
  for (const output of outputs) {
    if (!output.address.equals(address)) continue;
    const decoded = SDK.decodeStateQueueOutput(output, config.policyId);
    if (
      decoded?.kind === kind &&
      decoded.headerHash !== null &&
      HEADER_HASH.test(decoded.headerHash)
    )
      headers.add(decoded.headerHash);
  }
  return headers;
};

export const queueTerminalDerivation = (
  config: StateQueueProjectionConfig,
): DerivationHook => ({
  name: QUEUE_TERMINALS_TABLE,
  writes: [QUEUE_TERMINALS_TABLE],
  apply: async (context) => {
    const { block } = context;
    for (const { tx, created, spent } of context.qualified) {
      if (!tx.isValid || spent.length === 0) continue;
      // The block's facts are in: a spent output is still stored (spent at
      // this block), including one an earlier tx of the block created.
      const read = await changedUtxosIn(
        context.tx,
        context.dialect,
        { by: "outref", outRefs: spent },
        null,
      );
      if (read.kind !== "ok")
        throw new Error(
          `queue terminals: the facts of a block being applied are unreadable (${read.kind}: ${read.detail})`,
        );
      const spentNodes = headersOf(
        read.utxos.map((stored) => stored.output),
        config,
        "node",
      );
      if (spentNodes.size === 0) continue;
      const createdOutputs = created.map((entry) => entry.output);
      const keptNodes = headersOf(createdOutputs, config, "node");
      const confirmed = headersOf(createdOutputs, config, "root");
      for (const header of spentNodes) {
        if (keptNodes.has(header)) continue;
        await context.tx.query(
          `INSERT INTO ${QUEUE_TERMINALS_TABLE} (header_hash, transaction_hash, terminal_outcome, tx_index, block_hash, height, slot) VALUES (?, ?, ?, ?, ?, ?, ?)`,
          [
            Buffer.from(header, "hex"),
            tx.hash,
            confirmed.has(header) ? "merged" : "removed",
            tx.index,
            block.point.hash,
            block.height,
            block.point.slot,
          ],
        );
      }
    }
  },
});
