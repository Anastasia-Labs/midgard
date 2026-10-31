import type { FraudProofRawL1Utxo } from "@al-ft/midgard-fault-proofs";
import {
  depth,
  type FactStore,
  type OutRef,
  type SqlRow,
  type SqlTx,
  type StoredBlock,
  type StoredTx,
} from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import { recordsUnitHistory, stateQueueNodeUnitPattern } from "./projection.js";
import { partlyPrunedIn } from "./proof-retention.js";
import { headerPrunedIn, unitPrunedIn } from "./pruned-keys.js";
import {
  canonicalBlock,
  isTrackedAddress,
  missingRowReason,
  prunedSinceOrigin,
  samePoint,
} from "./raw-reads.facts.js";
import {
  type FollowerRawReads,
  type FollowerUnresolvedInput,
  type FollowerVerifiedSpend,
  type LedgerOutputsAt,
  ok,
  rawPointOf,
  type RawRead,
  refused,
} from "./raw-reads.types.js";
import { outRefLabel, parseOutRefLabel, resolveRawUtxoIn } from "./reads.js";
import {
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";
import { resolveStoredInputIn } from "./tx-inputs.js";

/**
 * The fraud-proof raw L1 reads, answered from the follower's facts and the
 * watcher projection (ticket W1). Each read returns the value in the raw
 * source's shape, or a refusal naming why the follower cannot answer it.
 *
 * Retention: a fact pruning may have removed is `beyond_retention`, never
 * "missing" or "unspent". An output created before the follower's origin
 * has no stored creating body; a read that needs its exact bytes is
 * `l1_input_before_origin`, never a stand-in value.
 *
 * Input resolution order: the stored creating body, then the input bytes
 * resolved at ingest for a tx a unit history records (tx-inputs.ts), then
 * the node's ledger state at the inclusion block's predecessor
 * (`ledgerOutputsAt`, only while that point is acquirable), then a named
 * refusal. Nothing is fetched by tx
 * id. Otherwise pure reads: no clock and no write.
 */

const hex = (value: unknown): string =>
  Buffer.from(value as Uint8Array).toString("hex");

/**
 * The follower-backed raw reads for one store. `stateQueuePolicyId` names
 * the node units whose history the watcher projection records, and
 * `unitHistoryPolicies` the policies whose units' histories it records
 * (`watcherUnitHistoryPolicies`);
 * `ledgerOutputsAt` is the node's ledger state (absent: inputs resolve from
 * stored bodies only).
 */
export const createFollowerRawReads = (
  store: FactStore,
  options: Readonly<{
    stateQueuePolicyId: string;
    unitHistoryPolicies?: ReadonlySet<string>;
    ledgerOutputsAt?: LedgerOutputsAt;
  }>,
): FollowerRawReads => {
  const nodeUnit = stateQueueNodeUnitPattern(options.stateQueuePolicyId);

  /** A live row's exact bytes from its creating body; a seed row has none. */
  const exactRow = async (
    tx: SqlTx,
    outRef: OutRef,
    seed: boolean,
  ): Promise<RawRead<FraudProofRawL1Utxo>> => {
    const utxo = await resolveRawUtxoIn(tx, outRef);
    if (utxo !== null) return ok(utxo);
    return seed
      ? refused(
          "l1_input_before_origin",
          `${outRefLabel(outRef)} was created before the follower's origin`,
        )
      : refused(
          "beyond_retention",
          `${outRefLabel(outRef)} has no stored creating body`,
        );
  };

  const addressUtxosAtPoint: FollowerRawReads["addressUtxosAtPoint"] = async (
    address,
    point,
  ) => {
    const parsed = CML.Address.from_bech32(address);
    try {
      if (!isTrackedAddress(store.trackedSet(), parsed))
        return refused(
          "untracked_address",
          `${address} is not in the tracked set`,
        );
      const block = await canonicalBlock(store, point);
      if (block.kind !== "ok") return block;
      const live = await store.liveUtxos(
        { by: "address", address: Buffer.from(parsed.to_raw_bytes()) },
        { slot: block.value.slot, hash: block.value.hash },
      );
      if (live.kind !== "ok") return refused(live.kind, live.detail);
      return await store.transaction("read", async (tx) => {
        const utxos: FraudProofRawL1Utxo[] = [];
        for (const row of live.utxos) {
          const exact = await exactRow(tx, row.outRef, row.created === null);
          if (exact.kind !== "ok") return exact;
          utxos.push(exact.value);
        }
        return ok(utxos);
      });
    } finally {
      parsed.free();
    }
  };

  const utxosByOutRefAtPoint: FollowerRawReads["utxosByOutRefAtPoint"] = async (
    outRefs,
    point,
  ) => {
    const block = await canonicalBlock(store, point);
    if (block.kind !== "ok") return block;
    const at = block.value.slot;
    const outputs: FraudProofRawL1Utxo[] = [];
    const spends: FollowerVerifiedSpend[] = [];
    const unknown: string[] = [];
    const beyondRetention: string[] = [];
    for (const label of outRefs) {
      const outRef = parseOutRefLabel(label);
      const stored = await store.output(outRef);
      if (stored === null) {
        if ((await missingRowReason(store, outRef)) === "unknown")
          unknown.push(label);
        else beyondRetention.push(label);
        continue;
      }
      if (stored.created !== null && stored.created.slot > at) {
        unknown.push(label);
        continue;
      }
      if (stored.spent !== null && stored.spent.slot <= at) {
        const spender = await store.blockAtOrBeforeSlot(stored.spent.slot);
        if (spender === null || spender.slot !== stored.spent.slot)
          return refused(
            "beyond_retention",
            `the block spending ${label} is not stored`,
          );
        spends.push({
          outRef: label,
          spendingTxHash: stored.spent.txHash.toString("hex"),
          spendPoint: rawPointOf(spender),
        });
        continue;
      }
      const exact = await store.transaction("read", (tx) =>
        exactRow(tx, outRef, stored.created === null),
      );
      if (exact.kind !== "ok") return exact;
      outputs.push(exact.value);
    }
    return ok({ outputs, spends, unknown, beyondRetention });
  };

  const unitHistoryAtPoint: FollowerRawReads["unitHistoryAtPoint"] = async (
    unit,
    point,
  ) => {
    const match = nodeUnit.exec(unit);
    const followed =
      options.unitHistoryPolicies !== undefined &&
      recordsUnitHistory(options.unitHistoryPolicies, unit);
    if (match === null && !followed)
      return refused(
        "unit_not_projected",
        `no projection records the history of unit ${unit}`,
      );
    const block = await canonicalBlock(store, point);
    if (block.kind !== "ok") return block;
    const [key, history, held, prunedRows] =
      match !== null
        ? [
            Buffer.from(match[1]!, "hex"),
            { table: WATCHER_QUEUE_UNIT_HISTORY_TABLE, column: "header_hash" },
            { table: WATCHER_PROOF_PINS_TABLE, column: "header_hash" },
            headerPrunedIn,
          ]
        : [
            Buffer.from(unit, "hex"),
            { table: WATCHER_UNIT_HISTORY_TABLE, column: "unit" },
            { table: WATCHER_PROOF_PIN_UNITS_TABLE, column: "unit" },
            unitPrunedIn,
          ];
    const read = await store.transaction(
      "read",
      async (tx): Promise<RawRead<readonly SqlRow[]>> => {
        const cursor = (
          await tx.query(
            "SELECT origin_slot, pruned_through_slot FROM l1_follower_cursor",
          )
        )[0];
        const prunedThrough =
          cursor === undefined ||
          Number(cursor.pruned_through_slot) <= Number(cursor.origin_slot)
            ? null
            : Number(cursor.pruned_through_slot);
        const rows = await tx.query(
          `SELECT tx_hash, block_hash, block_height, from_slot FROM ${history.table} WHERE ${history.column} = ? AND from_slot <= ? ORDER BY from_slot, tx_hash`,
          [key, block.value.slot],
        );
        // A unit's rows go once its removal (or burn) is k deep: after any
        // pruning, no rows may be a history that was pruned.
        if (prunedThrough !== null && rows.length === 0)
          return refused(
            "beyond_retention",
            `no history of ${unit} is retained; it may have been pruned`,
          );
        const unheld = async (): Promise<boolean> =>
          (
            await tx.query(
              `SELECT 1 AS one FROM ${held.table} WHERE ${held.column} = ? LIMIT 1`,
              [key],
            )
          ).length === 0;
        // A key's rows (a followed unit's, a header's queue history) all
        // close together and it can have rows again later; once a prune step
        // deleted its earlier rows, the rows left are not its whole history,
        // and nothing held them first.
        if ((await prunedRows(tx, key)) && (await unheld()))
          return refused(
            "beyond_retention",
            `a prune step has deleted earlier history of ${unit}`,
          );
        // Unpinned rows closed at or before the pruned slot sit in a
        // budget-cut prune step whose siblings may be gone: the same test as
        // a pin's (`partlyPrunedIn`).
        if (
          prunedThrough !== null &&
          (await unheld()) &&
          (await partlyPrunedIn(tx, prunedThrough, key, [history]))
        )
          return refused(
            "beyond_retention",
            `a prune step has deleted part of the history of ${unit}`,
          );
        return ok(rows);
      },
    );
    if (read.kind !== "ok") return read;
    const rows = read.value;
    return ok({
      checkpoint: point,
      transactions: rows.map((row) => ({
        txHash: hex(row.tx_hash),
        inclusionPoint: rawPointOf({
          slot: Number(row.from_slot),
          hash: Buffer.from(row.block_hash as Uint8Array),
          height: Number(row.block_height),
        }),
      })),
    });
  };

  const transactionInclusion: FollowerRawReads["transactionInclusion"] = async (
    txHash,
    landedNoEarlierThanSlot,
  ) => {
    const stored = await store.txByHash(Buffer.from(txHash, "hex"));
    if (stored === null) {
      const pruned = await prunedSinceOrigin(store);
      if (
        pruned !== null &&
        (landedNoEarlierThanSlot === undefined ||
          landedNoEarlierThanSlot <= pruned)
      )
        return refused(
          "beyond_retention",
          `${txHash} is not stored and may have landed at or below slot ${pruned.toString()}`,
        );
      return ok(null);
    }
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    if (block === null || block.slot !== stored.blockSlot)
      return refused(
        "beyond_retention",
        `the block holding ${txHash} is not stored`,
      );
    return ok(rawPointOf(block));
  };

  /** Inputs from stored bodies or ingest-resolved bytes, then the ledger at the predecessor, then a reason. */
  const resolveInputs = async (
    stored: StoredTx,
    block: StoredBlock,
    outRefs: readonly OutRef[],
    prunedThroughSlot: number,
  ): Promise<{
    resolved: FraudProofRawL1Utxo[];
    unresolved: FollowerUnresolvedInput[];
  }> => {
    const fromBodies = await store.transaction("read", async (tx) => {
      const utxos: (FraudProofRawL1Utxo | null)[] = [];
      for (const outRef of outRefs)
        utxos.push(
          (await resolveRawUtxoIn(tx, outRef)) ??
            (await resolveStoredInputIn(tx, outRef)),
        );
      return utxos;
    });
    const missing = outRefs.filter((_, i) => fromBodies[i] === null);
    const parent =
      missing.length === 0 ||
      block.parentHash === null ||
      options.ledgerOutputsAt === undefined
        ? null
        : await store.blockByHash(block.parentHash);
    const fromLedger =
      parent === null || options.ledgerOutputsAt === undefined
        ? null
        : await options
            .ledgerOutputsAt({ slot: parent.slot, hash: parent.hash }, missing)
            .catch(() => null);
    const resolved: FraudProofRawL1Utxo[] = [];
    const unresolved: FollowerUnresolvedInput[] = [];
    for (const [i, outRef] of outRefs.entries()) {
      const label = outRefLabel(outRef);
      const utxo = fromBodies[i] ?? fromLedger?.get(label) ?? null;
      if (utxo !== null) {
        resolved.push(utxo);
        continue;
      }
      const row = await store.output(outRef);
      unresolved.push({
        outRef: label,
        reason:
          row !== null && row.created === null
            ? "l1_input_before_origin"
            : stored.blockSlot <= prunedThroughSlot
              ? "beyond_retention"
              : "l1_input_unresolved",
      });
    }
    return { resolved, unresolved };
  };

  const rawTransaction: FollowerRawReads["rawTransaction"] = async (
    txHash,
    expectedInclusionPoint,
  ) => {
    const cursor = await store.cursor();
    if (cursor === null) return refused("not_initialized", "no cursor");
    const stored = await store.txByHash(Buffer.from(txHash, "hex"));
    if (stored === null)
      return Number(expectedInclusionPoint.slot) <= cursor.prunedThroughSlot
        ? refused(
            "beyond_retention",
            `${txHash} is not stored and its point is in the pruned range`,
          )
        : refused("not_stored", `${txHash} is not stored`);
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    if (block === null || block.slot !== stored.blockSlot)
      return refused(
        "beyond_retention",
        `the block holding ${txHash} is not stored`,
      );
    if (!samePoint(rawPointOf(block), expectedInclusionPoint))
      return refused(
        "not_at_point",
        `${txHash} is stored at another point than expected`,
      );
    if (!stored.isValid)
      return refused("phase2_invalid", `${txHash} failed phase 2`);
    const witness = CML.TransactionWitnessSet.from_cbor_bytes(
      stored.witnessCbor,
    );
    const redeemersCbor = witness.redeemers()?.to_canonical_cbor_hex() ?? null;
    witness.free();
    const inputs = await resolveInputs(
      stored,
      block,
      stored.inputs,
      cursor.prunedThroughSlot,
    );
    const references = await resolveInputs(
      stored,
      block,
      stored.referenceInputs,
      cursor.prunedThroughSlot,
    );
    return ok({
      transaction: {
        txHash,
        bodyCbor: stored.bodyCbor.toString("hex"),
        witnessSetCbor: stored.witnessCbor.toString("hex"),
        redeemersCbor,
        isValid: true,
        inclusionPoint: expectedInclusionPoint,
        confirmationDepth: depth(cursor.height, block.height),
        resolvedInputs: inputs.resolved,
        resolvedReferenceInputs: references.resolved,
      },
      unresolvedInputs: inputs.unresolved,
      unresolvedReferenceInputs: references.unresolved,
    });
  };

  const predecessorPoint: FollowerRawReads["predecessorPoint"] = async (
    point,
  ) => {
    const block = await canonicalBlock(store, point);
    if (block.kind !== "ok") return block;
    if (block.value.parentHash === null)
      return refused("beyond_retention", "the block has no stored parent");
    const parent = await store.blockByHash(block.value.parentHash);
    if (parent === null)
      return refused("beyond_retention", "the predecessor block is not stored");
    return ok(rawPointOf(parent));
  };

  return {
    addressUtxosAtPoint,
    utxosByOutRefAtPoint,
    unitHistoryAtPoint,
    transactionInclusion,
    rawTransaction,
    predecessorPoint,
  };
};
