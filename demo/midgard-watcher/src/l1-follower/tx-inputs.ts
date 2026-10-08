import type { FraudProofRawL1Utxo } from "@al-ft/midgard-fault-proofs";
import type { FactStore, OutRef, SqlTx } from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import type { LedgerOutputsQuery } from "./raw-reads.ledger.js";
import { outRefLabel, rawUtxo, resolveRawUtxoIn } from "./reads.js";
import {
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_TX_INPUTS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";

/**
 * The resolved inputs of every tx a unit history records (E1 ruling, facet
 * 2). A proof reads such a tx with every input and reference input
 * resolved; an input whose creating tx the follower never stored (an
 * operator's untracked UTxO) resolves only from the node's ledger state at
 * the inclusion block's predecessor, which the node serves only while that
 * point is within k of its tip. So each such tx's inputs are resolved once,
 * at ingest (stored bodies first, then the ledger at the predecessor), and
 * stored as exact output bytes per outref: a later proof never needs the
 * ledger once the predecessor is more than k deep.
 *
 * An input that cannot be resolved yet (node down, request error) holds the
 * named readiness reason `l1_tx_inputs_unresolved` and is retried with
 * backoff. One that can never resolve (beyond the node's window, absent from
 * its ledger state, or no stored predecessor) is the named non-blocking
 * degradation `l1_tx_inputs_unresolvable` in status and metrics; it fails
 * readiness, under the same name, only while a proof pin holds a header
 * whose history records the tx, since only then an acted-on objective cannot
 * complete. Releasing the pin, or pruning the tx's history, clears it with no
 * restart. A proof read that needs such an input still refuses with
 * `l1_input_unresolved`. Nothing is skipped silently and nothing here throws
 * or exits.
 */

export const L1_TX_INPUTS_UNRESOLVED = "l1_tx_inputs_unresolved";
export const L1_TX_INPUTS_UNRESOLVABLE = "l1_tx_inputs_unresolvable";

/** A named condition reported in status and metrics that never fails readiness. */
export type WatcherL1Degradation = Readonly<{
  reason: string;
  count: number;
  detail: string;
}>;

export type TxInputsAssessment = Readonly<{
  readiness: readonly Readonly<{ reason: string; detail: string }>[];
  degradations: readonly WatcherL1Degradation[];
}>;

/** A stored resolved input's exact bytes, or null. */
export const resolveStoredInputIn = async (
  tx: SqlTx,
  outRef: OutRef,
): Promise<FraudProofRawL1Utxo | null> => {
  const row = (
    await tx.query(
      `SELECT output_cbor FROM ${WATCHER_TX_INPUTS_TABLE} WHERE out_tx_hash = ? AND out_index = ? LIMIT 1`,
      [outRef.txHash, outRef.index],
    )
  )[0];
  if (row === undefined) return null;
  const output = CML.TransactionOutput.from_cbor_bytes(
    Buffer.from(row.output_cbor as Uint8Array),
  );
  try {
    return rawUtxo(outRefLabel(outRef), output);
  } finally {
    output.free();
  }
};

export type UnresolvedTxInputs = Readonly<{
  txHash: string;
  cause:
    | "too_old"
    | "absent_at_parent"
    | "no_parent"
    | "not_on_chain"
    | "unavailable"
    | "store_error";
  detail: string;
  /** Never resolvable: reported until the tx's history is pruned. */
  permanent: boolean;
}>;

export type TxInputsResolver = Readonly<{
  /** One pass over the unresolved txs; never throws. */
  step(): Promise<readonly UnresolvedTxInputs[]>;
  /** Runs a pass now, or once more after the running one. */
  trigger(): void;
  /** The unresolved txs of the last pass. */
  unresolved(): readonly UnresolvedTxInputs[];
  /**
   * The last pass's unresolved txs as readiness reasons and degradations,
   * split by whether a proof pin holds them now (read at call time, so a
   * release clears the reason with no pass). Throws on a store error.
   */
  assess(): Promise<TxInputsAssessment>;
  close(): Promise<void>;
}>;

const PENDING_SQL = `SELECT h.tx_hash AS tx_hash FROM (
  SELECT tx_hash FROM ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}
  UNION SELECT tx_hash FROM ${WATCHER_UNIT_HISTORY_TABLE}
) h JOIN l1_txs x ON x.tx_hash = h.tx_hash
WHERE NOT EXISTS (SELECT 1 FROM ${WATCHER_TX_INPUTS_TABLE} i WHERE i.tx_hash = h.tx_hash)
ORDER BY x.block_slot, x.block_tx_index`;

const SWEEP_SQL = `DELETE FROM ${WATCHER_TX_INPUTS_TABLE} WHERE NOT EXISTS (SELECT 1 FROM l1_txs x WHERE x.tx_hash = ${WATCHER_TX_INPUTS_TABLE}.tx_hash)`;

/** The txs a proof pin holds: the pinned headers' and held units' histories. */
const PIN_HELD_SQL = `SELECT q.tx_hash AS tx_hash FROM ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} q
  JOIN ${WATCHER_PROOF_PINS_TABLE} p ON p.header_hash = q.header_hash
UNION SELECT u.tx_hash AS tx_hash FROM ${WATCHER_UNIT_HISTORY_TABLE} u
  JOIN ${WATCHER_PROOF_PIN_UNITS_TABLE} pu ON pu.unit = u.unit`;

const BACKOFF_MIN_MS = 500;
const BACKOFF_MAX_MS = 30_000;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const distinct = (outRefs: readonly OutRef[]): OutRef[] => {
  const seen = new Map<string, OutRef>();
  for (const outRef of outRefs) seen.set(outRefLabel(outRef), outRef);
  return [...seen.values()];
};

export const createTxInputsResolver = (
  input: Readonly<{
    store: FactStore;
    ledger: LedgerOutputsQuery;
    log?: (line: string) => void;
  }>,
): TxInputsResolver => {
  const { store } = input;
  /** Txs that can never resolve, by hash; dropped once no longer pending. */
  const permanent = new Map<string, UnresolvedTxInputs>();
  let last: readonly UnresolvedTxInputs[] = [];
  let lastSweep: string | null = null;
  let running: Promise<readonly UnresolvedTxInputs[]> | null = null;
  let again = false;
  let closed = false;
  let timer: ReturnType<typeof setTimeout> | null = null;
  let backoffMs = BACKOFF_MIN_MS;

  /** The tx's inputs as exact bytes, or why not. */
  const resolveTx = async (
    txHash: Buffer,
  ): Promise<
    | Readonly<{ kind: "resolved"; utxos: FraudProofRawL1Utxo[] }>
    | Readonly<{ kind: "gone" }>
    | Readonly<{ kind: "unresolved"; entry: UnresolvedTxInputs }>
  > => {
    const label = txHash.toString("hex");
    const unresolved = (
      cause: UnresolvedTxInputs["cause"],
      detail: string,
      isPermanent: boolean,
    ) =>
      ({
        kind: "unresolved",
        entry: { txHash: label, cause, detail, permanent: isPermanent },
      }) as const;
    const stored = await store.txByHash(txHash);
    if (stored === null) return { kind: "gone" };
    const outRefs = distinct([...stored.inputs, ...stored.referenceInputs]);
    const known = await store.transaction("read", async (tx) => {
      const utxos: (FraudProofRawL1Utxo | null)[] = [];
      for (const outRef of outRefs)
        utxos.push(
          (await resolveRawUtxoIn(tx, outRef)) ??
            (await resolveStoredInputIn(tx, outRef)),
        );
      return utxos;
    });
    const missing = outRefs.filter((_, i) => known[i] === null);
    if (missing.length === 0)
      return { kind: "resolved", utxos: known as FraudProofRawL1Utxo[] };
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    const parent =
      block === null ||
      block.slot !== stored.blockSlot ||
      block.parentHash === null
        ? null
        : await store.blockByHash(block.parentHash);
    if (parent === null)
      return unresolved(
        "no_parent",
        "the inclusion block's predecessor is not stored",
        true,
      );
    const answer = await input.ledger(
      { slot: parent.slot, hash: parent.hash },
      missing,
    );
    if (answer.kind !== "ok")
      return unresolved(answer.kind, answer.detail, answer.kind === "too_old");
    const absent = missing.filter(
      (outRef) => !answer.outputs.has(outRefLabel(outRef)),
    );
    if (absent.length > 0)
      return unresolved(
        "absent_at_parent",
        `${absent.map(outRefLabel).join(", ")} not in the ledger state at slot ${parent.slot.toString()}`,
        true,
      );
    return {
      kind: "resolved",
      utxos: outRefs.map(
        (outRef, i) => known[i] ?? answer.outputs.get(outRefLabel(outRef))!,
      ),
    };
  };

  const pass = async (): Promise<readonly UnresolvedTxInputs[]> => {
    const cursor = await store.cursor();
    if (cursor === null) return [];
    const sweepKey = `${cursor.generation.toString()}:${cursor.prunedThroughSlot.toString()}`;
    // While a tracked-set reset replays from the origin, the txs whose
    // inputs are stored are not back in l1_txs yet, and an input more than
    // k deep could never be read again: the sweep waits for the first tip.
    if (
      sweepKey !== lastSweep &&
      (await store.trackedSetRecord())?.replaying !== true
    ) {
      await store.transaction("write", (tx) => tx.query(SWEEP_SQL));
      lastSweep = sweepKey;
    }
    const pending = (
      await store.transaction("read", (tx) => tx.query(PENDING_SQL))
    ).map((row) => Buffer.from(row.tx_hash as Uint8Array));
    const live = new Set(pending.map((hash) => hash.toString("hex")));
    for (const hash of [...permanent.keys()])
      if (!live.has(hash)) permanent.delete(hash);
    const unresolved: UnresolvedTxInputs[] = [];
    for (const txHash of pending) {
      if (closed) break;
      const known = permanent.get(txHash.toString("hex"));
      if (known !== undefined) {
        unresolved.push(known);
        continue;
      }
      const result = await resolveTx(txHash);
      if (result.kind === "gone") continue;
      if (result.kind === "unresolved") {
        if (result.entry.permanent)
          permanent.set(result.entry.txHash, result.entry);
        unresolved.push(result.entry);
        continue;
      }
      await store.transaction("write", async (tx) => {
        for (const utxo of result.utxos) {
          const [outTx, outIndex] = utxo.outRef.split("#") as [string, string];
          await tx.query(
            `INSERT INTO ${WATCHER_TX_INPUTS_TABLE} (tx_hash, out_tx_hash, out_index, output_cbor) VALUES (?, ?, ?, ?) ON CONFLICT DO NOTHING`,
            [
              txHash,
              Buffer.from(outTx, "hex"),
              Number(outIndex),
              Buffer.from(utxo.outputCbor, "hex"),
            ],
          );
        }
      });
    }
    return unresolved;
  };

  const schedule = (retry: boolean): void => {
    if (timer !== null) clearTimeout(timer);
    timer = null;
    if (!retry || closed) {
      backoffMs = BACKOFF_MIN_MS;
      return;
    }
    timer = setTimeout(() => {
      timer = null;
      trigger();
    }, backoffMs);
    timer.unref?.();
    backoffMs = Math.min(backoffMs * 2, BACKOFF_MAX_MS);
  };

  const step = (): Promise<readonly UnresolvedTxInputs[]> => {
    running ??= (async () => {
      try {
        last = await pass();
      } catch (error) {
        last = [
          {
            txHash: "",
            cause: "store_error",
            detail: message(error),
            permanent: false,
          },
        ];
        input.log?.(`tx input resolution failed: ${message(error)}`);
      } finally {
        running = null;
      }
      schedule(last.some((entry) => !entry.permanent));
      if (again && !closed) {
        again = false;
        trigger();
      }
      return last;
    })();
    return running;
  };

  const trigger = (): void => {
    if (closed) return;
    if (running !== null) {
      again = true;
      return;
    }
    void step();
  };

  return Object.freeze({
    step,
    trigger,
    unresolved: () => last,
    assess: async () => {
      const entries = last;
      const transient = entries.filter((entry) => !entry.permanent);
      const permanentEntries = entries.filter((entry) => entry.permanent);
      const pinned =
        permanentEntries.length === 0
          ? new Set<string>()
          : new Set(
              (
                await store.transaction("read", (tx) => tx.query(PIN_HELD_SQL))
              ).map((row) =>
                Buffer.from(row.tx_hash as Uint8Array).toString("hex"),
              ),
            );
      const held = permanentEntries.filter((entry) => pinned.has(entry.txHash));
      const unheld = permanentEntries.filter(
        (entry) => !pinned.has(entry.txHash),
      );
      const summary = (
        group: readonly UnresolvedTxInputs[],
        what: string,
        tail: string,
      ): string => {
        const first = group[0]!;
        return `${group.length.toString()} recorded tx(s) ${what}; first ${first.txHash || "(store)"}: ${first.cause}: ${first.detail} (${tail})`;
      };
      const readiness: { reason: string; detail: string }[] = [];
      if (transient.length > 0)
        readiness.push({
          reason: L1_TX_INPUTS_UNRESOLVED,
          detail: summary(transient, "hold unresolved inputs", "retrying"),
        });
      if (held.length > 0)
        readiness.push({
          reason: L1_TX_INPUTS_UNRESOLVABLE,
          detail: summary(
            held,
            "a proof pin holds have inputs that can never resolve",
            "the pinned proof cannot complete; clears when the pin releases or the tx's history is pruned",
          ),
        });
      const degradations: WatcherL1Degradation[] =
        unheld.length === 0
          ? []
          : [
              {
                reason: L1_TX_INPUTS_UNRESOLVABLE,
                count: unheld.length,
                detail: summary(
                  unheld,
                  "have inputs that can never resolve",
                  "no open proof needs them; a proof read needing one refuses; clears when the tx's history is pruned",
                ),
              },
            ];
      return { readiness, degradations };
    },
    close: async () => {
      closed = true;
      if (timer !== null) clearTimeout(timer);
      timer = null;
      await running?.catch(() => undefined);
    },
  });
};
