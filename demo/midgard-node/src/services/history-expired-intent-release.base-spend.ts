import { SqlClient } from "@effect/sql";
import { Effect, Schema } from "effect";
import JSONBig from "json-bigint";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { type EventHistorySourceBinding } from "../l1-event-history-source.js";
import { signedIntentReplacementDigest } from "./canonical-journal-recovery.js";
import { ROOT_TAIL_HEADER_HASH } from "./state-queue-correction-rewind.admitted-removals.js";

/**
 * Evidence that a signed commit can no longer land before its TTL, by the
 * owner ruling that an intent is replaced once it cannot land on the current
 * chain: the observed head is past its TTL, or its base output D is already
 * spent by something else.
 *
 * Every incarnation of a non-root base node is one chain of spends (a DA
 * attestation spends D's node in place, keeping its header), so every commit
 * built on any of them is mutually exclusive with every other: this node's
 * journals on the same base output, or on the same non-root base header and
 * base ledger root, are its siblings. Root incarnations are not one chain, so
 * a root base matches by output only.
 *
 * The journaled canonical history (complete rosters of the owner's canonical
 * block applications, to the checkpoint head) shows D's output spent by a
 * valid transaction other than the signed commit, or the signed commit of a
 * sibling abandoned for replacement included. Either means the signed commit
 * cannot land unless a rollback takes that block back. A rollback that brings
 * a sibling back is the same evidence the other way: its commit is then
 * included, and the sibling is revived. A journal abandoned by a correction
 * (or unattributed) is no sibling: it may have landed on an earlier
 * incarnation of D and been removed, which leaves D's current output
 * spendable.
 *
 * Read by output reference and transaction hash over SQL only, with no ledger
 * capture and without the receipt-digest and contiguity checks of the
 * canonical-coverage loader. That is safe because this evidence only opens
 * the recovery gate: the decision itself is taken from the authenticated
 * exact-point queue, and before the TTL an intent whose base output that
 * queue still holds is never replaced (`heldBaseOutput`), whatever a receipt
 * says.
 */

const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const hash = Schema.String.pipe(Schema.pattern(/^[0-9a-f]{64}$/u));
const receiptSchema = Schema.Struct({
  bindingDigest: hash,
  block: Schema.Struct({
    transactions: Schema.Array(
      Schema.Struct({
        txHash: hash,
        spends: Schema.Literal("inputs", "collaterals"),
        inputs: Schema.Array(
          Schema.Struct({ txHash: hash, outputIndex: Schema.Unknown }),
        ),
      }),
    ),
  }),
});

/** The base a journal was built on. */
export type JournalBase = Readonly<{
  outRef: string;
  headerHash: Buffer;
  utxosRoot: string;
}>;

export const journalBase = (record: Pending.Record): JournalBase => ({
  outRef: record[Pending.Columns.BASE_TAIL_OUT_REF],
  headerHash: record[Pending.Columns.BASE_TAIL_HEADER_HASH],
  utxosRoot: record[Pending.Columns.BASE_UTXOS_ROOT],
});

/** This node's journals other than `excluded` built on the same base (see
 * the module comment), oldest first. */
export const sameBaseJournals = (
  base: JournalBase,
  excluded: readonly Buffer[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<{
      header_hash: Buffer;
      status: Pending.Status;
      intended_tx_hash: Buffer | null;
    }>`SELECT header_hash, status, intended_tx_hash
      FROM pending_block_finalizations
      WHERE (base_tail_out_ref = ${base.outRef}
          OR (${!base.headerHash.equals(ROOT_TAIL_HEADER_HASH)}
            AND base_tail_header_hash = ${base.headerHash}
            AND base_utxos_root = ${base.utxosRoot}))
        AND header_hash NOT IN ${sql.in(excluded)}
      ORDER BY created_at, header_hash`;
  });

/** This node's journals on the same base (see `sameBaseJournals`) that were
 * abandoned for replacement, oldest first. SQL only. */
export const replacedSameBaseJournals = (
  base: JournalBase,
  excluded: readonly Buffer[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const abandoned = (yield* sameBaseJournals(base, excluded)).flatMap(
      (row) =>
        row.status === Pending.Status.Abandoned ? [row.header_hash] : [],
    );
    if (abandoned.length === 0) return [];
    const rows = yield* sql<{
      header_hash: Buffer;
      intended_tx_hash: Buffer | null;
      signed_tx_cbor: Buffer | null;
      correction_transition_digest: string | null;
    }>`SELECT header_hash, intended_tx_hash, signed_tx_cbor,
        correction_transition_digest
      FROM pending_block_finalizations
      WHERE header_hash IN ${sql.in(abandoned)}
      ORDER BY created_at, header_hash`;
    // As `journalAbandonment`: abandoned under its own replacement digest.
    return rows.filter(
      (row) =>
        row.correction_transition_digest !== null &&
        row.correction_transition_digest === signedIntentReplacementDigest(row),
    );
  });

/** Canonical evidence that the signed commit cannot land: `spent`, a valid
 * transaction other than it spent its base output; `sibling`, the signed
 * commit of an abandoned sibling (which spends the same node) is included. */
export type BaseSpend = Readonly<{
  kind: "spent" | "sibling";
  txHash: string;
  height: number;
}>;

export const describeBaseSpend = (spend: BaseSpend, outRef: string) =>
  spend.kind === "spent"
    ? `its base output ${outRef} was spent by transaction ${spend.txHash} (source height ${spend.height.toString()})`
    : `the signed commit ${spend.txHash} of this node's replaced block on the same base is in the journaled canonical history (source height ${spend.height.toString()})`;

/** The first evidence in `receipts` (ordered by height), for a signed commit
 * `intended` on base output `outRef` with sibling commits `siblings`, other
 * than evidence by a transaction in `declined` (already declined before the
 * TTL). A receipt that does not decode is no evidence. */
export const findBaseSpend = (
  receipts: readonly Readonly<{ height: number; receipt: string }>[],
  binding: string,
  outRef: string,
  intended: string,
  siblings: ReadonlySet<string>,
  declined: ReadonlySet<string> = new Set(),
): BaseSpend | undefined => {
  const [outTx, outIndex] = outRef.split("#");
  for (const { height, receipt } of receipts) {
    let decoded: typeof receiptSchema.Type;
    try {
      decoded = Schema.decodeUnknownSync(receiptSchema)(
        lossless.parse(receipt),
      );
    } catch {
      continue;
    }
    if (decoded.bindingDigest !== binding) continue;
    for (const tx of decoded.block.transactions) {
      if (
        tx.spends !== "inputs" ||
        tx.txHash === intended ||
        declined.has(tx.txHash)
      )
        continue;
      if (siblings.has(tx.txHash))
        return { kind: "sibling", txHash: tx.txHash, height };
      if (
        tx.inputs.some(
          (input) =>
            input.txHash === outTx && String(input.outputIndex) === outIndex,
        )
      )
        return { kind: "spent", txHash: tx.txHash, height };
    }
  }
  return undefined;
};

/** Scans the canonical applications in `(fromHeight, toHeight]` for evidence
 * that the signed commit `intended` of a journal on `base` cannot land. Only
 * receipts naming the base output's transaction or a sibling's signed commit
 * are read. A database failure fails the caller's transaction. */
export const scanBaseSpend = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly headerHash: Buffer;
  readonly intendedTxHash: Buffer;
  readonly base: JournalBase;
  readonly fromHeight: number;
  readonly toHeight: number;
  readonly declined?: ReadonlySet<string>;
}) =>
  Effect.gen(function* () {
    if (input.toHeight <= input.fromHeight) return undefined;
    const sql = yield* SqlClient.SqlClient;
    const siblings = new Set(
      (yield* replacedSameBaseJournals(input.base, [input.headerHash])).flatMap(
        (row) =>
          row.intended_tx_hash === null
            ? []
            : [row.intended_tx_hash.toString("hex")],
      ),
    );
    const outTx = input.base.outRef.split("#")[0] ?? "";
    // Hex only: every alternative is a 64-digit transaction hash.
    const pattern = [outTx, ...siblings]
      .filter((value) => /^[0-9a-f]{64}$/u.test(value))
      .join("|");
    if (pattern === "") return undefined;
    const rows = yield* sql<{ height: string; receipt: string }>`SELECT
        block_height::text AS height, ledger_receipt AS receipt
      FROM event_history_block_applications
      WHERE binding_digest = ${Buffer.from(input.binding.digest, "hex")}
        AND canonical
        AND block_height > ${input.fromHeight}
        AND block_height <= ${input.toHeight}
        AND ledger_receipt ~ ${pattern}
      ORDER BY block_height`;
    const found = findBaseSpend(
      rows.map((row) => ({ height: Number(row.height), receipt: row.receipt })),
      input.binding.digest,
      input.base.outRef,
      input.intendedTxHash.toString("hex"),
      siblings,
      input.declined,
    );
    if (rows.length > 0 && found === undefined)
      yield* Effect.logDebug(
        `No base-spend evidence for signed commit ${input.intendedTxHash.toString("hex")} in ${rows.length.toString()} canonical receipt(s) naming its base output or a sibling commit`,
      );
    return found;
  });
