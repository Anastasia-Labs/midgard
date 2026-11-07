import { assetsFromJson, assetsToJson, decodeOutRef } from "../codec.js";
import {
  asBuffer,
  asNullableBuffer,
  asNullableNumber,
  asNumber,
  asString,
  type Dialect,
  type SqlRow,
  type SqlTx,
} from "../sql/backend.js";
import type { Assets, OutputSummary, OutRef, View } from "../types.js";
import type { IntentEventKind } from "./schema.js";

/** A predicted own-wallet output of an intent (§8.5 reads them). */
export type OwnOutput = Readonly<{
  index: number;
  address: Buffer;
  lovelace: bigint;
  assets: Assets;
}>;

/**
 * One `l1_intents` row without its signed bytes: everything a status
 * derivation and a family predicate read. Only a resubmission needs the
 * bytes (`readIntentCborIn`).
 */
export type IntentHead = Readonly<{
  txHash: Buffer;
  family: string;
  workflowKey: string;
  inputs: readonly OutRef[];
  referenceInputs: readonly OutRef[];
  collaterals: readonly OutRef[];
  ownOutputs: readonly OwnOutput[];
  /** The body's `invalid_before` (inclusive). */
  validFromSlot: number | null;
  /** The body's `invalid_hereafter` (exclusive). */
  validToSlot: number | null;
  /** Recorded intents whose outputs this one spends, references or uses as collateral. */
  dependsOn: readonly Buffer[];
  /** The view the planner built under (§8.1). */
  built: View;
  /** Class B/C content the tx commits to (an own block's header hash). */
  contentRef: Buffer | null;
}>;

/** One `l1_intents` row. */
export type Intent = IntentHead &
  Readonly<{
    /** The signed bytes, exactly as first submitted. */
    txCbor: Buffer;
  }>;

export type IntentEvent = Readonly<{
  txHash: Buffer;
  seq: number;
  kind: IntentEventKind;
  detail: unknown;
  tipSlot: number | null;
}>;

export type RecordIntentInput = Readonly<{
  family: string;
  workflowKey: string;
  /** The whole signed transaction `[body, witnesses, isValid, auxiliary]`. */
  txCbor: Buffer;
  /** Whether a body output pays one of the role's own wallets. */
  isOwnOutput: (output: OutputSummary) => boolean;
  contentRef?: Buffer | null;
  /**
   * The planner's view V = (g, P_b) (§8.1): the generation it planned in and
   * a cursor point at or above every fact it read. Required: no record skips
   * the view. `viewValid(V)` runs in this transaction under the cursor share
   * lock (SQLite: the writer's `BEGIN IMMEDIATE`). A stale view still
   * records the row (class B never erases signed bytes), with a
   * `stale_at_write` event: nothing may send it on the strength of this
   * write, and S6 decides under the current view whether it can still land
   * and is still wanted.
   */
  builtAt: View;
  /**
   * Set when the caller already knows its plan is stale (a rewind landed
   * while it planned, so no point bounds what it read): recorded with
   * `stale_at_write` and this detail, whatever `viewValid(builtAt)` says.
   */
  staleBecause?: string;
}>;

export type RecordIntentResult =
  /**
   * Journaled. `stale`: the planner's view was not valid in the record
   * transaction (or the caller said so), and a `stale_at_write` event was
   * appended with the `signed` one.
   */
  | Readonly<{ kind: "recorded"; intent: Intent; stale: boolean }>
  /**
   * Already journaled: the first bytes stand and are what any resubmission
   * sends. `identical` is false when these bytes differ (another witness set
   * over the same body).
   */
  | Readonly<{ kind: "already_recorded"; intent: Intent; identical: boolean }>
  /**
   * §8.2's invariant: every input, reference input and collateral is a
   * tracked fact row or an output of a recorded intent, so reconciliation
   * can always decide from the facts. Refused, never recorded.
   */
  | Readonly<{ kind: "input_untracked"; txHash: Buffer; untracked: OutRef[] }>
  | Readonly<{ kind: "undecodable"; detail: string }>
  /** The follower has no cursor yet: there is no view to build under. */
  | Readonly<{ kind: "no_view"; txHash: Buffer }>;

export const INTENT_COLUMNS =
  "tx_hash, family, workflow_key, tx_cbor, inputs, reference_inputs, collaterals, own_outputs, valid_from_slot, valid_to_slot, depends_on, built_generation, built_slot, built_hash, content_ref";

/** Every column but `tx_cbor`: a status read never loads the signed bytes. */
const HEAD_COLUMNS =
  "tx_hash, family, workflow_key, inputs, reference_inputs, collaterals, own_outputs, valid_from_slot, valid_to_slot, depends_on, built_generation, built_slot, built_hash, content_ref";

/** Postgres and SQLite bind at most this many parameters comfortably. */
const IN_CHUNK = 500;

const parseJson = (value: unknown): unknown =>
  typeof value === "string" ? (JSON.parse(value) as unknown) : value;

export const ownOutputsToJson = (outputs: readonly OwnOutput[]): string =>
  JSON.stringify(
    outputs.map((output) => ({
      index: output.index,
      address: output.address.toString("hex"),
      lovelace: output.lovelace.toString(),
      assets: JSON.parse(assetsToJson(output.assets)) as unknown,
    })),
  );

const ownOutputsFromJson = (value: unknown): OwnOutput[] => {
  const parsed = parseJson(value);
  if (!Array.isArray(parsed)) throw new Error("own_outputs must be an array");
  return parsed.map((item: unknown) => {
    const entry = item as {
      index: number;
      address: string;
      lovelace: string;
      assets: unknown;
    };
    return {
      index: entry.index,
      address: Buffer.from(entry.address, "hex"),
      lovelace: BigInt(entry.lovelace),
      assets: assetsFromJson(entry.assets),
    };
  });
};

export const intentHeadFromRow = (
  dialect: Dialect,
  row: SqlRow,
): IntentHead => ({
  txHash: asBuffer(row.tx_hash),
  family: asString(row.family),
  workflowKey: asString(row.workflow_key),
  inputs: dialect.readOutRefList(row.inputs).map(decodeOutRef),
  referenceInputs: dialect
    .readOutRefList(row.reference_inputs)
    .map(decodeOutRef),
  collaterals: dialect.readOutRefList(row.collaterals).map(decodeOutRef),
  ownOutputs: ownOutputsFromJson(row.own_outputs),
  validFromSlot: asNullableNumber(row.valid_from_slot),
  validToSlot: asNullableNumber(row.valid_to_slot),
  dependsOn: dialect.readOutRefList(row.depends_on),
  built: {
    generation: asNumber(row.built_generation),
    point: { slot: asNumber(row.built_slot), hash: asBuffer(row.built_hash) },
    // The view's height is not journaled; the point identifies it.
    height: 0,
  },
  contentRef: asNullableBuffer(row.content_ref),
});

export const intentFromRow = (dialect: Dialect, row: SqlRow): Intent => ({
  ...intentHeadFromRow(dialect, row),
  txCbor: asBuffer(row.tx_cbor),
});

export const chunks = <T>(items: readonly T[]): T[][] => {
  const out: T[][] = [];
  for (let start = 0; start < items.length; start += IN_CHUNK)
    out.push(items.slice(start, start + IN_CHUNK));
  return out;
};

export const placeholders = (count: number): string =>
  Array.from({ length: count }, () => "?").join(", ");

export const distinctHashes = (hashes: readonly Buffer[]): Buffer[] => [
  ...new Map(hashes.map((hash) => [hash.toString("hex"), hash])).values(),
];

/** Every intent, or the ones named, oldest key first (by tx hash). */
export const readIntentsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  txHashes?: readonly Buffer[],
): Promise<Intent[]> => {
  if (txHashes === undefined)
    return (
      await tx.query(
        `SELECT ${INTENT_COLUMNS} FROM l1_intents ORDER BY tx_hash`,
      )
    ).map((row) => intentFromRow(dialect, row));
  const intents: Intent[] = [];
  for (const chunk of chunks(distinctHashes(txHashes)))
    for (const row of await tx.query(
      `SELECT ${INTENT_COLUMNS} FROM l1_intents WHERE tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    ))
      intents.push(intentFromRow(dialect, row));
  return intents.sort((a, b) => Buffer.compare(a.txHash, b.txHash));
};

/**
 * Every intent's head, or the named ones' (by tx hash), without the signed
 * bytes: the primary-key index serves the named form.
 */
export const readIntentHeadsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  txHashes?: readonly Buffer[],
): Promise<IntentHead[]> => {
  if (txHashes === undefined)
    return (
      await tx.query(`SELECT ${HEAD_COLUMNS} FROM l1_intents ORDER BY tx_hash`)
    ).map((row) => intentHeadFromRow(dialect, row));
  const heads: IntentHead[] = [];
  for (const chunk of chunks(distinctHashes(txHashes)))
    for (const row of await tx.query(
      `SELECT ${HEAD_COLUMNS} FROM l1_intents WHERE tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    ))
      heads.push(intentHeadFromRow(dialect, row));
  return heads.sort((a, b) => Buffer.compare(a.txHash, b.txHash));
};

/** Which of `txHashes` are journaled (primary-key probes). */
export const journaledHashesIn = async (
  tx: SqlTx,
  txHashes: readonly Buffer[],
): Promise<Set<string>> => {
  const found = new Set<string>();
  for (const chunk of chunks(distinctHashes(txHashes)))
    for (const row of await tx.query(
      `SELECT tx_hash FROM l1_intents WHERE tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    ))
      found.add(asBuffer(row.tx_hash).toString("hex"));
  return found;
};

/** One intent with its signed bytes, or null when it is not journaled. */
export const readIntentIn = async (
  tx: SqlTx,
  dialect: Dialect,
  txHash: Buffer,
): Promise<Intent | null> =>
  (await readIntentsIn(tx, dialect, [txHash]))[0] ?? null;

/** The intents journaled under one workflow key, oldest key first. */
export const readIntentsByWorkflowIn = async (
  tx: SqlTx,
  dialect: Dialect,
  workflowKey: string,
): Promise<Intent[]> =>
  (
    await tx.query(
      `SELECT ${INTENT_COLUMNS} FROM l1_intents WHERE workflow_key = ? ORDER BY tx_hash`,
      [workflowKey],
    )
  ).map((row) => intentFromRow(dialect, row));

/** The intents committing to one piece of content (an own block's header hash). */
export const readIntentsByContentIn = async (
  tx: SqlTx,
  dialect: Dialect,
  contentRef: Buffer,
): Promise<Intent[]> =>
  (
    await tx.query(
      `SELECT ${INTENT_COLUMNS} FROM l1_intents WHERE content_ref = ? ORDER BY tx_hash`,
      [contentRef],
    )
  ).map((row) => intentFromRow(dialect, row));

const eventFromRow = (row: SqlRow): IntentEvent => ({
  txHash: asBuffer(row.tx_hash),
  seq: asNumber(row.seq),
  kind: asString(row.kind) as IntentEventKind,
  detail:
    row.detail === null || row.detail === undefined
      ? null
      : parseJson(row.detail),
  tipSlot: asNullableNumber(row.tip_slot),
});

/** One intent's events in order, or every intent's (by tx hash, then seq). */
export const readIntentEventsIn = async (
  tx: SqlTx,
  txHash?: Buffer,
): Promise<IntentEvent[]> =>
  (txHash === undefined
    ? await tx.query(
        "SELECT tx_hash, seq, kind, detail, tip_slot FROM l1_intent_events ORDER BY tx_hash, seq",
      )
    : await tx.query(
        "SELECT tx_hash, seq, kind, detail, tip_slot FROM l1_intent_events WHERE tx_hash = ? ORDER BY seq",
        [txHash],
      )
  ).map(eventFromRow);

/**
 * Appends one event to an intent's log (append-only, class B). Returns the
 * new sequence number, or null when no such intent is journaled.
 */
export const appendIntentEventIn = async (
  tx: SqlTx,
  dialect: Dialect,
  txHash: Buffer,
  kind: IntentEventKind,
  options: Readonly<{ detail?: unknown; tipSlot: number | null }>,
): Promise<number | null> => {
  const owner = await tx.query(
    `SELECT tx_hash FROM l1_intents WHERE tx_hash = ?${dialect.lockClause("update")}`,
    [txHash],
  );
  if (owner.length === 0) return null;
  const last = await tx.query(
    "SELECT max(seq) AS seq FROM l1_intent_events WHERE tx_hash = ?",
    [txHash],
  );
  const seq = (asNullableNumber(last[0]?.seq) ?? -1) + 1;
  await tx.query(
    "INSERT INTO l1_intent_events (tx_hash, seq, kind, detail, tip_slot) VALUES (?, ?, ?, ?, ?)",
    [
      txHash,
      seq,
      kind,
      options.detail === undefined || options.detail === null
        ? null
        : dialect.json(JSON.stringify(options.detail)),
      options.tipSlot,
    ],
  );
  return seq;
};
