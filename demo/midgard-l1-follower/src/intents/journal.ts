import {
  assetsFromJson,
  assetsToJson,
  decodeOutRef,
  encodeOutRef,
} from "../codec.js";
import {
  type DecodedTransaction,
  decodeTransaction,
  TxDecodeError,
} from "../decode/tx.js";
import {
  asBuffer,
  asNullableBuffer,
  asNullableNumber,
  asNumber,
  asString,
  type Dialect,
  type SqlRow,
  type SqlTx,
  type SqlValue,
} from "../sql/backend.js";
import { readCursor } from "../store/rows.js";
import type { Assets, OutputSummary, OutRef, View } from "../types.js";
import type { IntentEventKind } from "./schema.js";

/** A predicted own-wallet output of an intent (§8.5 reads them). */
export type OwnOutput = Readonly<{
  index: number;
  address: Buffer;
  lovelace: bigint;
  assets: Assets;
}>;

/** One `l1_intents` row. */
export type Intent = Readonly<{
  txHash: Buffer;
  family: string;
  workflowKey: string;
  /** The signed bytes, exactly as first submitted. */
  txCbor: Buffer;
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
   * The view the planner built under. Defaults to the store's current view.
   * I5 checks it (`viewValid`) in this transaction and appends
   * `stale_at_write` when it no longer holds; this function only records it.
   */
  builtAt?: View;
}>;

export type RecordIntentResult =
  | Readonly<{ kind: "recorded"; intent: Intent }>
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

const INTENT_COLUMNS =
  "tx_hash, family, workflow_key, tx_cbor, inputs, reference_inputs, collaterals, own_outputs, valid_from_slot, valid_to_slot, depends_on, built_generation, built_slot, built_hash, content_ref";

/** Postgres and SQLite bind at most this many parameters comfortably. */
const IN_CHUNK = 500;

const parseJson = (value: unknown): unknown =>
  typeof value === "string" ? (JSON.parse(value) as unknown) : value;

const ownOutputsToJson = (outputs: readonly OwnOutput[]): string =>
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

export const intentFromRow = (dialect: Dialect, row: SqlRow): Intent => ({
  txHash: asBuffer(row.tx_hash),
  family: asString(row.family),
  workflowKey: asString(row.workflow_key),
  txCbor: asBuffer(row.tx_cbor),
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

const chunks = <T>(items: readonly T[]): T[][] => {
  const out: T[][] = [];
  for (let start = 0; start < items.length; start += IN_CHUNK)
    out.push(items.slice(start, start + IN_CHUNK));
  return out;
};

const placeholders = (count: number): string =>
  Array.from({ length: count }, () => "?").join(", ");

const distinctHashes = (hashes: readonly Buffer[]): Buffer[] => [
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

/** Which of `outRefs` have a fact row (spent or live). */
const factRowsIn = async (
  tx: SqlTx,
  outRefs: readonly OutRef[],
): Promise<Set<string>> => {
  const known = new Set<string>();
  const parents = distinctHashes(outRefs.map((outRef) => outRef.txHash));
  for (const chunk of chunks(parents))
    for (const row of await tx.query(
      `SELECT tx_hash, output_index FROM l1_outputs WHERE tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    ))
      known.add(
        encodeOutRef({
          txHash: asBuffer(row.tx_hash),
          index: asNumber(row.output_index),
        }).toString("hex"),
      );
  return known;
};

/** The output indexes a recorded transaction can create (its outputs, or a failed run's collateral return). */
const createdIndexes = (txCbor: Buffer): number => {
  const decoded = decodeTransaction(txCbor);
  return decoded.outputs.length + (decoded.collateralReturn === null ? 0 : 1);
};

/**
 * A decoded validity bound as a journal slot: null when absent, undefined
 * when past `Number.MAX_SAFE_INTEGER`, which the journal's slot columns and
 * status arithmetic do not carry.
 */
const slotBound = (bound: bigint | null): number | null | undefined =>
  bound === null
    ? null
    : bound <= BigInt(Number.MAX_SAFE_INTEGER)
      ? Number(bound)
      : undefined;

/**
 * S5: journals a newly signed transaction before its first submission
 * (§8.2), in the caller's write transaction. Idempotent per tx hash.
 */
export const recordIntentIn = async (
  tx: SqlTx,
  dialect: Dialect,
  input: RecordIntentInput,
): Promise<RecordIntentResult> => {
  let decoded: DecodedTransaction;
  try {
    decoded = decodeTransaction(input.txCbor);
  } catch (error) {
    if (error instanceof TxDecodeError)
      return { kind: "undecodable", detail: error.message };
    throw error;
  }
  if (decoded.bodyCbor.length === input.txCbor.length)
    return {
      kind: "undecodable",
      detail:
        "a bare transaction body carries no witnesses; journal the signed transaction",
    };
  const validFromSlot = slotBound(decoded.invalidBefore);
  const validToSlot = slotBound(decoded.invalidAfter);
  if (validFromSlot === undefined || validToSlot === undefined)
    return {
      kind: "undecodable",
      detail:
        "a validity bound is past the journal's slot range (Number.MAX_SAFE_INTEGER)",
    };
  const txHash = decoded.hash;
  const existing = await readIntentsIn(tx, dialect, [txHash]);
  const first = existing[0];
  if (first !== undefined)
    return {
      kind: "already_recorded",
      intent: first,
      identical: first.txCbor.equals(input.txCbor),
    };
  const cursor = await readCursor(tx, dialect);
  const view: View | null =
    input.builtAt ??
    (cursor === null
      ? null
      : {
          generation: cursor.generation,
          point: cursor.point,
          height: cursor.height,
        });
  if (view === null) return { kind: "no_view", txHash };
  const spends = [
    ...decoded.inputs,
    ...decoded.referenceInputs,
    ...decoded.collaterals,
  ];
  const facts = await factRowsIn(tx, spends);
  const missing = spends.filter(
    (outRef) => !facts.has(encodeOutRef(outRef).toString("hex")),
  );
  const parents = new Map(
    (
      await readIntentsIn(
        tx,
        dialect,
        spends.map((outRef) => outRef.txHash),
      )
    ).map((intent) => [intent.txHash.toString("hex"), intent]),
  );
  const untracked = missing.filter((outRef) => {
    const parent = parents.get(outRef.txHash.toString("hex"));
    return (
      parent === undefined || outRef.index >= createdIndexes(parent.txCbor)
    );
  });
  if (untracked.length > 0)
    return { kind: "input_untracked", txHash, untracked };
  const dependsOn = distinctHashes(
    spends
      .map((outRef) => outRef.txHash)
      .filter((hash) => parents.has(hash.toString("hex"))),
  ).sort(Buffer.compare);
  const ownOutputs: OwnOutput[] = decoded.outputs.flatMap((output, index) =>
    input.isOwnOutput(output)
      ? [
          {
            index,
            address: output.address,
            lovelace: output.lovelace,
            assets: output.assets,
          },
        ]
      : [],
  );
  const intent: Intent = {
    txHash,
    family: input.family,
    workflowKey: input.workflowKey,
    txCbor: input.txCbor,
    inputs: decoded.inputs,
    referenceInputs: decoded.referenceInputs,
    collaterals: decoded.collaterals,
    ownOutputs,
    validFromSlot,
    validToSlot,
    dependsOn,
    built: view,
    contentRef: input.contentRef ?? null,
  };
  await insertIntentIn(tx, dialect, intent);
  await appendIntentEventIn(tx, dialect, txHash, "signed", {
    tipSlot: cursor === null ? null : cursor.point.slot,
  });
  return { kind: "recorded", intent };
};

/**
 * Writes one intent row as given, without the §8.2 checks. Only
 * `recordIntentIn` and test tooling that copies a journal call it.
 */
export const insertIntentIn = async (
  tx: SqlTx,
  dialect: Dialect,
  intent: Intent,
): Promise<void> => {
  const values: SqlValue[] = [
    intent.txHash,
    intent.family,
    intent.workflowKey,
    intent.txCbor,
    dialect.outRefList(intent.inputs.map(encodeOutRef)),
    dialect.outRefList(intent.referenceInputs.map(encodeOutRef)),
    dialect.outRefList(intent.collaterals.map(encodeOutRef)),
    dialect.json(ownOutputsToJson(intent.ownOutputs)),
    intent.validFromSlot,
    intent.validToSlot,
    dialect.outRefList(intent.dependsOn),
    intent.built.generation,
    intent.built.point.slot,
    intent.built.point.hash,
    intent.contentRef,
  ];
  await tx.query(
    `INSERT INTO l1_intents (${INTENT_COLUMNS}) VALUES (${placeholders(values.length)})`,
    values,
  );
};
