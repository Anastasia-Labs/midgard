/**
 * Recording an intent (§8.2): its exact signed bytes, refused unless every
 * input, reference input and collateral is a fact or a recorded intent's
 * output, and its first event.
 */
import { encodeOutRef } from "../codec.js";
import {
  type DecodedTransaction,
  decodeTransaction,
  TxDecodeError,
} from "../decode/tx.js";
import {
  asBuffer,
  asNumber,
  type Dialect,
  type SqlTx,
  type SqlValue,
} from "../sql/backend.js";
import { readCursor } from "../store/rows.js";
import type { OutRef, View } from "../types.js";
import {
  appendIntentEventIn,
  chunks,
  distinctHashes,
  type Intent,
  INTENT_COLUMNS,
  type OwnOutput,
  ownOutputsToJson,
  placeholders,
  readIntentsIn,
  type RecordIntentInput,
  type RecordIntentResult,
} from "./journal.js";

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
