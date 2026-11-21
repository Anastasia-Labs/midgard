import {
  currentViewIn,
  type Dialect,
  recordIntentIn,
  type RecordIntentInput,
  type RecordIntentResult,
  type SqlTx,
} from "../../src/index.js";

/**
 * Records an intent planned and signed in this transaction: its view is the
 * store's cursor read here, so `viewValid` holds by the fast path. Tests
 * about view binding pass `builtAt` to `recordIntentIn` themselves. With
 * no cursor, the view passed is a placeholder and the record is `no_view`.
 */
export const recordAtCurrentView = async (
  tx: SqlTx,
  dialect: Dialect,
  input: Omit<RecordIntentInput, "builtAt">,
): Promise<RecordIntentResult> => {
  const builtAt = (await currentViewIn(tx, dialect)) ?? {
    generation: 0,
    point: { slot: 0, hash: Buffer.alloc(32) },
    height: 0,
  };
  return recordIntentIn(tx, dialect, { ...input, builtAt });
};
