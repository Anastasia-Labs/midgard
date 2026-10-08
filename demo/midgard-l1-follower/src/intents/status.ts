import { encodeOutRef } from "../codec.js";
import { depth } from "../heads.js";
import {
  asBuffer,
  asNullableBuffer,
  asNullableNumber,
  asNumber,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import { readCursor } from "../store/rows.js";
import type { Cursor, OutRef } from "../types.js";
import { type Intent, readIntentsIn } from "./journal.js";

/**
 * An intent's derived status (§8.2): a read over the facts, never a stored
 * column, so a rollback that un-lands an intent makes it live again with no
 * write. The facts win: an intent that lands is `landed` even after an
 * `abandoned` event.
 */
export type IntentStatus =
  | Readonly<{ kind: "landed"; slot: number; height: number; depth: number }>
  /** Landed failing phase 2: its collateral was consumed. Dead. */
  | Readonly<{
      kind: "failed_landed";
      slot: number;
      height: number;
      depth: number;
    }>
  /**
   * An input, reference input or collateral was spent by another tx. The
   * earliest such spend is reported. `ownSpender`: that tx is a journaled
   * intent (superseded), else foreign.
   */
  | Readonly<{
      kind: "conflicted";
      outRef: OutRef;
      spender: Buffer;
      ownSpender: boolean;
      slot: number;
    }>
  /** The tip slot reached the tx's `invalid_hereafter`. */
  | Readonly<{ kind: "expired"; validToSlot: number }>
  /** A journaled dependency is dead, or was pruned without landing. */
  | Readonly<{ kind: "dependency_dead"; dependency: Buffer }>
  | Readonly<{ kind: "abandoned" }>
  | Readonly<{
      kind: "live";
      /** Every input, reference input and collateral is a live fact row now. */
      inputsAvailable: boolean;
    }>;

export type IntentStatusKind = IntentStatus["kind"];

/** Dead: conflicted, expired, dependency_dead, abandoned or failed_landed (§8.2, §8.3). */
export const isDeadStatus = (status: IntentStatus): boolean =>
  status.kind === "conflicted" ||
  status.kind === "expired" ||
  status.kind === "dependency_dead" ||
  status.kind === "abandoned" ||
  status.kind === "failed_landed";

export type IntentState = Readonly<{
  intent: Intent;
  status: IntentStatus;
  /**
   * The slot from which the intent is terminal: the retention boundary (the
   * block k below the tip) at or past it means it has been terminal for k
   * blocks and may be pruned. Null while it may still change (live, or
   * abandoned with nothing else deciding it).
   */
  terminalSlot: number | null;
}>;

/** Every derived status read at once, with the cursor they were read at. */
export type IntentStatuses = Readonly<{
  cursor: Cursor | null;
  states: readonly IntentState[];
}>;

const IN_CHUNK = 500;

const placeholders = (count: number): string =>
  Array.from({ length: count }, () => "?").join(", ");

const chunked = <T>(items: readonly T[]): T[][] => {
  const out: T[][] = [];
  for (let start = 0; start < items.length; start += IN_CHUNK)
    out.push(items.slice(start, start + IN_CHUNK));
  return out;
};

type Landing = Readonly<{ isValid: boolean; slot: number; height: number }>;
type OutputFact = Readonly<{
  spentTx: Buffer | null;
  spentSlot: number | null;
}>;

const hex = (bytes: Buffer): string => bytes.toString("hex");
const outRefHex = (outRef: OutRef): string => hex(encodeOutRef(outRef));

const landingsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  hashes: readonly Buffer[],
): Promise<Map<string, Landing>> => {
  const landed = new Map<string, Landing>();
  for (const chunk of chunked(hashes))
    for (const row of await tx.query(
      `SELECT t.tx_hash, t.is_valid, t.block_slot, b.height FROM l1_txs t
         JOIN l1_blocks b ON b.slot = t.block_slot
        WHERE t.tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    ))
      landed.set(hex(asBuffer(row.tx_hash)), {
        isValid: dialect.readBool(row.is_valid),
        slot: asNumber(row.block_slot),
        height: asNumber(row.height),
      });
  return landed;
};

const outputFactsIn = async (
  tx: SqlTx,
  outRefs: readonly OutRef[],
): Promise<Map<string, OutputFact>> => {
  const facts = new Map<string, OutputFact>();
  const parents = [
    ...new Map(outRefs.map((o) => [hex(o.txHash), o.txHash])).values(),
  ];
  for (const chunk of chunked(parents))
    for (const row of await tx.query(
      `SELECT tx_hash, output_index, spent_tx, spent_slot FROM l1_outputs
        WHERE tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    ))
      facts.set(
        outRefHex({
          txHash: asBuffer(row.tx_hash),
          index: asNumber(row.output_index),
        }),
        {
          spentTx: asNullableBuffer(row.spent_tx),
          spentSlot: asNullableNumber(row.spent_slot),
        },
      );
  return facts;
};

const minSlot = (a: number | null, b: number | null): number | null =>
  a === null ? b : b === null ? a : Math.min(a, b);

/**
 * Derives every journaled intent's status from the facts at the cursor, in
 * the caller's transaction (one consistent read). Precedence, after the
 * landing facts: conflicted, expired, dependency_dead, abandoned, live.
 * `terminalSlot` is the earliest slot by which any dead reason became
 * irrevocable, whatever the reported reason.
 */
export const deriveIntentStatusesIn = async (
  tx: SqlTx,
  dialect: Dialect,
): Promise<IntentStatuses> => {
  const cursor = await readCursor(tx, dialect);
  const intents = await readIntentsIn(tx, dialect);
  const byHash = new Map(intents.map((i) => [hex(i.txHash), i]));
  const abandoned = new Set(
    (
      await tx.query(
        "SELECT DISTINCT tx_hash FROM l1_intent_events WHERE kind = 'abandoned'",
      )
    ).map((row) => hex(asBuffer(row.tx_hash))),
  );
  const landings = await landingsIn(tx, dialect, [
    ...intents.map((i) => i.txHash),
    ...intents.flatMap((i) => i.dependsOn),
  ]);
  const spends = (intent: Intent): OutRef[] => [
    ...intent.inputs,
    ...intent.referenceInputs,
    ...intent.collaterals,
  ];
  const outputs = await outputFactsIn(
    tx,
    intents
      .filter((i) => !landings.has(hex(i.txHash)))
      .flatMap((intent) => spends(intent)),
  );
  const tipSlot = cursor?.point.slot ?? null;
  const memo = new Map<string, IntentState>();
  const deriving = new Set<string>();

  const derive = (intent: Intent): IntentState => {
    const key = hex(intent.txHash);
    const cached = memo.get(key);
    if (cached !== undefined) return cached;
    if (deriving.has(key)) throw new Error(`intent dependency cycle at ${key}`);
    deriving.add(key);
    const state = deriveOne(intent);
    deriving.delete(key);
    memo.set(key, state);
    return state;
  };

  const deriveOne = (intent: Intent): IntentState => {
    const landing = landings.get(hex(intent.txHash));
    if (landing !== undefined) {
      const at = {
        slot: landing.slot,
        height: landing.height,
        depth: cursor === null ? 0 : depth(cursor.height, landing.height),
      };
      return {
        intent,
        status: landing.isValid
          ? { kind: "landed", ...at }
          : { kind: "failed_landed", ...at },
        terminalSlot: landing.slot,
      };
    }
    let conflict: Extract<IntentStatus, { kind: "conflicted" }> | null = null;
    let inputsAvailable = true;
    for (const outRef of spends(intent)) {
      const fact = outputs.get(outRefHex(outRef));
      if (fact === undefined || fact.spentTx === null) {
        if (fact === undefined) inputsAvailable = false;
        continue;
      }
      inputsAvailable = false;
      if (fact.spentTx.equals(intent.txHash)) continue;
      const slot = fact.spentSlot ?? 0;
      if (
        conflict === null ||
        slot < conflict.slot ||
        (slot === conflict.slot &&
          Buffer.compare(fact.spentTx, conflict.spender) < 0)
      )
        conflict = {
          kind: "conflicted",
          outRef,
          spender: fact.spentTx,
          ownSpender: byHash.has(hex(fact.spentTx)),
          slot,
        };
    }
    const expired =
      intent.validToSlot !== null &&
      tipSlot !== null &&
      tipSlot >= intent.validToSlot;
    let deadDependency: Buffer | null = null;
    let dependencyTerminal: number | null = null;
    for (const dependency of intent.dependsOn) {
      const landed = landings.get(hex(dependency));
      if (landed?.isValid === true) continue;
      const parent = byHash.get(hex(dependency));
      // Neither journaled nor landed: pruned dead (a landed parent keeps its
      // tx row while the outputs this intent uses are retained).
      const parentState = parent === undefined ? null : derive(parent);
      if (parentState !== null && !isDeadStatus(parentState.status)) continue;
      deadDependency ??= dependency;
      dependencyTerminal = minSlot(
        dependencyTerminal,
        parentState === null ? 0 : parentState.terminalSlot,
      );
    }
    const terminalSlot = minSlot(
      minSlot(
        conflict === null ? null : conflict.slot,
        expired ? (intent.validToSlot as number) : null,
      ),
      dependencyTerminal,
    );
    const status: IntentStatus =
      conflict !== null
        ? conflict
        : expired
          ? { kind: "expired", validToSlot: intent.validToSlot as number }
          : deadDependency !== null
            ? { kind: "dependency_dead", dependency: deadDependency }
            : abandoned.has(hex(intent.txHash))
              ? { kind: "abandoned" }
              : { kind: "live", inputsAvailable };
    return { intent, status, terminalSlot };
  };

  return { cursor, states: intents.map(derive) };
};
