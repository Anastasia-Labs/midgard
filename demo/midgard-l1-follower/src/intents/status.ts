import { encodeOutRef } from "../codec.js";
import { depth } from "../heads.js";
import {
  asBuffer,
  asNullableBuffer,
  asNullableNumber,
  asNumber,
  type Dialect,
  type SqlRow,
  type SqlTx,
} from "../sql/backend.js";
import { readCursor } from "../store/rows.js";
import type { Cursor, OutRef } from "../types.js";
import {
  type IntentHead,
  journaledHashesIn,
  readIntentHeadsIn,
} from "./journal.js";

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
  intent: IntentHead;
  status: IntentStatus;
  /**
   * The slot from which the intent is terminal: the retention boundary (the
   * block k below the tip) at or past it means it has been terminal for k
   * blocks and may be pruned. Null while it may still change (live). An
   * abandoned intent is terminal from the tip slot its abandon event was
   * written at.
   */
  terminalSlot: number | null;
}>;

/** Every derived status read at once, with the cursor they were read at. */
export type IntentStatuses = Readonly<{
  cursor: Cursor | null;
  states: readonly IntentState[];
}>;

/** One intent's derived status, with the cursor it was read at. */
export type IntentStatusRead = Readonly<{
  cursor: Cursor | null;
  /** Null when the intent is not journaled (never recorded, or pruned). */
  state: IntentState | null;
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

type Landing = Readonly<{
  isValid: boolean;
  slot: number;
  height: number;
  /** The output index a phase-2 failure's collateral return has, or null. */
  collateralReturnIndex: number | null;
}>;
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
  const distinct = [...new Map(hashes.map((h) => [hex(h), h])).values()];
  for (const chunk of chunked(distinct))
    for (const row of await tx.query(
      `SELECT t.tx_hash, t.is_valid, t.block_slot, t.output_count, t.has_collateral_return, b.height FROM l1_txs t
         JOIN l1_blocks b ON b.slot = t.block_slot
        WHERE t.tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    ))
      landed.set(hex(asBuffer(row.tx_hash)), {
        isValid: dialect.readBool(row.is_valid),
        slot: asNumber(row.block_slot),
        height: asNumber(row.height),
        collateralReturnIndex: dialect.readBool(row.has_collateral_return)
          ? asNumber(row.output_count)
          : null,
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

/**
 * The abandoned intents among `txHashes` (all of them when omitted), each
 * with the tip slot of its first abandon event: the partial index on
 * abandon events serves the whole-journal form, the primary key the named
 * one.
 */
const abandonedIn = async (
  tx: SqlTx,
  txHashes?: readonly Buffer[],
): Promise<Map<string, number | null>> => {
  const abandoned = new Map<string, number | null>();
  const collect = (rows: readonly SqlRow[]): void => {
    for (const row of rows)
      abandoned.set(hex(asBuffer(row.tx_hash)), asNullableNumber(row.tip_slot));
  };
  if (txHashes === undefined) {
    collect(
      await tx.query(
        "SELECT tx_hash, min(tip_slot) AS tip_slot FROM l1_intent_events WHERE kind = 'abandoned' GROUP BY tx_hash",
      ),
    );
    return abandoned;
  }
  for (const chunk of chunked(txHashes))
    collect(
      await tx.query(
        `SELECT tx_hash, min(tip_slot) AS tip_slot FROM l1_intent_events
          WHERE tx_hash IN (${placeholders(chunk.length)}) AND kind = 'abandoned' GROUP BY tx_hash`,
        chunk,
      ),
    );
  return abandoned;
};

const minSlot = (a: number | null, b: number | null): number | null =>
  a === null ? b : b === null ? a : Math.min(a, b);

const spendsOf = (intent: IntentHead): OutRef[] => [
  ...intent.inputs,
  ...intent.referenceInputs,
  ...intent.collaterals,
];

/**
 * Derives the statuses of `heads` (which must hold every journaled
 * dependency of each, transitively) from the facts at `cursor`, in the
 * caller's transaction. `journaled` answers whether a conflicting spender
 * is an own intent when it is not among `heads`.
 */
const deriveOver = async (
  tx: SqlTx,
  dialect: Dialect,
  cursor: Cursor | null,
  heads: readonly IntentHead[],
  abandoned: ReadonlyMap<string, number | null>,
  journaled: (spenders: readonly Buffer[]) => Promise<ReadonlySet<string>>,
): Promise<Map<string, IntentState>> => {
  const byHash = new Map(heads.map((i) => [hex(i.txHash), i]));
  const landings = await landingsIn(tx, dialect, [
    ...heads.map((i) => i.txHash),
    ...heads.flatMap((i) => i.dependsOn),
  ]);
  const outputs = await outputFactsIn(
    tx,
    heads
      .filter((i) => !landings.has(hex(i.txHash)))
      .flatMap((intent) => spendsOf(intent)),
  );
  const foreignSpenders = [...outputs.values()].flatMap((fact) =>
    fact.spentTx === null || byHash.has(hex(fact.spentTx))
      ? []
      : [fact.spentTx],
  );
  const ownElsewhere =
    foreignSpenders.length === 0
      ? new Set<string>()
      : await journaled(foreignSpenders);
  const isOwn = (spender: Buffer): boolean =>
    byHash.has(hex(spender)) || ownElsewhere.has(hex(spender));
  const tipSlot = cursor?.point.slot ?? null;
  const memo = new Map<string, IntentState>();
  const deriving = new Set<string>();

  const derive = (intent: IntentHead): IntentState => {
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

  const deriveOne = (intent: IntentHead): IntentState => {
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
    for (const outRef of spendsOf(intent)) {
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
          ownSpender: isOwn(fact.spentTx),
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
      // A phase-2 failure still creates its collateral return: a child
      // that spends only that output from it depends on nothing dead.
      if (
        landed !== undefined &&
        landed.collateralReturnIndex !== null &&
        spendsOf(intent).every(
          (outRef) =>
            !outRef.txHash.equals(dependency) ||
            outRef.index === landed.collateralReturnIndex,
        )
      )
        continue;
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
    const abandonSlot = abandoned.get(hex(intent.txHash));
    const terminalSlot = minSlot(
      minSlot(
        minSlot(
          conflict === null ? null : conflict.slot,
          expired ? (intent.validToSlot as number) : null,
        ),
        dependencyTerminal,
      ),
      abandonSlot ?? null,
    );
    const status: IntentStatus =
      conflict !== null
        ? conflict
        : expired
          ? { kind: "expired", validToSlot: intent.validToSlot as number }
          : deadDependency !== null
            ? { kind: "dependency_dead", dependency: deadDependency }
            : abandonSlot !== undefined
              ? { kind: "abandoned" }
              : { kind: "live", inputsAvailable };
    return { intent, status, terminalSlot };
  };

  return new Map(heads.map((head) => [hex(head.txHash), derive(head)]));
};

/**
 * Derives every journaled intent's status from the facts at the cursor, in
 * the caller's transaction (one consistent read). Precedence, after the
 * landing facts: conflicted, expired, dependency_dead, abandoned, live.
 * `terminalSlot` is the earliest slot by which any dead reason became
 * irrevocable, whatever the reported reason. Reads the retained intents'
 * heads (never their signed bytes) and probes the facts by primary key;
 * retention bounds the retained set to the live intents plus those terminal
 * for less than k blocks.
 */
export const deriveIntentStatusesIn = async (
  tx: SqlTx,
  dialect: Dialect,
): Promise<IntentStatuses> => {
  const cursor = await readCursor(tx, dialect);
  const heads = await readIntentHeadsIn(tx, dialect);
  const derived = await deriveOver(
    tx,
    dialect,
    cursor,
    heads,
    await abandonedIn(tx),
    () => Promise.resolve(new Set<string>()),
  );
  return { cursor, states: heads.map((h) => derived.get(hex(h.txHash))!) };
};

/**
 * One intent's derived status, reading only it and its journaled
 * dependencies (transitively), by primary key. Equal to its entry in
 * `deriveIntentStatusesIn` at the same cursor.
 */
export const deriveIntentStatusIn = async (
  tx: SqlTx,
  dialect: Dialect,
  txHash: Buffer,
): Promise<IntentStatusRead> => {
  const cursor = await readCursor(tx, dialect);
  const closure = new Map<string, IntentHead>();
  let frontier: Buffer[] = [txHash];
  while (frontier.length > 0) {
    const found = await readIntentHeadsIn(tx, dialect, frontier);
    frontier = [];
    for (const head of found) {
      closure.set(hex(head.txHash), head);
      for (const dependency of head.dependsOn)
        if (!closure.has(hex(dependency))) frontier.push(dependency);
    }
  }
  if (!closure.has(hex(txHash))) return { cursor, state: null };
  const heads = [...closure.values()];
  const derived = await deriveOver(
    tx,
    dialect,
    cursor,
    heads,
    await abandonedIn(
      tx,
      heads.map((h) => h.txHash),
    ),
    (spenders) => journaledHashesIn(tx, spenders),
  );
  return { cursor, state: derived.get(hex(txHash)) ?? null };
};
