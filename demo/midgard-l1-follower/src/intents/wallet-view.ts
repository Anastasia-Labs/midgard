/**
 * The wallet view (§8.5): what one own address may spend now.
 *
 *   available = live own outputs at the tip (facts)
 *             − inputs and collaterals of live intents
 *             + own outputs of live intents (predicted change)
 *
 * `walletView` is the formula, pure: no L1 read, no clock. Every role
 * imports it from here, next to the facts and the intent statuses it is
 * computed over. `readWalletViewIn` reads its operands in the caller's
 * transaction, so it is recomputed at whatever head the store has applied.
 *
 * Live is the derived status `live` only (`isLiveStatus`), with or without
 * its inputs available yet. A landed intent is in the facts already; a dead
 * one (conflicted, expired, dependency_dead, abandoned, failed_landed) holds
 * nothing back, so its inputs that are still live facts are available again
 * on the next read.
 */
import { outRefKey as key } from "../codec.js";
import { decodeTransaction, transactionOutputAt } from "../decode/tx.js";
import type { Dialect, SqlTx } from "../sql/backend.js";
import { liveUtxosIn } from "../store/reads.js";
import type { Cursor, OutputSummary, OutRef } from "../types.js";
import { readIntentsIn } from "./journal.js";
import { deriveIntentStatusesIn, type IntentStatus } from "./status.js";

/** One spendable output of the wallet. */
export type WalletViewOutput = Readonly<{
  outRef: OutRef;
  output: OutputSummary;
  /**
   * The live intent that predicts this output (its tx id, which is also
   * `outRef.txHash`), so a builder can chain on it; null for a fact.
   */
  predictedBy: Buffer | null;
}>;

/** What the view takes from one live intent. */
export type WalletViewIntent = Readonly<{
  txHash: Buffer;
  inputs: readonly OutRef[];
  collaterals: readonly OutRef[];
  /** The intent's outputs (from its signed bytes), by output index. */
  outputs: readonly Readonly<{ index: number; output: OutputSummary }>[];
}>;

export type WalletView = Readonly<{
  address: Buffer;
  /** Facts first (in the order given), then predicted outputs by intent. */
  available: readonly WalletViewOutput[];
  /**
   * The inputs and collaterals of the live intents, at any address: what no
   * other build may spend while they are live.
   */
  held: readonly OutRef[];
}>;

/** The statuses whose inputs, collaterals and outputs the view counts. */
export const LIVE_INTENT_STATUSES = ["live"] as const;

export const isLiveStatus = (status: IntentStatus): boolean =>
  (LIVE_INTENT_STATUSES as readonly string[]).includes(status.kind);

/**
 * §8.5 over `facts` (the live outputs at the head, any address) and the
 * live intents: the outputs at `ownAddress` no live intent spends or uses
 * as collateral, plus the live intents' outputs at `ownAddress` no other
 * live intent spends.
 */
export const walletView = (
  facts: readonly Readonly<{ outRef: OutRef; output: OutputSummary }>[],
  liveIntents: readonly WalletViewIntent[],
  ownAddress: Buffer,
): WalletView => {
  const held = new Map<string, OutRef>();
  for (const intent of liveIntents)
    for (const outRef of [...intent.inputs, ...intent.collaterals])
      held.set(key(outRef), outRef);
  const seen = new Set<string>();
  const available: WalletViewOutput[] = [];
  const offer = (entry: WalletViewOutput): void => {
    const k = key(entry.outRef);
    if (held.has(k) || seen.has(k) || !entry.output.address.equals(ownAddress))
      return;
    seen.add(k);
    available.push(entry);
  };
  for (const fact of facts)
    offer({ outRef: fact.outRef, output: fact.output, predictedBy: null });
  for (const intent of liveIntents)
    for (const { index, output } of intent.outputs)
      offer({
        outRef: { txHash: intent.txHash, index },
        output,
        predictedBy: intent.txHash,
      });
  return { address: ownAddress, available, held: [...held.values()] };
};

export type WalletViewRead = Readonly<{
  /** The head the view was read at (null before the store is initialized). */
  cursor: Cursor | null;
  view: WalletView;
}>;

/**
 * Reads `walletView` for `ownAddress` in the caller's transaction: the live
 * facts at the address, the derived statuses, and the signed bytes of the
 * live intents that predict an output there (their exact outputs, datum
 * and script reference included).
 */
export const readWalletViewIn = async (
  tx: SqlTx,
  dialect: Dialect,
  ownAddress: Buffer,
): Promise<WalletViewRead> => {
  const { cursor, states } = await deriveIntentStatusesIn(tx, dialect);
  const facts = await liveUtxosIn(tx, dialect, {
    by: "address",
    address: ownAddress,
  });
  if (facts.kind !== "ok")
    throw new Error(`the wallet's facts are unreadable: ${facts.detail}`);
  const live = states
    .filter((state) => isLiveStatus(state.status))
    .map((state) => state.intent);
  const predicting = live.filter((head) =>
    head.ownOutputs.some((own) => own.address.equals(ownAddress)),
  );
  const outputsOf = new Map<string, WalletViewIntent["outputs"]>();
  for (const intent of await readIntentsIn(
    tx,
    dialect,
    predicting.map((head) => head.txHash),
  )) {
    const decoded = decodeTransaction(intent.txCbor);
    outputsOf.set(
      intent.txHash.toString("hex"),
      intent.ownOutputs.flatMap(({ index }) => {
        const output = transactionOutputAt(decoded, index);
        return output === null ? [] : [{ index, output }];
      }),
    );
  }
  return {
    cursor,
    view: walletView(
      facts.utxos,
      live.map((head) => ({
        txHash: head.txHash,
        inputs: head.inputs,
        collaterals: head.collaterals,
        outputs: outputsOf.get(head.txHash.toString("hex")) ?? [],
      })),
      ownAddress,
    ),
  };
};
