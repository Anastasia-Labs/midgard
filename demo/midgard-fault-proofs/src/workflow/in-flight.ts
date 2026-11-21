/**
 * What this process has sent and not yet seen settle, held in memory only.
 *
 * One `WorkflowInFlight` per process. It keeps, for each proof objective,
 * the transactions this process submitted that may still land (hash, every
 * input, the wallet coins among them, the upper validity bound and the exact
 * signed bytes), and one set of wallet-coin holds shared by every build of
 * the process: a coin a transaction of ours spends, or posts as collateral,
 * is offered to no other build until that transaction settles, and a hold an
 * action outside any workflow takes (the own-thread sweep, a proven
 * correction's removal) is honoured the same way.
 *
 * A restart forgets all of it. After a restart a transaction spending only
 * wallet coins waits out one validity window (`wallet-only-send-gate.ts`), a
 * transaction spending a specific output of ours excludes a duplicate by
 * that input, and publications are found by content.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import type { HoldWalletInputs } from "../proven-correction-removal.js";
import type { ProofMaterialsView } from "./proof-materials-view.js";

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

/** A wallet coin another action of this process holds. */
export class WalletCoinHeldError extends Error {
  constructor(readonly outRef: string) {
    super(`wallet coin ${outRef} is held by another action of this process`);
    this.name = "WalletCoinHeldError";
  }
}

/** A transaction this process submitted for an objective. */
export type InFlightTransaction = Readonly<{
  actionId: string;
  txHash: string;
  /** The exact signed bytes: the chain is asked about these, and these
   * alone are broadcast again. */
  signedTransactionCborHex: string;
  /** Every input and collateral input the transaction names. */
  inputs: readonly string[];
  /** The wallet coins among `inputs`. */
  walletInputs: readonly string[];
  /** Whether every input is a wallet coin. */
  walletOnly: boolean;
  /** The upper validity bound slot; absent for a transaction with none. */
  validToSlot?: number;
  /** Wall-clock time of the last broadcast of these bytes. */
  broadcastAtMs: number;
}>;

/** The wallet a workflow sweeps its proof materials through, and the view
 * its finds go through: installed by its restricted wallet. */
export type WorkflowSweepWallet = Readonly<{
  lucid: import("@lucid-evolution/lucid").LucidEvolution;
  fundingAddress: string;
  view: ProofMaterialsView;
}>;

/** One objective's part of the process's in-flight set. */
export type WorkflowSends = Readonly<{
  category: FraudProofCatalogueCategoryName;
  headerHash: string;
  /** The objective's transactions that may still land, oldest first. */
  inFlight: () => readonly InFlightTransaction[];
  /** The transactions of every other objective of the process that may
   * still land: one objective's run, record or certificate may be another's
   * too. */
  othersInFlight: () => readonly InFlightTransaction[];
  /** Adds a submitted transaction and holds its inputs until it settles.
   * Throws `WalletCoinHeldError`, adding nothing, when another action of the
   * process holds one of them. */
  record: (transaction: InFlightTransaction) => void;
  /** Notes another broadcast of the same bytes. */
  rebroadcast: (txHash: string, atMs: number) => void;
  /** Drops a transaction that landed or can no longer land, releasing its
   * inputs. With `landed`, it landed final: it stays the action's landed
   * transaction until another transaction of the action is recorded. */
  settle: (txHash: string, landed?: boolean) => void;
  /** The last transaction of `actionId` this process saw land final, until
   * another transaction of it is recorded; undefined when none did. */
  landed: (actionId: string) => InFlightTransaction | undefined;
  /** How many transactions of the action this process sent since it last
   * landed final: a restart starts the count again. */
  attempts: (actionId: string) => number;
  /** The wallet coins the next build of `actionId` must spend: those of its
   * predecessor, which expired unseen. Empty for any other action. */
  requiredWalletInputs: (actionId: string) => readonly string[];
  /** Sets the coins the next build of `actionId` must spend. */
  require: (actionId: string, outRefs: readonly string[]) => void;
  /** Whether `outRef` is a coin the process's wallet listed last. */
  isWalletCoin: (outRef: string) => boolean;
  /** Notes every coin the process's wallet lists now, in place of its
   * previous listing. */
  noteWalletCoins: (outRefs: readonly string[]) => void;
  /** Whether another action of the process holds `outRef`. */
  isHeld: (outRef: string) => boolean;
  /** The sweep wallet the objective's restricted wallet installed, if any. */
  sweepWallet: () => WorkflowSweepWallet | undefined;
  installSweepWallet: (wallet: WorkflowSweepWallet) => void;
  /** The proof-materials view of the objective's signer, if known. */
  proofMaterialsView: () => ProofMaterialsView | undefined;
  installProofMaterialsView: (view: ProofMaterialsView) => void;
}>;

export type WorkflowInFlight = Readonly<{
  /** The objective's part, the same object for the same objective until it
   * is forgotten. */
  forObjective: (objective: {
    readonly category: FraudProofCatalogueCategoryName;
    readonly headerHash: string;
  }) => WorkflowSends;
  /** Whether the objective's part holds a transaction that may still land. */
  sending: (objective: {
    readonly category: FraudProofCatalogueCategoryName;
    readonly headerHash: string;
  }) => boolean;
  /** Drops an objective's part and releases what it holds. */
  forget: (objective: {
    readonly category: FraudProofCatalogueCategoryName;
    readonly headerHash: string;
  }) => void;
  /** Holds wallet coins for an action outside any workflow; throws
   * `WalletCoinHeldError` when one is held already. */
  holdWalletInputs: HoldWalletInputs;
  /** Every coin held now, by a workflow or by another action. */
  heldOutRefs: () => readonly string[];
}>;

const HELD_UNTIL_SETTLED = Number.MAX_SAFE_INTEGER;

export const createWorkflowInFlight = ({
  nowMs = Date.now,
}: {
  readonly nowMs?: () => number;
} = {}): WorkflowInFlight => {
  const holds = new Map<string, Readonly<{ untilMs: number }>>();
  let walletCoins: ReadonlySet<string> = new Set<string>();
  const isHeld = (outRef: string): boolean => {
    const hold = holds.get(outRef);
    if (hold === undefined) return false;
    if (hold.untilMs > nowMs()) return true;
    holds.delete(outRef);
    return false;
  };
  const holdWalletInputs: HoldWalletInputs = ({ outRefs, untilMs }) => {
    const held = [...new Set(outRefs)];
    if (
      held.some((outRef) => !OUT_REF.test(outRef)) ||
      !Number.isSafeInteger(untilMs)
    )
      throw new Error("wallet coin hold is invalid");
    const taken = held.find(isHeld);
    if (taken !== undefined) throw new WalletCoinHeldError(taken);
    const hold = Object.freeze({ untilMs });
    for (const outRef of held) holds.set(outRef, hold);
    return () => {
      for (const outRef of held)
        if (holds.get(outRef) === hold) holds.delete(outRef);
    };
  };

  const scopes = new Map<string, WorkflowSends>();
  const releases = new Map<string, Map<string, () => void>>();
  const requiredReleases = new Map<string, () => void>();
  const keyOf = ({
    category,
    headerHash,
  }: {
    readonly category: string;
    readonly headerHash: string;
  }): string => `${category}:${headerHash}`;

  const forObjective: WorkflowInFlight["forObjective"] = (objective) => {
    const key = keyOf(objective);
    const existing = scopes.get(key);
    if (existing !== undefined) return existing;
    const transactions: InFlightTransaction[] = [];
    const landed = new Map<string, InFlightTransaction>();
    const attempts = new Map<string, number>();
    const releasing = new Map<string, () => void>();
    releases.set(key, releasing);
    let required: Readonly<{ actionId: string; outRefs: readonly string[] }> = {
      actionId: "",
      outRefs: [],
    };
    // The predecessor's coins stay held for this objective until its
    // replacement is recorded, so no other build takes them in between.
    let releaseRequired = (): void => undefined;
    const clearRequired = (): void => {
      releaseRequired();
      releaseRequired = () => undefined;
      required = { actionId: "", outRefs: [] };
    };
    const holdRequired = (
      next: Readonly<{ actionId: string; outRefs: readonly string[] }>,
    ): void => {
      clearRequired();
      const free = next.outRefs.filter((outRef) => !isHeld(outRef));
      releaseRequired = holdWalletInputs({
        outRefs: free,
        untilMs: HELD_UNTIL_SETTLED,
      });
      required = Object.freeze({
        actionId: next.actionId,
        outRefs: Object.freeze(free),
      });
    };
    let sweep: WorkflowSweepWallet | undefined;
    let view: ProofMaterialsView | undefined;
    const sends: WorkflowSends = Object.freeze({
      category: objective.category,
      headerHash: objective.headerHash,
      inFlight: () => Object.freeze([...transactions]),
      othersInFlight: () =>
        Object.freeze(
          [...scopes.entries()].flatMap(([other, scope]) =>
            other === key ? [] : scope.inFlight(),
          ),
        ),
      record: (transaction) => {
        if (transactions.some(({ txHash }) => txHash === transaction.txHash))
          return;
        const kept = required;
        clearRequired();
        let release: () => void;
        try {
          release = holdWalletInputs({
            outRefs: transaction.inputs,
            untilMs: HELD_UNTIL_SETTLED,
          });
        } catch (cause) {
          if (kept.outRefs.length > 0) holdRequired(kept);
          throw cause;
        }
        releasing.set(transaction.txHash, release);
        landed.delete(transaction.actionId);
        attempts.set(
          transaction.actionId,
          (attempts.get(transaction.actionId) ?? 0) + 1,
        );
        transactions.push(Object.freeze({ ...transaction }));
      },
      rebroadcast: (txHash, atMs) => {
        const index = transactions.findIndex(
          (transaction) => transaction.txHash === txHash,
        );
        if (index >= 0)
          transactions[index] = Object.freeze({
            ...transactions[index]!,
            broadcastAtMs: atMs,
          });
      },
      settle: (txHash, settledLanded) => {
        const index = transactions.findIndex(
          (transaction) => transaction.txHash === txHash,
        );
        if (index < 0) return;
        const settled = transactions[index]!;
        if (settledLanded === true) {
          landed.set(settled.actionId, settled);
          attempts.delete(settled.actionId);
        }
        transactions.splice(index, 1);
        releasing.get(txHash)?.();
        releasing.delete(txHash);
      },
      landed: (actionId) => landed.get(actionId),
      attempts: (actionId) => attempts.get(actionId) ?? 0,
      requiredWalletInputs: (actionId) =>
        required.actionId === actionId ? required.outRefs : [],
      require: (actionId, outRefs) =>
        holdRequired({ actionId, outRefs: [...new Set(outRefs)] }),
      isWalletCoin: (outRef) => walletCoins.has(outRef),
      noteWalletCoins: (outRefs) => {
        walletCoins = new Set(outRefs);
      },
      isHeld: (outRef) => !required.outRefs.includes(outRef) && isHeld(outRef),
      sweepWallet: () => sweep,
      installSweepWallet: (wallet) => {
        sweep = wallet;
      },
      proofMaterialsView: () => view,
      installProofMaterialsView: (installed) => {
        view = installed;
      },
    });
    requiredReleases.set(key, clearRequired);
    scopes.set(key, sends);
    return sends;
  };

  return Object.freeze({
    forObjective,
    sending: (objective) =>
      (scopes.get(keyOf(objective))?.inFlight().length ?? 0) > 0,
    forget: (objective) => {
      const key = keyOf(objective);
      for (const release of releases.get(key)?.values() ?? []) release();
      releases.delete(key);
      requiredReleases.get(key)?.();
      requiredReleases.delete(key);
      scopes.delete(key);
    },
    holdWalletInputs,
    heldOutRefs: () => [...holds.keys()].filter(isHeld),
  });
};
