/**
 * First-seen input claims (design D6, stateful part; owner ruling
 * 2026-11-06: first seen wins, an acknowledged tx is never evicted or
 * replaced).
 *
 * Every tx admission holds, from the moment it enters the writer's pending
 * set until its verdict, claims on the inputs it spends and an index entry
 * for each output it produces. A `/submit` tx is refused with 409 when one
 * of its inputs is claimed by another pending tx, or is neither in the
 * mempool ledger state nor produced by a pending tx. Peer-forwarded txs are
 * never refused here (a peer's tx may spend an output this node has not seen
 * yet), but they take the claims nobody holds and index their outputs, so a
 * user's tx that spends a peer tx's output is admitted and one that
 * double-spends its input is not.
 *
 * The check is advisory: full validation stays authoritative. It never
 * refuses on a ledger view it cannot trust (`SpendableView` answering
 * `undefined`), only on claims.
 *
 * Lifetime: a claim is taken in the writer's synchronous admit section, in
 * one step with the tx's registration in the pending set, and released when
 * the tx can no longer be pending: its verdict (mark-accepted,
 * mark-rejected), the peer forget path, or a write that stored no new row
 * (poisoned, or a stored variant). A lease requeue keeps the claims: the tx
 * is still pending and will be validated again, so releasing them would let
 * a second spender in. A restart rebuilds the claims from the backlog before
 * the node serves.
 *
 * Bound: the claims belong to txs that hold a durable backlog slot (pending
 * in the writer or a queued or validating row), so there are at most
 * `MAX_DURABLE_ADMISSION_BACKLOG` of them, and each index entry stands for
 * one input or output of a tx whose bytes count against
 * `MAX_DURABLE_ADMISSION_BACKLOG_BYTES` (an input or an output costs a tx
 * more than 32 bytes).
 */
import { Data } from "effect";

/**
 * A `/submit` tx spends an input that another pending tx claimed, or that is
 * neither in the mempool ledger state nor produced by a pending tx (409). The
 * client reads `/tx-status` before treating its tx as failed: the tx itself
 * may already be accepted.
 */
export class AdmissionInputSpentError extends Data.TaggedError(
  "AdmissionInputSpentError",
)<{
  readonly message: string;
  readonly txIdHex: string;
  readonly outRefHex: string;
  /** `claimed`: a pending tx spends the input. `not_in_ledger`: the input is
   * spent, or this node never saw it. */
  readonly reason: "claimed" | "not_in_ledger";
  /** The pending tx that claimed the input, for `claimed`. */
  readonly spentByTxIdHex?: string;
}> {}

/**
 * Whether an outref is in the mempool ledger state: `undefined` when the view
 * cannot tell (the validation cache is not loaded or is being rebuilt).
 */
export type SpendableView = (outRefHex: string) => boolean | undefined;

/** A view that cannot tell: only claims refuse. */
export const UNKNOWN_SPENDABLE_VIEW: SpendableView = () => undefined;

export type InputClaimRequest = {
  readonly txIdHex: string;
  readonly spentOutRefs: readonly string[];
  readonly producedOutRefs: readonly string[];
  /** `false` for a peer-forwarded tx: it is never refused, and takes only
   * the claims nobody holds. */
  readonly check: boolean;
  readonly spendable: SpendableView;
};

export type InputClaimOutcome =
  /** The claims were taken now: release them if the tx is not written. */
  | { readonly _tag: "Claimed" }
  /** This tx id already holds its claims (it is pending or rebuilt). */
  | { readonly _tag: "Held" }
  | { readonly _tag: "Refused"; readonly error: AdmissionInputSpentError };

export type AdmissionInputClaimStats = {
  /** Txs holding claims. */
  readonly txs: number;
  /** Inputs claimed. */
  readonly claims: number;
  /** Outputs of pending txs indexed as spendable. */
  readonly produced: number;
};

/**
 * The claim index. Every method is synchronous: Effect fibers share one
 * event loop, so a check and the claims it takes cannot interleave with
 * another admission.
 */
export type AdmissionInputClaims = {
  readonly take: (request: InputClaimRequest) => InputClaimOutcome;
  /** Drops the claims and produced outputs `txIdHex` holds; a no-op for a
   * tx that holds none. */
  readonly release: (txIdHex: string) => void;
  readonly stats: () => AdmissionInputClaimStats;
};

type HeldClaims = {
  readonly spent: readonly string[];
  readonly produced: readonly string[];
};

export const makeAdmissionInputClaims = (): AdmissionInputClaims => {
  /** Input outref → the pending tx that claimed it. */
  const claimant = new Map<string, string>();
  /** Output outref → the pending tx that produces it. */
  const producer = new Map<string, string>();
  const held = new Map<string, HeldClaims>();

  const refusal = (
    txIdHex: string,
    outRefHex: string,
    spentByTxIdHex: string | undefined,
  ): InputClaimOutcome => ({
    _tag: "Refused",
    error:
      spentByTxIdHex === undefined
        ? new AdmissionInputSpentError({
            message: `Input ${outRefHex} is spent or unknown to this node; read /tx-status for transaction ${txIdHex} before treating it as failed`,
            txIdHex,
            outRefHex,
            reason: "not_in_ledger",
          })
        : new AdmissionInputSpentError({
            message: `Input ${outRefHex} is spent by pending transaction ${spentByTxIdHex}; read /tx-status for transaction ${txIdHex} before treating it as failed`,
            txIdHex,
            outRefHex,
            reason: "claimed",
            spentByTxIdHex,
          }),
  });

  return {
    take: ({ txIdHex, spentOutRefs, producedOutRefs, check, spendable }) => {
      if (held.has(txIdHex)) return { _tag: "Held" };
      if (check) {
        for (const outRefHex of spentOutRefs) {
          const owner = claimant.get(outRefHex);
          if (owner !== undefined) return refusal(txIdHex, outRefHex, owner);
          if (!producer.has(outRefHex) && spendable(outRefHex) === false)
            return refusal(txIdHex, outRefHex, undefined);
        }
      }
      for (const outRefHex of spentOutRefs) {
        if (!claimant.has(outRefHex)) claimant.set(outRefHex, txIdHex);
      }
      for (const outRefHex of producedOutRefs) {
        producer.set(outRefHex, txIdHex);
      }
      held.set(txIdHex, { spent: spentOutRefs, produced: producedOutRefs });
      return { _tag: "Claimed" };
    },
    release: (txIdHex) => {
      const claims = held.get(txIdHex);
      if (claims === undefined) return;
      held.delete(txIdHex);
      for (const outRefHex of claims.spent) {
        if (claimant.get(outRefHex) === txIdHex) claimant.delete(outRefHex);
      }
      for (const outRefHex of claims.produced) {
        if (producer.get(outRefHex) === txIdHex) producer.delete(outRefHex);
      }
    },
    stats: () => ({
      txs: held.size,
      claims: claimant.size,
      produced: producer.size,
    }),
  };
};
