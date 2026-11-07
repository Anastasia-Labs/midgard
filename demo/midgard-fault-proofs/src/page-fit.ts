/** Sizes one page of a proof-opening walk to one transaction's budget. A page
 * opens MPF proofs per item, so its cost grows with the depth of the tries it
 * opens; the page is built at the configured maximum and halved until each of
 * its transactions fits. */
import type {
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";

import { scriptDecodeLedgerLimits } from "./script-decode/record.js";
import { transactionExUnits } from "./script-decode/submit.js";

/** One page at the size taken. */
export type FittedPage<Transition> = Readonly<{
  size: number;
  transitions: readonly Transition[];
}>;

/** An unsubmitted thread transaction and the thread output index its
 * completed layout chose. */
export type PageTransaction = Readonly<{
  complete: () => Promise<TxSignBuilder>;
  outputIndex: () => bigint | undefined;
}>;

/** Whether a completed signed transaction is within the byte limit and keeps
 * the 20% execution reserve below the ledger's limits. */
const withinBudget = (lucid: LucidEvolution, cbor: string) => {
  const limits = scriptDecodeLedgerLimits(lucid);
  const used = transactionExUnits(cbor);
  return (
    cbor.length / 2 <= limits.bytes &&
    used.mem * 5n <= BigInt(limits.mem) * 4n &&
    used.steps * 5n <= BigInt(limits.steps) * 4n
  );
};

/** Builds, evaluates and signs each transaction of a page without submitting
 * it, each spending the thread output the previous one would create. */
const pageFits = async <Transition extends { readonly datum: string }>(
  lucid: LucidEvolution,
  thread: UTxO,
  transitions: readonly Transition[],
  build: (thread: UTxO, transition: Transition) => Promise<PageTransaction>,
  nextAddress: (transition: Transition) => string,
) => {
  let current = thread;
  for (const transition of transitions) {
    const built = await build(current, transition);
    const signed = await (await built.complete()).sign.withWallet().complete();
    const outputIndex = built.outputIndex();
    if (outputIndex === undefined || !withinBudget(lucid, signed.toCBOR()))
      return false;
    current = {
      txHash: signed.toHash(),
      outputIndex: Number(outputIndex),
      address: nextAddress(transition),
      assets: thread.assets,
      datum: transition.datum,
    };
  }
  return true;
};

/** The first transition of the largest page continuing `thread`, from the
 * configured maximum (`page()`) halving down to one item, whose transactions
 * all fit one transaction's budget. A page of one item that does not fit
 * throws its build error. */
export const fitPage = async <Transition extends { readonly datum: string }>({
  lucid,
  thread,
  label,
  page,
  build,
  nextAddress,
}: {
  readonly lucid: LucidEvolution;
  readonly thread: UTxO;
  /** Names the paged role in the error a page of one item raises. */
  readonly label: string;
  /** The page at `size` items; the configured maximum when omitted. */
  readonly page: (size?: number) => FittedPage<Transition>;
  readonly build: (
    thread: UTxO,
    transition: Transition,
  ) => Promise<PageTransaction>;
  /** The address the thread output of `transition` pays. */
  readonly nextAddress: (transition: Transition) => string;
}): Promise<Transition> => {
  let current = page();
  for (;;) {
    let error: unknown = new Error(
      `${label} page of ${current.size.toString()} exceeds the transaction budget`,
    );
    const fits = await pageFits(
      lucid,
      thread,
      current.transitions,
      build,
      nextAddress,
    ).catch((failure: unknown) => {
      error = failure;
      return false;
    });
    if (fits) return current.transitions[0]!;
    if (current.size === 1) throw error;
    current = page(Math.floor(current.size / 2));
  }
};
