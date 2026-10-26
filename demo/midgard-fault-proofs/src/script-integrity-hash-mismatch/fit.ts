/** Sizes each page of the `scriptIntegrityHashMismatch` all-purposes walk to
 * one transaction's budget. A page opens one MPF proof per item (two for a
 * spend), so its cost grows with the depth of the tries it opens; the page is
 * built at the configured maximum and halved until it fits. */
import type { UTxO } from "@lucid-evolution/lucid";

import { scriptDecodeLedgerLimits } from "../script-decode/record.js";
import { transactionExUnits } from "../script-decode/submit.js";
import { scriptIntegrityHashMismatchContractRoles } from "./contracts.js";
import type { ScriptIntegrityHashMismatchTransition } from "./custody.js";
import {
  buildScriptIntegrityHashMismatchTransition,
  type ScriptIntegrityHashMismatchCustody,
} from "./submit.js";
import type {
  ScriptIntegrityHashMismatchPagedRole,
  ScriptIntegrityHashMismatchWalk,
} from "./walk.js";

/** Whether a completed signed transaction is within the byte limit and keeps
 * the 20% execution reserve below the ledger's limits. */
const withinBudget = (
  custody: ScriptIntegrityHashMismatchCustody,
  cbor: string,
) => {
  const limits = scriptDecodeLedgerLimits(custody.lucid);
  const used = transactionExUnits(cbor);
  return (
    cbor.length / 2 <= limits.bytes &&
    used.mem * 5n <= BigInt(limits.mem) * 4n &&
    used.steps * 5n <= BigInt(limits.steps) * 4n
  );
};

/** Builds, evaluates and signs each transaction of a page without submitting
 * it, each spending the thread output the previous one would create. */
const pageFits = async (
  custody: ScriptIntegrityHashMismatchCustody,
  thread: UTxO,
  transitions: readonly ScriptIntegrityHashMismatchTransition[],
) => {
  const roles = scriptIntegrityHashMismatchContractRoles(custody.contracts);
  let current = thread;
  for (const transition of transitions) {
    const built = await buildScriptIntegrityHashMismatchTransition(
      custody,
      current,
      transition,
    );
    const signed = await (await built.complete()).sign.withWallet().complete();
    const outputIndex = built.outputIndex();
    if (outputIndex === undefined || !withinBudget(custody, signed.toCBOR()))
      return false;
    current = {
      txHash: signed.toHash(),
      outputIndex: Number(outputIndex),
      address: roles[transition.next].spendingScriptAddress,
      assets: thread.assets,
      datum: transition.datum,
    };
  }
  return true;
};

/** The transition continuing `thread` at a paged walk role: the largest page,
 * from the configured maximum halving down to one item, whose transactions
 * all fit one transaction's budget. A page of one item that does not fit
 * throws its build error. */
export const fitScriptIntegrityHashMismatchPage = async ({
  custody,
  walk,
  role,
  thread,
}: {
  readonly custody: ScriptIntegrityHashMismatchCustody;
  readonly walk: ScriptIntegrityHashMismatchWalk;
  readonly role: ScriptIntegrityHashMismatchPagedRole;
  readonly thread: UTxO;
}): Promise<ScriptIntegrityHashMismatchTransition> => {
  const datum = thread.datum;
  if (typeof datum !== "string")
    throw new Error(`scriptIntegrityHashMismatch ${role} thread has no datum`);
  let page = walk.page(role, datum);
  for (;;) {
    let error: unknown = new Error(
      `scriptIntegrityHashMismatch ${role} page of ${page.size.toString()} exceeds the transaction budget`,
    );
    const fits = await pageFits(custody, thread, page.transitions).catch(
      (failure: unknown) => {
        error = failure;
        return false;
      },
    );
    if (fits) return page.transitions[0]!;
    if (page.size === 1) throw error;
    page = walk.page(role, datum, Math.floor(page.size / 2));
  }
};
