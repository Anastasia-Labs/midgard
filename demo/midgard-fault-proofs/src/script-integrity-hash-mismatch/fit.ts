/** Sizes each page of the `scriptIntegrityHashMismatch` all-purposes walk to
 * one transaction's budget (`fitPage`). A page opens one MPF proof per item
 * (two for a spend). */
import type { UTxO } from "@lucid-evolution/lucid";

import { fitPage } from "../page-fit.js";
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
  const roles = scriptIntegrityHashMismatchContractRoles(custody.contracts);
  return fitPage({
    lucid: custody.lucid,
    thread,
    label: `scriptIntegrityHashMismatch ${role}`,
    page: (size) => walk.page(role, datum, size),
    build: (current, transition) =>
      buildScriptIntegrityHashMismatchTransition(custody, current, transition),
    nextAddress: (transition) => roles[transition.next].spendingScriptAddress,
  });
};
