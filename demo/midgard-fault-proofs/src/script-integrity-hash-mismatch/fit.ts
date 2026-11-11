/** Sizes each page of the `scriptIntegrityHashMismatch` all-purposes walk to
 * one transaction's budget (`fitPage`). A page opens one MPF proof per item
 * (two for a spend). */
import type { UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import { directPurposeRoleValidator } from "../direct-purpose-family/family.js";
import {
  buildDirectPurposeTransition,
  type DirectPurposeCustody,
} from "../direct-purpose-family/submit.js";
import { fitPage } from "../page-fit.js";
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
  readonly custody: DirectPurposeCustody;
  readonly walk: ScriptIntegrityHashMismatchWalk;
  readonly role: ScriptIntegrityHashMismatchPagedRole;
  readonly thread: UTxO;
}): Promise<DirectPurposeTransition> => {
  const datum = thread.datum;
  if (typeof datum !== "string")
    throw new Error(`scriptIntegrityHashMismatch ${role} thread has no datum`);
  return fitPage({
    lucid: custody.lucid,
    thread,
    label: `scriptIntegrityHashMismatch ${role}`,
    page: (size) => walk.page(role, datum, size),
    build: (current, transition) =>
      buildDirectPurposeTransition(custody, current, transition),
    nextAddress: (transition) =>
      directPurposeRoleValidator(
        custody.family,
        custody.contracts,
        transition.next,
      ).spendingScriptAddress,
  });
};
