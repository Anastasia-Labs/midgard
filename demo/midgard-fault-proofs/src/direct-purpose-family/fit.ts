/** Sizes each page of a direct purpose thread's proof-opening runs to one
 * transaction's budget (`fitPage`). */
import type { UTxO } from "@lucid-evolution/lucid";

import { fitPage } from "../page-fit.js";
import type { DirectPurposeTransition } from "./custody.js";
import {
  type DirectPurposeRole,
  directPurposeRoleValidator,
} from "./family.js";
import type { DirectPurposePaging } from "./paging.js";
import {
  buildDirectPurposeTransition,
  type DirectPurposeCustody,
} from "./submit.js";

/** The transition continuing `thread` at `role`: at a page boundary, the
 * first of the largest page, from the configured maximum halving down to one
 * item, whose transactions all fit one transaction's budget; the planned
 * transition elsewhere. */
export const fitDirectPurposeTransition = async ({
  custody,
  paging,
  role,
  thread,
}: {
  readonly custody: DirectPurposeCustody;
  readonly paging: DirectPurposePaging;
  readonly role: DirectPurposeRole;
  readonly thread: UTxO;
}): Promise<DirectPurposeTransition> => {
  const datum = thread.datum;
  if (typeof datum !== "string")
    throw new Error(`${custody.family.label} ${role} thread has no datum`);
  if (!paging.isPageStart(role, datum)) return paging.transitionAt(role, datum);
  return fitPage({
    lucid: custody.lucid,
    thread,
    label: `${custody.family.label} ${role}`,
    page: (size) => paging.page(role, datum, size),
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
