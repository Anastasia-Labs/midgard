/** Sizes each proof-opening or evaluation page of the
 * `executionNativeScriptInvalid` thread to one transaction's budget
 * (`fitPage`): the shared source and purpose pages, through the kit's
 * paging, and the signer and evaluation pages of the walk. */
import type { UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import {
  type DirectPurposeRole,
  directPurposeRoleValidator,
} from "../direct-purpose-family/family.js";
import type { DirectPurposePaging } from "../direct-purpose-family/paging.js";
import {
  buildDirectPurposeTransition,
  type DirectPurposeCustody,
} from "../direct-purpose-family/submit.js";
import { fitPage } from "../page-fit.js";
import type {
  ExecutionNativeScriptInvalidWalk,
  ExecutionNativeScriptInvalidWalkRole,
} from "./walk.js";

const threadDatum = (thread: UTxO, role: string) => {
  if (typeof thread.datum !== "string")
    throw new Error(`executionNativeScriptInvalid ${role} thread has no datum`);
  return thread.datum;
};

const fitWith = (
  custody: DirectPurposeCustody,
  thread: UTxO,
  role: string,
  page: (size?: number) => {
    readonly size: number;
    readonly transitions: readonly DirectPurposeTransition[];
  },
) => {
  return fitPage({
    lucid: custody.lucid,
    thread,
    label: `executionNativeScriptInvalid ${role}`,
    page,
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

/** The transition continuing `thread` at a signer or evaluation role: the
 * largest page, from the configured maximum halving down to one step, whose
 * transaction fits one transaction's budget. A page of one step that does
 * not fit throws its build error. */
export const fitExecutionNativeScriptInvalidWalkPage = async ({
  custody,
  walk,
  role,
  thread,
}: {
  readonly custody: DirectPurposeCustody;
  readonly walk: ExecutionNativeScriptInvalidWalk;
  readonly role: ExecutionNativeScriptInvalidWalkRole;
  readonly thread: UTxO;
}): Promise<DirectPurposeTransition> => {
  const datum = threadDatum(thread, role);
  return fitWith(custody, thread, role, (size) => walk.page(role, datum, size));
};

/** The transition continuing `thread` at a shared source or purpose role:
 * at a page boundary, the first of the largest fitting page; the planned
 * transition elsewhere. */
export const fitExecutionNativeScriptInvalidSourceTransition = async ({
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
  const datum = threadDatum(thread, role);
  if (!paging.isPageStart(role, datum)) return paging.transitionAt(role, datum);
  return fitWith(custody, thread, role, (size) =>
    paging.page(role, datum, size),
  );
};
