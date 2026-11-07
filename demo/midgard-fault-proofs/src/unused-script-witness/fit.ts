/** Sizes each proof-opening page of the `unusedScriptWitness` thread to one
 * transaction's budget (`fitPage`): the shared source walk's pages, through
 * the kit's paging, and the all-purposes walk's pages. A page opens one or
 * two MPF proofs per item. */
import type { UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeRole } from "../direct-purpose-family/family.js";
import type { DirectPurposePaging } from "../direct-purpose-family/paging.js";
import { fitPage } from "../page-fit.js";
import { unusedScriptWitnessContractRoles } from "./contracts.js";
import {
  unusedScriptWitnessSharedTransition,
  type UnusedScriptWitnessTransition,
} from "./custody.js";
import {
  buildUnusedScriptWitnessTransition,
  type UnusedScriptWitnessCustody,
} from "./submit.js";
import type {
  UnusedScriptWitnessPagedRole,
  UnusedScriptWitnessWalk,
} from "./walk.js";

const threadDatum = (thread: UTxO, role: string) => {
  if (typeof thread.datum !== "string")
    throw new Error(`unusedScriptWitness ${role} thread has no datum`);
  return thread.datum;
};

const fitWith = (
  custody: UnusedScriptWitnessCustody,
  thread: UTxO,
  role: string,
  page: (size?: number) => {
    readonly size: number;
    readonly transitions: readonly UnusedScriptWitnessTransition[];
  },
) => {
  const roles = unusedScriptWitnessContractRoles(custody.contracts);
  return fitPage({
    lucid: custody.lucid,
    thread,
    label: `unusedScriptWitness ${role}`,
    page,
    build: (current, transition) =>
      buildUnusedScriptWitnessTransition(custody, current, transition),
    nextAddress: (transition) => roles[transition.next].spendingScriptAddress,
  });
};

/** The transition continuing `thread` at a paged all-purposes walk role:
 * the first of the largest page, from the configured maximum halving down to
 * one item, whose transactions all fit one transaction's budget. A page of
 * one item that does not fit throws its build error. */
export const fitUnusedScriptWitnessPage = async ({
  custody,
  walk,
  role,
  thread,
}: {
  readonly custody: UnusedScriptWitnessCustody;
  readonly walk: UnusedScriptWitnessWalk;
  readonly role: UnusedScriptWitnessPagedRole;
  readonly thread: UTxO;
}): Promise<UnusedScriptWitnessTransition> => {
  const datum = threadDatum(thread, role);
  return fitWith(custody, thread, role, (size) => walk.page(role, datum, size));
};

/** The transition continuing `thread` at a shared source role: at a page
 * boundary, the first of the largest fitting page; the planned transition
 * elsewhere. */
export const fitUnusedScriptWitnessSourceTransition = async ({
  custody,
  paging,
  role,
  thread,
}: {
  readonly custody: UnusedScriptWitnessCustody;
  readonly paging: DirectPurposePaging;
  readonly role: DirectPurposeRole;
  readonly thread: UTxO;
}): Promise<UnusedScriptWitnessTransition> => {
  const datum = threadDatum(thread, role);
  if (!paging.isPageStart(role, datum))
    return unusedScriptWitnessSharedTransition(
      paging.transitionAt(role, datum),
    );
  return fitWith(custody, thread, role, (size) => {
    const page = paging.page(role, datum, size);
    return {
      size: page.size,
      transitions: page.transitions.map(unusedScriptWitnessSharedTransition),
    };
  });
};
