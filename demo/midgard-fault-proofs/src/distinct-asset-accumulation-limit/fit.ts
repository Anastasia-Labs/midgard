/** Sizes each paged step of the `distinctAssetAccumulationLimit` fold to one
 * transaction's budget. A spend page opens one MPF proof per input, an output
 * step walks up to its work budget and a mint step counts up to its policy
 * count; the step is built at the configured maximum and halved until it
 * fits. */
import type { UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import { directPurposeRoleValidator } from "../direct-purpose-family/family.js";
import {
  buildDirectPurposeTransition,
  type DirectPurposeCustody,
} from "../direct-purpose-family/submit.js";
import { scriptDecodeLedgerLimits } from "../script-decode/record.js";
import { transactionExUnits } from "../script-decode/submit.js";
import type {
  DistinctAssetAccumulationLimitPagedRole,
  DistinctAssetAccumulationLimitWalk,
} from "./walk.js";

/** Whether a completed signed transaction is within the byte limit and keeps
 * the 20% execution reserve below the ledger's limits. */
const withinBudget = (custody: DirectPurposeCustody, cbor: string) => {
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
  custody: DirectPurposeCustody,
  thread: UTxO,
  transitions: readonly DirectPurposeTransition[],
) => {
  let current = thread;
  for (const transition of transitions) {
    const built = await buildDirectPurposeTransition(
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
      address: directPurposeRoleValidator(
        custody.family,
        custody.contracts,
        transition.next,
      ).spendingScriptAddress,
      assets: thread.assets,
      datum: transition.datum,
    };
  }
  return true;
};

/** The transition continuing `thread` at a paged fold role: the largest page,
 * from the configured maximum halving down to one unit, whose transactions
 * all fit one transaction's budget. A page of one unit that does not fit
 * throws its build error. */
export const fitDistinctAssetAccumulationLimitPage = async ({
  custody,
  walk,
  role,
  thread,
}: {
  readonly custody: DirectPurposeCustody;
  readonly walk: DistinctAssetAccumulationLimitWalk;
  readonly role: DistinctAssetAccumulationLimitPagedRole;
  readonly thread: UTxO;
}): Promise<DirectPurposeTransition> => {
  const datum = thread.datum;
  if (typeof datum !== "string")
    throw new Error(
      `distinctAssetAccumulationLimit ${role} thread has no datum`,
    );
  let page = walk.page(role, datum);
  for (;;) {
    let error: unknown = new Error(
      `distinctAssetAccumulationLimit ${role} page of ${page.size.toString()} exceeds the transaction budget`,
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
