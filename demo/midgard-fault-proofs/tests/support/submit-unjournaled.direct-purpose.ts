/**
 * The direct-purpose transitions the tests drive, each built, submitted with
 * no journal and confirmed (see ./submit-unjournaled.ts), with the thread's
 * next output read back once it is live.
 */
import type { UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../../src/direct-purpose-family/custody.js";
import {
  buildDirectPurposeTransitionTx,
  type DirectPurposeCustody,
} from "../../src/direct-purpose-family/submit.js";
import { parseOutRef } from "../../src/runtime.js";
import { submitUnjournaled } from "../../src/workflow/transaction-boundary.js";

/** Submits one honest transition and returns the thread's next UTxO. */
export const submitDirectPurposeTransition = async (
  custody: DirectPurposeCustody,
  thread: UTxO,
  transition: DirectPurposeTransition,
): Promise<{ readonly txHash: string; readonly thread: UTxO }> => {
  const { transaction, nextThreadOutRef } =
    await buildDirectPurposeTransitionTx(custody, thread, transition);
  const txHash = await submitUnjournaled(custody.lucid, transaction, {
    label: `${custody.family.label} ${transition.label}`,
    awaitConfirmation: true,
  });
  const [next] = await custody.lucid.utxosByOutRef([
    parseOutRef(nextThreadOutRef, `${transition.label} next thread`),
  ]);
  if (next === undefined)
    throw new Error(`${transition.label}: next thread output absent`);
  return { txHash, thread: next };
};

/** Submits transitions in order; returns the thread at the last role. */
export const submitDirectPurposeTransitions = async (
  custody: DirectPurposeCustody,
  thread: UTxO,
  transitions: readonly DirectPurposeTransition[],
): Promise<UTxO> => {
  let current = thread;
  for (const transition of transitions)
    current = (
      await submitDirectPurposeTransition(custody, current, transition)
    ).thread;
  return current;
};
