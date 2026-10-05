import type {
  WorkflowFundingAbandonmentHandoff,
  WorkflowFundingPreparedTransition,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import { transactionInputOutRefs } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";

type Abandoned = Readonly<{
  transition: WorkflowFundingPreparedTransition;
  handoff: WorkflowFundingAbandonmentHandoff;
}>;

/** Every ordinary input the signed body spends: funding and protocol inputs
 * alike. Collateral does not count, because a valid transaction leaves it. */
export const signedOrdinaryInputs = (
  signedTransactionCborHex: string,
): readonly string[] =>
  transactionInputOutRefs(
    CML.Transaction.from_cbor_hex(signedTransactionCborHex).body().inputs(),
  );

const shareAnInput = (
  left: readonly string[],
  right: readonly string[],
): boolean => left.some((outRef) => right.includes(outRef));

/**
 * Owner ruling (whichever lands wins): a superseded attempt holds nothing,
 * but a later attempt must be mutually exclusive with it. Any ordinary input
 * both spend is enough, whether a funding input or a protocol input such as
 * the challenged node. A superseded attempt is covered once a recorded, not
 * superseded, attempt shares such an input; retirement past k ends it.
 */
export const uncoveredSupersededAttempts = ({
  abandoned,
  submissions,
}: {
  readonly abandoned: readonly Abandoned[];
  readonly submissions: readonly WorkflowFundingPreparedTransition[];
}): readonly WorkflowFundingPreparedTransition[] => {
  const superseded = new Set(
    abandoned.map(({ transition }) => transition.transactionHash),
  );
  const live = submissions
    .filter(({ transactionHash }) => !superseded.has(transactionHash))
    .map(({ signedTransactionCborHex }) =>
      signedOrdinaryInputs(signedTransactionCborHex),
    );
  return abandoned
    .filter(({ handoff }) => handoff.reconciliation.retirement === undefined)
    .map(({ transition }) => transition)
    .filter((transition) => {
      const inputs = signedOrdinaryInputs(transition.signedTransactionCborHex);
      return !live.some((attempt) => shareAnInput(attempt, inputs));
    });
};

/** Per uncovered attempt, the funding inputs it spent. A fresh attempt draws
 * one of each that is still reserved, so that it is mutually exclusive with
 * that attempt even when it spends no common protocol input. */
export const supersededAttemptFundingOutRefs = (
  uncovered: readonly WorkflowFundingPreparedTransition[],
): readonly (readonly string[])[] =>
  uncovered.map(({ consumedOutRefs }) => [...consumedOutRefs].sort());

/**
 * A fresh attempt is admitted when, for each uncovered superseded attempt, it
 * shares an ordinary input with that attempt, or no input of that attempt is
 * still a reserved funding input. In the second case no shared input is
 * possible: whatever lands first wins, and protocol state makes a duplicate
 * proof fail on-chain. Nothing waits for retirement past k.
 */
export const excludesEveryUncoveredAttempt = ({
  uncovered,
  signedTransactionCborHex,
  reservedFundingOutRefs,
}: {
  readonly uncovered: readonly WorkflowFundingPreparedTransition[];
  readonly signedTransactionCborHex: string;
  readonly reservedFundingOutRefs: readonly string[];
}): boolean => {
  if (uncovered.length === 0) return true;
  const inputs = signedOrdinaryInputs(signedTransactionCborHex);
  return uncovered.every(
    (transition) =>
      shareAnInput(
        inputs,
        signedOrdinaryInputs(transition.signedTransactionCborHex),
      ) || !shareAnInput(transition.consumedOutRefs, reservedFundingOutRefs),
  );
};
