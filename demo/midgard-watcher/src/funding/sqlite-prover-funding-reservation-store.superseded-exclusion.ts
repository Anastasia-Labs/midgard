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

/** A superseded attempt with its exclusion set, as a later attempt must
 * treat it. */
export type UncoveredSupersededAttempt = Readonly<{
  transition: WorkflowFundingPreparedTransition;
  exclusion: readonly string[];
}>;

/**
 * An attempt's lineage: the attempt itself and, recursively, every recorded
 * attempt whose outputs it spends. The exclusion set is every ordinary input
 * and every consumed funding input along that lineage. Spending any of them
 * excludes the attempt: it cannot land without each ancestor landing first.
 */
const lineage = (
  transition: WorkflowFundingPreparedTransition,
  recorded: ReadonlyMap<string, WorkflowFundingPreparedTransition>,
): Readonly<{
  exclusion: readonly string[];
  ancestors: ReadonlySet<string>;
}> => {
  const exclusion = new Set<string>();
  const visited = new Set<string>();
  const pending = [transition];
  while (pending.length !== 0) {
    const next = pending.pop()!;
    if (visited.has(next.transactionHash)) continue;
    visited.add(next.transactionHash);
    for (const outRef of [
      ...signedOrdinaryInputs(next.signedTransactionCborHex),
      ...next.consumedOutRefs,
    ]) {
      exclusion.add(outRef);
      const parent = recorded.get(outRef.slice(0, outRef.indexOf("#")));
      if (parent !== undefined) pending.push(parent);
    }
  }
  visited.delete(transition.transactionHash);
  return { exclusion: [...exclusion].sort(), ancestors: visited };
};

/**
 * Owner ruling (whichever lands wins): a superseded attempt holds nothing,
 * but a later attempt must be mutually exclusive with it. Any ordinary input
 * of its lineage is enough, whether a funding input or a protocol input such
 * as the challenged node. A superseded attempt is covered once a recorded,
 * not superseded, attempt that is not its own ancestor spends such an input;
 * retirement past k ends it.
 */
export const uncoveredSupersededAttempts = ({
  abandoned,
  submissions,
}: {
  readonly abandoned: readonly Abandoned[];
  readonly submissions: readonly WorkflowFundingPreparedTransition[];
}): readonly UncoveredSupersededAttempt[] => {
  const superseded = new Set(
    abandoned.map(({ transition }) => transition.transactionHash),
  );
  const recorded = new Map(
    [...submissions, ...abandoned.map(({ transition }) => transition)].map(
      (transition) => [transition.transactionHash, transition] as const,
    ),
  );
  const live = submissions
    .filter(({ transactionHash }) => !superseded.has(transactionHash))
    .map(({ transactionHash, signedTransactionCborHex }) => ({
      transactionHash,
      inputs: signedOrdinaryInputs(signedTransactionCborHex),
    }));
  return abandoned
    .filter(({ handoff }) => handoff.reconciliation.retirement === undefined)
    .map(({ transition }) => {
      const { exclusion, ancestors } = lineage(transition, recorded);
      return { transition, exclusion, ancestors };
    })
    .filter(
      ({ exclusion, ancestors }) =>
        !live.some(
          ({ transactionHash, inputs }) =>
            !ancestors.has(transactionHash) && shareAnInput(inputs, exclusion),
        ),
    )
    .map(({ transition, exclusion }) => ({ transition, exclusion }));
};

/** Per uncovered attempt, its exclusion set. A fresh attempt draws one of
 * each that is still a reserved funding input, so that it is mutually
 * exclusive with that attempt even when it spends no common protocol input. */
export const supersededAttemptFundingOutRefs = (
  uncovered: readonly UncoveredSupersededAttempt[],
): readonly (readonly string[])[] =>
  uncovered.map(({ exclusion }) => exclusion);

/**
 * A fresh attempt is admitted when, for each uncovered superseded attempt, it
 * shares an input with that attempt's exclusion set, or nothing in that set
 * is still a reserved funding input. In the second case no shared input is
 * possible: whatever lands first wins, and protocol state makes a duplicate
 * proof fail on-chain. Nothing waits for retirement past k.
 */
export const excludesEveryUncoveredAttempt = ({
  uncovered,
  signedTransactionCborHex,
  reservedFundingOutRefs,
}: {
  readonly uncovered: readonly UncoveredSupersededAttempt[];
  readonly signedTransactionCborHex: string;
  readonly reservedFundingOutRefs: readonly string[];
}): boolean => {
  if (uncovered.length === 0) return true;
  const inputs = signedOrdinaryInputs(signedTransactionCborHex);
  return uncovered.every(
    ({ exclusion }) =>
      shareAnInput(inputs, exclusion) ||
      !shareAnInput(exclusion, reservedFundingOutRefs),
  );
};
