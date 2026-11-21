import type { ComputationThreadStep } from "./computation-thread-steps.js";
import { historicalTransactions } from "./raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import { authenticateFamilySnapshot } from "./raw-l1-family-derivation.derive-terminal.js";
import {
  type FraudProofRawL1FamilyDefinition,
  outputQuantity,
  ownsThread,
  rawToUtxo,
  unitHolders,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

/**
 * Our live computation thread on the header. Init, Open and a case's
 * VerifySource let any number of threads stand on one header, each carrying
 * the same unit; another prover's, or one whose datum does not decode, is no
 * part of our workflow. Among ours the newest comes first, so the successor
 * our own last move created stays the thread we act on, then the furthest
 * step, then the lowest out-ref.
 */
export const ownThread = ({
  snapshot,
  definition,
  threadUnit,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly threadUnit: string;
}):
  | { readonly step: ComputationThreadStep; readonly raw: FraudProofRawL1Utxo }
  | undefined => {
  const createdAt = new Map(
    snapshot.transactions.map(
      (transaction) =>
        [transaction.txHash, BigInt(transaction.inclusionPoint.slot)] as const,
    ),
  );
  const created = (raw: FraudProofRawL1Utxo) =>
    createdAt.get(raw.outRef.split("#")[0]!) ?? -1n;
  const threads = definition.computationThread.steps.flatMap((step, index) =>
    unitHolders({ snapshot, role: step.role, unit: threadUnit })
      .filter((raw) =>
        ownsThread({
          raw,
          schema: step.datumSchema,
          proverCredential: definition.proverCredential,
        }),
      )
      .map((raw) => ({ step: (index + 1) as ComputationThreadStep, raw })),
  );
  return threads.reduce<(typeof threads)[number] | undefined>(
    (best, candidate) => {
      if (best === undefined) return candidate;
      const age = created(candidate.raw) - created(best.raw);
      if (age !== 0n) return age > 0n ? candidate : best;
      if (candidate.step !== best.step)
        return candidate.step > best.step ? candidate : best;
      return candidate.raw.outRef < best.raw.outRef ? candidate : best;
    },
    undefined,
  );
};

/** One of our threads still holding funds on the header. */
export type FraudProofStandingThread = Readonly<{
  threadOutRef: string;
  step: ComputationThreadStep;
  lovelace: string;
}>;

/** Our threads on the header as L1 shows them: the one still standing, if
 * any, and every transaction that spent one of ours, at its depth. */
export type FraudProofOwnThreads = Readonly<{
  standing?: FraudProofStandingThread;
  spends: readonly Readonly<{
    txHash: string;
    threadOutRef: string;
    confirmationDepth: number;
  }>[];
}>;

export const standingThreadOf = (
  thread: NonNullable<ReturnType<typeof ownThread>>,
): FraudProofStandingThread => ({
  threadOutRef: thread.raw.outRef,
  step: thread.step,
  lovelace: (rawToUtxo(thread.raw).assets.lovelace ?? 0n).toString(),
});

/**
 * Our threads over an admitted family snapshot, by the one ownership rule of
 * `ownThread`: a thread at one of the family's step addresses, holding one
 * thread unit, under a step datum naming our key.
 */
export const deriveFraudProofRawL1OwnThreads = ({
  snapshot,
  definition,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
}): FraudProofOwnThreads => {
  const { threadUnit } = authenticateFamilySnapshot({ snapshot, definition });
  const standing = ownThread({ snapshot, definition, threadUnit });
  const ours = (raw: FraudProofRawL1Utxo) => {
    if (outputQuantity(raw, threadUnit) !== 1n) return false;
    const address = rawToUtxo(raw).address;
    return definition.computationThread.steps.some(
      (step) =>
        step.address === address &&
        ownsThread({
          raw,
          schema: step.datumSchema,
          proverCredential: definition.proverCredential,
        }),
    );
  };
  const spends = historicalTransactions({ snapshot, unit: threadUnit }).flatMap(
    (transaction) =>
      transaction.resolvedInputs.filter(ours).map((raw) => ({
        txHash: transaction.txHash,
        threadOutRef: raw.outRef,
        confirmationDepth: transaction.confirmationDepth,
      })),
  );
  return {
    ...(standing === undefined ? {} : { standing: standingThreadOf(standing) }),
    spends,
  };
};
