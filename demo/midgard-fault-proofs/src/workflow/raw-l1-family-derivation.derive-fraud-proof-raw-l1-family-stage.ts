import { CML } from "@lucid-evolution/lucid";

import { selectOwnFraudProofRecord } from "../fraud-proof-record-selection.js";
import type { ComputationThreadStep } from "./computation-thread-steps.js";
import { type FraudProofWorkflowTerminal } from "./journal.js";
import { historicalTransactions } from "./raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import {
  authenticateFamilySnapshot,
  deriveTerminal,
  type FraudProofRawL1TerminalNotOurs,
} from "./raw-l1-family-derivation.derive-terminal.js";
import type { FraudProofRawL1HistoryReader } from "./raw-l1-family-derivation.lost-defense-slash.js";
import {
  FraudProofRawL1DerivationError,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1FamilyStage,
  type FraudProofRawL1TerminalDefinition,
  mintQuantity,
  outRef,
  ownsThread,
  scope,
  stateQueueTopology,
  unitHolders,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";
import { type VerifiedFraudProofReleaseEconomicsPolicy } from "./release-economics-policy.js";
import {
  FraudProofResolvedByAnotherRecordError,
  isFraudProofResolvedByAnotherRecordError,
  recordBackedRemoval,
} from "./resolved-by-another-record.js";

/** Our permanent proof token on the header, by the one rule of
 * `selectOwnFraudProofRecord`: once the header is gone, the record its
 * removal referenced comes first. */
const ownProof = ({
  snapshot,
  definition,
  stateUnit,
  proofUnit,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly stateUnit: string;
  readonly proofUnit: string;
}): FraudProofRawL1Utxo | undefined =>
  selectOwnFraudProofRecord({
    records: unitHolders({
      snapshot,
      role: "permanent_proof_token",
      unit: proofUnit,
    }),
    outRefOf: (raw) => raw.outRef,
    datumOf: (raw) => raw.datumCbor,
    prover: definition.proverCredential,
    referenced: () =>
      new Set(
        historicalTransactions({ snapshot, unit: stateUnit })
          .filter(
            (transaction) =>
              mintQuantity(
                CML.TransactionBody.from_cbor_hex(transaction.bodyCbor),
                stateUnit,
              ) === -1n,
          )
          .flatMap((transaction) =>
            transaction.resolvedReferenceInputs.map(
              (candidate) => candidate.outRef,
            ),
          ),
      ),
  });

/**
 * Our live computation thread on the header. Init, Open and a case's
 * VerifySource let any number of threads stand on one header, each carrying
 * the same unit; another prover's, or one whose datum does not decode, is no
 * part of our workflow. Among ours the newest comes first, so the successor
 * our own last move created stays the thread we act on, then the furthest
 * step, then the lowest out-ref.
 */
const ownThread = ({
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

const assertStepAddresses = (
  snapshot: FraudProofRawL1Snapshot,
  definition: FraudProofRawL1TerminalDefinition,
): void => {
  definition.computationThread.steps.forEach((step, index) => {
    if (scope(snapshot, step.role).address !== step.address) {
      throw new Error(
        `raw L1 snapshot changed step ${(index + 1).toString()} address`,
      );
    }
  });
};

/** What read-only completion finds on raw L1: the header still live; gone
 * with no terminal of ours (`deriveTerminal`'s not-ours cases, or no record of
 * ours at all); or removed against our record, with our terminal. */
export type FraudProofRawL1CompletedTerminal =
  | Readonly<{ kind: "target_live" }>
  | FraudProofRawL1TerminalNotOurs
  | Readonly<{ kind: "removed"; terminal: FraudProofWorkflowTerminal }>;

/** Read-only completion applicability: a live authenticated target invalidates
 * completion. No computation datum is decoded or used to authorize execution;
 * live threads, ours or another case's, do not bear on a removed header. */
export const deriveFraudProofRawL1CompletedTerminal = async ({
  snapshot,
  definition,
  releaseEconomics,
  history,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly history?: FraudProofRawL1HistoryReader | undefined;
}): Promise<FraudProofRawL1CompletedTerminal> => {
  const { stateUnit, proofUnit } = authenticateFamilySnapshot({
    snapshot,
    definition,
  });
  assertStepAddresses(snapshot, definition);
  const topology = await stateQueueTopology({ snapshot, definition });
  if (topology.target !== undefined) return { kind: "target_live" };
  const proof = ownProof({ snapshot, definition, stateUnit, proofUnit });
  if (proof === undefined)
    return {
      kind: "terminal_not_ours",
      reason: "the header is gone and no record of ours stands",
    };
  const derived = await deriveTerminal({
    snapshot,
    definition,
    stateUnit,
    proofUnit,
    proof,
    releaseEconomics,
    history,
  });
  return derived.kind === "ours"
    ? { kind: "removed", terminal: derived.terminal }
    : derived;
};

const deriveStage = async ({
  snapshot,
  definition,
  releaseEconomics,
  history,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly history?: FraudProofRawL1HistoryReader | undefined;
}): Promise<FraudProofRawL1FamilyStage> => {
  const { stateUnit, threadUnit, proofUnit } = authenticateFamilySnapshot({
    snapshot,
    definition,
  });
  assertStepAddresses(snapshot, definition);
  // Our proof decides the stage once it exists: another case's thread on the
  // same header, still open, does not hold back our removal.
  const proof = ownProof({ snapshot, definition, stateUnit, proofUnit });
  const topology = await stateQueueTopology({ snapshot, definition });
  if (topology.target === undefined) {
    const derived =
      proof === undefined
        ? undefined
        : await deriveTerminal({
            snapshot,
            definition,
            stateUnit,
            proofUnit,
            proof,
            releaseEconomics,
            history,
          });
    if (derived?.kind === "ours")
      return { kind: "removed", terminal: derived.terminal };
    // No terminal of ours stands. A removal against a record of the header,
    // whoever's, resolved it; any other exit leaves a derivation failure,
    // which the orchestrator records as a stall.
    const resolution = recordBackedRemoval({ snapshot, definition, stateUnit });
    if (resolution !== undefined)
      throw new FraudProofResolvedByAnotherRecordError(resolution);
    throw new Error(
      derived === undefined
        ? "fraudulent header disappeared without a retained proof token"
        : `no terminal of ours stands: ${derived.reason}`,
    );
  }
  const stateQueueBlockOutRef = outRef(topology.target.utxo);
  if (proof !== undefined) {
    return {
      kind: "proof_token",
      fraudProofOutRef: proof.outRef,
      stateQueueBlockOutRef,
      nextRemovalOutRef:
        topology.successor === undefined
          ? stateQueueBlockOutRef
          : outRef(topology.successor.utxo),
    };
  }
  const thread = ownThread({ snapshot, definition, threadUnit });
  if (thread !== undefined) {
    return {
      kind: "step",
      step: thread.step,
      threadOutRef: thread.raw.outRef,
      stateQueueBlockOutRef,
    };
  }
  return { kind: "not_started", stateQueueBlockOutRef };
};

/**
 * The family stage over an admitted raw L1 snapshot. A failure of the
 * derivation itself is a `FraudProofRawL1DerivationError`, which the
 * orchestrator records against this workflow alone. A failure of a further
 * history read, an L1 wait among them, reaches the caller unchanged, as does
 * a `FraudProofResolvedByAnotherRecordError`: the header is gone through a
 * removal against a record that is not ours.
 */
export const deriveFraudProofRawL1FamilyStage = async (input: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  /** Further token histories, for a bond a lost-defense receipt slashed. */
  readonly history?: FraudProofRawL1HistoryReader | undefined;
}): Promise<FraudProofRawL1FamilyStage> => {
  const read = input.history;
  const readFailures = new WeakSet<object>();
  const history: FraudProofRawL1HistoryReader | undefined =
    read === undefined
      ? undefined
      : async (unit) => {
          try {
            return await read(unit);
          } catch (cause) {
            if (typeof cause === "object" && cause !== null)
              readFailures.add(cause);
            throw cause;
          }
        };
  try {
    return await deriveStage({ ...input, history });
  } catch (cause) {
    // `has` is false for a primitive; it never throws.
    if (
      readFailures.has(cause as object) ||
      isFraudProofResolvedByAnotherRecordError(cause)
    )
      throw cause;
    throw new FraudProofRawL1DerivationError(input.definition.category, cause);
  }
};
