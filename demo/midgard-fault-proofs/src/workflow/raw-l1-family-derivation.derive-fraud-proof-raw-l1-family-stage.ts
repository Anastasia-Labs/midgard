import { type FraudProofWorkflowTerminal } from "./journal.js";
import {
  authenticateFamilySnapshot,
  deriveTerminal,
} from "./raw-l1-family-derivation.derive-terminal.js";
import {
  currentUnit,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1FamilyStage,
  type FraudProofRawL1TerminalDefinition,
  outRef,
  requireProofDatum,
  requireThreadDatum,
  scope,
  stateQueueTopology,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type { FraudProofRawL1Snapshot } from "./raw-l1-snapshot.js";
import { type VerifiedFraudProofReleaseEconomicsPolicy } from "./release-economics-policy.js";

/** Read-only completion applicability: a live authenticated target invalidates
 * completion. No computation datum is decoded or used to authorize execution. */
export const deriveFraudProofRawL1CompletedTerminal = async ({
  snapshot,
  definition,
  releaseEconomics,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
}): Promise<FraudProofWorkflowTerminal | null> => {
  const { stateUnit, threadUnit, proofUnit } = authenticateFamilySnapshot({
    snapshot,
    definition,
  });
  const threads = definition.computationThread.steps.flatMap((step) => {
    if (scope(snapshot, step.role).address !== step.address)
      throw new Error("raw L1 terminal snapshot changed computation address");
    const current = currentUnit({
      snapshot,
      role: step.role,
      unit: threadUnit,
    });
    return current === undefined ? [] : [current];
  });
  if (threads.length > 1)
    throw new Error("raw L1 snapshot has more than one live computation step");
  const proof = currentUnit({
    snapshot,
    role: "permanent_proof_token",
    unit: proofUnit,
  });
  if (proof !== undefined)
    requireProofDatum({
      raw: proof,
      proverCredential: definition.proverCredential,
    });
  if (proof !== undefined && threads.length > 0)
    throw new Error(
      "raw L1 snapshot has both a computation thread and proof token",
    );
  const topology = await stateQueueTopology({ snapshot, definition });
  if (topology.target !== undefined) return null;
  if (proof === undefined || threads.length > 0)
    throw new Error(
      "fraudulent header disappeared without a retained proof token",
    );
  return deriveTerminal({
    snapshot,
    definition,
    stateUnit,
    proofUnit,
    proof,
    releaseEconomics,
  });
};

export const deriveFraudProofRawL1FamilyStage = async ({
  snapshot,
  definition,
  releaseEconomics,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
}): Promise<FraudProofRawL1FamilyStage> => {
  const { stateUnit, threadUnit, proofUnit } = authenticateFamilySnapshot({
    snapshot,
    definition,
  });
  const threads = definition.computationThread.steps.flatMap((step, index) => {
    if (scope(snapshot, step.role).address !== step.address) {
      throw new Error(
        `raw L1 snapshot changed step ${(index + 1).toString()} address`,
      );
    }
    const current = currentUnit({
      snapshot,
      role: step.role,
      unit: threadUnit,
    });
    if (current === undefined) return [];
    requireThreadDatum({
      raw: current,
      schema: step.datumSchema,
      proverCredential: definition.proverCredential,
      label: `computation step ${(index + 1).toString()}`,
    });
    return [
      {
        step: (index + 1) as
          | 1
          | 2
          | 3
          | 4
          | 5
          | 6
          | 7
          | 8
          | 9
          | 10
          | 11
          | 12
          | 13,
        raw: current,
      },
    ];
  });
  if (threads.length > 1) {
    throw new Error("raw L1 snapshot has more than one live computation step");
  }
  const proof = currentUnit({
    snapshot,
    role: "permanent_proof_token",
    unit: proofUnit,
  });
  if (proof !== undefined) {
    requireProofDatum({
      raw: proof,
      proverCredential: definition.proverCredential,
    });
  }
  if (proof !== undefined && threads.length > 0) {
    throw new Error(
      "raw L1 snapshot has both a computation thread and proof token",
    );
  }
  const topology = await stateQueueTopology({ snapshot, definition });
  if (topology.target === undefined) {
    if (proof === undefined || threads.length > 0) {
      throw new Error(
        "fraudulent header disappeared without a retained proof token",
      );
    }
    return {
      kind: "removed",
      terminal: await deriveTerminal({
        snapshot,
        definition,
        stateUnit,
        proofUnit,
        proof,
        releaseEconomics,
      }),
    };
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
  const thread = threads[0];
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
