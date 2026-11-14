import {
  faultRemovalOf,
  type IndexedStateQueueHeader,
  readIndexedStateQueueHeader,
} from "./honest-successor.observables.js";
import {
  HELD_AWAITING_DA_REMOVED_BEFORE_ATTESTATION,
  type JourneySuccessorStage,
} from "./honest-successor.stage.js";

/**
 * The fault's state-queue outputs as Kupo indexed them, oldest first, and
 * its DA attestation: the fault's block time, read from its commit's
 * output, and its DA apply, the transaction whose output first carried the
 * fault attested, or null when none did. Every output of the fault's unit is
 * read, spent ones too, so the apply is seen after the fault was removed.
 */
export const readFaultAttestation = async (
  input: Readonly<{
    kupoUrl: string;
    policyId: string;
    faultHeaderHash: string;
    slotToUnixTime(slot: number): number;
  }>,
  fetchImpl: typeof fetch = fetch,
): Promise<{
  outputs: IndexedStateQueueHeader[];
  blockTimeMs: number;
  apply: Readonly<{ txHash: string; slot: number; atMs: number }> | null;
  lagAfterBlockTimeMs: number | null;
}> => {
  const outputs = await readIndexedStateQueueHeader(
    {
      kupoUrl: input.kupoUrl,
      policyId: input.policyId,
      headerHash: input.faultHeaderHash,
    },
    fetchImpl,
  );
  const [commit] = outputs;
  if (commit === undefined)
    throw new Error(
      `Kupo indexes no state-queue output of the fault ${input.faultHeaderHash}`,
    );
  const applied = outputs.find(({ attested }) => attested);
  const apply =
    applied === undefined
      ? null
      : {
          txHash: applied.txHash,
          slot: applied.createdAtSlot,
          atMs: input.slotToUnixTime(applied.createdAtSlot),
        };
  return {
    outputs,
    blockTimeMs: commit.blockTimeMs,
    apply,
    lagAfterBlockTimeMs:
      apply === null ? null : apply.atMs - commit.blockTimeMs,
  };
};

/**
 * Observable 1 without a `landed_block_invalid` report: our node reported
 * the fault only awaiting its DA, which it does while the fault is
 * unattested (#925). That is accepted only when L1 shows the fault's output
 * spent by the watcher's own removal strictly before the fault's DA apply,
 * or no DA apply at all. A DA apply in the removal's slot is not before it.
 * Otherwise names what L1 does not show.
 */
export const faultRemovedBeforeAttestation = (
  attestation: Awaited<ReturnType<typeof readFaultAttestation>>,
  removalTxHash: string,
  slotToUnixTime: (slot: number) => number,
): JourneySuccessorStage | string => {
  const removed = faultRemovalOf(attestation.outputs, removalTxHash);
  if (removed === null)
    return `Kupo shows no output of the fault spent by the watcher's removal ${removalTxHash}`;
  const removal = { ...removed, atMs: slotToUnixTime(removed.slot) };
  const { apply } = attestation;
  if (apply !== null && apply.slot <= removal.slot)
    return `the fault's DA apply ${apply.txHash} at slot ${apply.slot.toString()} is not strictly after the removal at slot ${removal.slot.toString()}`;
  return {
    successorStage: HELD_AWAITING_DA_REMOVED_BEFORE_ATTESTATION,
    successorStageEvidence: { removal, attestationApply: apply },
  };
};
