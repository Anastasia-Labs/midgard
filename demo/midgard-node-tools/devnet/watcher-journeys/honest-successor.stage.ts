/** A transaction on L1, at its slot and that slot's time. */
export type JourneyL1Point = Readonly<{
  txHash: string;
  slot: number;
  atMs: number;
}>;

/**
 * Our node reported the fault only `landed_block_awaiting_da`, never
 * `landed_block_invalid`, and L1 shows the watcher's removal of the fault
 * strictly before any DA attestation of it was applied. While a block is
 * unattested our node reports an invalid block as awaiting its DA (#925), so
 * a fault removed before its attestation is never reported invalid.
 */
export const HELD_AWAITING_DA_REMOVED_BEFORE_ATTESTATION =
  "held-awaiting-da-removed-before-attestation";

/**
 * Whether the family's successor stage tested our node's refusal to build on
 * the fault, and on what evidence.
 * - `exercised`: our node held `landed_block_invalid` on the fault while it
 *   was the tail.
 * - `held-awaiting-da-removed-before-attestation`: our node held the fault
 *   awaiting its DA only, and the watcher's removal landed strictly before
 *   the fault's DA apply (`attestationApply`), or there was no DA apply.
 * - `not-reached-fault-removed-before-our-shift`: the watcher's removal of
 *   the fault landed strictly before our operator's first shift after the
 *   fault's commit, so our node never had a shift in which it could have
 *   built on the fault. That still passes the proof and the removal; the
 *   evidence is what L1 shows of the three.
 */
export type JourneySuccessorStage =
  | Readonly<{ successorStage: "exercised" }>
  | Readonly<{
      successorStage: typeof HELD_AWAITING_DA_REMOVED_BEFORE_ATTESTATION;
      successorStageEvidence: Readonly<{
        removal: JourneyL1Point;
        attestationApply: JourneyL1Point | null;
      }>;
    }>
  | Readonly<{
      successorStage: "not-reached-fault-removed-before-our-shift";
      successorStageEvidence: Readonly<{
        removal: Readonly<{ txHash: string; slot: number; atMs: number }>;
        ourFirstShiftStartMs: number;
        operator2ShiftEndMs: number;
      }>;
    }>;

/**
 * When the fault's DA attestation was applied after its block time (the
 * state-queue node's `block_time`). `apply` is the transaction whose output
 * first carried the fault attested, and `lagAfterBlockTimeMs` its slot's
 * time minus the block time; both are null when no DA apply landed.
 */
export type JourneyFaultAttestation = Readonly<{
  blockTimeMs: number;
  apply: JourneyL1Point | null;
  lagAfterBlockTimeMs: number | null;
}>;

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

const isSlot = (value: unknown): value is number =>
  Number.isSafeInteger(value) && (value as number) >= 0;

/**
 * A family's result evidence claims the stage of a fault removed before its
 * DA apply only with that evidence: the removal is the result's correction
 * at the slot the chain shows it in, and a DA apply, if any, is a named
 * transaction strictly after that slot. Other stages are not checked here.
 */
export const verifyJourneySuccessorStageEvidence = (
  result: Readonly<{
    successorStage?: unknown;
    successorStageEvidence?: unknown;
  }>,
  removal: Readonly<{ txHash: string; slot: number }>,
): void => {
  if (result.successorStage !== HELD_AWAITING_DA_REMOVED_BEFORE_ATTESTATION)
    return;
  const refuse = (why: string): never => {
    throw new Error(
      `The result claims the successor stage ${HELD_AWAITING_DA_REMOVED_BEFORE_ATTESTATION}, but ${why}`,
    );
  };
  const evidence = result.successorStageEvidence;
  if (!isRecord(evidence) || !isRecord(evidence["removal"]))
    return refuse("it carries no evidence of the fault's removal");
  const claimed = evidence["removal"];
  if (claimed["txHash"] !== removal.txHash)
    return refuse(
      `its removal ${String(claimed["txHash"])} is not the correction's removal ${removal.txHash}`,
    );
  if (claimed["slot"] !== removal.slot)
    return refuse(
      `its removal slot ${String(claimed["slot"])} is not slot ${removal.slot.toString()}, where the chain shows the removal`,
    );
  const apply = evidence["attestationApply"];
  if (apply === null) return;
  if (
    !isRecord(apply) ||
    typeof apply["txHash"] !== "string" ||
    !/^[0-9a-f]{64}$/u.test(apply["txHash"]) ||
    !isSlot(apply["slot"])
  )
    return refuse(
      "its evidence names neither a DA apply of the fault nor that there was none",
    );
  if (apply["slot"] <= removal.slot)
    return refuse(
      `the fault's DA apply at slot ${apply["slot"].toString()} is not strictly after the removal at slot ${removal.slot.toString()}`,
    );
};
