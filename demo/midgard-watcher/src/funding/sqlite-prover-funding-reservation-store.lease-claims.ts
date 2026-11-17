import type { WatcherProverFundingReservationRecord } from "./prover-funding-reservation.js";

type ProverFundingLeaseClaim = Readonly<{
  outRef: string;
  reservationId: string;
  phase: "active" | "pending";
}>;

/** One lease per output: a reservation's active claim on an output takes
 * precedence over its pending one, and two reservations never share one. */
export const projectRetainedProverFundingLeases = ({
  records,
  readInputs,
  transferred,
}: {
  records: readonly WatcherProverFundingReservationRecord[];
  readInputs: (
    record: WatcherProverFundingReservationRecord,
  ) => readonly string[];
  transferred: (reservationId: string, outRef: string) => boolean;
}) => {
  const byOutput = new Map<string, ProverFundingLeaseClaim>();
  const claim = (
    record: WatcherProverFundingReservationRecord,
    outRef: string,
    phase: "active" | "pending",
  ) => {
    const previous = byOutput.get(outRef);
    if (
      previous !== undefined &&
      previous.reservationId !== record.reservationId
    )
      throw new Error("prover funding reservation repeats an output lease");
    if (previous === undefined || phase === "active")
      byOutput.set(outRef, {
        outRef,
        reservationId: record.reservationId,
        phase,
      });
  };
  for (const record of records) {
    for (const outRef of readInputs(record)) claim(record, outRef, "active");
    for (const { outRef } of record.pendingTransition?.producedInputs ?? [])
      if (!transferred(record.reservationId, outRef))
        claim(record, outRef, "pending");
  }
  return { leases: [...byOutput.values()] };
};
