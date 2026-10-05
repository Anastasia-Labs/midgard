import type { WorkflowFundingPreparedTransition } from "@al-ft/midgard-fault-proofs";

import type { WatcherProverFundingReservationRecord } from "./prover-funding-reservation.js";
import { retainedProverFundingInputs } from "./prover-funding-reservation.retained-signed-inputs.js";

export type ProverFundingLeaseClaim = Readonly<{
  outRef: string;
  reservationId: string;
  phase: "active" | "pending";
  role: "funding" | "collateral";
  legacy: boolean;
}>;

/** Signed legacy promises remain in their original handoffs. The unique lease
 * projection keeps the current owner while every overlapping promise holds
 * fresh actuation globally until canonical retirement resolves the overlap. */
export const projectProverFundingLeaseClaims = (
  claims: readonly ProverFundingLeaseClaim[],
  previousOwners: ReadonlyMap<string, string>,
) => {
  const byOutput = new Map<string, Map<string, ProverFundingLeaseClaim>>();
  for (const claim of claims) {
    let owners = byOutput.get(claim.outRef);
    if (owners === undefined) {
      owners = new Map();
      byOutput.set(claim.outRef, owners);
    }
    const previous = owners.get(claim.reservationId);
    if (previous === undefined || claim.phase === "active")
      owners.set(claim.reservationId, claim);
  }
  let reconciliationOnly = false;
  const leases: ProverFundingLeaseClaim[] = [];
  for (const [outRef, owners] of byOutput) {
    const values = [...owners.values()];
    const ordinary = values.filter((claim) => !claim.legacy);
    if (ordinary.length > 1)
      throw new Error("prover funding reservation repeats an output lease");
    if (values.length > 1) reconciliationOnly = true;
    const owner =
      ordinary[0] ?? owners.get(previousOwners.get(outRef) ?? "") ?? values[0]!;
    leases.push(owner);
  }
  return {
    claims: [...byOutput.values()].flatMap((owners) => [...owners.values()]),
    leases,
    reconciliationOnly,
  };
};

export const projectRetainedProverFundingLeases = ({
  records,
  readInputs,
  readAttempts,
  readUnverifiedHashes,
  transferred,
  previousOwners,
}: {
  records: readonly WatcherProverFundingReservationRecord[];
  readInputs: (
    record: WatcherProverFundingReservationRecord,
  ) => readonly { outRef: string; role: "funding" | "collateral" }[];
  readAttempts: (
    reservationId: string,
  ) => readonly WorkflowFundingPreparedTransition[];
  readUnverifiedHashes: (reservationId: string) => ReadonlySet<string>;
  transferred: (reservationId: string, outRef: string) => boolean;
  previousOwners: ReadonlyMap<string, string>;
}) => {
  const claims: ProverFundingLeaseClaim[] = [];
  for (const record of records) {
    const unverified = readUnverifiedHashes(record.reservationId);
    const legacyOutRefs = new Set(
      retainedProverFundingInputs({
        record: { ...record, activeInputs: [] },
        submissions: readAttempts(record.reservationId).filter(
          ({ transactionHash }) => unverified.has(transactionHash),
        ),
        abandonedTransactionHashes: new Set(),
        unverifiedAbandonedTransactionHashes: unverified,
        completed: false,
      }).map(({ outRef }) => outRef),
    );
    const add = (
      value: { outRef: string; role: "funding" | "collateral" },
      phase: "active" | "pending",
    ) =>
      claims.push({
        ...value,
        reservationId: record.reservationId,
        phase,
        legacy: legacyOutRefs.has(value.outRef),
      });
    for (const value of readInputs(record)) add(value, "active");
    for (const value of record.pendingTransition?.producedInputs ?? [])
      if (!transferred(record.reservationId, value.outRef))
        add(value, "pending");
  }
  return projectProverFundingLeaseClaims(claims, previousOwners);
};
