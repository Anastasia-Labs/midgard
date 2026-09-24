import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

/** The raw observation fixes the exact thread outRef. Reopening it for action
 * planning must preserve its token, owner and frozen proof identity. */
export const transitionDepositCheckpointRequiresQueueLease = ({
  thread,
  address,
  unit,
  prover,
  proofHash,
}: {
  readonly thread: UTxO;
  readonly address: string;
  readonly unit: string;
  readonly prover: string;
  readonly proofHash: string;
}): boolean => {
  if (
    thread.address !== address ||
    thread.datum == null ||
    thread.scriptRef != null ||
    thread.assets[unit] !== 1n ||
    Object.entries(thread.assets).some(
      ([asset, amount]) =>
        asset !== "lovelace" && (asset !== unit || amount !== 1n),
    )
  )
    throw new Error("Transition deposit checkpoint authentication changed");
  const datum = Data.from(
    thread.datum,
    SDK.TransitionTraceProofCommitmentDatum,
  );
  if (
    datum.fraud_prover !== prover ||
    datum.data === null ||
    datum.data.kind !== 1n ||
    datum.data.proof_commitment.hash !== proofHash
  )
    throw new Error(
      "Transition deposit checkpoint differs from admitted proof",
    );
  return datum.data.phase === 5n;
};
