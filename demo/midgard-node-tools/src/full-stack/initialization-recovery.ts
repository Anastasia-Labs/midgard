import type { ProtocolDeploymentStatus } from "midgard-node/transactions/initialization";

export type InitializationObservation = {
  manifest: { ok: boolean };
  protocol: Pick<
    ProtocolDeploymentStatus,
    "status" | "nonce" | "hubOracleWitness"
  >;
};
/** A finalized initialization that the tip no longer shows was rolled back; it can still re-enter the chain. */
export const PENDING_REINCLUSION =
  "Finalized initialization is not at the Cardano tip and is pending re-inclusion; wait, then rerun. Preserve its data";
export function initializationRecovery(
  status: InitializationObservation,
  manifest:
    | { steps?: { initProtocol?: { txHash?: string; status?: string } } }
    | undefined,
) {
  if (status.protocol.status === "complete") {
    if (
      manifest?.steps?.initProtocol?.status === "complete" &&
      !status.manifest.ok
    )
      throw new Error(
        "Finalized deployment manifest disagrees with Cardano; preserve it",
      );
    const recorded = manifest?.steps?.initProtocol?.txHash;
    const witnessed = status.protocol.hubOracleWitness?.txHash;
    // Cardano is authoritative: never journal a hash it contradicts.
    if (recorded && witnessed && recorded !== witnessed)
      throw new Error(
        "Recorded initialization transaction disagrees with Cardano; preserve it",
      );
    const initHash = recorded ?? witnessed;
    if (!initHash || !/^[0-9a-f]{64}$/.test(initHash))
      throw new Error(
        "Cannot establish the initialization transaction identity",
      );
    return {
      status: "complete" as const,
      initHash,
      reconstruct: !status.manifest.ok,
    };
  }
  if (manifest?.steps?.initProtocol?.status === "complete")
    throw new Error(PENDING_REINCLUSION);
  // Empty is always safe to retry: no attempt was recorded. A recorded
  // attempt that is not yet visible waits for Cardano.
  return {
    status:
      status.protocol.status === "empty"
        ? ("retry" as const)
        : ("pending" as const),
  };
}
