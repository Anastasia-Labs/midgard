import type { ProtocolDeploymentStatus } from "midgard-node/transactions/initialization";

import type { StepRecord } from "./journal.js";

export type InitializationObservation = {
  manifest: { ok: boolean };
  protocol: Pick<
    ProtocolDeploymentStatus,
    "complete" | "empty" | "hubOracleWitness"
  >;
};
export function initializationRecovery(
  status: InitializationObservation,
  manifest:
    | { steps?: { initProtocol?: { txHash?: string; status?: string } } }
    | undefined,
  record: StepRecord | undefined,
) {
  if (status.protocol.complete) {
    if (
      manifest?.steps?.initProtocol?.status === "complete" &&
      !status.manifest.ok
    )
      throw new Error(
        "Finalized deployment manifest disagrees with Cardano; preserve it",
      );
    const initHash =
      manifest?.steps?.initProtocol?.txHash ??
      status.protocol.hubOracleWitness?.txHash;
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
  return {
    status:
      !status.protocol.empty || record !== undefined
        ? ("pending" as const)
        : ("retry" as const),
  };
}
