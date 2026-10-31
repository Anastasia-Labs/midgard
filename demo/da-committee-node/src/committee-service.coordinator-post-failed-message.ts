import type { AttestationCoordinator } from "./coordinator/coordinator.js";
import type { DaSignatureRecord } from "./domain.js";
export const coordinatorPostFailedMessage = (
  coordinator: AttestationCoordinator | undefined,
  record: Pick<DaSignatureRecord, "headerHash" | "signerIndex">,
): string => {
  const coordinatorError = coordinator?.lastPublishError?.(record);
  return `failed to publish DA signature for ${record.headerHash} signer ${record.signerIndex.toString()}${coordinatorError === undefined ? "" : `: ${coordinatorError}`}`;
};

export const shouldRepublishSignature = (
  coordinator: AttestationCoordinator | undefined,
  record: DaSignatureRecord,
): boolean =>
  coordinator !== undefined &&
  (record.broadcastStatus !== "posted" ||
    coordinator.retryPublishedSignatures === true);
export const shouldRepublishSignatureForHeader = (
  coordinator: AttestationCoordinator | undefined,
  record: DaSignatureRecord,
  status: import("./domain.js").StateQueueHeaderRecord["status"],
): boolean =>
  shouldRepublishSignature(coordinator, record) &&
  (status === "unattested" ||
    status === "attesting" ||
    (status === "attested" &&
      coordinator?.retryPublishedSignaturesForAttestedHeaders === true));
