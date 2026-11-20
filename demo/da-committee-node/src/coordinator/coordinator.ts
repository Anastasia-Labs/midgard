import type { DaSignatureRecord } from "../domain.js";

export interface AttestationCoordinator {
  readonly retryPublishedSignatures?: boolean;
  readonly retryPublishedSignaturesForAttestedHeaders?: boolean;
  lastPublishError?(
    record: Pick<DaSignatureRecord, "headerHash">,
  ): string | undefined;
  publishSignature(
    record: DaSignatureRecord,
  ): Promise<"posted" | "post_failed">;
}
