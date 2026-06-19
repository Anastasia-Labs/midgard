import type { DaSignatureRecord } from "../domain.js";

export interface AttestationCoordinator {
  readonly retryPublishedSignatures?: boolean;
  readonly retryPublishedSignaturesForAttestedHeaders?: boolean;
  lastPublishError?(record: Pick<DaSignatureRecord, "headerHash">): string | undefined;
  publishSignature(record: DaSignatureRecord): Promise<"posted" | "post_failed">;
}

export class HttpSignatureCoordinator implements AttestationCoordinator {
  private readonly endpoint: string;

  constructor(endpoint: string) {
    this.endpoint = endpoint;
  }

  async publishSignature(
    record: DaSignatureRecord,
  ): Promise<"posted" | "post_failed"> {
    try {
      const response = await fetch(this.endpoint, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(record),
      });
      return response.ok ? "posted" : "post_failed";
    } catch {
      return "post_failed";
    }
  }
}
