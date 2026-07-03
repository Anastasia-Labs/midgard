export type DaPayloadCandidate = {
  readonly sourcePeerId: string;
  readonly payloadCbor: Buffer;
  readonly metadata?: unknown;
};

export type DaPayloadFetchFailureStatus =
  | "not_found"
  | "transport_error"
  | "timeout"
  | "invalid_content"
  | "rejected"
  | "conflict";

export type DaPayloadFetchFailure = {
  readonly ok: false;
  readonly attempts: readonly {
    readonly sourcePeerId: string;
    readonly status: DaPayloadFetchFailureStatus;
    readonly detail: string;
  }[];
};

export type DaPayloadCandidatesSuccess = {
  readonly ok: true;
  readonly candidates: readonly DaPayloadCandidate[];
  readonly attempts: DaPayloadFetchFailure["attempts"];
};

export type DaPayloadCandidatesResult =
  | DaPayloadCandidatesSuccess
  | DaPayloadFetchFailure;

export interface DaPayloadSource {
  fetchPayloadCandidates(
    headerHash: string,
  ): Promise<DaPayloadCandidatesResult>;
}
