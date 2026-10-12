import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import {
  admitEvidenceProvenance,
  assertSecurityGradeEvidence,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk";

export type RetainedDaLibp2pPeer = {
  readonly peerId: string;
};

export type RetainedDaLibp2pRequest = {
  readonly peer: RetainedDaLibp2pPeer;
  readonly protocol: DaRequestResponseProtocol;
  readonly payload: Buffer;
  readonly timeoutMs: number;
};

export interface RetainedDaLibp2pTransport {
  request(args: RetainedDaLibp2pRequest): Promise<Uint8Array>;
}

export type RetainedDaFetchAttemptStatus =
  | "not_found"
  | "transport_error"
  | "timeout"
  | "invalid_content"
  | "rejected"
  | "conflict"
  | "failed_verification";

export type RetainedDaFetchAttempt = {
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly protocol: DaRequestResponseProtocol;
  readonly status: RetainedDaFetchAttemptStatus;
  readonly detail: string;
};

export type RetainedDaPayloadFetchResult = {
  readonly provenance: EvidenceProvenance;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly payloadEnvelopeCbor: Buffer;
  readonly metadata?: unknown;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaProofBundle = {
  readonly provenance: EvidenceProvenance;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly proofBundleHash: Buffer;
  readonly proofBundleBytes: Buffer;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaTraceStep = {
  readonly provenance: EvidenceProvenance;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly stepIndex: number;
  readonly transitionStepBytes: Buffer;
  readonly membershipProofBytes: Buffer;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaEventToStep = {
  readonly provenance: EvidenceProvenance;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly eventKey: Buffer;
  readonly eventToStepEntryBytes: Buffer | null;
  readonly membershipOrNonmembershipProofBytes: Buffer;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaPayloadSourceResult =
  | ({
      readonly ok: true;
    } & RetainedDaPayloadFetchResult)
  | {
      readonly ok: false;
      readonly sourceId: string;
      readonly attempts: readonly RetainedDaFetchAttempt[];
    };

/**
 * A peer's payload hash is self-reported, so a well-formed response proves
 * nothing about the requested header. A verifier judges one served copy;
 * `reason` names why the copy is not the requested payload.
 */
export type RetainedDaPayloadVerdict =
  | { readonly ok: true }
  | { readonly ok: false; readonly reason: string };

export type RetainedDaPayloadVerifier = (
  payloadEnvelopeCbor: Buffer,
) => Promise<RetainedDaPayloadVerdict>;

export type RetainedDaPayloadFetchOptions = {
  /** A source that holds several peers tries the next one on a refusal. */
  readonly verifyPayload?: RetainedDaPayloadVerifier;
};

export interface RetainedDaPayloadSource {
  readonly sourceId: string;
  fetchPayloadByHeaderHash(
    headerHash: string,
    options?: RetainedDaPayloadFetchOptions,
  ): Promise<RetainedDaPayloadSourceResult>;
}

export type FetchRetainedDaPayloadOptions = {
  readonly headerHash: string;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
};

export type DaLibp2pRetainedDaSourceOptions = {
  readonly sourceId?: string;
  readonly deploymentFingerprint: string;
  readonly peers: readonly RetainedDaLibp2pPeer[];
  readonly transport: RetainedDaLibp2pTransport;
  readonly timeoutMs?: number;
  readonly maxInlineResponseBytes?: number;
  readonly maxChunkBytes?: number;
};

export type SourceSuccess<T> = { readonly ok: true } & T;

export type SourceFailure = {
  readonly ok: false;
  readonly sourceId: string;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

/**
 * Public retained-DA records are security inputs. Construct and admit their
 * provenance at the transport boundary, before any payload or proof bytes can
 * leave this module.
 */
export const admitRetainedDaProvenance = (
  sourceId: string,
  sourcePeerId: string,
): EvidenceProvenance =>
  assertSecurityGradeEvidence(
    admitEvidenceProvenance({
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: `${sourceId}/${sourcePeerId}`,
        grade: "security",
      },
    }),
  );

export const normalizeHeaderHash = (value: string): string =>
  normalizeHex(value, {
    fieldName: "header_hash",
    byteLength: 28,
    trim: true,
  });

export const bytesFromHexOrBytes = (
  value: string | Uint8Array,
  fieldName: string,
): Buffer => {
  if (typeof value !== "string") {
    return Buffer.from(value);
  }
  return Buffer.from(
    normalizeHex(value, { fieldName, trim: true, allowEmpty: false }),
    "hex",
  );
};
