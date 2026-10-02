import {
  assertSecurityGradeEvidence,
  CanonicalEvidenceRejection,
  requireEvidenceTrustClass,
} from "@al-ft/midgard-sdk";

import { transitionTraceError } from "./errors.js";
import {
  type FetchRetainedDaPayloadOptions,
  normalizeHeaderHash,
  type RetainedDaFetchAttempt,
  type RetainedDaPayloadFetchResult,
} from "./fetch.admit-retained-da-provenance.js";
import {
  retainedDaAttemptsAvailability,
  retainedDaAttemptsOnlyUnavailable,
  RetainedDaPayloadUnavailableError,
} from "./fetch.retained-da-payload-unavailable.js";

export const fetchRetainedDaPayloadByHeaderHash = async ({
  headerHash,
  sources,
  retries = 1,
}: FetchRetainedDaPayloadOptions): Promise<RetainedDaPayloadFetchResult> => {
  const normalizedHeaderHash = normalizeHeaderHash(headerHash);
  const attempts: RetainedDaFetchAttempt[] = [];

  for (const source of sources) {
    for (let attempt = 0; attempt <= retries; attempt += 1) {
      const result =
        await source.fetchPayloadByHeaderHash(normalizedHeaderHash);
      attempts.push(...result.attempts);
      if (result.ok) {
        const provenance = requireEvidenceTrustClass({
          provenance: assertSecurityGradeEvidence(result.provenance),
          expected: "public_or_permissionless_da",
          code: "da_evidence_wrong_trust_class",
        });
        const expectedSourceId = `${result.sourceId}/${result.sourcePeerId}`;
        if (provenance.sourceId !== expectedSourceId) {
          throw new CanonicalEvidenceRejection(
            "da_evidence_wrong_trust_class",
            `provenance.sourceId=${provenance.sourceId} expected=${expectedSourceId}`,
          );
        }
        return {
          provenance,
          sourceId: result.sourceId,
          sourcePeerId: result.sourcePeerId,
          payloadEnvelopeCbor: result.payloadEnvelopeCbor,
          metadata: result.metadata,
          attempts,
        };
      }
      if (attempt < retries) {
        await sleep(50 * 2 ** attempt);
      }
    }
  }

  const message = `Unable to fetch retained DA payload for header_hash ${normalizedHeaderHash}: ${attempts
    .map(
      (attempt) =>
        `${attempt.sourceId}/${attempt.sourcePeerId} ${attempt.protocol} ${attempt.status} ${attempt.detail}`,
    )
    .join("; ")}`;
  if (attempts.length > 0 && retainedDaAttemptsOnlyUnavailable(attempts))
    throw new RetainedDaPayloadUnavailableError(
      normalizedHeaderHash,
      message,
      retainedDaAttemptsAvailability(attempts),
    );
  throw transitionTraceError("fetchFailed", message);
};

const sleep = async (ms: number): Promise<void> => {
  await new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
};
