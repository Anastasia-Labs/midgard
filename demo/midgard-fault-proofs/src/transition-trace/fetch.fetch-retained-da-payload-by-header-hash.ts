import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
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
  retainedDaAttemptsRetryable,
  RetainedDaPayloadUnavailableError,
} from "./fetch.retained-da-payload-unavailable.js";
import {
  rememberingRetainedDaPayloadVerifier,
  retainedDaPayloadHeaderVerifier,
} from "./fetch.verify-retained-da-payload-header.js";

/**
 * Returns the first served copy that verifies against `headerHash`, not the
 * first answer: one peer serving wrong bytes must not hide the honest ones.
 * A source is retried only while every attempt it reported is plausibly
 * transient, and never after it served a bad copy. Without a verified copy the
 * fetch reports the payload unavailable, whatever the attempts said.
 */
export const fetchRetainedDaPayloadByHeaderHash = async ({
  headerHash,
  sources,
  retries = 1,
}: FetchRetainedDaPayloadOptions): Promise<RetainedDaPayloadFetchResult> => {
  const normalizedHeaderHash = normalizeHeaderHash(headerHash);
  const attempts: RetainedDaFetchAttempt[] = [];
  // Sources that ignore the verifier still have their copy checked below.
  const verifyPayload = rememberingRetainedDaPayloadVerifier(
    retainedDaPayloadHeaderVerifier(normalizedHeaderHash),
  );

  for (const source of sources) {
    for (let attempt = 0; ; attempt += 1) {
      const result = await source.fetchPayloadByHeaderHash(
        normalizedHeaderHash,
        { verifyPayload },
      );
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
        const verdict = await verifyPayload(result.payloadEnvelopeCbor);
        if (verdict.ok)
          return {
            provenance,
            sourceId: result.sourceId,
            sourcePeerId: result.sourcePeerId,
            payloadEnvelopeCbor: result.payloadEnvelopeCbor,
            metadata: result.metadata,
            attempts,
          };
        attempts.push({
          sourceId: result.sourceId,
          sourcePeerId: result.sourcePeerId,
          protocol: DaRequestResponseProtocol.payloadByHeader,
          status: "failed_verification",
          detail: verdict.reason,
        });
        break;
      }
      if (attempt >= retries || !retainedDaAttemptsRetryable(result.attempts))
        break;
      await sleep(50 * 2 ** attempt);
    }
  }

  const message = `Unable to fetch retained DA payload for header_hash ${normalizedHeaderHash}: ${attempts
    .map(
      (attempt) =>
        `${attempt.sourceId}/${attempt.sourcePeerId} ${attempt.protocol} ${attempt.status} ${attempt.detail}`,
    )
    .join("; ")}`;
  if (attempts.length > 0)
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
