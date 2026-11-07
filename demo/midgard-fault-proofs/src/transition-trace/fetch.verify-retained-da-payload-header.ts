import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  normalizeHeaderHash,
  type RetainedDaPayloadVerifier,
} from "./fetch.admit-retained-da-provenance.js";

/**
 * Accepts a copy only when it is a canonical DaPayloadV1 whose embedded header
 * hashes to its own `header_hash` and to the requested one.
 *
 * These are exactly the checks every consumer of a fetched payload applies
 * before anything else, including the raw-leaf fault-proof families, so no
 * consumer could have used a copy this refuses. Root and entry checks stay
 * with the consumers, because the raw-leaf fault-proof families must still
 * receive the honest published copy of a block that is fraudulent in a root.
 */
export const retainedDaPayloadHeaderVerifier = (
  headerHash: string,
): RetainedDaPayloadVerifier => {
  const expected = normalizeHeaderHash(headerHash);
  return async (payloadEnvelopeCbor) => {
    let payloadCbor: Buffer;
    let payload: SDK.DaPayload;
    let embedded: string;
    // Every step below is a pure function of the served bytes, so any throw
    // is a defect of this copy rather than of the fetch.
    try {
      payloadCbor = Buffer.from(
        (
          await unwrapDaPayload(payloadEnvelopeCbor, {
            maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
          })
        ).innerBytes,
      );
      payload = SDK.decodeDaPayload(payloadCbor);
      embedded = await Effect.runPromise(
        SDK.hashBlockHeader(payload.block_body.header),
      );
    } catch (cause) {
      return refuse(`malformed payload: ${String(cause)}`);
    }
    if (!SDK.encodeDaPayload(payload).equals(payloadCbor))
      return refuse("payload CBOR is not canonical DaPayloadV1");
    if (payload.version !== SDK.DA_PAYLOAD_VERSION)
      return refuse(
        `payload version ${payload.version.toString()} is not ${SDK.DA_PAYLOAD_VERSION.toString()}`,
      );
    const declared = payload.block_body.header_hash.toLowerCase();
    if (embedded !== declared || embedded !== expected)
      return refuse(
        `header mismatch: embedded=${embedded} declared=${declared} requested=${expected}`,
      );
    return { ok: true };
  };
};

const refuse = (reason: string) => ({ ok: false as const, reason });

/**
 * Wraps a verifier so a buffer it already accepted is accepted again without
 * re-verifying. A source that ran the verifier returns the very buffer it
 * checked, so the caller's own check of the result costs nothing.
 */
export const rememberingRetainedDaPayloadVerifier = (
  verifier: RetainedDaPayloadVerifier,
): RetainedDaPayloadVerifier => {
  const accepted = new WeakSet<Buffer>();
  return async (payloadEnvelopeCbor) => {
    if (accepted.has(payloadEnvelopeCbor)) return { ok: true };
    const verdict = await verifier(payloadEnvelopeCbor);
    if (verdict.ok) accepted.add(payloadEnvelopeCbor);
    return verdict;
  };
};

/**
 * Accepts exactly the payload an attested availability commitment binds. It
 * remembers what it accepted, so a caller can re-check a source's result for
 * free, which it must: a source may ignore the verifier it was handed.
 */
export const retainedDaPayloadCommitmentVerifier = (
  commitment: SDK.DaAvailabilityCommitment,
): RetainedDaPayloadVerifier =>
  rememberingRetainedDaPayloadVerifier(async (payload) =>
    SDK.verifyDaAvailabilityPayloadCommitment({ commitment, payload })
      ? { ok: true }
      : refuse("payload does not match the attested commitment"),
  );
