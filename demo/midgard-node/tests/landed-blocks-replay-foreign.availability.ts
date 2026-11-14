/**
 * The availability commitments the foreign replayer checks a payload
 * against: the authority it is configured with, and a node's status once the
 * committee attested (or published) a payload's envelope.
 */
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";

/**
 * A payload served as a zstd envelope: the same body under the same header,
 * in bytes that differ from the identity envelope. The header commits the
 * body, not the envelope's encoding, so only the availability commitment
 * tells the two copies apart.
 */
export type RecompressedDaPayload = Readonly<{ recompressed: SDK.DaPayload }>;

/** A copy of a block's payload: as itself (identity envelope) or recompressed. */
export type DaCopy = SDK.DaPayload | RecompressedDaPayload;

/** `payload` in a zstd envelope. */
export const recompressed = (
  payload: SDK.DaPayload,
): RecompressedDaPayload => ({
  recompressed: payload,
});

/** The payload a copy carries. */
export const payloadOf = (copy: DaCopy): SDK.DaPayload =>
  "recompressed" in copy ? copy.recompressed : copy;

/** The envelope bytes a copy is served and retained as. */
export const envelope = (copy: DaCopy) =>
  wrapDaPayload(SDK.encodeDaPayload(payloadOf(copy)), {
    mode: "recompressed" in copy ? "zstd" : "identity",
  });

/** The commitment authority the replayer checks payloads with. */
export const AVAILABILITY: SDK.DaPayloadAvailabilityAuthority = {
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
};

/** The commitment the committee would attest for `copy`'s envelope. */
export const commitmentOf = async (copy: DaCopy) =>
  SDK.buildDaAvailabilityCommitment({
    ...AVAILABILITY,
    headerHash: payloadOf(copy).block_body.header_hash,
    payload: await envelope(copy),
  });

/** `payload`'s node status once the committee attested its envelope. */
export const attested = async (
  copy: DaCopy,
): Promise<SDK.DaAvailabilityStateQueueStatus> => ({
  Attested: {
    commitment_hash: SDK.daAvailabilityCommitmentHash(await commitmentOf(copy)),
  },
});

/** `payload`'s node status once its availability was published. */
export const published = async (
  copy: DaCopy,
): Promise<SDK.DaAvailabilityStateQueueStatus> => ({
  Published: {
    terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
      await commitmentOf(copy),
    ),
  },
});
