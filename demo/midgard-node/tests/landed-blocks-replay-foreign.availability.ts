/**
 * The availability commitments the foreign replayer checks a payload
 * against: the authority it is configured with, and a node's status once the
 * committee attested (or published) a payload's envelope.
 */
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";

const envelope = (payload: SDK.DaPayload) =>
  wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" });

/** The commitment authority the replayer checks payloads with. */
export const AVAILABILITY: SDK.DaPayloadAvailabilityAuthority = {
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
};

/** The commitment the committee would attest for `payload`'s envelope. */
export const commitmentOf = async (payload: SDK.DaPayload) =>
  SDK.buildDaAvailabilityCommitment({
    ...AVAILABILITY,
    headerHash: payload.block_body.header_hash,
    payload: await envelope(payload),
  });

/** `payload`'s node status once the committee attested its envelope. */
export const attested = async (
  payload: SDK.DaPayload,
): Promise<SDK.DaAvailabilityStateQueueStatus> => ({
  Attested: {
    commitment_hash: SDK.daAvailabilityCommitmentHash(
      await commitmentOf(payload),
    ),
  },
});

/** `payload`'s node status once its availability was published. */
export const published = async (
  payload: SDK.DaPayload,
): Promise<SDK.DaAvailabilityStateQueueStatus> => ({
  Published: {
    terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
      await commitmentOf(payload),
    ),
  },
});
