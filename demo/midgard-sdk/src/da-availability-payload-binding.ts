import { daAvailabilityCommitmentHash } from "./availability-challenge.assert-canonical-da-availability-commitment.js";
import { daAvailabilityPublishedTerminalCommitment } from "./availability-challenge.assert-canonical-da-availability-publication-datum.js";
import { buildDaAvailabilityCommitment } from "./availability-challenge.build-da-availability-commitment.js";
import type { DaAvailabilityResponseGeometry } from "./availability-challenge.da-availability-tranche-datum-schema.js";
import type { DaAvailabilityStateQueueStatus } from "./da-availability-state.js";

/** The deployment inputs an availability commitment is built from. */
export type DaPayloadAvailabilityAuthority = Readonly<{
  /** The hub oracle policy id the committee signs for. */
  deploymentIdentity: string;
  responseGeometry: DaAvailabilityResponseGeometry;
}>;

/**
 * How a block's DA payload bytes stand against its state-queue node's
 * availability status:
 * - `unbound`: the node is Unattested, so no commitment fixes the bytes yet;
 * - `bound`: the commitment rebuilt from the bytes is the one the status
 *   carries (its hash for Attested and Challenged, its published terminal
 *   commitment for Published);
 * - `mismatch`: the status carries a commitment and the bytes do not rebuild
 *   it (or do not rebuild one at all).
 */
export type DaPayloadAvailabilityBinding = "unbound" | "bound" | "mismatch";

/**
 * Checks fetched block-payload bytes (the stored DA envelope the committee
 * signs) against the attested payload commitment of the block's state-queue
 * node. The commitment is rebuilt from the bytes with the deployment's
 * authority and compared with the status.
 */
export const daPayloadAvailabilityBinding = (input: {
  readonly status: DaAvailabilityStateQueueStatus;
  readonly authority: DaPayloadAvailabilityAuthority;
  readonly headerHash: string;
  readonly payload: Uint8Array;
}): DaPayloadAvailabilityBinding => {
  const { status } = input;
  if (status === "Unattested") return "unbound";
  try {
    const rebuilt = buildDaAvailabilityCommitment({
      deploymentIdentity: input.authority.deploymentIdentity,
      headerHash: input.headerHash,
      payload: input.payload,
      responseGeometry: input.authority.responseGeometry,
    });
    const [expected, actual] =
      "Published" in status
        ? [
            status.Published.terminal_commitment,
            daAvailabilityPublishedTerminalCommitment(rebuilt),
          ]
        : [
            "Attested" in status
              ? status.Attested.commitment_hash
              : status.Challenged.commitment_hash,
            daAvailabilityCommitmentHash(rebuilt),
          ];
    return expected.toLowerCase() === actual.toLowerCase()
      ? "bound"
      : "mismatch";
  } catch {
    return "mismatch";
  }
};

/** Whether `status` carries a commitment a payload can be checked against. */
export const daAvailabilityStatusBinds = (
  status: DaAvailabilityStateQueueStatus,
): boolean => status !== "Unattested";
