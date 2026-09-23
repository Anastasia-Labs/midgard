import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { CommitteeStore } from "../store.js";

/** Read the exact retained envelope that the committee committed to signing. */
export const retainedAvailabilityPayload = async (args: {
  readonly store: Pick<CommitteeStore, "getDaPayload">;
  readonly deploymentFingerprint: string;
  readonly deploymentIdentity: string;
  readonly commitment: SDK.DaAvailabilityCommitment;
}): Promise<Uint8Array | undefined> => {
  if (args.commitment.deployment_identity !== args.deploymentIdentity) {
    throw new Error("availability commitment belongs to another deployment");
  }
  const record = await args.store.getDaPayload(args.commitment.header_hash);
  if (record === undefined || record.validationStatus === "missing_da") {
    return undefined;
  }
  if (
    record.deploymentFingerprint !== args.deploymentFingerprint ||
    record.headerHash !== args.commitment.header_hash ||
    record.validationStatus !== "verified" ||
    record.conflictStatus === "conflicting_bytes"
  ) {
    throw new Error(
      "availability response requires a verified retained payload from this deployment",
    );
  }
  if (!/^(?:[0-9a-f]{2})+$/u.test(record.payloadCborHex)) {
    throw new Error(
      "retained availability payload is not canonical hexadecimal",
    );
  }
  const payload = Buffer.from(record.payloadCborHex, "hex");
  if (computeDaSha256Hash(payload).toString("hex") !== record.payloadSha256) {
    throw new Error(
      "retained availability payload does not match its stored digest",
    );
  }
  if (
    !SDK.verifyDaAvailabilityPayloadCommitment({
      commitment: args.commitment,
      payload,
    })
  ) {
    throw new Error(
      "retained availability payload differs from the frozen signed commitment",
    );
  }
  return payload;
};
