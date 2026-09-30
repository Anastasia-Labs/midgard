import { type CommittedFieldClaim } from "@al-ft/midgard-sdk";

import { type PreparedFieldPreimageLengthWorkflow } from "../src/field-preimage-length-mismatch/workflow.js";
import { WORKFLOW } from "./field-preimage-length-mismatch-lifecycle.registered-contracts.js";
import { setup } from "./field-preimage-length-mismatch-lifecycle.setup.js";
import { FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX } from "./support/field-preimage-length-mismatch-forced-fixture.js";

export const forcedPrepared = ({
  headerHash,
  transactionId,
  direction,
  declaredLength,
  preimage,
}: {
  readonly headerHash: string;
  readonly transactionId: string;
  readonly direction: "wrongfulAcceptance" | "wrongfulRejection";
  readonly declaredLength: number;
  readonly preimage: Uint8Array;
}): PreparedFieldPreimageLengthWorkflow => ({
  schemaVersion: WORKFLOW,
  headerHash,
  transactionId,
  direction,
  fieldIndex: FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
  declaredLength,
  actualLength: preimage.length,
  preimageHex: Buffer.from(preimage).toString("hex"),
  carriage: "Inline",
  evidenceDigest: "00".repeat(32),
});

export const inlineBodyClaim = (
  fieldIndex: number,
  preimage: Uint8Array,
): CommittedFieldClaim => ({
  BodyFieldClaim: {
    field_index: BigInt(fieldIndex),
    carriage: { Inline: { preimage: Buffer.from(preimage).toString("hex") } },
  },
});

/** `setup` over a retained-root forced leaf, exposing that leaf directly. */
export const forcedSetup = async (
  verdict: "rejected" | "valid",
  mismatch: boolean,
) => {
  const fixture = await setup({
    honestAccepted: true,
    forcedLeaf: { verdict, mismatch },
  });
  if (fixture.familyForced === undefined)
    throw new Error("missing family forced leaf");
  return { ...fixture, forced: fixture.familyForced };
};
