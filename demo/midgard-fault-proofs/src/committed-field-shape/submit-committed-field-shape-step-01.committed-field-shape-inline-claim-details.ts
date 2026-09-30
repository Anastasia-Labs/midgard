import {
  encodeMidgardNativeTxWitnessSetCompact,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  type CommittedFieldClaim,
  type CommittedFieldShapeEvidence,
  type CommittedFieldShapeStep02State,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX,
} from "@al-ft/midgard-sdk";

import {
  committedFieldShapeStepLabel,
  committedFieldShapeSubmitError,
} from "./submit-common.js";

export const STEP_LABEL = committedFieldShapeStepLabel(0);

export type SubmitCommittedFieldShapeStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly evidence: CommittedFieldShapeEvidence;
  readonly step02State: CommittedFieldShapeStep02State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly proofCarriage: "redeemer" | "published-chunks";
  readonly awaitedConfirmation: boolean;
};

export const committedFieldShapeInlineClaimDetails = (
  claim: CommittedFieldClaim,
): {
  readonly fieldIndex: number;
  readonly preimage: Buffer;
  readonly witnessSetHash?: string;
} => {
  const witnessSetHash = (() => {
    if (!("WitnessFieldClaim" in claim)) {
      return undefined;
    }
    const h32 = (value: string, label: string): Buffer => {
      if (!/^[0-9a-f]{64}$/u.test(value)) {
        throw committedFieldShapeSubmitError(
          `${label} must be canonical lowercase 32-byte hexadecimal.`,
        );
      }
      return Buffer.from(value, "hex");
    };
    return Buffer.from(
      computeHash32(
        encodeMidgardNativeTxWitnessSetCompact({
          addrTxWitsHash: h32(
            claim.WitnessFieldClaim.witness_set.addr_tx_wits_hash,
            "witness claim addr_tx_wits_hash",
          ),
          scriptTxWitsHash: h32(
            claim.WitnessFieldClaim.witness_set.script_tx_wits_hash,
            "witness claim script_tx_wits_hash",
          ),
          redeemerTxWitsHash: h32(
            claim.WitnessFieldClaim.witness_set.redeemer_tx_wits_hash,
            "witness claim redeemer_tx_wits_hash",
          ),
        }),
      ),
    ).toString("hex");
  })();
  const selected =
    "BodyFieldClaim" in claim
      ? {
          kind: "body" as const,
          fieldIndex: Number(claim.BodyFieldClaim.field_index),
          carriage: claim.BodyFieldClaim.carriage,
        }
      : {
          kind: "witness" as const,
          fieldIndex: Number(claim.WitnessFieldClaim.field_index),
          carriage: claim.WitnessFieldClaim.carriage,
        };
  if (
    !Number.isSafeInteger(selected.fieldIndex) ||
    selected.fieldIndex < 0 ||
    selected.fieldIndex >= 9
  ) {
    throw committedFieldShapeSubmitError(
      `claim field index ${selected.fieldIndex.toString()} is outside 0..8.`,
    );
  }
  const expectedKind =
    selected.fieldIndex < MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX
      ? "body"
      : "witness";
  if (selected.kind !== expectedKind) {
    throw committedFieldShapeSubmitError(
      `${selected.kind} claim cannot name field ${selected.fieldIndex.toString()} (${expectedKind} slot).`,
    );
  }
  if (!("Inline" in selected.carriage)) {
    throw committedFieldShapeSubmitError(
      "this submitter wave admits only tier-1 Inline claim carriage.",
    );
  }
  const preimageHex = selected.carriage.Inline.preimage;
  if (!/^(?:[0-9a-f]{2})*$/u.test(preimageHex)) {
    throw committedFieldShapeSubmitError(
      "inline preimage must be canonical lowercase whole-byte hexadecimal.",
    );
  }
  const preimage = Buffer.from(preimageHex, "hex");
  if (preimage.length > MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES) {
    throw committedFieldShapeSubmitError(
      `inline preimage is ${preimage.length.toString()} bytes, above the tier-1 frontier ${MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES.toString()}; tier-2/3 TypeScript carriage is deferred.`,
    );
  }
  return {
    fieldIndex: selected.fieldIndex,
    preimage,
    ...(witnessSetHash === undefined ? {} : { witnessSetHash }),
  };
};
