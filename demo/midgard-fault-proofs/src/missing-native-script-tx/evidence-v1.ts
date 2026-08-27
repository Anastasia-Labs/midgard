import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonicalV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
  missingNativeScriptIsAbsentV1,
  missingNativeScriptTxVersionedScriptHashV1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import { planFaultProofFieldOpeningV1 } from "../field-opening-v1.js";
import {
  requireNativeTxMatchesCompactCbor,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { missingNativeScriptTxSubmitError } from "./submit-common-v1.js";

/** Complete, consumer-agnostic evidence needed by the six submitters. */
export type MissingNativeScriptTxEvidenceV1 = {
  readonly badTxInclusion: SubmitStep01TxInclusion;
  readonly badTxSpendInputs: readonly MidgardTxInput[];
  readonly badInputIndex: bigint;
  readonly producingTxInclusion: SubmitStep01TxInclusion;
  readonly producingOutputItemCbors: readonly Uint8Array[];
  readonly missingNativeScriptBytes: Uint8Array;
  readonly badTxWitnessSet: NativeTxWitnessSetCompact;
  readonly badTxScriptWitnessItemCbors: readonly Uint8Array[];
  readonly expectedMissingScriptHash: string;
};

/**
 * Assembles evidence only after replaying the family classification and all
 * three §8.8 anchor/field pairings. Unknown script preimages therefore refuse
 * before a computation thread is opened.
 */
export const buildMissingNativeScriptTxEvidenceV1 = ({
  badTxInclusion,
  badTxSpendInputs,
  badInputIndex,
  producingTxInclusion,
  producingOutputItemCbors,
  missingNativeScriptBytes,
  badTxWitnessSet,
  badTxScriptWitnessItemCbors,
  owner,
}: Omit<MissingNativeScriptTxEvidenceV1, "expectedMissingScriptHash"> & {
  readonly owner: string;
}): MissingNativeScriptTxEvidenceV1 => {
  requireNativeTxMatchesCompactCbor(badTxInclusion);
  requireNativeTxMatchesCompactCbor(producingTxInclusion);
  const accusedInput = badTxSpendInputs[Number(badInputIndex)];
  if (badInputIndex < 0n || accusedInput === undefined) {
    throw missingNativeScriptTxSubmitError(
      `bad input index ${badInputIndex.toString()} is outside the committed input list.`,
    );
  }
  if (accusedInput.tx_id !== producingTxInclusion.nativeTxId) {
    throw missingNativeScriptTxSubmitError(
      "the selected input does not name the supplied producing transaction.",
    );
  }
  const outputItem =
    producingOutputItemCbors[Number(accusedInput.output_index)];
  if (accusedInput.output_index < 0n || outputItem === undefined) {
    throw missingNativeScriptTxSubmitError(
      "the selected input's output index is outside the producing transaction.",
    );
  }
  const credential = decodeMidgardAddressBytes(
    decodeMidgardTxOutput(outputItem).address,
  ).paymentCredential;
  if (credential.kind !== "Script") {
    throw missingNativeScriptTxSubmitError(
      "the selected producing output is not script-locked.",
    );
  }
  const expectedMissingScriptHash = credential.hash.toString("hex");
  const derived = missingNativeScriptTxVersionedScriptHashV1(
    missingNativeScriptBytes,
  );
  if (derived !== expectedMissingScriptHash) {
    throw missingNativeScriptTxSubmitError(
      `known native script preimage hashes to ${derived}, not ${expectedMissingScriptHash}.`,
    );
  }
  if (
    !missingNativeScriptIsAbsentV1({
      scriptTxWitsItems: badTxScriptWitnessItemCbors,
      expectedMissingScriptHash,
    })
  ) {
    throw missingNativeScriptTxSubmitError(
      "the required script is present; this is an honest transaction.",
    );
  }
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
    anchorTxId: badTxInclusion.nativeTxId,
    nativeTxCompactCbor: badTxInclusion.nativeTxCompactCbor,
    itemCbors: badTxSpendInputs.map(encodeMidgardTxInputCanonicalV1),
    owner,
    label: "missing-native-script-tx evidence spend inputs",
  });
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: producingTxInclusion.nativeTxId,
    nativeTxCompactCbor: producingTxInclusion.nativeTxCompactCbor,
    itemCbors: producingOutputItemCbors,
    owner,
    label: "missing-native-script-tx evidence producing outputs",
  });
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
    anchorTxId: badTxInclusion.nativeTxId,
    nativeTxCompactCbor: badTxInclusion.nativeTxCompactCbor,
    itemCbors: badTxScriptWitnessItemCbors,
    owner,
    witnessSet: badTxWitnessSet,
    anchorWitnessSetHash: badTxInclusion.nativeTx.witness_set_hash,
    label: "missing-native-script-tx evidence script witnesses",
  });
  return {
    badTxInclusion,
    badTxSpendInputs,
    badInputIndex,
    producingTxInclusion,
    producingOutputItemCbors,
    missingNativeScriptBytes,
    badTxWitnessSet,
    badTxScriptWitnessItemCbors,
    expectedMissingScriptHash,
  };
};
