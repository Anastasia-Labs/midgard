import {
  asArray,
  asBytes,
  decodeMidgardNativeTxProofFieldLengths,
  decodeSingleCbor,
  verifyMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import { verifyMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { midgardTxFieldCommitmentsFromSource } from "@al-ft/midgard-core/consensus-validation";
import {
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type RedeemerItemStageKey } from "../../redeemer-item-plan.js";
import { fetchUtxoByOutRef, outRefLabel, parseOutRef } from "../../runtime.js";

export type ValidationCekProgramMaterialReferenceOutRefs = {
  /** Exact immutable complete-material datum outref. */
  readonly singlePublication?: string;
  /** Exact entry datums in strict material-root order. */
  readonly minimumMultiOutput?: readonly string[];
};

/**
 * Published spending-validator references for the multi-transaction semantic
 * routes. Keys mirror the deployed validation-dispute stage names; every
 * stage selected by a route must have a published entry.
 */
export type ValidationDisputeStageReferenceScriptUtxos = {
  readonly sharedRedeemerItem?: ReadonlyMap<string, UTxO>;
  readonly scriptSourcesEnvelope?: UTxO;
  readonly scriptSourcesTraversalNormalizer?: UTxO;
  readonly scriptSourcesOuterNormalizer?: UTxO;
  readonly scriptSourcesFoldMapExecutor?: UTxO;
  readonly scriptSourcesFinalizeFrameExecutor?: UTxO;
  readonly scriptSourcesSettlement?: UTxO;
  readonly canonicalDecodeItemSource?: UTxO;
  readonly canonicalDecodeItemProof?: UTxO;
  readonly canonicalDecodeItemSettlement?: UTxO;
};

export type ValidationCekRejectedLocalRouteAttempt = {
  readonly route:
    | "directProof"
    | "completeSinglePublicationReference"
    | "minimumMultiOutputReconstruction";
  readonly failure: string;
};

export const errorMessage = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

export const isDeterministicLocalCekFitFailure = (cause: unknown): boolean => {
  const message = errorMessage(cause);
  return /(?:complete signed L1 proof transaction must be no larger|maximum transaction size|maxTxSize|transaction.{0,24}(?:too large|too big)|maxValueSize|maximum value size|value.{0,24}(?:too large|too big)|maximum execution|execution (?:memory|cpu|units).{0,24}(?:exceed|too (?:large|big))|execution went over budget|ExUnitsTooBig)/iu.test(
    message,
  );
};

export const requireConfirmedCekMaterialReferenceUtxo = async ({
  lucid,
  outRef,
  expectedAddress,
  expectedDatum,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly outRef: string;
  readonly expectedAddress: string;
  readonly expectedDatum: string;
  readonly label: string;
}): Promise<UTxO> => {
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(outRef, label),
    label,
  });
  if (utxo.address !== expectedAddress) {
    throw new Error(
      `${label} ${outRefLabel(utxo)} is locked at ${utxo.address}, expected immutable CEK material address ${expectedAddress}`,
    );
  }
  if (utxo.datum !== expectedDatum) {
    throw new Error(
      `${label} ${outRefLabel(utxo)} does not carry the exact expected inline datum`,
    );
  }
  return utxo;
};

export type SubmitValidationDisputeSemanticResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly proofItemCarriage: "direct" | "reference";
  /**
   * How the semantic *validator itself* rode on the single resolution
   * transaction: attached inline, or supplied by a published reference script
   * (the CEK entries, and since #634 the ValueAndMint ones). Absent on the
   * staged-chain routes, which have no single semantic-validator attachment.
   */
  readonly semanticValidatorCarriage?: "inline" | "reference";
  readonly proofItemReferenceOutRef?: string;
  /**
   * Present when an inline observe build was refused pre-sign for exceeding
   * the L1 proof envelope and the builder fell back to the reference route
   * (#621). The refused transaction was never signed or submitted.
   */
  readonly proofItemInlineEnvelopeRefusal?: {
    readonly projectedSignedBytes: number;
    readonly maxTransactionBytes: number;
  };
  readonly proofItemPublication?: {
    readonly txHash: string;
    readonly outRef: string;
    readonly outputIndex: number;
    readonly completeSignedBytes: number;
    readonly lovelace: bigint;
    readonly awaitedConfirmation: true;
  };
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
  readonly stageTransactions?: readonly {
    readonly kind:
      | RedeemerItemStageKey
      | "authenticate"
      | "source"
      | "observe"
      | "proof"
      | "envelope"
      | "traversal"
      | "outer"
      | "execute-fold-map"
      | "execute-finalize-frame"
      | "settle";
    readonly txHash: string;
    readonly nextThreadOutRef: string;
    readonly completeSignedBytes: number;
    /**
     * The pre-sign envelope projection this stage was admitted under, when
     * the inline delivery route projected it (#621). Signed bytes equal to
     * the projection are the projection's own correctness pin.
     */
    readonly projectedSignedBytes?: number;
  }[];
  /**
   * Present for a CEK execution selection (resolver 11, semantic resolver 1):
   * the program-material route the submitted transaction carried, the
   * material reference inputs it named (root order, canonical indices), and
   * every local route attempt refused pre-sign for a deterministic fit
   * failure before the selected route fit.
   */
  readonly cekRoute?: ValidationCekSelectedRoute;
  readonly cekMaterialReferenceInputOutRefs?: readonly string[];
  readonly cekMaterialReferenceInputIndices?: readonly number[];
  readonly cekRejectedLocalRouteAttempts?: readonly ValidationCekRejectedLocalRouteAttempt[];
};

export type ValidationCekSelectedRoute =
  | "noCekMaterial"
  | "authenticatedMaterialTraversal"
  | "directProof"
  | "completeSinglePublicationReference"
  | "minimumMultiOutputReconstruction";

const exactSafeCborInteger = (value: unknown, label: string): number => {
  const integer =
    typeof value === "bigint"
      ? value
      : typeof value === "number" && Number.isSafeInteger(value)
        ? BigInt(value)
        : undefined;
  if (
    integer === undefined ||
    integer < BigInt(Number.MIN_SAFE_INTEGER) ||
    integer > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(`${label} must be an exact safe CBOR integer`);
  }
  return Number(integer);
};

const canonicalCborArgumentHeaderSize = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("Canonical CBOR argument must be non-negative");
  }
  if (value < 24) return 1;
  if (value < 0x100) return 2;
  if (value < 0x1_0000) return 3;
  if (value < 0x1_0000_0000) return 5;
  return 9;
};

const canonicalFieldItemEncodedLength = ({
  fieldIndex,
  itemLength,
}: {
  readonly fieldIndex: number;
  readonly itemLength: number;
}): number | null => {
  if ([0, 1, 2, 3, 4, 7].includes(fieldIndex)) {
    return canonicalCborArgumentHeaderSize(itemLength) + itemLength;
  }
  if (fieldIndex === 6 || fieldIndex === 8) return itemLength;
  if (fieldIndex !== 5) {
    throw new Error(`Unknown canonical field index ${fieldIndex.toString()}`);
  }
  return itemLength === 0 ? null : itemLength - 1;
};

/**
 * #597. The staged datums observe what the §8 door established, not what a
 * prover claimed: `fieldPreimage` is the whole §5.1 preimage the carriage
 * delivers, the item count is §5.2's own decode of it, and the item's bytes are
 * a slice. The retired `collectionProof`/`itemCbor` pair claimed both, and §4
 * left the claim nothing to be checked against.
 */
export const deriveCanonicalDecodeItemStageData = ({
  preparedResolution,
  transition,
  fieldPreimage,
}: {
  readonly preparedResolution: NonNullable<
    PreparedValidationResolutionDatumData["data"]
  >;
  readonly transition: ValidationOneStepWitness;
  readonly fieldPreimage: string;
}) => {
  const control = asArray(
    decodeSingleCbor(Buffer.from(transition.work_witness_cbor, "hex")),
    "canonical_decode_item.control",
  );
  if (control.length !== 9) {
    throw new Error("Canonical decode item control must contain nine fields");
  }
  const compactCbor = asBytes(control[0], "canonical_decode_item.compact");
  const witnessSetCompactCbor = asBytes(
    control[1],
    "canonical_decode_item.witness_set",
  );
  const fieldPreimageLengthsCbor = asBytes(
    control[2],
    "canonical_decode_item.field_lengths",
  );
  const fieldIndex = exactSafeCborInteger(
    control[4],
    "canonical_decode_item.field_index",
  );
  const itemIndex = exactSafeCborInteger(
    control[5],
    "canonical_decode_item.item_index",
  );
  const chunkIndex = exactSafeCborInteger(
    control[6],
    "canonical_decode_item.chunk_index",
  );
  const itemCount = exactSafeCborInteger(
    control[7],
    "canonical_decode_item.item_count",
  );
  const encodedLength = exactSafeCborInteger(
    control[8],
    "canonical_decode_item.encoded_length",
  );
  const proofSource = {
    compactCbor,
    witnessSetCompactCbor,
    fieldPreimageLengthsCbor,
  };
  // Called for its verification, not its value: it binds these compact structures
  // to the disputed transaction id, which the positional extraction below does not.
  const sourceKind =
    preparedResolution.resolution.pre_state.source_kind === "Forced"
      ? "forced"
      : "normal";
  (sourceKind === "forced"
    ? verifyMidgardForcedTxProofSource
    : verifyMidgardNativeTxProofSource)({
    transactionId: Buffer.from(
      preparedResolution.resolution.pre_state.transaction_id,
      "hex",
    ),
    source: proofSource,
  });
  const lengths = decodeMidgardNativeTxProofFieldLengths(
    fieldPreimageLengthsCbor,
  );
  // The §4 positional extraction, taken from the one implementation of it rather
  // than hand-copied for a third time. `verifyMidgardNativeTxProofSource` above
  // stays: the helper deliberately does not authenticate the source, and binding
  // these structures to `pre_state.transaction_id` is what that call is for.
  const fieldCommitments = midgardTxFieldCommitmentsFromSource(
    proofSource,
    sourceKind,
  );
  const expectedFieldCommitment = fieldCommitments[fieldIndex];
  const expectedFieldLength = lengths[fieldIndex];
  if (
    expectedFieldCommitment === undefined ||
    expectedFieldLength === undefined
  ) {
    throw new Error("Canonical decode item field index is out of range");
  }
  // §8: the door authenticates the whole preimage against the flat §4
  // commitment, so the item count and the item's bytes are *derived* here rather
  // than claimed. Authenticating first is what makes the derivation meaningful.
  const fieldPreimageBytes = Buffer.from(fieldPreimage, "hex");
  const actualFieldCommitment = midgardFieldCommitment(fieldPreimageBytes);
  if (!actualFieldCommitment.equals(Buffer.from(expectedFieldCommitment))) {
    throw new Error(
      "Canonical decode item carriage preimage does not hash to the committed field",
    );
  }
  if (fieldPreimageBytes.length !== expectedFieldLength) {
    throw new Error(
      "Canonical decode item carriage preimage contradicts its declared field length",
    );
  }
  const fieldItems = decodeMidgardFieldPreimage(fieldPreimageBytes);
  const itemBytes = fieldItems[itemIndex];
  if (itemBytes === undefined) {
    throw new Error("Canonical decode item index is outside the field");
  }
  const proofItemCount = fieldItems.length;
  const firstItem =
    itemIndex === 0 &&
    chunkIndex === 0 &&
    itemCount === -1 &&
    encodedLength === 0;
  const continuingItem =
    chunkIndex === 0 &&
    itemCount > 0 &&
    itemIndex < itemCount &&
    proofItemCount === itemCount;
  if (!firstItem && !continuingItem) {
    throw new Error("Canonical decode item control is not an active item");
  }
  const activeItemCount = firstItem ? proofItemCount : itemCount;
  const itemEncodedLength = canonicalFieldItemEncodedLength({
    fieldIndex,
    itemLength: itemBytes.length,
  });
  const nextEncodedLength =
    itemEncodedLength === null
      ? 0
      : (firstItem
          ? canonicalCborArgumentHeaderSize(activeItemCount)
          : encodedLength) + itemEncodedLength;
  const authenticated = {
    version: 1n,
    base: preparedResolution,
    transition,
  };
  const prepared = {
    version: 1n,
    authenticated,
    source: {
      expected_field_commitment: Buffer.from(expectedFieldCommitment).toString(
        "hex",
      ),
      expected_field_length: BigInt(expectedFieldLength),
    },
  };
  const observed = {
    version: 1n,
    prepared,
    observation: {
      item_count: BigInt(proofItemCount),
      item_length: BigInt(itemBytes.length),
    },
  };
  const verified = {
    version: 1n,
    observed,
    proof: {
      active_item_count: BigInt(activeItemCount),
      item_encoding_is_valid: itemEncodedLength !== null,
      next_encoded_length: BigInt(nextEncodedLength),
    },
  };
  return { authenticated, prepared, observed, verified };
};
