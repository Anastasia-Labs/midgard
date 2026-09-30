import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  buildUnsignedValidationProofItemPublicationProgram,
  deriveValidationProofItemPublication,
  minimumLovelaceForValidationProofItemPublication,
  ValidationProofItemDatum,
  type ValidationProofItemPublication,
} from "@al-ft/midgard-sdk";
import {
  Data,
  PROTOCOL_PARAMETERS_DEFAULT,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCanonicalDecodeItemCase,
  type CompleteSignedTransactionMeasurement,
  type EmulatorHarness,
  measureCompleteSignedTransaction,
  sameDatumValue,
  setupEmulator,
} from "./complete-item-proof-fit-emulator.build-canonical-decode-item-case.js";
import {
  type CanonicalDecodeItemCase,
  makeExactSizeOutputItem,
  TIER1_MAX_COMPLETE_ITEM_BYTES,
} from "./complete-item-proof-fit-emulator.build-trace-with-outputs.js";
import {
  type CompleteItemJourney,
  publishReferenceScript,
  runJourneyToObserve,
} from "./complete-item-proof-fit-emulator.submit-stage.js";

type PublishedProofItem = {
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly utxo: UTxO;
  readonly datumCbor: string;
  readonly minAdaLovelace: bigint;
};

const publishProofItemPublication = async ({
  harness,
  publication,
}: {
  readonly harness: EmulatorHarness;
  readonly publication: ValidationProofItemPublication;
}): Promise<PublishedProofItem> => {
  const minAdaLovelace = minimumLovelaceForValidationProofItemPublication({
    contracts: harness.contracts,
    publication,
    coinsPerUtxoByte: BigInt(PROTOCOL_PARAMETERS_DEFAULT.coinsPerUtxoByte),
  });
  const unsigned = await Effect.runPromise(
    buildUnsignedValidationProofItemPublicationProgram(
      harness.lucid,
      harness.contracts,
      publication,
    ),
  );
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  const utxo = (
    await harness.lucid.utxosAt(
      harness.contracts.validationTraceDispute.proofItem.spendingScriptAddress,
    )
  ).find(
    (candidate) =>
      candidate.txHash === txHash &&
      candidate.datum != null &&
      sameDatumValue(candidate.datum, publication.datumCbor),
  );
  if (utxo === undefined) {
    throw new Error("published proof item was not found");
  }
  return {
    measurement: measureCompleteSignedTransaction(signedCbor),
    utxo,
    datumCbor: publication.datumCbor,
    minAdaLovelace,
  };
};

export const publishProofItem = async ({
  harness,
  itemCase,
  fieldPreimageHexOverride,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly fieldPreimageHexOverride?: string;
}): Promise<PublishedProofItem> => {
  const preState = itemCase.preState;
  const publication = deriveValidationProofItemPublication({
    transactionId: preState.transaction_id,
    transactionCommitment: preState.transaction_commitment,
    fieldPreimage: fieldPreimageHexOverride ?? itemCase.fieldPreimageHex,
  });
  return await publishProofItemPublication({ harness, publication });
};

const buildRawProofItemPublicationForNegativeControl = ({
  itemCase,
  fieldPreimage,
}: {
  readonly itemCase: CanonicalDecodeItemCase;
  readonly fieldPreimage: string;
}): ValidationProofItemPublication => {
  const datum: ValidationProofItemPublication["datum"] = {
    version: 1n,
    transaction_id: itemCase.preState.transaction_id,
    transaction_commitment: itemCase.preState.transaction_commitment,
    field_preimage: fieldPreimage,
  };
  return {
    datum,
    datumCbor: Data.to(datum, ValidationProofItemDatum),
  };
};

export const publishRawProofItemForNegativeControl = async ({
  harness,
  itemCase,
  fieldPreimage,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly fieldPreimage: string;
}): Promise<PublishedProofItem> =>
  await publishProofItemPublication({
    harness,
    publication: buildRawProofItemPublicationForNegativeControl({
      itemCase,
      fieldPreimage,
    }),
  });

/**
 * The size-bearing measurement, post-Option-B: one journey from the semantic
 * resolver to the observe door with the §5.1 preimage delivered INLINE, which
 * is where the item now rides. Both reference scripts are parked first, so the
 * measured transactions carry indices rather than validator bodies — the
 * production basis since the #617 reference-script wiring.
 *
 * The whole journey is returned, not just the observe row: the authenticate
 * and source rows are what make "every non-observe stage is item-size
 * independent" (#622's finding, the precondition of the lane-level re-pins)
 * checkable here rather than merely quoted.
 */
export const measureObserveAt = async (
  itemBytes: number,
  options: { readonly embedObserveValidator?: boolean } = {},
): Promise<CompleteItemJourney> => {
  const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
  const harness = await setupEmulator([itemCase.preparedThreadDatum]);
  const semanticScriptReference = await publishReferenceScript(
    harness,
    harness.semanticScript,
    "2f",
  );
  const observeScriptReference =
    options.embedObserveValidator === true
      ? undefined
      : await publishReferenceScript(
          harness,
          harness.stages.observe.spendingScript,
          "3f",
        );
  return await runJourneyToObserve({
    harness,
    itemCase,
    threadUtxo: harness.threadUtxos[0]!,
    delivery: { kind: "inline" },
    ...(observeScriptReference === undefined ? {} : { observeScriptReference }),
    semanticScriptReference,
  });
};

// `measureReferenceAt` lived here and went with the publication-maximum row it
// was the only caller of — see the removal note in the describe block below.
// Reference-carriage consumption is still measured in this file by the
// "reaches the identical terminal state through direct and reference carriage"
// row, which resolves a published item at a tier-1 size.

export const measurePublicationFrontierAt = async (
  itemByteCandidates: readonly number[],
): Promise<
  readonly {
    readonly itemBytes: number;
    readonly publication: Awaited<ReturnType<typeof publishProofItem>>;
  }[]
> => {
  // Retargeted 2026-08-14 (owner ruling) from
  // `maxSinglePublicationCompleteItemBytes` (14,396) to the tier-1 maximum. The
  // base case only supplies the thread datum and the transaction identity — every
  // probe below overrides the field preimage outright — but it still has to be a
  // case this tier-1-only harness can build, and a 14,396-byte item's field-2
  // preimage is 14,400 bytes, i.e. tier-2. See TIER1_MAX_COMPLETE_ITEM_BYTES.
  const itemCase = await buildCanonicalDecodeItemCase(
    TIER1_MAX_COMPLETE_ITEM_BYTES,
  );
  const harness = await setupEmulator([itemCase.preparedThreadDatum]);
  const measurements = [];
  for (const itemBytes of itemByteCandidates) {
    measurements.push({
      itemBytes,
      publication: await publishProofItem({
        harness,
        itemCase,
        fieldPreimageHexOverride: encodeMidgardFieldPreimage([
          makeExactSizeOutputItem(itemBytes),
        ]).toString("hex"),
      }),
    });
  }
  return measurements;
};
