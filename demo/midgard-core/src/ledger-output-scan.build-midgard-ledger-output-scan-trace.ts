import { MIDGARD_BOUNDED_ITEM_CHUNK_BYTES } from "./bounded-item.js";
import { readCborBytes, readCborUnsigned } from "./codec/cbor.js";
import { type MidgardLedgerOutputAsset } from "./ledger-output-commitment.js";
import {
  initialMidgardLedgerOutputScanControl,
  isWellFormedMidgardLedgerOutputScanControl,
  type MidgardLedgerOutputScanControl,
  MidgardLedgerOutputScanStages,
  type MidgardLedgerOutputScanTrace,
  type MidgardLedgerOutputScanTraceStep,
  optionalFieldsComplete,
  stepRequiredFields,
} from "./ledger-output-scan.encode-midgard-ledger-output-scan-control.js";
import {
  stepAsset,
  stepOptionalField,
  stepPolicyHeader,
  stepValueHeader,
} from "./ledger-output-scan.step-asset.js";

const stepPayload = ({
  control,
  totalLength,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly totalLength: number;
}): MidgardLedgerOutputScanControl => {
  const chunkRemaining =
    MIDGARD_BOUNDED_ITEM_CHUNK_BYTES -
    (control.cursor % MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
  const consumed = Math.min(control.payloadRemaining, chunkRemaining);
  if (
    control.payloadRemaining <= 0 ||
    consumed <= 0 ||
    consumed > totalLength - control.cursor
  ) {
    throw new Error("Invalid V1 ledger output payload span");
  }
  const payloadRemaining = control.payloadRemaining - consumed;
  return {
    ...control,
    stage:
      payloadRemaining === 0
        ? MidgardLedgerOutputScanStages.OptionalField
        : control.stage,
    cursor: control.cursor + consumed,
    payloadRemaining,
  };
};

export const advanceMidgardLedgerOutputScan = ({
  control,
  totalLength,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly totalLength: number;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl | null => {
  try {
    if (
      !isWellFormedMidgardLedgerOutputScanControl(control) ||
      !Number.isSafeInteger(control.cursor) ||
      control.cursor < 0 ||
      control.cursor > totalLength ||
      !Number.isSafeInteger(windowOffset) ||
      windowOffset < 0 ||
      windowOffset >= window.length
    ) {
      return null;
    }
    let next: MidgardLedgerOutputScanControl;
    // Unknown numeric stages fail closed through the default arm.
    // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
    switch (control.stage) {
      case MidgardLedgerOutputScanStages.RequiredFields:
        next = stepRequiredFields({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStages.ValueHeader:
        next = stepValueHeader({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStages.PolicyHeader:
        next = stepPolicyHeader({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStages.Asset:
        next = stepAsset({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStages.OptionalField:
        next = stepOptionalField({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStages.DatumPayload:
      case MidgardLedgerOutputScanStages.ReferenceScriptPayload:
        next = stepPayload({ control, totalLength });
        break;
      default:
        return null;
    }
    if (
      !isWellFormedMidgardLedgerOutputScanControl(next) ||
      next.cursor < control.cursor ||
      next.cursor > totalLength ||
      (next.cursor === control.cursor && next.stage === control.stage)
    ) {
      return null;
    }
    return next;
  } catch {
    return null;
  }
};

export const finishMidgardLedgerOutputScan = ({
  control,
  totalLength,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly totalLength: number;
}): MidgardLedgerOutputScanControl | null =>
  isWellFormedMidgardLedgerOutputScanControl(control) &&
  control.stage === MidgardLedgerOutputScanStages.OptionalField &&
  optionalFieldsComplete(control) &&
  control.cursor === totalLength &&
  control.payloadRemaining === 0
    ? {
        ...control,
        stage: MidgardLedgerOutputScanStages.Terminal,
      }
    : null;

export const isExactMidgardLedgerOutputScanTerminal = ({
  control,
  totalLength,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly totalLength: number;
}): boolean =>
  isWellFormedMidgardLedgerOutputScanControl(control) &&
  control.stage === MidgardLedgerOutputScanStages.Terminal &&
  control.cursor === totalLength &&
  control.mapEntryCount >= 2 &&
  control.mapEntryCount <= 4 &&
  optionalFieldsComplete(control) &&
  (control.address.length === 29 || control.address.length === 57) &&
  control.lovelace >= 0n &&
  control.cardanoValueSize > 0 &&
  control.policyRemaining === 0 &&
  control.assetRemaining === 0 &&
  control.policyAssetCursor === 0 &&
  control.currentPolicy.length === 0 &&
  control.previousAssetName.length === 0 &&
  control.payloadRemaining === 0 &&
  (control.datumOffset === -1
    ? control.datumLength === 0
    : control.datumOffset >= 0 &&
      control.datumLength > 0 &&
      control.datumOffset + control.datumLength <= totalLength) &&
  (control.referenceScriptLanguage === -1
    ? control.referenceScriptItemOffset === -1 &&
      control.referenceScriptOffset === -1 &&
      control.referenceScriptLength === 0
    : control.referenceScriptItemOffset >= 0 &&
      control.referenceScriptItemOffset < control.referenceScriptOffset &&
      control.referenceScriptLength >= 0 &&
      control.referenceScriptOffset + control.referenceScriptLength ===
        totalLength);

export const buildMidgardLedgerOutputScanTrace = (
  outputCbor: Uint8Array,
): MidgardLedgerOutputScanTrace => {
  const bytes = Buffer.from(outputCbor);
  const initial = initialMidgardLedgerOutputScanControl();
  const steps: MidgardLedgerOutputScanTraceStep[] = [];
  let control = initial;
  const maximumSteps = bytes.length + 32;
  while (
    control.stage !== MidgardLedgerOutputScanStages.Terminal &&
    steps.length < maximumSteps
  ) {
    const finished = finishMidgardLedgerOutputScan({
      control,
      totalLength: bytes.length,
    });
    if (finished !== null) {
      steps.push({
        control,
        next: finished,
        chunkIndex: null,
        nextChunkIndex: null,
        asset: null,
      });
      control = finished;
      continue;
    }
    const chunkIndex = Math.floor(
      control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
    );
    const chunkStart = chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
    const currentChunk = bytes.subarray(
      chunkStart,
      chunkStart + MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
    );
    const tokenStage =
      control.stage <= MidgardLedgerOutputScanStages.OptionalField;
    const nextChunkStart = chunkStart + MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
    const hasNextChunk = nextChunkStart < bytes.length;
    const nextChunk =
      tokenStage && hasNextChunk
        ? bytes.subarray(
            nextChunkStart,
            nextChunkStart + MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
          )
        : Buffer.alloc(0);
    const window = Buffer.concat([currentChunk, nextChunk]);
    const next = advanceMidgardLedgerOutputScan({
      control,
      totalLength: bytes.length,
      window,
      windowOffset: control.cursor - chunkStart,
    });
    if (next === null) {
      throw new Error("Canonical V1 ledger output scan failed closed");
    }
    let asset: MidgardLedgerOutputAsset | null = null;
    if (control.stage === MidgardLedgerOutputScanStages.Asset) {
      const assetName = readCborBytes(
        window,
        control.cursor - chunkStart,
        "ledger_output.trace.asset_name",
      );
      const quantity = readCborUnsigned(
        window,
        assetName.nextOffset,
        "ledger_output.trace.quantity",
      );
      asset = {
        policyId: Buffer.from(control.currentPolicy),
        assetName: assetName.value,
        quantity: quantity.value,
      };
    }
    steps.push({
      control,
      next,
      chunkIndex,
      nextChunkIndex: tokenStage && hasNextChunk ? chunkIndex + 1 : null,
      asset,
    });
    control = next;
  }
  if (
    !isExactMidgardLedgerOutputScanTerminal({
      control,
      totalLength: bytes.length,
    })
  ) {
    throw new Error("Canonical V1 ledger output scan did not terminate");
  }
  return { initial, steps, terminal: control };
};
