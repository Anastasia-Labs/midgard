import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
} from "./bounded-item-v1.js";
import { decodeMidgardAddressBytes } from "./codec/address.js";
import {
  compareBytes,
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborBytesHeader,
  readCborMapHeader,
  readCborUnsigned,
} from "./codec/cbor.js";
import {
  hashMidgardLedgerOutputAssetLeafV1,
} from "./ledger-output-commitment-v1.js";
import {
  appendMidgardValidationMerkleLeafV1,
  emptyMidgardValidationMerkleFrontierV1,
  MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT,
  type MidgardValidationMerkleFrontierV1,
  validateMidgardValidationMerkleFrontierV1,
} from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_SCAN_V1_VERSION = 1 as const;

export const MidgardLedgerOutputScanStagesV1 = Object.freeze({
  RequiredFields: 0,
  ValueHeader: 1,
  PolicyHeader: 2,
  Asset: 3,
  OptionalField: 4,
  DatumPayload: 5,
  ReferenceScriptPayload: 6,
  Terminal: 7,
} as const);

export type MidgardLedgerOutputScanStageV1 =
  (typeof MidgardLedgerOutputScanStagesV1)[keyof typeof MidgardLedgerOutputScanStagesV1];

export type MidgardLedgerOutputScanControlV1 = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_SCAN_V1_VERSION;
  readonly stage: MidgardLedgerOutputScanStageV1;
  readonly cursor: number;
  readonly mapEntryCount: number;
  readonly optionalFieldCount: number;
  readonly address: Buffer;
  readonly lovelace: bigint;
  readonly policyRemaining: number;
  readonly assetRemaining: number;
  readonly policyAssetCursor: number;
  readonly previousPolicy: Buffer;
  readonly currentPolicy: Buffer;
  readonly previousAssetName: Buffer;
  readonly assetFrontier: MidgardValidationMerkleFrontierV1;
  readonly datumOffset: number;
  readonly datumLength: number;
  readonly payloadRemaining: number;
  readonly referenceScriptLanguage: -1 | 0 | 3 | 128;
  readonly referenceScriptOffset: number;
  readonly referenceScriptLength: number;
};

export type MidgardLedgerOutputScanTraceStepV1 = {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly next: MidgardLedgerOutputScanControlV1;
  readonly chunkIndex: number | null;
  readonly nextChunkIndex: number | null;
};

export type MidgardLedgerOutputScanTraceV1 = {
  readonly initial: MidgardLedgerOutputScanControlV1;
  readonly steps: readonly MidgardLedgerOutputScanTraceStepV1[];
  readonly terminal: MidgardLedgerOutputScanControlV1;
};

export const initialMidgardLedgerOutputScanControlV1 =
  (): MidgardLedgerOutputScanControlV1 => ({
    version: MIDGARD_LEDGER_OUTPUT_SCAN_V1_VERSION,
    stage: MidgardLedgerOutputScanStagesV1.RequiredFields,
    cursor: 0,
    mapEntryCount: 0,
    optionalFieldCount: 0,
    address: Buffer.alloc(0),
    lovelace: 0n,
    policyRemaining: 0,
    assetRemaining: 0,
    policyAssetCursor: 0,
    previousPolicy: Buffer.alloc(0),
    currentPolicy: Buffer.alloc(0),
    previousAssetName: Buffer.alloc(0),
    assetFrontier: emptyMidgardValidationMerkleFrontierV1(),
    datumOffset: -1,
    datumLength: 0,
    payloadRemaining: 0,
    referenceScriptLanguage: -1,
    referenceScriptOffset: -1,
    referenceScriptLength: 0,
  });

const assertSafeControlInteger = ({
  value,
  field,
  minimum,
  maximum = Number.MAX_SAFE_INTEGER,
}: {
  readonly value: number;
  readonly field: string;
  readonly minimum: number;
  readonly maximum?: number;
}): void => {
  if (
    !Number.isSafeInteger(value) ||
    value < minimum ||
    value > maximum
  ) {
    throw new Error(`Invalid V1 ledger output scan ${field}`);
  }
};

export const encodeMidgardLedgerOutputScanControlV1 = (
  control: MidgardLedgerOutputScanControlV1,
): Buffer => {
  if (control.version !== MIDGARD_LEDGER_OUTPUT_SCAN_V1_VERSION) {
    throw new Error("Invalid V1 ledger output scan version");
  }
  assertSafeControlInteger({
    value: control.stage,
    field: "stage",
    minimum: MidgardLedgerOutputScanStagesV1.RequiredFields,
    maximum: MidgardLedgerOutputScanStagesV1.Terminal,
  });
  assertSafeControlInteger({
    value: control.cursor,
    field: "cursor",
    minimum: 0,
  });
  assertSafeControlInteger({
    value: control.mapEntryCount,
    field: "map entry count",
    minimum: 0,
    maximum: 4,
  });
  if (
    control.mapEntryCount !== 0 &&
    control.mapEntryCount < 2
  ) {
    throw new Error("Invalid V1 ledger output scan map entry count");
  }
  assertSafeControlInteger({
    value: control.optionalFieldCount,
    field: "optional field count",
    minimum: 0,
    maximum: 2,
  });
  if (control.address.length !== 0) {
    decodeMidgardAddressBytes(control.address);
  }
  if (control.lovelace < 0n) {
    throw new Error("Invalid V1 ledger output scan lovelace");
  }
  for (const [field, value] of [
    ["policy remaining", control.policyRemaining],
    ["asset remaining", control.assetRemaining],
    ["policy asset cursor", control.policyAssetCursor],
    ["datum length", control.datumLength],
    ["payload remaining", control.payloadRemaining],
    ["reference script length", control.referenceScriptLength],
  ] as const) {
    assertSafeControlInteger({ value, field, minimum: 0 });
  }
  for (const [field, bytes, maximum] of [
    ["previous policy", control.previousPolicy, 28],
    ["current policy", control.currentPolicy, 28],
    ["previous asset name", control.previousAssetName, 32],
  ] as const) {
    if (
      bytes.length > maximum ||
      (maximum === 28 && bytes.length !== 0 && bytes.length !== 28)
    ) {
      throw new Error(`Invalid V1 ledger output scan ${field}`);
    }
  }
  validateMidgardValidationMerkleFrontierV1(control.assetFrontier);
  for (const [field, value] of [
    ["datum offset", control.datumOffset],
    ["reference script offset", control.referenceScriptOffset],
  ] as const) {
    assertSafeControlInteger({ value, field, minimum: -1 });
  }
  if (control.datumOffset === -1 && control.datumLength !== 0) {
    throw new Error("Invalid V1 ledger output scan datum span");
  }
  if (
    control.referenceScriptLanguage !== -1 &&
    control.referenceScriptLanguage !== 0 &&
    control.referenceScriptLanguage !== 3 &&
    control.referenceScriptLanguage !== 128
  ) {
    throw new Error("Invalid V1 ledger output scan reference language");
  }
  if (
    control.referenceScriptLanguage === -1
      ? control.referenceScriptOffset !== -1 ||
        control.referenceScriptLength !== 0
      : control.referenceScriptOffset < 0
  ) {
    throw new Error("Invalid V1 ledger output scan reference span");
  }
  return encodeCbor([
    1n,
    BigInt(control.stage),
    BigInt(control.cursor),
    BigInt(control.mapEntryCount),
    BigInt(control.optionalFieldCount),
    control.address,
    control.lovelace,
    BigInt(control.policyRemaining),
    BigInt(control.assetRemaining),
    BigInt(control.policyAssetCursor),
    control.previousPolicy,
    control.currentPolicy,
    control.previousAssetName,
    BigInt(control.assetFrontier.count),
    control.assetFrontier.peaks.map(({ height, hash }) => [
      BigInt(height),
      hash,
    ]),
    BigInt(control.datumOffset),
    BigInt(control.datumLength),
    BigInt(control.payloadRemaining),
    BigInt(control.referenceScriptLanguage),
    BigInt(control.referenceScriptOffset),
    BigInt(control.referenceScriptLength),
  ]);
};

export const isWellFormedMidgardLedgerOutputScanControlV1 = (
  control: MidgardLedgerOutputScanControlV1,
): boolean => {
  try {
    encodeMidgardLedgerOutputScanControlV1(control);
    return true;
  } catch {
    return false;
  }
};

const absoluteOffset = ({
  control,
  windowOffset,
  localOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly windowOffset: number;
  readonly localOffset: number;
}): number => control.cursor + localOffset - windowOffset;

const readKey = (
  window: Uint8Array,
  offset: number,
  expected: bigint,
): number => {
  const key = readCborUnsigned(window, offset, "ledger_output.key");
  if (key.value !== expected) {
    throw new Error(
      `V1 ledger output expected key ${expected.toString(10)}`,
    );
  }
  return key.nextOffset;
};

const optionalFieldsComplete = (
  control: MidgardLedgerOutputScanControlV1,
): boolean =>
  control.optionalFieldCount + 2 === control.mapEntryCount;

const stepRequiredFields = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControlV1 => {
  const outputMap = readCborMapHeader(
    window,
    windowOffset,
    "ledger_output",
  );
  if (outputMap.length < 2 || outputMap.length > 4) {
    throw new Error("V1 ledger output must contain two to four fields");
  }
  const addressOffset = readKey(window, outputMap.nextOffset, 0n);
  const address = readCborBytes(
    window,
    addressOffset,
    "ledger_output.address",
  );
  decodeMidgardAddressBytes(address.value);
  return {
    ...control,
    stage: MidgardLedgerOutputScanStagesV1.ValueHeader,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: address.nextOffset,
    }),
    mapEntryCount: outputMap.length,
    address: address.value,
  };
};

const stepValueHeader = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControlV1 => {
  const valueOffset = readKey(window, windowOffset, 1n);
  const value = readCborArrayHeader(
    window,
    valueOffset,
    "ledger_output.value",
  );
  if (value.length !== 2) {
    throw new Error("V1 ledger output Value must contain two fields");
  }
  const lovelace = readCborUnsigned(
    window,
    value.nextOffset,
    "ledger_output.value.lovelace",
  );
  const policies = readCborMapHeader(
    window,
    lovelace.nextOffset,
    "ledger_output.value.assets",
  );
  return {
    ...control,
    stage:
      policies.length === 0
        ? MidgardLedgerOutputScanStagesV1.OptionalField
        : MidgardLedgerOutputScanStagesV1.PolicyHeader,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: policies.nextOffset,
    }),
    lovelace: lovelace.value,
    policyRemaining: policies.length,
  };
};

const stepPolicyHeader = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControlV1 => {
  const policy = readCborBytes(
    window,
    windowOffset,
    "ledger_output.value.policy",
  );
  const assets = readCborMapHeader(
    window,
    policy.nextOffset,
    "ledger_output.value.policy.assets",
  );
  if (
    policy.value.length !== 28 ||
    assets.length === 0 ||
    assets.length > MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT ||
    control.policyRemaining <= 0 ||
    (control.previousPolicy.length !== 0 &&
      compareBytes(control.previousPolicy, policy.value) >= 0)
  ) {
    throw new Error("Invalid V1 ledger output policy header");
  }
  return {
    ...control,
    stage: MidgardLedgerOutputScanStagesV1.Asset,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: assets.nextOffset,
    }),
    assetRemaining: assets.length,
    policyAssetCursor: 0,
    currentPolicy: policy.value,
    previousAssetName: Buffer.alloc(0),
  };
};

const compareCanonicalAssetNames = (
  left: Uint8Array,
  right: Uint8Array,
): number => left.length - right.length || compareBytes(left, right);

const stepAsset = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControlV1 => {
  const assetName = readCborBytes(
    window,
    windowOffset,
    "ledger_output.value.asset_name",
  );
  const quantity = readCborUnsigned(
    window,
    assetName.nextOffset,
    "ledger_output.value.quantity",
  );
  if (
    control.currentPolicy.length !== 28 ||
    assetName.value.length > 32 ||
    quantity.value <= 0n ||
    control.assetRemaining <= 0 ||
    control.assetFrontier.count >=
      MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT ||
    (control.policyAssetCursor > 0 &&
      compareCanonicalAssetNames(
        control.previousAssetName,
        assetName.value,
      ) >= 0)
  ) {
    throw new Error("Invalid V1 ledger output asset");
  }
  const nextAssetRemaining = control.assetRemaining - 1;
  const policyComplete = nextAssetRemaining === 0;
  const nextPolicyRemaining =
    control.policyRemaining - (policyComplete ? 1 : 0);
  return {
    ...control,
    stage: policyComplete
      ? nextPolicyRemaining === 0
        ? MidgardLedgerOutputScanStagesV1.OptionalField
        : MidgardLedgerOutputScanStagesV1.PolicyHeader
      : MidgardLedgerOutputScanStagesV1.Asset,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: quantity.nextOffset,
    }),
    policyRemaining: nextPolicyRemaining,
    assetRemaining: nextAssetRemaining,
    policyAssetCursor: policyComplete
      ? 0
      : control.policyAssetCursor + 1,
    previousPolicy: policyComplete
      ? control.currentPolicy
      : control.previousPolicy,
    currentPolicy: policyComplete
      ? Buffer.alloc(0)
      : control.currentPolicy,
    previousAssetName: policyComplete
      ? Buffer.alloc(0)
      : assetName.value,
    assetFrontier: appendMidgardValidationMerkleLeafV1(
      control.assetFrontier,
      hashMidgardLedgerOutputAssetLeafV1({
        policyId: control.currentPolicy,
        assetName: assetName.value,
        quantity: quantity.value,
      }),
    ),
  };
};

const stepReferenceScriptHeader = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControlV1 => {
  const scriptOffset = readKey(window, windowOffset, 3n);
  const script = readCborArrayHeader(
    window,
    scriptOffset,
    "ledger_output.reference_script",
  );
  if (script.length !== 2) {
    throw new Error("V1 reference script must contain two fields");
  }
  const language = readCborUnsigned(
    window,
    script.nextOffset,
    "ledger_output.reference_script.language",
  );
  if (
    language.value !== 0n &&
    language.value !== 3n &&
    language.value !== 128n
  ) {
    throw new Error("Unsupported V1 reference-script language");
  }
  const payload = readCborBytesHeader(
    window,
    language.nextOffset,
    "ledger_output.reference_script.payload",
  );
  const payloadOffset = absoluteOffset({
    control,
    windowOffset,
    localOffset: payload.nextOffset,
  });
  return {
    ...control,
    stage:
      payload.length === 0
        ? MidgardLedgerOutputScanStagesV1.Terminal
        : MidgardLedgerOutputScanStagesV1.ReferenceScriptPayload,
    cursor: payloadOffset,
    optionalFieldCount: control.optionalFieldCount + 1,
    payloadRemaining: payload.length,
    referenceScriptLanguage: Number(language.value) as 0 | 3 | 128,
    referenceScriptOffset: payloadOffset,
    referenceScriptLength: payload.length,
  };
};

const stepDatumHeader = ({
  control,
  window,
  windowOffset,
  terminalWhenEmpty,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
  readonly terminalWhenEmpty: boolean;
}): MidgardLedgerOutputScanControlV1 => {
  const datumOffset = readKey(window, windowOffset, 2n);
  const datum = readCborBytesHeader(
    window,
    datumOffset,
    "ledger_output.datum",
  );
  const payloadOffset = absoluteOffset({
    control,
    windowOffset,
    localOffset: datum.nextOffset,
  });
  return {
    ...control,
    stage:
      datum.length === 0
        ? terminalWhenEmpty
          ? MidgardLedgerOutputScanStagesV1.Terminal
          : MidgardLedgerOutputScanStagesV1.OptionalField
        : MidgardLedgerOutputScanStagesV1.DatumPayload,
    cursor: payloadOffset,
    optionalFieldCount: control.optionalFieldCount + 1,
    datumOffset: payloadOffset,
    datumLength: datum.length,
    payloadRemaining: datum.length,
  };
};

const stepOptionalField = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControlV1 => {
  if (optionalFieldsComplete(control)) {
    return {
      ...control,
      stage: MidgardLedgerOutputScanStagesV1.Terminal,
    };
  }
  if (control.mapEntryCount === 4 && control.optionalFieldCount === 0) {
    return stepDatumHeader({
      control,
      window,
      windowOffset,
      terminalWhenEmpty: false,
    });
  }
  const key = readCborUnsigned(window, windowOffset, "ledger_output.key");
  if (key.value === 2n) {
    return stepDatumHeader({
      control,
      window,
      windowOffset,
      terminalWhenEmpty: true,
    });
  }
  if (key.value === 3n) {
    return stepReferenceScriptHeader({ control, window, windowOffset });
  }
  throw new Error("Unknown V1 ledger output optional field");
};

const stepPayload = ({
  control,
  totalLength,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly totalLength: number;
}): MidgardLedgerOutputScanControlV1 => {
  const chunkRemaining =
    MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 -
    (control.cursor % MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
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
        ? MidgardLedgerOutputScanStagesV1.OptionalField
        : control.stage,
    cursor: control.cursor + consumed,
    payloadRemaining,
  };
};

export const advanceMidgardLedgerOutputScanV1 = ({
  control,
  totalLength,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly totalLength: number;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControlV1 | null => {
  try {
    if (
      !isWellFormedMidgardLedgerOutputScanControlV1(control) ||
      !Number.isSafeInteger(control.cursor) ||
      control.cursor < 0 ||
      control.cursor > totalLength ||
      !Number.isSafeInteger(windowOffset) ||
      windowOffset < 0 ||
      windowOffset >= window.length
    ) {
      return null;
    }
    let next: MidgardLedgerOutputScanControlV1;
    switch (control.stage) {
      case MidgardLedgerOutputScanStagesV1.RequiredFields:
        next = stepRequiredFields({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStagesV1.ValueHeader:
        next = stepValueHeader({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStagesV1.PolicyHeader:
        next = stepPolicyHeader({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStagesV1.Asset:
        next = stepAsset({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStagesV1.OptionalField:
        next = stepOptionalField({ control, window, windowOffset });
        break;
      case MidgardLedgerOutputScanStagesV1.DatumPayload:
      case MidgardLedgerOutputScanStagesV1.ReferenceScriptPayload:
        next = stepPayload({ control, totalLength });
        break;
      default:
        return null;
    }
    if (
      !isWellFormedMidgardLedgerOutputScanControlV1(next) ||
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

export const finishMidgardLedgerOutputScanV1 = ({
  control,
  totalLength,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly totalLength: number;
}): MidgardLedgerOutputScanControlV1 | null =>
  isWellFormedMidgardLedgerOutputScanControlV1(control) &&
  control.stage === MidgardLedgerOutputScanStagesV1.OptionalField &&
  optionalFieldsComplete(control) &&
  control.cursor === totalLength &&
  control.payloadRemaining === 0
    ? {
        ...control,
        stage: MidgardLedgerOutputScanStagesV1.Terminal,
      }
    : null;

export const isExactMidgardLedgerOutputScanTerminalV1 = ({
  control,
  totalLength,
}: {
  readonly control: MidgardLedgerOutputScanControlV1;
  readonly totalLength: number;
}): boolean =>
  isWellFormedMidgardLedgerOutputScanControlV1(control) &&
  control.stage === MidgardLedgerOutputScanStagesV1.Terminal &&
  control.cursor === totalLength &&
  control.mapEntryCount >= 2 &&
  control.mapEntryCount <= 4 &&
  optionalFieldsComplete(control) &&
  (control.address.length === 29 || control.address.length === 57) &&
  control.lovelace >= 0n &&
  control.policyRemaining === 0 &&
  control.assetRemaining === 0 &&
  control.policyAssetCursor === 0 &&
  control.currentPolicy.length === 0 &&
  control.previousAssetName.length === 0 &&
  control.payloadRemaining === 0 &&
  (control.datumOffset === -1
    ? control.datumLength === 0
    : control.datumOffset >= 0 &&
      control.datumLength >= 0 &&
      control.datumOffset + control.datumLength <= totalLength) &&
  (control.referenceScriptLanguage === -1
    ? control.referenceScriptOffset === -1 &&
      control.referenceScriptLength === 0
    : control.referenceScriptOffset >= 0 &&
      control.referenceScriptLength >= 0 &&
      control.referenceScriptOffset + control.referenceScriptLength ===
        totalLength);

export const buildMidgardLedgerOutputScanTraceV1 = (
  outputCbor: Uint8Array,
): MidgardLedgerOutputScanTraceV1 => {
  const bytes = Buffer.from(outputCbor);
  const initial = initialMidgardLedgerOutputScanControlV1();
  const steps: MidgardLedgerOutputScanTraceStepV1[] = [];
  let control = initial;
  const maximumSteps = bytes.length + 32;
  while (
    control.stage !== MidgardLedgerOutputScanStagesV1.Terminal &&
    steps.length < maximumSteps
  ) {
    const finished = finishMidgardLedgerOutputScanV1({
      control,
      totalLength: bytes.length,
    });
    if (finished !== null) {
      steps.push({
        control,
        next: finished,
        chunkIndex: null,
        nextChunkIndex: null,
      });
      control = finished;
      continue;
    }
    const chunkIndex = Math.floor(
      control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
    );
    const chunkStart =
      chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
    const currentChunk = bytes.subarray(
      chunkStart,
      chunkStart + MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
    );
    const tokenStage =
      control.stage <= MidgardLedgerOutputScanStagesV1.OptionalField;
    const nextChunkStart =
      chunkStart + MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
    const hasNextChunk = nextChunkStart < bytes.length;
    const nextChunk =
      tokenStage && hasNextChunk
        ? bytes.subarray(
            nextChunkStart,
            nextChunkStart + MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
          )
        : Buffer.alloc(0);
    const next = advanceMidgardLedgerOutputScanV1({
      control,
      totalLength: bytes.length,
      window: Buffer.concat([currentChunk, nextChunk]),
      windowOffset: control.cursor - chunkStart,
    });
    if (next === null) {
      throw new Error("Canonical V1 ledger output scan failed closed");
    }
    steps.push({
      control,
      next,
      chunkIndex,
      nextChunkIndex: tokenStage && hasNextChunk
        ? chunkIndex + 1
        : null,
    });
    control = next;
  }
  if (
    !isExactMidgardLedgerOutputScanTerminalV1({
      control,
      totalLength: bytes.length,
    })
  ) {
    throw new Error("Canonical V1 ledger output scan did not terminate");
  }
  return { initial, steps, terminal: control };
};
