import {
  compareBytes,
  readCborArrayHeader,
  readCborBytes,
  readCborBytesHeader,
  readCborMapHeader,
  readCborUnsigned,
} from "./codec/cbor.js";
import { hashMidgardLedgerOutputAssetLeaf } from "./ledger-output-commitment.js";
import {
  absoluteOffset,
  encodedCborLength,
  encodedMapHeaderLength,
  type MidgardLedgerOutputScanControl,
  MidgardLedgerOutputScanStages,
  optionalFieldsComplete,
  readKey,
} from "./ledger-output-scan.encode-midgard-ledger-output-scan-control.js";
import {
  appendMidgardValidationMerkleLeaf,
  MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT,
} from "./validation-merkle.js";

export const stepValueHeader = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl => {
  const valueOffset = readKey(window, windowOffset, 1n);
  const value = readCborArrayHeader(window, valueOffset, "ledger_output.value");
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
        ? MidgardLedgerOutputScanStages.OptionalField
        : MidgardLedgerOutputScanStages.PolicyHeader,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: policies.nextOffset,
    }),
    lovelace: lovelace.value,
    cardanoValueSize:
      policies.length === 0
        ? encodedCborLength(lovelace.value)
        : 1 +
          encodedCborLength(lovelace.value) +
          encodedMapHeaderLength(policies.length),
    policyRemaining: policies.length,
  };
};

export const stepPolicyHeader = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl => {
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
    stage: MidgardLedgerOutputScanStages.Asset,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: assets.nextOffset,
    }),
    assetRemaining: assets.length,
    policyAssetCursor: 0,
    currentPolicy: policy.value,
    previousAssetName: Buffer.alloc(0),
    cardanoValueSize:
      control.cardanoValueSize +
      encodedCborLength(policy.value) +
      encodedMapHeaderLength(assets.length),
  };
};

const compareCanonicalAssetNames = (
  left: Uint8Array,
  right: Uint8Array,
): number => left.length - right.length || compareBytes(left, right);

export const stepAsset = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl => {
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
    control.assetFrontier.count >= MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT ||
    (control.policyAssetCursor > 0 &&
      compareCanonicalAssetNames(control.previousAssetName, assetName.value) >=
        0)
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
        ? MidgardLedgerOutputScanStages.OptionalField
        : MidgardLedgerOutputScanStages.PolicyHeader
      : MidgardLedgerOutputScanStages.Asset,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: quantity.nextOffset,
    }),
    policyRemaining: nextPolicyRemaining,
    assetRemaining: nextAssetRemaining,
    policyAssetCursor: policyComplete ? 0 : control.policyAssetCursor + 1,
    previousPolicy: policyComplete
      ? control.currentPolicy
      : control.previousPolicy,
    currentPolicy: policyComplete ? Buffer.alloc(0) : control.currentPolicy,
    previousAssetName: policyComplete ? Buffer.alloc(0) : assetName.value,
    assetFrontier: appendMidgardValidationMerkleLeaf(
      control.assetFrontier,
      hashMidgardLedgerOutputAssetLeaf({
        policyId: control.currentPolicy,
        assetName: assetName.value,
        quantity: quantity.value,
      }),
    ),
    cardanoValueSize:
      control.cardanoValueSize +
      encodedCborLength(assetName.value) +
      encodedCborLength(quantity.value),
  };
};

const stepReferenceScriptHeader = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl => {
  const scriptOffset = readKey(window, windowOffset, 3n);
  const scriptItemOffset = absoluteOffset({
    control,
    windowOffset,
    localOffset: scriptOffset,
  });
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
        ? MidgardLedgerOutputScanStages.Terminal
        : MidgardLedgerOutputScanStages.ReferenceScriptPayload,
    cursor: payloadOffset,
    optionalFieldCount: control.optionalFieldCount + 1,
    payloadRemaining: payload.length,
    referenceScriptLanguage: Number(language.value) as 0 | 3 | 128,
    referenceScriptItemOffset: scriptItemOffset,
    referenceScriptOffset: payloadOffset,
    referenceScriptLength: payload.length,
  };
};

const stepDatumHeader = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl => {
  const datumOffset = readKey(window, windowOffset, 2n);
  const datum = readCborBytesHeader(window, datumOffset, "ledger_output.datum");
  if (datum.length === 0) {
    throw new Error("V1 inline datum must contain canonical Plutus Data");
  }
  const payloadOffset = absoluteOffset({
    control,
    windowOffset,
    localOffset: datum.nextOffset,
  });
  return {
    ...control,
    stage: MidgardLedgerOutputScanStages.DatumPayload,
    cursor: payloadOffset,
    optionalFieldCount: control.optionalFieldCount + 1,
    datumOffset: payloadOffset,
    datumLength: datum.length,
    payloadRemaining: datum.length,
  };
};

export const stepOptionalField = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl => {
  if (optionalFieldsComplete(control)) {
    return {
      ...control,
      stage: MidgardLedgerOutputScanStages.Terminal,
    };
  }
  if (control.mapEntryCount === 4 && control.optionalFieldCount === 0) {
    return stepDatumHeader({
      control,
      window,
      windowOffset,
    });
  }
  const key = readCborUnsigned(window, windowOffset, "ledger_output.key");
  if (key.value === 2n) {
    return stepDatumHeader({
      control,
      window,
      windowOffset,
    });
  }
  if (key.value === 3n) {
    return stepReferenceScriptHeader({ control, window, windowOffset });
  }
  throw new Error("Unknown V1 ledger output optional field");
};
