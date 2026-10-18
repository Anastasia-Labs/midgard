import { encodeCborInteger } from "./codec/cbor.js";
import { readCborInteger, readCborTag } from "./codec/cbor.read-argument.js";
import { type Hash32 } from "./codec/hash.js";
import {
  fail,
  hashDomain,
  REJECTION_CODE_DOMAIN,
} from "./validation-trace.encode-midgard-validation-machine-state.js";

/**
 * Structural twin of `midgard/rejection_reason_v1.RejectionReasonV1`.
 *
 * Declaration order is wire-normative: arm `i` is the Plutus Data
 * constructor index `i`, and each arm's field list fixes its positional
 * field order. Arms only ever append.
 */
export const MIDGARD_REJECTION_REASON_V1_ARMS = [
  { name: "FieldPreimageLengthMismatch", fields: ["field_index"] },
  { name: "FieldItemWidthIllegal", fields: ["field_index", "item_index"] },
  { name: "EmptyInputs", fields: [] },
  {
    name: "DuplicateInput",
    fields: [
      "first_field_index",
      "first_item_index",
      "second_field_index",
      "second_item_index",
    ],
  },
  { name: "ValidityIntervalMalformed", fields: [] },
  { name: "NetworkIdMismatch", fields: [] },
  { name: "FeeBelowMinimum", fields: [] },
  { name: "AddressWitnessSignatureInvalid", fields: ["witness_index"] },
  { name: "RequiredSignerUnsigned", fields: ["signer_index"] },
  { name: "WitnessScriptHeaderMalformed", fields: ["script_index"] },
  { name: "WitnessNativeScriptMalformed", fields: ["script_index"] },
  { name: "WitnessNativeScriptNodeLimit", fields: ["script_index"] },
  { name: "WitnessNativeScriptDepthLimit", fields: ["script_index"] },
  { name: "WitnessNativeScriptFalse", fields: ["script_index"] },
  { name: "ScriptIntegrityHashMissing", fields: [] },
  { name: "ObserversForbiddenOnUntaggedNetwork", fields: [] },
  { name: "ObserverOrderInvalid", fields: ["observer_index"] },
  { name: "ValidityIntervalExcludesBlockSlot", fields: [] },
  { name: "InputNotFound", fields: ["source_kind", "input_index"] },
  {
    name: "InputSpentOutputNonCanonical",
    fields: ["source_kind", "input_index"],
  },
  {
    name: "ResolvedReferenceScriptMalformed",
    fields: ["source_kind", "input_index"],
  },
  {
    name: "ResolvedReferenceScriptNodeLimit",
    fields: ["source_kind", "input_index"],
  },
  {
    name: "ResolvedReferenceScriptDepthLimit",
    fields: ["source_kind", "input_index"],
  },
  { name: "SpendInputSignerMissing", fields: ["input_index"] },
  { name: "RedeemerMalformed", fields: ["redeemer_index"] },
  { name: "OutputNonCanonical", fields: ["output_index"] },
  { name: "OutputReferenceScriptMalformed", fields: ["output_index"] },
  { name: "OutputReferenceScriptNodeLimit", fields: ["output_index"] },
  { name: "OutputReferenceScriptDepthLimit", fields: ["output_index"] },
  { name: "ProtectedOutputSignerMissing", fields: ["output_index"] },
  { name: "MintDeclaredAssetLimit", fields: ["policy_index"] },
  { name: "ScriptSourceMissing", fields: ["purpose_kind", "purpose_index"] },
  { name: "RedeemerMissing", fields: ["purpose_kind", "purpose_index"] },
  { name: "UnusedScriptWitness", fields: ["script_index"] },
  { name: "UnusedRedeemer", fields: ["redeemer_index"] },
  {
    name: "ExecutionNativeScriptMalformed",
    fields: ["purpose_kind", "purpose_index"],
  },
  {
    name: "ExecutionNativeScriptNodeLimit",
    fields: ["purpose_kind", "purpose_index"],
  },
  {
    name: "ExecutionNativeScriptDepthLimit",
    fields: ["purpose_kind", "purpose_index"],
  },
  {
    name: "ExecutionNativeScriptFalse",
    fields: ["purpose_kind", "purpose_index"],
  },
  { name: "ScriptIntegrityHashMismatch", fields: [] },
  {
    name: "ReceivePurposePlutusV3Forbidden",
    fields: ["purpose_kind", "purpose_index"],
  },
  { name: "PlutusExecutionFailed", fields: ["purpose_kind", "purpose_index"] },
  {
    name: "InputAssetAccumulationLimit",
    fields: ["input_index", "asset_index"],
  },
  {
    name: "OutputAssetAccumulationLimit",
    fields: ["output_index", "asset_index"],
  },
  { name: "MintAssetAccumulationLimit", fields: ["mint_index"] },
  { name: "OutputBelowMinAda", fields: ["output_index"] },
  { name: "ValueNotPreserved", fields: [] },
  { name: "MintItemMalformed", fields: ["policy_index"] },
  {
    name: "PlutusScriptUndecodable",
    fields: ["item_kind", "item_index", "class"],
  },
] as const;

type MidgardRejectionReasonV1ArmEntry =
  (typeof MIDGARD_REJECTION_REASON_V1_ARMS)[number];

export type MidgardRejectionReasonV1ArmName =
  MidgardRejectionReasonV1ArmEntry["name"];

type ReasonOfArm<Entry> = Entry extends {
  readonly name: infer Name extends string;
  readonly fields: readonly [];
}
  ? Name
  : Entry extends {
        readonly name: infer Name extends string;
        readonly fields: infer Fields extends readonly string[];
      }
    ? { readonly [Arm in Name]: { readonly [Field in Fields[number]]: bigint } }
    : never;

/**
 * A rejection reason in the same shape the SDK's Lucid schema produces: a
 * bare arm name for a field-less arm, otherwise a single-key object naming
 * the arm and carrying its coordinates.
 */
export type MidgardRejectionReasonV1 =
  ReasonOfArm<MidgardRejectionReasonV1ArmEntry>;

const ARM_INDEX_BY_NAME: ReadonlyMap<string, number> = new Map(
  MIDGARD_REJECTION_REASON_V1_ARMS.map((entry, index) => [entry.name, index]),
);

const MAX_COORDINATE_MAGNITUDE = 1n << 64n;

const constructorTag = (armIndex: number): bigint =>
  armIndex <= 6 ? BigInt(121 + armIndex) : BigInt(1280 + (armIndex - 7));

const encodeTagHeader = (tag: bigint): Buffer =>
  tag < 256n
    ? Buffer.from([0xd8, Number(tag)])
    : Buffer.from([0xd9, Number(tag >> 8n), Number(tag & 0xffn)]);

/** The arm index and positional coordinates of `reason`. */
export const midgardRejectionReasonV1Parts = (
  reason: MidgardRejectionReasonV1,
): { readonly armIndex: number; readonly coordinates: readonly bigint[] } => {
  const armName: string =
    typeof reason === "string" ? reason : (Object.keys(reason)[0] ?? "");
  const armIndex = ARM_INDEX_BY_NAME.get(armName);
  if (armIndex === undefined) {
    return fail("Unknown RejectionReasonV1 arm", armName);
  }
  const fields: readonly string[] =
    MIDGARD_REJECTION_REASON_V1_ARMS[armIndex]!.fields;
  if (typeof reason === "string") {
    if (fields.length !== 0) {
      return fail("RejectionReasonV1 arm requires coordinates", armName);
    }
    return { armIndex, coordinates: [] };
  }
  if (fields.length === 0) {
    return fail(
      "Field-less RejectionReasonV1 arm must be a bare name",
      armName,
    );
  }
  if (Object.keys(reason).length !== 1) {
    return fail("RejectionReasonV1 must name exactly one arm", armName);
  }
  const payload = (reason as Readonly<Record<string, unknown>>)[armName];
  if (typeof payload !== "object" || payload === null) {
    return fail("RejectionReasonV1 coordinates must be an object", armName);
  }
  const payloadKeys = Object.keys(payload);
  if (
    payloadKeys.length !== fields.length ||
    payloadKeys.some((key) => !fields.includes(key))
  ) {
    return fail(
      "RejectionReasonV1 coordinates do not match the arm's fields",
      armName,
    );
  }
  const coordinates = fields.map((field) => {
    const value = (payload as Readonly<Record<string, unknown>>)[field];
    if (typeof value !== "bigint") {
      return fail("RejectionReasonV1 coordinate must be a bigint", field);
    }
    if (
      value >= MAX_COORDINATE_MAGNITUDE ||
      value < -MAX_COORDINATE_MAGNITUDE
    ) {
      return fail("RejectionReasonV1 coordinate is out of range", field);
    }
    return value;
  });
  return { armIndex, coordinates };
};

/**
 * Plutus Data CBOR of `reason`, byte-identical to Aiken's
 * `cbor.serialise(reason)`: the constructor tag, then the coordinates as an
 * indefinite-length list, or a definite empty list for a field-less arm.
 */
export const encodeMidgardRejectionReasonV1Cbor = (
  reason: MidgardRejectionReasonV1,
): Buffer => {
  const { armIndex, coordinates } = midgardRejectionReasonV1Parts(reason);
  const body =
    coordinates.length === 0
      ? Buffer.from([0x80])
      : Buffer.concat([
          Buffer.from([0x9f]),
          ...coordinates.map(encodeCborInteger),
          Buffer.from([0xff]),
        ]);
  return Buffer.concat([encodeTagHeader(constructorTag(armIndex)), body]);
};

/**
 * The descriptor's rejection-code hash for `reason`:
 * blake2b-256("MidgardValidationRejectCodeV1" ‖ reason CBOR).
 */
export const hashMidgardValidationRejectionReasonV1 = (
  reason: MidgardRejectionReasonV1,
): Hash32 =>
  hashDomain(REJECTION_CODE_DOMAIN, encodeMidgardRejectionReasonV1Cbor(reason));

const armIndexOfTag = (tag: bigint): number | undefined => {
  if (tag >= 121n && tag <= 127n) return Number(tag - 121n);
  if (
    tag >= 1280n &&
    tag < 1280n + BigInt(MIDGARD_REJECTION_REASON_V1_ARMS.length - 7)
  )
    return Number(tag - 1280n) + 7;
  return undefined;
};

/**
 * Decodes the one canonical `RejectionReasonV1` Plutus Data item that starts
 * at `offset`, refusing an unknown constructor, a wrong field count and any
 * encoding other than the one {@link encodeMidgardRejectionReasonV1Cbor}
 * writes.
 */
export const decodeMidgardRejectionReasonV1Cbor = (
  bytes: Uint8Array,
  offset = 0,
): {
  readonly reason: MidgardRejectionReasonV1;
  readonly nextOffset: number;
} => {
  const tag = readCborTag(bytes, offset, "RejectionReasonV1 constructor");
  const armIndex = armIndexOfTag(tag.value);
  if (armIndex === undefined) {
    return fail("Unknown RejectionReasonV1 constructor", String(tag.value));
  }
  const arm = MIDGARD_REJECTION_REASON_V1_ARMS[armIndex]!;
  const fields: readonly string[] = arm.fields;
  let cursor = tag.nextOffset;
  const coordinates: bigint[] = [];
  if (bytes[cursor] === 0x9f) {
    cursor += 1;
    while (cursor < bytes.length && bytes[cursor] !== 0xff) {
      const coordinate = readCborInteger(
        bytes,
        cursor,
        "RejectionReasonV1 coordinate",
      );
      coordinates.push(coordinate.value);
      cursor = coordinate.nextOffset;
    }
    if (bytes[cursor] !== 0xff) {
      return fail("Unterminated RejectionReasonV1 fields", arm.name);
    }
    cursor += 1;
  } else if (bytes[cursor] === 0x80) {
    cursor += 1;
  } else {
    return fail(
      "RejectionReasonV1 fields must be a Plutus Data list",
      arm.name,
    );
  }
  if (coordinates.length !== fields.length) {
    return fail("RejectionReasonV1 field count differs from its arm", arm.name);
  }
  const reason = (
    fields.length === 0
      ? arm.name
      : {
          [arm.name]: Object.fromEntries(
            fields.map((field, index) => [field, coordinates[index]!]),
          ),
        }
  ) as MidgardRejectionReasonV1;
  if (
    !encodeMidgardRejectionReasonV1Cbor(reason).equals(
      Buffer.from(bytes.subarray(offset, cursor)),
    )
  ) {
    return fail("RejectionReasonV1 CBOR is not canonical", arm.name);
  }
  return { reason, nextOffset: cursor };
};
