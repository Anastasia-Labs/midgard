import { encodeCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";

import { RejectCodes } from "../src/index.js";
import {
  type Expected,
  funded,
  keyAddress,
  keySpent,
  type NativeTxOptions,
  type Row,
} from "./emission-order.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * Emission order of a forced transaction. The tx-order mint admits a forced
 * transaction whose outputs or script items do not decode, so the machine's
 * first fault can sit at any phase before such an item is read. An output is
 * read only at the ScriptSources output scan, after input resolution; a
 * script item's header only at its turn in the Phase A native-script scan,
 * after the signatures; and an item's width at the canonical scan, before
 * anything else.
 *
 * Each multi-fault row has a single-fault control per fault, and one row
 * accepts.
 */

const ABSENT = outRefFromByte(0x70);
/** A key-locked output whose key does not sign the rows. */
const unsigned = makeOutput(FUNDED_OUTPUT_LOVELACE, keyAddress(0, 0x6c));
/** An output item that does not decode. */
const BAD_OUTPUT = Buffer.from([0xff]);
/** An output item one byte over the width bound, which does not decode. */
const WIDE_OUTPUT = Buffer.alloc(
  MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes + 1,
  0xff,
);
/** A field-6 item whose versioned-script header does not parse. */
const BAD_HEADER = Buffer.from("8209", "hex");

const SPEND_NOT_FOUND: Expected = {
  code: RejectCodes.InputNotFound,
  reason: {
    pendingInputIndex: { InputNotFound: { source_kind: 0n } },
    producerInputOrdinal: 0n,
  },
};
const SPEND_SIGNER_MISSING: Expected = {
  code: RejectCodes.MissingRequiredWitness,
  reason: {
    pendingInputIndex: { SpendInputSignerMissing: {} },
    producerInputOrdinal: 0n,
  },
};
const SIGNATURE_INVALID: Expected = {
  code: RejectCodes.InvalidSignature,
  reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
};
const HEADER_MALFORMED: Expected = {
  code: RejectCodes.InvalidFieldType,
  reason: { WitnessScriptHeaderMalformed: { script_index: 0n } },
};

type Spend = { readonly outRef: Buffer; readonly output?: Buffer };
const SIGNED: Spend = { outRef: keySpent, output: funded };

export const at = (
  name: string,
  kind: Row["kind"],
  guard: boolean,
  expected: Expected | null,
  spend: Spend,
  options: Omit<NativeTxOptions, "spendInputs"> & {
    readonly scriptItems?: readonly Buffer[];
  } = {},
): Row => {
  const { scriptItems, ...tx } = options;
  return {
    name: `f_${name}`,
    kind,
    guard,
    source: "forced",
    state:
      spend.output === undefined ? [] : [[spend.outRef, spend.output] as const],
    tx: { outputs: [funded], ...tx, spendInputs: [spend.outRef] },
    ...(scriptItems === undefined ? {} : { scriptItems }),
    expected,
  };
};

export const FORCED_ROWS: readonly Row[] = [
  at("accept", "accept", true, null, SIGNED),
  // Input resolution precedes the output scan.
  at(
    "missing_spend_before_bad_output",
    "multi",
    false,
    SPEND_NOT_FOUND,
    { outRef: ABSENT },
    { outputs: [funded, BAD_OUTPUT] },
  ),
  at(
    "unsigned_spend_before_bad_output",
    "multi",
    false,
    SPEND_SIGNER_MISSING,
    { outRef: ABSENT, output: unsigned },
    { outputs: [funded, BAD_OUTPUT] },
  ),
  // The signatures precede the output scan and the native-script scan.
  at(
    "invalid_signature_before_bad_output",
    "multi",
    false,
    SIGNATURE_INVALID,
    SIGNED,
    { invalidVkeyWitness: true, outputs: [funded, BAD_OUTPUT] },
  ),
  at(
    "invalid_signature_before_bad_header",
    "multi",
    false,
    SIGNATURE_INVALID,
    SIGNED,
    { invalidVkeyWitness: true, scriptItems: [BAD_HEADER] },
  ),
  // The native-script scan precedes the output scan.
  at("bad_header_before_bad_output", "multi", false, HEADER_MALFORMED, SIGNED, {
    scriptItems: [BAD_HEADER],
    outputs: [funded, BAD_OUTPUT],
  }),
  at("control_missing_spend", "control", true, SPEND_NOT_FOUND, {
    outRef: ABSENT,
  }),
  at("control_unsigned_spend", "control", true, SPEND_SIGNER_MISSING, {
    outRef: ABSENT,
    output: unsigned,
  }),
  at("control_invalid_signature", "control", true, SIGNATURE_INVALID, SIGNED, {
    invalidVkeyWitness: true,
  }),
  at("control_bad_header", "control", false, HEADER_MALFORMED, SIGNED, {
    scriptItems: [BAD_HEADER],
  }),
];

/**
 * The width bound precedes everything: the item is also not an output. Its
 * field is past the inline carriage cap, so the stopping step's one-step
 * argument needs the transaction's reference-input carriage, which a row has
 * not; the row asserts the producer and the trace.
 */
export const WIDE_OUTPUT_ROW: Row = at(
  "wide_output",
  "multi",
  false,
  {
    code: RejectCodes.InvalidFieldType,
    reason: { FieldItemWidthIllegal: { field_index: 2n, item_index: 0n } },
  },
  SIGNED,
  { outputs: [WIDE_OUTPUT] },
);

/** The non-canonical output alone. */
export const BAD_OUTPUT_CONTROL: Row = at(
  "control_bad_output",
  "control",
  true,
  {
    code: RejectCodes.InvalidOutput,
    reason: { OutputNonCanonical: { output_index: 1n } },
  },
  SIGNED,
  { outputs: [funded, BAD_OUTPUT] },
);

/** `funded` with one more field, `key`, holding `value` (its CBOR). */
const withField = (key: bigint, value: string): Buffer =>
  Buffer.concat([
    Buffer.from([funded[0]! + 1]),
    funded.subarray(1),
    encodeCbor(key),
    Buffer.from(value, "hex"),
  ]);
const withScriptRef = (scriptRef: string): Buffer => withField(3n, scriptRef);
/** An inline datum, the integer 1 in a non-minimal head: not canonical. */
const NON_CANONICAL_DATUM = withField(2n, "421801");
/** `funded` followed by one more byte: the output does not end at its map. */
const TRAILING_BYTE = Buffer.concat([funded, Buffer.from([0x00])]);
/** A native reference script requiring one key: canonical. */
const NATIVE_REF = withScriptRef(`820058208200581c${"6b".repeat(28)}`);
/** A native reference script whose body is not a native script. */
const MALFORMED_REF = withScriptRef("820043820700");
/** A native reference script with an empty body. */
const EMPTY_NATIVE_REF = withScriptRef("820040");
/** A Plutus V3 reference script whose program does not decode. */
export const UNDECODABLE_PLUTUS_REF = withScriptRef("820341" + "00");

const output = (
  name: string,
  kind: Row["kind"],
  guard: boolean,
  expected: Expected | null,
  outputs: readonly Buffer[],
): Row => ({
  ...at(name, kind, guard, expected, SIGNED, { outputs }),
  name: `fo_${name}`,
});
const refMalformed = (index: bigint): Expected => ({
  code: RejectCodes.InvalidFieldType,
  reason: { OutputReferenceScriptMalformed: { output_index: index } },
});
const plutusRefUndecodable = (index: bigint): Expected => ({
  code: RejectCodes.ScriptProgramEncoding,
  reason: {
    PlutusScriptUndecodable: { item_kind: 1n, item_index: index, class: 1n },
  },
});
const nonCanonical = (index: bigint): Expected => ({
  code: RejectCodes.InvalidOutput,
  reason: { OutputNonCanonical: { output_index: index } },
});

/**
 * The class of a forced transaction's faulty output, as the output proof
 * decides it. A native reference script that does not parse is its own
 * class; an output that does not parse is non-canonical; the first faulty
 * output is the one cited. An undecodable Plutus reference script is refused
 * at its own output (D3, forced too); the output proof has no class for it
 * yet, so its rows' traces are red until it does (TI-3).
 */
export const FORCED_OUTPUT_ROWS: readonly Row[] = [
  output("accept_native_ref", "accept", true, null, [NATIVE_REF]),
  output("control_malformed_ref", "control", false, refMalformed(0n), [
    MALFORMED_REF,
  ]),
  output("control_empty_native_ref", "control", false, refMalformed(0n), [
    EMPTY_NATIVE_REF,
  ]),
  output("control_bad_output", "control", true, nonCanonical(0n), [BAD_OUTPUT]),
  output("control_trailing_byte", "control", true, nonCanonical(0n), [
    TRAILING_BYTE,
  ]),
  output("control_non_canonical_datum", "control", true, nonCanonical(0n), [
    NON_CANONICAL_DATUM,
  ]),
  output(
    "non_canonical_datum_before_malformed_ref",
    "multi",
    true,
    nonCanonical(0n),
    [NON_CANONICAL_DATUM, MALFORMED_REF],
  ),
  output("malformed_ref_before_bad_output", "multi", false, refMalformed(0n), [
    MALFORMED_REF,
    BAD_OUTPUT,
  ]),
  output("bad_output_before_malformed_ref", "multi", true, nonCanonical(0n), [
    BAD_OUTPUT,
    MALFORMED_REF,
  ]),
  output(
    "plutus_ref_before_malformed_ref",
    "multi",
    false,
    plutusRefUndecodable(0n),
    [UNDECODABLE_PLUTUS_REF, MALFORMED_REF],
  ),
];

/**
 * An undecodable Plutus reference script alone. Its script bytes are not a
 * CBOR byte string: decode-record class 1.
 */
export const UNDECODABLE_PLUTUS_REF_ROW: Row = {
  ...at("plutus_ref", "control", false, plutusRefUndecodable(0n), SIGNED, {
    outputs: [UNDECODABLE_PLUTUS_REF],
  }),
  name: "fp_plutus_ref",
};
