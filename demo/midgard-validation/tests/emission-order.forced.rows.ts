import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";

import { RejectCodes } from "../src/index.js";
import {
  EMPTY_SIDECAR,
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
 * accepts. The non-canonical output's own control is `BAD_OUTPUT_CONTROL`:
 * the trace builder does not build a rejected output proof, so that row
 * asserts the producer alone.
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
  reason: { pendingInputIndex: { InputNotFound: { source_kind: 0n } } },
};
const SPEND_SIGNER_MISSING: Expected = {
  code: RejectCodes.MissingRequiredWitness,
  reason: { pendingInputIndex: { SpendInputSignerMissing: {} } },
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

const at = (
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
    sidecar: EMPTY_SIDECAR,
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

/** The non-canonical output alone; see the header. */
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
