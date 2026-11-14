import { encodeCborUnsigned } from "./cbor.js";
import {
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldPreimage,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
  midgardFieldCommitment,
} from "./native-tx-field-access.js";
import {
  encodeMidgardHash28Item,
  encodeMidgardMintFieldItems,
  encodeMidgardSpendInputItem,
  exactBytes,
  fail,
  type MidgardMintPolicyItem,
  type MidgardTxInput,
} from "./native-tx-field-items.encode-midgard-mint-policy-item.js";
import { encodeMidgardTxOutput, type MidgardTxOutput } from "./output.js";
import {
  encodeMidgardVersionedScript,
  type MidgardVersionedScript,
} from "./versioned-script.js";

// ---------------------------------------------------------------------------
// §5.3 field 7 — address (vkey) witnesses
// ---------------------------------------------------------------------------

export type MidgardAddressWitness = {
  /** 32-byte Ed25519 verification key. */
  readonly verificationKey: Uint8Array;
  /** 64-byte Ed25519 signature. */
  readonly signature: Uint8Array;
};

/**
 * §5.3 field 7: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)` — fixed 101 bytes,
 * stride 103. Twin of `encode_midgard_address_witness`.
 */
export const encodeMidgardAddressWitnessItem = (
  witness: MidgardAddressWitness,
): Buffer => {
  const encoded = Buffer.concat([
    Buffer.from([0x82]),
    encodeMidgardDefiniteBytes(
      exactBytes(witness.verificationKey, 32, "witness verification key"),
    ),
    encodeMidgardDefiniteBytes(
      exactBytes(witness.signature, 64, "witness signature"),
    ),
  ]);
  if (encoded.length !== MIDGARD_ADDRESS_WITNESS_ITEM_BYTES) {
    return fail(
      "address witness item is not the §5.3 fixed width",
      `length=${encoded.length}`,
    );
  }
  return encoded;
};

// ---------------------------------------------------------------------------
// §5.3 field 8 — redeemer witnesses
// ---------------------------------------------------------------------------

/**
 * §5.3's `purpose_tag` value set — exactly seven values, every one ≤ 23 so each
 * occupies one byte equal to its value. Values 0–5 reuse Cardano's own
 * `RedeemerTag` numbering; 6 (`Receive`) is Midgard-only.
 *
 * Two narrower sets sit inside this one and are deliberately **not** the
 * format's bound: the Midgard builder emits only `Spend`/`Mint`/`Reward`/
 * `Receive`, and the Cardano conversion bridge admits only `Spend`/`Mint`/
 * `Reward`. Twin of `midgard_redeemer_purpose_to_tag`.
 */
export const MIDGARD_REDEEMER_PURPOSE_TAGS = {
  Spend: 0,
  Mint: 1,
  Cert: 2,
  Reward: 3,
  Vote: 4,
  Propose: 5,
  Receive: 6,
} as const;

export type MidgardRedeemerPurpose = keyof typeof MIDGARD_REDEEMER_PURPOSE_TAGS;

// The inverse map (tag → purpose) deliberately has no twin here. This module is
// a producer; a tag-to-purpose reader belongs on the decoding side, and §5.3
// names `midgard_redeemer_purpose_from_tag` in
// `fraud-proofs/native-tx/components.ak` as the place that rejects an
// out-of-set tag. Spelling a second one here — with no caller — would put a
// decoder in a producer module and give the value set two homes.

export type MidgardExecutionUnits = {
  readonly memory: bigint;
  readonly steps: bigint;
};

export type MidgardRedeemerWitness = {
  readonly purpose: MidgardRedeemerPurpose;
  /** Non-negative, canonical minimal CBOR uint. */
  readonly index: bigint;
  readonly redeemerCbor: Uint8Array;
  readonly executionUnits: MidgardExecutionUnits;
};

const exactNonNegative = (value: bigint, label: string): bigint => {
  if (value < 0n) {
    return fail(`${label} must be non-negative`, `${value}`);
  }
  return value;
};

/**
 * §5.3 field 8:
 * `84 ‖ uint(purpose_tag) ‖ uint(index) ‖ bytes(redeemer_cbor) ‖ 82 ‖ uint(ex_memory) ‖ uint(ex_steps)`.
 *
 * Note the shape: a four-element array whose last element is a two-element
 * array, spelled inline rather than nested through a helper — that is what the
 * Aiken twin emits, and the §5.1 envelope wraps the whole thing. Twin of
 * `encode_midgard_redeemer_witness`.
 */
export const encodeMidgardRedeemerWitnessItem = (
  witness: MidgardRedeemerWitness,
): Buffer =>
  Buffer.concat([
    Buffer.from([0x84]),
    encodeCborUnsigned(BigInt(MIDGARD_REDEEMER_PURPOSE_TAGS[witness.purpose])),
    encodeCborUnsigned(exactNonNegative(witness.index, "redeemer index")),
    encodeMidgardDefiniteBytes(witness.redeemerCbor),
    Buffer.from([0x82]),
    encodeCborUnsigned(
      exactNonNegative(witness.executionUnits.memory, "ex_memory"),
    ),
    encodeCborUnsigned(
      exactNonNegative(witness.executionUnits.steps, "ex_steps"),
    ),
  ]);

// ---------------------------------------------------------------------------
// The nine fields, dispatched by §2.5 index
// ---------------------------------------------------------------------------

/**
 * A single field's items, tagged by the §2.5 field index they belong to. The
 * tag is the field index rather than a name because §4 makes field identity
 * *positional*: fields 0/1 and 3/4 share item encoders, so identical content
 * aliases across those pairs, and the index is the only thing that says which
 * slot a preimage was built for.
 */
export type MidgardFieldItems =
  | { readonly fieldIndex: 0 | 1; readonly items: readonly MidgardTxInput[] }
  | { readonly fieldIndex: 2; readonly items: readonly MidgardTxOutput[] }
  | { readonly fieldIndex: 3 | 4; readonly items: readonly Uint8Array[] }
  | {
      readonly fieldIndex: 5;
      readonly items: readonly MidgardMintPolicyItem[];
    }
  | {
      readonly fieldIndex: 6;
      readonly items: readonly MidgardVersionedScript[];
    }
  | {
      readonly fieldIndex: 7;
      readonly items: readonly MidgardAddressWitness[];
    }
  | {
      readonly fieldIndex: 8;
      readonly items: readonly MidgardRedeemerWitness[];
    };

/**
 * The §5.3 `enc_i` bytes of one field's items — the per-item half of the
 * grammar, before the §5.1 envelope goes on.
 */
export const encodeMidgardFieldItems = (
  field: MidgardFieldItems,
): readonly Buffer[] => {
  switch (field.fieldIndex) {
    case 0:
    case 1:
      return field.items.map(encodeMidgardSpendInputItem);
    case 2:
      return field.items.map(encodeMidgardTxOutput);
    case 3:
    case 4:
      return field.items.map(encodeMidgardHash28Item);
    case 5:
      return encodeMidgardMintFieldItems(field.items);
    case 6:
      return field.items.map(encodeMidgardVersionedScript);
    case 7:
      return field.items.map(encodeMidgardAddressWitnessItem);
    case 8:
      return field.items.map(encodeMidgardRedeemerWitnessItem);
  }
};

/**
 * The §5.1 preimage of one field: `definite_array_header(N)` followed by one
 * definite byte-string-wrapped `enc_i` per item. An empty field is exactly `80`
 * — all nine, including mint (§5.6), which under the retired scheme spelled it
 * `a0`.
 *
 * Twin of the six `encode_*_preimage` producers in `preimages.ak`, which differ
 * from each other only in the item encoder they map.
 */
export const encodeMidgardFieldPreimageForField = (
  field: MidgardFieldItems,
): Buffer => encodeMidgardFieldPreimage(encodeMidgardFieldItems(field));

/**
 * §4 — the flat commitment of one field: `blake2b_256(preimage)`, plain, with
 * no domain tag, version prefix or field index in the hash input.
 */
export const midgardFieldCommitmentForField = (
  field: MidgardFieldItems,
): Buffer => midgardFieldCommitment(encodeMidgardFieldPreimageForField(field));

/**
 * The §2.5 field names, positionally indexed, for diagnostics and vector
 * labelling. §4 makes field identity positional, so the index — not the name —
 * is what any encoder dispatches on; this array only ever labels one.
 */
export const MIDGARD_FIELD_NAMES = [
  "spend_inputs",
  "reference_inputs",
  "outputs",
  "required_observers",
  "required_signers",
  "mint",
  "script_witnesses",
  "address_witnesses",
  "redeemers",
] as const;
