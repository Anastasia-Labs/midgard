import { RejectCodes } from "../src/index.js";
import {
  type Expected,
  funded,
  keyAddress,
  mintedOutput,
  mintOf,
  native,
  NATIVE_POLICY,
  type NativeTxOptions,
  type Row,
} from "./emission-order.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * Emission order of input resolution, case (a). The machine resolves spend and
 * reference inputs as one schedule in ascending out-ref order, a spend before
 * a reference with the same key, and checks a key-locked spend's signer when
 * it resolves that spend. Phase B has two routes to that
 * resolution: the full route (local script discovery, here a native mint) and
 * the plain route (no scripts). Each row runs on both, and Phase A's routing
 * is asserted, so a row cannot pass on one route for the other.
 *
 * Each multi-fault row has a single-fault control per fault, and each route
 * has one accepting row. A `guard` row is one the producer already got right
 * before the reconciliation; it pins the order rather than proving a change.
 *
 * `emission-order.aiken-rows.test.ts` checks each row's stopping step on
 * chain, through the Aiken one-step dispatcher.
 *
 * The rows assert each reason's arm and, for InputNotFound, its source kind.
 * The machine's input ordinal (`input_index`) is pending X-N2C2 and is not
 * asserted; the producer's subject ordinal, the input's position in its own
 * field, is.
 */

const LOW = outRefFromByte(0x70);
const HIGH = outRefFromByte(0x90);
/** A key-locked output whose key does not sign the rows. */
const unsigned = makeOutput(FUNDED_OUTPUT_LOVELACE, keyAddress(0, 0x6c));

/** An absent input of `source_kind`, at `ordinal` in its own field. */
const inputNotFound = (source_kind: bigint, ordinal: bigint): Expected => ({
  code: RejectCodes.InputNotFound,
  reason: {
    pendingInputIndex: { InputNotFound: { source_kind } },
    producerInputOrdinal: ordinal,
  },
});
/** An unsigned key spend at `ordinal` in the spend field. */
const spendSignerMissing = (ordinal: bigint): Expected => ({
  code: RejectCodes.MissingRequiredWitness,
  reason: {
    pendingInputIndex: { SpendInputSignerMissing: {} },
    producerInputOrdinal: ordinal,
  },
});
// The vectors below carry one input per field.
const SPEND_NOT_FOUND = inputNotFound(0n, 0n);
const REFERENCE_NOT_FOUND = inputNotFound(1n, 0n);
const SPEND_SIGNER_MISSING = spendSignerMissing(0n);

type Input = { readonly outRef: Buffer; readonly output?: Buffer };

/** One spend and one reference input; an input without output is absent. */
const vector = (
  route: "full" | "plain",
  spend: Input,
  reference: Input,
): Pick<Row, "route" | "state" | "tx"> => {
  const routeTx: NativeTxOptions =
    route === "full"
      ? {
          outputs: [mintedOutput(NATIVE_POLICY)],
          scriptWitnesses: [native],
          mintPreimageCbor: mintOf(NATIVE_POLICY),
        }
      : { outputs: [funded] };
  return {
    route,
    state: [spend, reference].flatMap(({ outRef, output }) =>
      output === undefined ? [] : [[outRef, output] as const],
    ),
    tx: {
      spendInputs: [spend.outRef],
      referenceInputs: [reference.outRef],
      ...routeTx,
    },
  };
};

const rowsOf = (route: "full" | "plain"): readonly Row[] => {
  const at = (
    name: string,
    kind: Row["kind"],
    guard: boolean,
    expected: Expected | null,
    spend: Input,
    reference: Input,
  ): Row => ({
    name: `a_${route}_${name}`,
    kind,
    guard,
    ...vector(route, spend, reference),
    expected,
  });
  return [
    at(
      "accept",
      "accept",
      true,
      null,
      { outRef: LOW, output: funded },
      { outRef: HIGH, output: funded },
    ),
    // A spend below a reference input resolves first.
    at(
      "unsigned_spend_before_missing_reference",
      "multi",
      false,
      SPEND_SIGNER_MISSING,
      { outRef: LOW, output: unsigned },
      { outRef: HIGH },
    ),
    at(
      "missing_spend_before_missing_reference",
      "multi",
      false,
      SPEND_NOT_FOUND,
      { outRef: LOW },
      { outRef: HIGH },
    ),
    at(
      "control_unsigned_low_spend",
      "control",
      true,
      SPEND_SIGNER_MISSING,
      { outRef: LOW, output: unsigned },
      { outRef: HIGH, output: funded },
    ),
    at(
      "control_missing_low_spend",
      "control",
      true,
      SPEND_NOT_FOUND,
      { outRef: LOW },
      { outRef: HIGH, output: funded },
    ),
    at(
      "control_missing_high_reference",
      "control",
      true,
      REFERENCE_NOT_FOUND,
      { outRef: LOW, output: funded },
      { outRef: HIGH },
    ),
    // A reference input below a spend resolves first.
    at(
      "missing_reference_before_unsigned_spend",
      "multi",
      true,
      REFERENCE_NOT_FOUND,
      { outRef: HIGH, output: unsigned },
      { outRef: LOW },
    ),
    at(
      "control_unsigned_high_spend",
      "control",
      true,
      SPEND_SIGNER_MISSING,
      { outRef: HIGH, output: unsigned },
      { outRef: LOW, output: funded },
    ),
    at(
      "control_missing_low_reference",
      "control",
      true,
      REFERENCE_NOT_FOUND,
      { outRef: HIGH, output: funded },
      { outRef: LOW },
    ),
  ];
};

export const FULL = rowsOf("full");
export const PLAIN = rowsOf("plain");

const PRESENT = outRefFromByte(0x05);
const MID_LOW = outRefFromByte(0x20);
const MID_HIGH = outRefFromByte(0x60);
const LEDGER: readonly (readonly [Buffer, Buffer])[] = [[PRESENT, funded]];

/** A vector over a ledger holding `PRESENT` alone, unless `state` is given. */
const overPresent = (
  name: string,
  kind: Row["kind"],
  expected: Expected | null,
  inputs: {
    readonly spend: readonly Buffer[];
    readonly reference?: readonly Buffer[];
    readonly state?: readonly (readonly [Buffer, Buffer])[];
  },
): Row => ({
  name: `a_keys_${name}`,
  kind,
  guard: true,
  state: inputs.state ?? LEDGER,
  tx: {
    spendInputs: inputs.spend,
    referenceInputs: inputs.reference ?? [],
    outputs: [funded],
  },
  expected,
});

/**
 * Several inputs over a non-empty ledger, in field orders other than the
 * schedule's. The trace proves each key's presence or absence against the
 * one ledger trie, key by key, and the producer names the first faulty node
 * of the ascending schedule by its position in its own field.
 */
export const ABSENT_KEYS: readonly Row[] = [
  overPresent("accept", "accept", null, {
    spend: [PRESENT],
    reference: [HIGH, LOW],
    state: [
      [PRESENT, funded],
      [LOW, funded],
      [HIGH, funded],
    ],
  }),
  overPresent("missing_spends", "multi", inputNotFound(0n, 1n), {
    spend: [PRESENT, LOW, HIGH],
  }),
  overPresent("control_missing_low_spend", "control", inputNotFound(0n, 1n), {
    spend: [PRESENT, LOW],
  }),
  overPresent("control_missing_high_spend", "control", inputNotFound(0n, 1n), {
    spend: [PRESENT, HIGH],
  }),
  // The lowest absent key is the last spend in field order.
  overPresent("unsorted_missing_spends", "multi", inputNotFound(0n, 2n), {
    spend: [HIGH, PRESENT, LOW],
  }),
  overPresent(
    "control_unsorted_missing_high_spend",
    "control",
    inputNotFound(0n, 0n),
    { spend: [HIGH, PRESENT] },
  ),
  // An absent key below an unsigned one, each out of field order.
  overPresent(
    "unsorted_missing_spend_before_unsigned_spend",
    "multi",
    inputNotFound(0n, 2n),
    {
      spend: [HIGH, PRESENT, LOW],
      state: [...LEDGER, [HIGH, unsigned]],
    },
  ),
  overPresent(
    "unsorted_unsigned_spend_before_missing_spend",
    "multi",
    spendSignerMissing(2n),
    {
      spend: [HIGH, PRESENT, LOW],
      state: [...LEDGER, [LOW, unsigned]],
    },
  ),
  overPresent(
    "control_unsorted_unsigned_spend",
    "control",
    spendSignerMissing(0n),
    { spend: [HIGH, PRESENT], state: [...LEDGER, [HIGH, unsigned]] },
  ),
  overPresent("missing_references", "multi", inputNotFound(1n, 1n), {
    spend: [PRESENT],
    reference: [HIGH, LOW],
  }),
  overPresent(
    "control_missing_low_reference",
    "control",
    inputNotFound(1n, 0n),
    { spend: [PRESENT], reference: [LOW] },
  ),
  overPresent(
    "control_missing_high_reference",
    "control",
    inputNotFound(1n, 0n),
    { spend: [PRESENT], reference: [HIGH] },
  ),
  // Spend and reference keys interleave in the schedule.
  overPresent(
    "interleaved_missing_spend_before_missing_reference",
    "multi",
    inputNotFound(0n, 1n),
    {
      spend: [HIGH, MID_LOW],
      reference: [MID_HIGH, PRESENT],
      state: [...LEDGER, [HIGH, funded]],
    },
  ),
  overPresent(
    "interleaved_missing_reference_before_missing_spend",
    "multi",
    inputNotFound(1n, 0n),
    {
      spend: [HIGH, MID_HIGH],
      reference: [MID_LOW, PRESENT],
      state: [...LEDGER, [HIGH, funded]],
    },
  ),
];
