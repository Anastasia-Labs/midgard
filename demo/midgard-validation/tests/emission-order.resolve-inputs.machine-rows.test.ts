import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import {
  EMPTY_SIDECAR,
  expectControlledRows,
  type Expected,
  expectRowVerdicts,
  funded,
  keyAddress,
  machineTrace,
  mintedOutput,
  mintOf,
  native,
  NATIVE_POLICY,
  type NativeTxOptions,
  PLUTUS_SIDECAR,
  producer,
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
 * it resolves that spend. The program-material bundle check needs every input
 * resolved, so it follows the whole schedule. Phase B has two routes to that
 * resolution: the full route (local script discovery, here a native mint) and
 * the plain route (no scripts). Each row runs on both, and Phase A's routing
 * is asserted, so a row cannot pass on one route for the other.
 *
 * Each multi-fault row has a single-fault control per fault, and each route
 * has one accepting row. A `guard` row is one the producer already got right
 * before the reconciliation; it pins the order rather than proving a change.
 *
 * The rows assert each reason's arm and, for InputNotFound, its source kind;
 * the input ordinal (`input_index`) is pending X-N2C2 and is not asserted.
 */

const LOW = outRefFromByte(0x70);
const HIGH = outRefFromByte(0x90);
/** A key-locked output whose key does not sign the rows. */
const unsigned = makeOutput(FUNDED_OUTPUT_LOVELACE, keyAddress(0, 0x6c));

const inputNotFound = (source_kind: bigint): Expected => ({
  code: RejectCodes.InputNotFound,
  reason: { pendingInputIndex: { InputNotFound: { source_kind } } },
});
const SPEND_NOT_FOUND = inputNotFound(0n);
const REFERENCE_NOT_FOUND = inputNotFound(1n);
const SPEND_SIGNER_MISSING: Expected = {
  code: RejectCodes.MissingRequiredWitness,
  reason: { pendingInputIndex: { SpendInputSignerMissing: {} } },
};

type Input = { readonly outRef: Buffer; readonly output?: Buffer };

/** One spend and one reference input; an input without output is absent. */
const vector = (
  route: "full" | "plain",
  spend: Input,
  reference: Input,
  sidecar = EMPTY_SIDECAR,
): Pick<Row, "route" | "state" | "tx" | "sidecar"> => {
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
    sidecar,
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
    sidecar?: Buffer,
  ): Row => ({
    name: `a_${route}_${name}`,
    kind,
    guard,
    ...vector(route, spend, reference, sidecar),
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
    // The bundle check follows the whole schedule. Its control is below.
    at(
      "missing_spend_and_program_bundle",
      "multi",
      false,
      SPEND_NOT_FOUND,
      { outRef: LOW },
      { outRef: HIGH, output: funded },
      PLUTUS_SIDECAR,
    ),
  ];
};

const FULL = rowsOf("full");
const PLAIN = rowsOf("plain");

/**
 * The bundle check's lone fault. The machine has no arm for it: the producer
 * refuses the material before execution, and the trace builder refuses to
 * build a trace for that verdict.
 */
const bundleControl = (route: "full" | "plain"): Row => ({
  name: `a_${route}_control_program_bundle`,
  kind: "control",
  guard: true,
  ...vector(
    route,
    { outRef: LOW, output: funded },
    { outRef: HIGH, output: funded },
    PLUTUS_SIDECAR,
  ),
  // No arm names this fault; the trace builder reads only the code.
  expected: { code: RejectCodes.CekProgramMaterial, reason: "EmptyInputs" },
});

describe("emission order (a): merged input resolution on both Phase B routes", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(FULL, ["a_full_"]);
    expectControlledRows(PLAIN, ["a_plain_"]);
    // The plain route is the full route's twin, row for row.
    expect(PLAIN.map(({ name }) => name.replace("a_plain_", ""))).toEqual(
      FULL.map(({ name }) => name.replace("a_full_", "")),
    );
  });

  it.each([...FULL, ...PLAIN])("$name", expectRowVerdicts, 180_000);

  it.each([bundleControl("full"), bundleControl("plain")])(
    "$name",
    async (row) => {
      const { rejection, route } = await producer(row);
      expect(route).toBe(row.route);
      expect(rejection?.code).toBe(RejectCodes.CekProgramMaterial);
      expect(rejection?.consensusPhase).toBe("scriptSources");
      expect(rejection?.subject).toBeUndefined();
      await expect(machineTrace(row)).rejects.toThrow(
        "V1 validation reports a ScriptSources rejection but all exact discovery and audit instructions accepted",
      );
    },
    180_000,
  );
});
