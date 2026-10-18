import { type RejectionReason, rejectionReasonPhase } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  forcedTxOutputFaultRejection,
  locateMidgardForcedTxOutputFault,
  RejectCodes,
} from "../src/index.js";
import {
  BAD_OUTPUT_CONTROL,
  FORCED_OUTPUT_ROWS,
  FORCED_ROWS,
  UNDECODABLE_PLUTUS_REF_ROW,
  WIDE_OUTPUT_ROW,
} from "./emission-order.forced.rows.js";
import {
  expectControlledRows,
  expectProducerAndTraceVerdicts,
  expectProducerVerdict,
  expectProducerVerdictNotProvableByTrace,
  expectRowVerdicts,
  expectRowVerdictsTrace,
  type Row,
  stoppingStep,
  transactionOf,
} from "./emission-order.support.js";

/** The forced rows; see `emission-order.forced.rows.ts`. */
describe("emission order: a forced transaction's first fault", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(
      [...FORCED_ROWS, WIDE_OUTPUT_ROW, BAD_OUTPUT_CONTROL],
      ["f_"],
    );
    expectControlledRows(FORCED_OUTPUT_ROWS, ["fo_"]);
  });

  it.each([...FORCED_ROWS, BAD_OUTPUT_CONTROL])(
    "$name",
    expectRowVerdicts,
    180_000,
  );

  it(
    WIDE_OUTPUT_ROW.name,
    async () => {
      const trace = await expectProducerAndTraceVerdicts(WIDE_OUTPUT_ROW);
      expect(trace.witnesses[stoppingStep(trace)]!.phase).toBe(
        "canonicalDecode",
      );
    },
    180_000,
  );
});

/** An undecodable Plutus item's row: the machine has no successor for it. */
const notProvableByTrace = (row: Row) =>
  typeof row.expected?.reason === "object" &&
  row.expected.reason !== null &&
  "PlutusScriptUndecodable" in row.expected.reason;

const OUTPUT_ROWS = [...FORCED_OUTPUT_ROWS, UNDECODABLE_PLUTUS_REF_ROW];
const rowNamed = (name: string) =>
  OUTPUT_ROWS.find((row) => row.name === name)!;
const phaseOf = (reason: unknown) =>
  rejectionReasonPhase(reason as RejectionReason, {
    body_network_mismatch: false,
  });

/** A forced transaction's faulty output, by class. */
describe("emission order: a forced transaction's faulty output", () => {
  it.each(OUTPUT_ROWS.filter((row) => !notProvableByTrace(row)))(
    "$name",
    async (row) => {
      const trace = await expectRowVerdictsTrace(row);
      // The machine reads outputs at the ScriptSources scan.
      if (row.expected !== null)
        expect(trace.witnesses[stoppingStep(trace)]!.phase).toBe(
          "scriptSources",
        );
    },
    180_000,
  );

  // An undecodable Plutus reference script is refused at the script-source
  // phase (first-fault-v1 phase 8), and the trace builder returns that the
  // refusal is not provable by trace.
  it.each(OUTPUT_ROWS.filter(notProvableByTrace))(
    "$name",
    async (row) => {
      const reason = await expectProducerVerdictNotProvableByTrace(row);
      expect(phaseOf(reason)).toBe(8);
    },
    180_000,
  );

  // With a native and a Plutus reference-script fault in either order, the
  // producer's first fault is first under first-fault-v1: no fault of the
  // row, alone, is in an earlier phase.
  it.each([
    ["fo_plutus_ref_before_malformed_ref"],
    ["fo_malformed_ref_before_plutus_ref"],
  ])(
    "%s: the producer's first fault is first under first-fault-v1",
    async (name) => {
      const reason = await expectProducerVerdict(rowNamed(name));
      const lone = await Promise.all(
        ["fp_plutus_ref", "fo_control_malformed_ref"].map((control) =>
          expectProducerVerdict(rowNamed(control)),
        ),
      );
      expect(phaseOf(reason)).toBe(Math.min(...lone.map(phaseOf)));
    },
  );
});

/**
 * An undecodable Plutus reference script is its own output class: the
 * locator names it, with its decode record's class.
 */
describe("a forced transaction's undecodable Plutus reference script", () => {
  const { txCbor } = transactionOf(UNDECODABLE_PLUTUS_REF_ROW);

  it("is its own class, refused with its record's class", () => {
    const fault = locateMidgardForcedTxOutputFault(txCbor);
    // Its script bytes are not a CBOR byte string: class 1.
    expect(fault).toEqual({
      itemIndex: 0,
      itemClass: "plutusScriptUndecodable",
      refusalClass: 1,
    });
    expect(forcedTxOutputFaultRejection(fault!)).toEqual({
      code: RejectCodes.ScriptProgramEncoding,
      subject: {
        arm: "PlutusScriptUndecodable",
        itemKind: 1n,
        index: 0n,
        refusalClass: 1n,
      },
    });
  });
});
