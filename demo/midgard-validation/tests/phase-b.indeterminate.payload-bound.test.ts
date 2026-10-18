import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { DataB } from "@harmoniclabs/plutus-data";
import {
  Builtin,
  constT,
  ErrorUPLC,
  UPLCConst,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  type PhaseBConfig,
  RejectCodes,
  runPhaseBValidationWithPatch,
  ScriptEvaluationIndeterminateError,
} from "../src/index.js";
import {
  app,
  data,
  discard,
  ifThenElse,
  int,
  lam,
  mkCons,
} from "./helpers/cek-uplc-terms.js";
import {
  plutusProgram,
  plutusProgramSpend,
  type ProgramSpend,
} from "./phase-b.indeterminate.fixture.js";

/**
 * Builtin steps past the evaluator's 9,215-byte bounds: a direct builtin
 * whose arguments and result together pass the direct payload bound, and
 * serialiseData of Data whose CBOR passes the revealed-preimage envelope.
 * Cardano accepts each script past a bound. Phase B has no verdict for its
 * transaction (`ScriptEvaluationIndeterminateError`) in both modes. One byte
 * under each bound, Phase B returns the script's own verdict, accepted or
 * rejected.
 */

const config: PhaseBConfig = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
  enforceScriptBudget: true,
};

const bytes = (length: number, fill: number): UPLCTerm =>
  UPLCConst.byteString(new Uint8Array(length).fill(fill) as never);

const dataBytes = (length: number, fill: number) =>
  new DataB(new Uint8Array(length).fill(fill));

/** Unit when `condition` holds, a script failure otherwise. */
const unitWhen = (condition: UPLCTerm): UPLCTerm =>
  ifThenElse(condition, UPLCConst.unit, new ErrorUPLC());

/** equalsByteString over two strings of `length` bytes. */
const equalStrings = (length: number, rightFill = 0x61) =>
  unitWhen(
    app(
      Builtin.equalsByteString,
      bytes(length, 0x61),
      bytes(length, rightFill),
    ),
  );

/** appendByteString of two strings of `length` bytes, checked by length. */
const appendStrings = (length: number, expected = 2 * length) =>
  unitWhen(
    app(
      Builtin.equalsInteger,
      app(
        Builtin.lengthOfByteString,
        app(Builtin.appendByteString, bytes(length, 0x61), bytes(length, 0x62)),
      ),
      int(expected),
    ),
  );

/** serialiseData of a two-item list of byte strings of `length` bytes. */
const serialised = (length: number) =>
  app(
    Builtin.serialiseData,
    app(
      Builtin.listData,
      mkCons(
        data(dataBytes(length, 0x61)),
        UPLCConst.listOf(constT.data)([dataBytes(length, 0x62)] as never),
      ),
    ),
  );

const serialiseList = (length: number) => discard(serialised(length));

/** The same serialisation, followed by a script failure. */
const serialiseListThenFail = (length: number) =>
  app(lam(new ErrorUPLC()), serialised(length));

let seed = 0x40;
const spendOf = (body: UPLCTerm): ProgramSpend =>
  plutusProgramSpend((seed += 1), plutusProgram(body));

const runPhaseB = (
  spend: ProgramSpend,
  mode: NonNullable<PhaseBConfig["cekExecutionMode"]>,
) =>
  Effect.runPromise(
    Effect.either(
      runPhaseBValidationWithPatch([spend.candidate], spend.preState, {
        ...config,
        cekExecutionMode: mode,
      }),
    ),
  );

const MODES = ["verdict", "proof"] as const;

const ROWS = [
  {
    label: "equalsByteString over two 4,465-byte strings",
    over: spendOf(equalStrings(4_465)),
    under: spendOf(equalStrings(4_464)),
    failing: spendOf(equalStrings(4_464, 0x62)),
  },
  {
    label: "appendByteString of two 2,233-byte strings",
    over: spendOf(appendStrings(2_233)),
    under: spendOf(appendStrings(2_232)),
    failing: spendOf(appendStrings(2_232, 0)),
  },
  {
    label: "serialiseData of two 4,465-byte strings in a list",
    over: spendOf(serialiseList(4_465)),
    under: spendOf(serialiseList(4_464)),
    failing: spendOf(serialiseListThenFail(4_464)),
  },
] as const;

describe("Phase B: a builtin step past the evaluator's payload bounds", () => {
  describe.each(MODES)("%s mode", (mode) => {
    it.each(ROWS)("$label has no verdict", async ({ over }) => {
      const result = await runPhaseB(over, mode);
      expect(Either.isLeft(result)).toBe(true);
      if (Either.isRight(result)) return;
      expect(result.left).toBeInstanceOf(ScriptEvaluationIndeterminateError);
      expect(
        (result.left as ScriptEvaluationIndeterminateError).txId.equals(
          over.candidate.ledgerTx.txId,
        ),
      ).toBe(true);
    });

    it.each(ROWS)(
      "$label, one byte smaller, is accepted",
      async ({ under }) => {
        const result = await runPhaseB(under, mode);
        expect(Either.isRight(result)).toBe(true);
        if (Either.isLeft(result)) return;
        expect(result.right.rejected).toStrictEqual([]);
        expect(result.right.accepted).toHaveLength(1);
      },
    );

    it.each(ROWS)(
      "$label, one byte smaller and failing on its own, is rejected",
      async ({ failing }) => {
        const result = await runPhaseB(failing, mode);
        expect(Either.isRight(result)).toBe(true);
        if (Either.isLeft(result)) return;
        expect(result.right.accepted).toStrictEqual([]);
        expect(
          result.right.rejected.map((rejected) => rejected.code),
        ).toStrictEqual([RejectCodes.PlutusScriptInvalid]);
      },
    );
  });
});
