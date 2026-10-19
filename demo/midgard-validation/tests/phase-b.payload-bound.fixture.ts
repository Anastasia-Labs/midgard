import { DataB } from "@harmoniclabs/plutus-data";
import {
  Builtin,
  constT,
  ErrorUPLC,
  UPLCConst,
  type UPLCTerm,
} from "@harmoniclabs/uplc";

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
 * Plutus spends whose script takes one builtin step past the proof
 * evaluator's 9,215-byte bounds: a direct builtin whose arguments and result
 * together pass the direct payload bound (equalsByteString, appendByteString,
 * decodeUtf8), and serialiseData of Data whose CBOR passes the
 * revealed-preimage envelope. Cardano accepts each one. Every row has the
 * step past the bound and one byte under it, each in an accepting script and
 * in one that fails on its own after the step.
 */

const bytes = (length: number, fill: number): UPLCTerm =>
  UPLCConst.byteString(new Uint8Array(length).fill(fill) as never);

const dataBytes = (length: number, fill: number) =>
  new DataB(new Uint8Array(length).fill(fill));

/** Unit when `condition` holds, a script failure otherwise. */
const unitWhen = (condition: UPLCTerm): UPLCTerm =>
  ifThenElse(condition, UPLCConst.unit, new ErrorUPLC());

/** Evaluates `term`, then fails. */
const thenFail = (term: UPLCTerm): UPLCTerm => app(lam(new ErrorUPLC()), term);

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

/** decodeUtf8 of `length` ASCII bytes. */
const decoded = (length: number) =>
  app(Builtin.decodeUtf8, bytes(length, 0x61));

export type PayloadBoundRow = Readonly<{
  label: string;
  /** Past the bound, accepting. */
  over: ProgramSpend;
  /** Past the bound, then failing on its own. */
  overFailing: ProgramSpend;
  /** One byte under the bound, accepting. */
  under: ProgramSpend;
  /** One byte under the bound, then failing on its own. */
  failing: ProgramSpend;
}>;

/** The rows, their locked outputs seeded from `firstSeed` upward. */
export const payloadBoundRows = (
  firstSeed: number,
): readonly PayloadBoundRow[] => {
  let seed = firstSeed;
  const spendOf = (body: UPLCTerm): ProgramSpend =>
    plutusProgramSpend(seed++, plutusProgram(body));
  return [
    {
      label: "equalsByteString over two 4,465-byte strings",
      over: spendOf(equalStrings(4_465)),
      overFailing: spendOf(equalStrings(4_465, 0x62)),
      under: spendOf(equalStrings(4_464)),
      failing: spendOf(equalStrings(4_464, 0x62)),
    },
    {
      label: "appendByteString of two 2,233-byte strings",
      over: spendOf(appendStrings(2_233)),
      overFailing: spendOf(appendStrings(2_233, 0)),
      under: spendOf(appendStrings(2_232)),
      failing: spendOf(appendStrings(2_232, 0)),
    },
    {
      label: "serialiseData of two 4,465-byte strings in a list",
      over: spendOf(discard(serialised(4_465))),
      overFailing: spendOf(thenFail(serialised(4_465))),
      under: spendOf(discard(serialised(4_464))),
      failing: spendOf(thenFail(serialised(4_464))),
    },
    {
      label: "decodeUtf8 of a 4,466-byte string",
      over: spendOf(discard(decoded(4_466))),
      overFailing: spendOf(thenFail(decoded(4_466))),
      under: spendOf(discard(decoded(4_465))),
      failing: spendOf(thenFail(decoded(4_465))),
    },
  ];
};
