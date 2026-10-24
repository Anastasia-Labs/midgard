import { readdirSync, readFileSync } from "node:fs";

import {
  type ConstType,
  ConstTyTag,
  UPLCEncoder,
  type UPLCProgram as ReferenceUPLCProgram,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import {
  encodeCardanoFlatProgram,
  type FlatProgram,
} from "../src/cek-program.cardano-flat.encode.js";
import {
  decodeCardanoFlatProgram,
  decodeMidgardCekCardanoFlatProgram,
} from "../src/cek-program.cardano-flat.js";
import { storedFlatProgramBytes } from "../src/cek-program.unwrap-canonical-cbor-byte-string.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { isData } from "../src/uplc-data.js";
import {
  Application,
  ErrorUPLC,
  Lambda,
  UPLCProgram,
  type UPLCTerm,
} from "../src/uplc-term.js";
import * as boundaries from "./cek-program.cardano-boundaries.vectors.js";
import * as flatVectors from "./cek-program.cardano-flat.vectors.js";
import * as dataSpelling from "./cek-program.data-constant-spelling.vectors.js";
import { deepFlatProgram } from "./cek-program.deep-flat.fixture.js";
import * as emptyBytes from "./cek-program.empty-bytes.vectors.js";
import { CASE_CONSTANT_CONFORMANCE } from "./fixtures/cek-case-constant-v1.corpus.js";
import { CEK_CORE_STEP_PROGRAMS } from "./fixtures/cek-core-step-v1.vectors.mjs";
import {
  cekStepFuzzProgram,
  FUZZ_PRIMARY_TAGS,
} from "./fuzz/cek-step-fuzz.programs.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * The owned Flat encoder against HarmonicLabs' `UPLCEncoder` with the Data
 * override production used: byte-identical over every program the fixtures,
 * corpora and vector tables hold.
 */
class HarmonicCardanoFlatEncoder extends UPLCEncoder {
  override encodeConstValue(type: ConstType, value: unknown): void {
    if (type[0] === ConstTyTag.data) {
      if (!isData(value))
        throw new Error("Flat Data constant contains invalid Data");
      this.encodeByteString(encodeMidgardCekPlutusData(value));
      return;
    }
    super.encodeConstValue(type, value);
  }
}

const harmonic = (program: FlatProgram): string =>
  Buffer.from(
    new HarmonicCardanoFlatEncoder().compile(
      program as unknown as ReferenceUPLCProgram,
    ),
  ).toString("hex");
const owned = (program: FlatProgram): string =>
  Buffer.from(encodeCardanoFlatProgram(program)).toString("hex");

const v110 = (term: UPLCTerm): UPLCProgram => new UPLCProgram([1, 1, 0], term);

/** Every even-length hex string anywhere in `value`, as bytes. */
const hexStrings = (value: unknown, found: Set<string>): Set<string> => {
  const stack = [value];
  while (stack.length > 0) {
    const next = stack.pop();
    if (typeof next === "string") {
      if (
        next.length >= 8 &&
        next.length % 2 === 0 &&
        /^[0-9a-f]+$/u.test(next)
      )
        found.add(next);
    } else if (Array.isArray(next)) stack.push(...(next as unknown[]));
    else if (next !== null && typeof next === "object")
      stack.push(...(Object.values(next) as unknown[]));
  }
  return found;
};

/** The Flat programs among the strings: raw, or wrapped in a CBOR byte string. */
const decodablePrograms = (strings: Iterable<string>): FlatProgram[] => {
  const programs: FlatProgram[] = [];
  for (const string of strings) {
    const bytes = Buffer.from(string, "hex");
    const candidates = [bytes];
    try {
      candidates.push(storedFlatProgramBytes(bytes));
    } catch {
      // Not one wrapped byte string.
    }
    for (const candidate of candidates) {
      const decoded = decodeCardanoFlatProgram(candidate);
      if (decoded.ok) programs.push(decoded.program);
    }
  }
  return programs;
};

const fixtureDirectories = [
  new URL("./fixtures/", import.meta.url),
  new URL("./fuzz/", import.meta.url),
];
const jsonFixtures = fixtureDirectories.flatMap((directory) =>
  readdirSync(directory)
    .filter((name) => name.endsWith(".json"))
    .map((name) => new URL(name, directory)),
);

const expectIdentical = (programs: readonly FlatProgram[]): void => {
  const differing = programs.filter(
    (program) => owned(program) !== harmonic(program),
  );
  expect(differing.map((program) => harmonic(program))).toEqual([]);
};

describe("the owned Flat encoder writes HarmonicLabs' bytes", () => {
  it("for every program in the JSON fixtures and the fuzz corpus", () => {
    const strings = new Set<string>();
    for (const url of jsonFixtures)
      hexStrings(JSON.parse(readFileSync(url, "utf8")) as unknown, strings);
    const programs = decodablePrograms(strings);
    // 32 core-step, 34 script-decode-step and 19 fuzz-corpus programs.
    expect(programs.length).toBeGreaterThanOrEqual(85);
    expectIdentical(programs);
  });

  it("for every program in the Flat vector tables", () => {
    const strings = new Set<string>();
    for (const table of [boundaries, flatVectors, dataSpelling, emptyBytes])
      hexStrings(Object.values(table), strings);
    const programs = decodablePrograms(strings);
    expect(programs.length).toBeGreaterThan(100);
    expectIdentical(programs);
  });

  it("for the core-step, case-constant and fuzz program builders", () => {
    const programs = [
      ...CEK_CORE_STEP_PROGRAMS.map(({ term }) => v110(term())),
      ...CASE_CONSTANT_CONFORMANCE.map(({ term }) => v110(term())),
      // The program of `scripts/generate-cek-program-large-integer-fixture.mjs`.
      v110(
        new Lambda(
          new Application(
            new Lambda(UPLCConst.unit),
            new Application(
              new Application(builtin("addInteger"), UPLCConst.int(1n << 512n)),
              UPLCConst.int(1),
            ),
          ),
        ),
      ),
      ...[0, 1, 2, 3].flatMap((seed) =>
        FUZZ_PRIMARY_TAGS.map((tag) =>
          v110(cekStepFuzzProgram(seed, tag).term),
        ),
      ),
    ];
    expect(programs.length).toBeGreaterThan(300);
    expectIdentical(programs);
  });

  it("for nested case, constr and application terms", () => {
    expectIdentical(
      (["case", "constr", "application"] as const).flatMap((shape) =>
        [1, 2, 64, 1_000].map((depth) =>
          decodeMidgardCekCardanoFlatProgram(deepFlatProgram(shape, depth)),
        ),
      ),
    );
  });
});

describe("the owned Flat encoder is the inverse of the Cardano reader", () => {
  it("writes the hand-built bytes of terms nested past any JavaScript stack", () => {
    for (const shape of ["case", "constr", "application"] as const) {
      const flat = deepFlatProgram(shape, 200_000);
      expect(
        Buffer.from(
          encodeCardanoFlatProgram(decodeMidgardCekCardanoFlatProgram(flat)),
        ).equals(flat),
      ).toBe(true);
    }
  });

  it("writes version numbers above 127 as Flat naturals", () => {
    const flat = encodeCardanoFlatProgram(
      new UPLCProgram([200, 128, 255], new ErrorUPLC()),
    );
    const decoded = decodeCardanoFlatProgram(flat);
    expect(decoded.ok && decoded.program.version.toString()).toBe(
      "200.128.255",
    );
  });
});
