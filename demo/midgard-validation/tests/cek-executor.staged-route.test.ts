import { describe, expect, it } from "vitest";

import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import type { UPLCTerm } from "../src/uplc-term.js";
import {
  built,
  builtText,
  bytes,
  call,
  int,
  run,
  staged,
  text,
  wide,
} from "./helpers/cek-staged-route-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * A byte-string, string or linear integer builtin whose arguments or result
 * do not fit the direct carriage bound is proved by a staged step (arm 31)
 * instead of leaving the run indeterminate. The proof-mode run (the
 * structural executor, every step checked by the consensus verifier) reaches
 * the verdict and budget of the PV11 evaluator, and each staged step still
 * verifies once its operands are staged. The staged steps that fail are in
 * cek-executor.staged-route-failures.test.ts.
 */

// Flat builtin tags.
const ADD = 0n;
const SUBTRACT = 1n;
const EQUALS_INTEGER = 7n;
const LESS_THAN_INTEGER = 8n;
const LESS_THAN_EQUALS_INTEGER = 9n;
const APPEND = 10n;
const CONS = 11n;
const SLICE = 12n;
const LENGTH = 13n;
const INDEX = 14n;
const EQUALS_BYTES = 15n;
const LESS_THAN_BYTES = 16n;
const SHA2 = 18n;
const BLAKE2B = 20n;
const APPEND_STRING = 22n;
const EQUALS_STRING = 23n;
const ENCODE_UTF8 = 24n;
const DECODE_UTF8 = 25n;
const KECCAK = 71n;

// Each honest case: its label, the script body, and the tag of the last
// staged step the proof-mode run records.
const honest: readonly (readonly [string, UPLCTerm, bigint])[] = [
  [
    "appendByteString whose result passes the direct bound",
    call(builtin("appendByteString"), bytes(5_000), bytes(5_000, 1)),
    APPEND,
  ],
  ["sha2_256 of 20,000 bytes", call(builtin("sha2_256"), built(20_000)), SHA2],
  [
    "sha2_256 at the 65,536-byte argument bound",
    call(builtin("sha2_256"), built(65_536)),
    SHA2,
  ],
  [
    "appendByteString of two 65,536-byte operands",
    call(
      builtin("lengthOfByteString"),
      call(builtin("appendByteString"), built(65_536), built(65_536, 5)),
    ),
    LENGTH,
  ],
  [
    "blake2b_256 of 9,216 bytes",
    call(builtin("blake2b_256"), built(9_216)),
    BLAKE2B,
  ],
  [
    "keccak_256 of 9,216 bytes",
    call(builtin("keccak_256"), built(9_216)),
    KECCAK,
  ],
  [
    "lengthOfByteString of 70,000 bytes",
    call(builtin("lengthOfByteString"), built(70_000)),
    LENGTH,
  ],
  [
    "equalsByteString of equal 10,000-byte values",
    call(builtin("equalsByteString"), built(10_000), built(10_000)),
    EQUALS_BYTES,
  ],
  [
    "lessThanByteString of 10,000-byte values",
    call(builtin("lessThanByteString"), built(10_000), built(10_000, 1)),
    LESS_THAN_BYTES,
  ],
  [
    "consByteString onto 10,000 bytes",
    call(builtin("consByteString"), int(65n), built(10_000)),
    CONS,
  ],
  [
    "sliceByteString of 20,000 bytes",
    call(builtin("sliceByteString"), int(100n), int(12_000n), built(20_000)),
    SLICE,
  ],
  [
    "indexByteString of 20,000 bytes",
    call(builtin("indexByteString"), built(20_000), int(19_999n)),
    INDEX,
  ],
  [
    "appendString of 5,000-character strings",
    call(builtin("appendString"), text(5_000), text(5_000, "y")),
    APPEND_STRING,
  ],
  [
    "equalsString of unequal 10,000-character strings",
    call(builtin("equalsString"), builtText(10_000), builtText(10_000, "y")),
    EQUALS_STRING,
  ],
  [
    "encodeUtf8 of 10,000 characters",
    call(builtin("encodeUtf8"), builtText(10_000)),
    ENCODE_UTF8,
  ],
  [
    "decodeUtf8 of 10,000 bytes",
    call(builtin("decodeUtf8"), call(builtin("encodeUtf8"), builtText(10_000))),
    DECODE_UTF8,
  ],
  [
    "addInteger of 5,000-byte integers",
    call(builtin("addInteger"), int(wide(5_000)), int(-wide(5_000, 7n))),
    ADD,
  ],
  [
    "subtractInteger of 5,000-byte integers",
    call(builtin("subtractInteger"), int(wide(5_000)), int(-wide(5_000, 7n))),
    SUBTRACT,
  ],
  [
    "equalsInteger of equal 5,000-byte integers",
    call(builtin("equalsInteger"), int(wide(5_000)), int(wide(5_000))),
    EQUALS_INTEGER,
  ],
  [
    "lessThanInteger of 5,000-byte integers",
    call(builtin("lessThanInteger"), int(wide(5_000)), int(wide(5_000, 1n))),
    LESS_THAN_INTEGER,
  ],
  [
    "lessThanEqualsInteger of 5,000-byte integers",
    call(builtin("lessThanEqualsInteger"), int(-wide(5_000)), int(wide(5_000))),
    LESS_THAN_EQUALS_INTEGER,
  ],
];

const stagedSteps = (
  outcome: ReturnType<typeof run>,
): MidgardCekExecutionStep[] =>
  (outcome.execution?.steps ?? []).filter(
    (step) => step.witness.kind === "executeBuiltinStaged",
  );

describe("CEK scalar builtins past the direct carriage bound", () => {
  it.each(honest)(
    "proves %s with the PV11 evaluator's verdict and budget",
    (_, body, tag) => {
      const verdict = run(body, "verdict");
      const proof = run(body, "proof");
      expect(verdict.result.kind).toBe("accepted");
      expect(proof.result).toEqual(verdict.result);
      const steps = stagedSteps(proof);
      const last = steps.at(-1)?.witness;
      expect(last?.kind === "executeBuiltinStaged" ? last.tag : null).toBe(tag);
      for (const step of steps) {
        expect(
          verifyMidgardCekCoreStep(step.pre, step.post, staged(step, 9)),
        ).toBe(true);
      }
    },
  );

  it("binds each staged step to its staged operands' bytes", () => {
    const [step] = stagedSteps(
      run(call(builtin("sha2_256"), built(20_000)), "proof"),
    );
    const witness = staged(step!, 9);
    const operand = Buffer.from(witness.stagedOperands[0]!);
    operand[operand.length - 1]! ^= 1;
    expect(
      verifyMidgardCekCoreStep(step!.pre, step!.post, {
        ...witness,
        stagedOperands: [operand],
      }),
    ).toBe(false);
  });

  it("keeps a builtin whose arguments and result fit on the direct route", () => {
    const proof = run(
      call(builtin("appendByteString"), bytes(2_000), bytes(2_000)),
      "proof",
    );
    expect(proof.result.kind).toBe("accepted");
    expect(stagedSteps(proof)).toEqual([]);
    expect(
      proof.execution?.steps.some(
        (step) => step.witness.kind === "executeBuiltinDirect",
      ),
    ).toBe(true);
  });
});
