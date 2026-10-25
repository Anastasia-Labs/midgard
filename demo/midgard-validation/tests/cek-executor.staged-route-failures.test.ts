import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { DataConstr } from "../src/uplc-data.js";
import type { UPLCTerm } from "../src/uplc-term.js";
import {
  built,
  builtText,
  bytes,
  call,
  int,
  raw,
  run,
  staged,
  wide,
} from "./helpers/cek-staged-route-terms.js";
import {
  app,
  discard,
  lam,
  prepareMidgardCekProgram,
  runMidgardCekProgram,
  v,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * A staged step that fails (an argument past protocol version 11's bounds,
 * an index outside the byte string, a byte or slice bound outside its range,
 * bytes that are not UTF-8) is proved by its failure step, as the PV11
 * evaluator fails it, before the builtin is costed.
 */

// Flat builtin tags.
const APPEND = 10n;
const CONS = 11n;
const SLICE = 12n;
const INDEX = 14n;
const EQUALS_BYTES = 15n;
const LESS_THAN_BYTES = 16n;
const SHA2 = 18n;
const BLAKE2B = 20n;
const DECODE_UTF8 = 25n;
const KECCAK = 71n;

/** decodeUtf8 of `length` characters of text with `tail` appended. */
const decodeWithTail = (length: number, ...tail: readonly number[]): UPLCTerm =>
  call(
    builtin("decodeUtf8"),
    call(
      builtin("appendByteString"),
      call(builtin("encodeUtf8"), builtText(length)),
      raw(...tail),
    ),
  );

const STAGED_FAILURE = "executeBuiltinStagedFailure";
const DIRECT_FAILURE = "executeBuiltinFailure";

// Each failing case: its label, the script body, and the tag and witness
// kind of the failure step that ends the proof-mode run.
const failing: readonly (readonly [
  string,
  UPLCTerm,
  bigint,
  typeof STAGED_FAILURE | typeof DIRECT_FAILURE,
])[] = [
  ...(
    [
      ["appendByteString", APPEND, [built(65_537), bytes(1)]],
      ["consByteString", CONS, [int(65n), built(65_537)]],
      ["sliceByteString", SLICE, [int(0n), int(10n), built(65_537)]],
      ["indexByteString", INDEX, [built(65_537), int(0n)]],
      ["equalsByteString", EQUALS_BYTES, [built(65_537), bytes(1)]],
      ["lessThanByteString", LESS_THAN_BYTES, [bytes(1), built(65_537)]],
      ["lessThanEqualsByteString", 17n, [built(65_537), bytes(1)]],
      ["sha2_256", SHA2, [built(65_537)]],
      ["sha3_256", 19n, [built(65_537)]],
      ["blake2b_256", BLAKE2B, [built(65_537)]],
      ["keccak_256", KECCAK, [built(65_537)]],
      ["blake2b_224", 72n, [built(65_537)]],
      ["ripemd_160", 86n, [built(65_537)]],
      ["decodeUtf8", DECODE_UTF8, [built(65_537)]],
    ] as const
  ).map(
    ([name, tag, args]) =>
      [
        `${name} of a 65,537-byte argument`,
        call(builtin(name), ...args),
        tag,
        STAGED_FAILURE,
      ] as const,
  ),
  [
    "consByteString of 256 onto 10,000 bytes",
    call(builtin("consByteString"), int(256n), built(10_000)),
    CONS,
    DIRECT_FAILURE,
  ],
  [
    "consByteString of -1 onto 10,000 bytes",
    call(builtin("consByteString"), int(-1n), built(10_000)),
    CONS,
    DIRECT_FAILURE,
  ],
  [
    "consByteString of 2^64 onto 10,000 bytes",
    call(builtin("consByteString"), int(1n << 64n), built(10_000)),
    CONS,
    STAGED_FAILURE,
  ],
  [
    "consByteString of an 8,930-byte integer onto 1,000 bytes",
    call(builtin("consByteString"), int(wide(8_930)), bytes(1_000)),
    CONS,
    STAGED_FAILURE,
  ],
  [
    "sliceByteString from 2^63 of 20,000 bytes",
    call(builtin("sliceByteString"), int(1n << 63n), int(10n), built(20_000)),
    SLICE,
    DIRECT_FAILURE,
  ],
  [
    "sliceByteString of length -2^63 - 1 of 20,000 bytes",
    call(
      builtin("sliceByteString"),
      int(0n),
      int(-(1n << 63n) - 1n),
      built(20_000),
    ),
    SLICE,
    DIRECT_FAILURE,
  ],
  [
    "sliceByteString from -2^64 of 20,000 bytes",
    call(builtin("sliceByteString"), int(-(1n << 64n)), int(1n), built(20_000)),
    SLICE,
    STAGED_FAILURE,
  ],
  [
    "indexByteString past the end of 20,000 bytes",
    call(builtin("indexByteString"), built(20_000), int(20_000n)),
    INDEX,
    STAGED_FAILURE,
  ],
  [
    "indexByteString at -1 of 20,000 bytes",
    call(builtin("indexByteString"), built(20_000), int(-1n)),
    INDEX,
    STAGED_FAILURE,
  ],
  [
    "indexByteString at 2^63 of 20,000 bytes",
    call(builtin("indexByteString"), built(20_000), int(1n << 63n)),
    INDEX,
    STAGED_FAILURE,
  ],
  [
    "indexByteString at 2^64 of 20,000 bytes",
    call(builtin("indexByteString"), built(20_000), int(1n << 64n)),
    INDEX,
    STAGED_FAILURE,
  ],
  [
    "indexByteString of 1,000 bytes at an 8,930-byte integer",
    call(builtin("indexByteString"), bytes(1_000), int(wide(8_930))),
    INDEX,
    STAGED_FAILURE,
  ],
  [
    "decodeUtf8 of 10,000 bytes ending mid-character",
    decodeWithTail(10_000, 0xc3),
    DECODE_UTF8,
    STAGED_FAILURE,
  ],
  [
    "decodeUtf8 of 10,000 bytes and 0xff",
    decodeWithTail(10_000, 0xff),
    DECODE_UTF8,
    STAGED_FAILURE,
  ],
  [
    "decodeUtf8 of 10,000 bytes and a surrogate",
    decodeWithTail(10_000, 0xed, 0xa0, 0x80, 0x41),
    DECODE_UTF8,
    STAGED_FAILURE,
  ],
  [
    "decodeUtf8 of 10,000 bytes and an overlong slash",
    decodeWithTail(10_000, 0xc0, 0xaf),
    DECODE_UTF8,
    STAGED_FAILURE,
  ],
  [
    "decodeUtf8 of 10,000 bytes after a stray continuation byte",
    call(
      builtin("decodeUtf8"),
      call(
        builtin("appendByteString"),
        raw(0x80),
        call(builtin("encodeUtf8"), builtText(10_000)),
      ),
    ),
    DECODE_UTF8,
    STAGED_FAILURE,
  ],
  [
    "decodeUtf8 of 65,536 bytes whose last is 0xff",
    decodeWithTail(65_535, 0xff),
    DECODE_UTF8,
    STAGED_FAILURE,
  ],
  [
    "decodeUtf8 of 65,537 bytes",
    call(builtin("decodeUtf8"), call(builtin("encodeUtf8"), builtText(65_537))),
    DECODE_UTF8,
    STAGED_FAILURE,
  ],
];

describe("staged builtin failures", () => {
  it.each(failing)(
    "proves %s fails where the PV11 evaluator fails it",
    (_, body, tag, kind) => {
      expect(run(body, "verdict").result.kind).toBe("script_invalid");
      const proof = run(body, "proof");
      expect(proof.result.kind).toBe("script_invalid");
      const last = proof.execution!.steps.at(-1)!;
      expect(last.witness.kind).toBe(kind);
      expect("tag" in last.witness ? last.witness.tag : null).toBe(tag);
      expect(last.post.mode).toBe("haltError");
      // Each failure is raised before the builtin is costed.
      expect([last.post.cpu, last.post.memory]).toEqual([
        last.pre.cpu,
        last.pre.memory,
      ]);
      const witness =
        last.witness.kind === STAGED_FAILURE ? staged(last, 9) : last.witness;
      expect(verifyMidgardCekCoreStep(last.pre, last.post, witness)).toBe(true);
    },
  );

  it.each(failing)(
    "ends the unchecked structural run of %s in an error",
    (_, body) => {
      const prepared = prepareMidgardCekProgram(
        lam(discard(body)),
        new DataConstr(0n, []),
      );
      expect(runMidgardCekProgram(prepared, "verdict").terminalState.mode).toBe(
        "haltError",
      );
    },
  );
});

// The range is reached on whole scripts through multiplyInteger, whose
// staged step is not on this route, so the PV11 evaluator alone decides
// them here; the step-level rows of cek-executor.staged-failure.test.ts
// decide the same values.
const P65535 = int(2n ** 65_535n);
const P65536 = int(2n ** 65_536n);
/** 2^262142, every product's arguments in range. */
const HALF = call(
  builtin("multiplyInteger"),
  call(
    builtin("multiplyInteger"),
    call(builtin("multiplyInteger"), P65535, P65536),
    P65536,
  ),
  P65535,
);
/** `body` with `value` bound to its innermost variable. */
const bind = (value: UPLCTerm, body: UPLCTerm): UPLCTerm =>
  app(lam(body), value);
const MAX_TERM = bind(
  HALF,
  call(
    builtin("addInteger"),
    v(0),
    call(builtin("subtractInteger"), v(0), int(1n)),
  ),
);
const MIN_TERM = call(
  builtin("subtractInteger"),
  call(builtin("subtractInteger"), int(0n), MAX_TERM),
  int(1n),
);

const rangeRows: readonly (readonly [string, UPLCTerm, string])[] = [
  ["2^262143 - 1", MAX_TERM, "accepted"],
  [
    "2^262143",
    call(builtin("addInteger"), MAX_TERM, int(1n)),
    "script_invalid",
  ],
  ["-2^262143", MIN_TERM, "accepted"],
  [
    "-2^262143 - 1",
    call(builtin("subtractInteger"), MIN_TERM, int(1n)),
    "script_invalid",
  ],
];

describe("the PV11 evaluator on the integer range", () => {
  it.each(
    rangeRows.flatMap(([label, term, verdict]) =>
      (
        [
          ["addInteger", builtin("addInteger")],
          ["subtractInteger", builtin("subtractInteger")],
          ["lessThanInteger", builtin("lessThanInteger")],
          ["lessThanEqualsInteger", builtin("lessThanEqualInteger")],
        ] as const
      ).map(
        ([name, builtin]) =>
          [name, label, verdict, call(builtin, term, int(0n))] as const,
      ),
    ),
  )("gives %s of %s the verdict %s", (_, __, verdict, body) => {
    expect(run(body, "verdict").result.kind).toBe(verdict);
  });

  it("gives equalsInteger of 2^262143 with itself a success", () => {
    const top = call(builtin("addInteger"), MAX_TERM, int(1n));
    expect(
      run(bind(top, call(builtin("equalsInteger"), v(0), v(0))), "verdict")
        .result.kind,
    ).toBe("accepted");
  });
});
