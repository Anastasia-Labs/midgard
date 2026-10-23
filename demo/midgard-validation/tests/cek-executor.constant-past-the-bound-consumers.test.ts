import { createHash } from "node:crypto";

import { encodeCbor } from "@al-ft/midgard-core";
import {
  DataB,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import {
  Builtin,
  ErrorUPLC,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { stageMidgardCekStagedBuiltinMaterial } from "../src/cek-staged-builtin.material.js";
import { evaluateMidgardCekScript } from "../src/phase-b.evaluate-cek-script.js";
import { forced } from "./cek-executor.semantic-presentation.shapes.js";
import {
  app,
  data,
  discard,
  ifThenElse,
  lam,
  MIDGARD_CEK_TX_BUDGET,
} from "./helpers/cek-uplc-terms.js";

/**
 * A script holding a constant past the direct payload bound and consuming it:
 * the proof-mode run (every step checked by the consensus verifier) reaches
 * the PV11 evaluator's verdict and budget, for Data consumers and for the
 * byte-string, string and integer builtins over the constant.
 */

const LONG = 12_000;

/** `length` bytes, the pattern moved by `salt`. */
const pattern = (length: number, salt = 0): Buffer =>
  Buffer.from(
    Uint8Array.from({ length }, (_, index) => (index * 7 + salt) & 0xff),
  );

/** 1,200 integers of nine CBOR bytes each: a list past the bound whose every
 * leaf is within it. */
const WIDE_LIST = Array.from(
  { length: 1_200 },
  (_, index) => new DataI(1_000_000_000_000n + BigInt(index)),
);

/** An integer of `LONG` magnitude bytes. */
const LONG_INTEGER = 2n ** BigInt(8 * LONG - 1) + 5n;

/** `LONG / 3` three-byte characters. */
const LONG_TEXT = "€".repeat(LONG / 3);

const run = (body: UPLCTerm, mode: "proof" | "verdict") =>
  evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(
        Buffer.from(
          encodeMidgardCekCardanoFlatProgram(
            new UPLCProgram([1, 1, 0], lam(discard(body))),
          ),
        ),
      ),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from("d87980", "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: MIDGARD_CEK_TX_BUDGET,
    },
    mode,
  );

const call = (builtin: UPLCTerm, ...args: readonly UPLCTerm[]): UPLCTerm =>
  app(builtin, ...args);
/** Unit when `condition` holds, else a failure. */
const check = (condition: UPLCTerm): UPLCTerm =>
  ifThenElse(condition, UPLCConst.unit, new ErrorUPLC());

/** The step's witness with its operands staged on made-up outputs. */
const staged = (step: MidgardCekExecutionStep) => {
  const witness = step.witness;
  if (witness.kind !== "executeBuiltinStaged") throw new Error("not staged");
  let index = 0;
  const staging = witness.stagedOperands.map((operand) =>
    Array.from({ length: Math.ceil(operand.length / 12_285) }, () => ({
      transactionId: Buffer.alloc(32, 9),
      outputIndex: index++,
    })),
  );
  return {
    ...witness,
    material: stageMidgardCekStagedBuiltinMaterial(
      witness.material,
      witness.stagedOperands,
      staging,
    ),
  };
};

const everyStepVerifies = (proof: ReturnType<typeof run>): boolean =>
  (proof.execution?.steps ?? []).every((step) =>
    verifyMidgardCekCoreStep(
      step.pre,
      step.post,
      step.witness.kind === "executeBuiltinStaged"
        ? staged(step)
        : step.witness,
    ),
  );

const RUNS: readonly (readonly [
  string,
  UPLCTerm,
  "accepted" | "script_invalid",
])[] = [
  [
    "equalsData of a 12,000-byte Data bytes leaf and itself",
    check(
      call(
        Builtin.equalsData,
        data(new DataB(pattern(LONG))),
        data(new DataB(pattern(LONG))),
      ),
    ),
    "accepted",
  ],
  [
    "equalsData of a 12,000-byte Data bytes leaf and another",
    check(
      call(
        Builtin.equalsData,
        data(new DataB(pattern(LONG))),
        data(new DataB(pattern(LONG, 1))),
      ),
    ),
    "script_invalid",
  ],
  [
    "equalsData of a Data list of 1,200 integers and itself",
    check(
      call(
        Builtin.equalsData,
        data(new DataList(WIDE_LIST)),
        data(new DataList(WIDE_LIST)),
      ),
    ),
    "accepted",
  ],
  [
    "headList of unListData of a Data list of 1,200 integers",
    check(
      call(
        Builtin.equalsData,
        call(
          forced(Builtin.headList, 1),
          call(Builtin.unListData, data(new DataList(WIDE_LIST))),
        ),
        data(WIDE_LIST[0]!),
      ),
    ),
    "accepted",
  ],
  [
    "the key of unMapData of a Data map with a 10,000-byte key",
    check(
      call(
        Builtin.equalsData,
        call(
          forced(Builtin.fstPair, 2),
          call(
            forced(Builtin.headList, 1),
            call(
              Builtin.unMapData,
              data(
                new DataMap([
                  new DataPair(new DataB(pattern(10_000)), new DataI(1n)),
                ]),
              ),
            ),
          ),
        ),
        data(new DataB(pattern(10_000))),
      ),
    ),
    "accepted",
  ],
  [
    "lengthOfByteString of a 12,000-byte string",
    check(
      call(
        Builtin.equalsInteger,
        call(Builtin.lengthOfByteString, UPLCConst.byteString(pattern(LONG))),
        UPLCConst.int(BigInt(LONG)),
      ),
    ),
    "accepted",
  ],
  [
    "sha2_256 of a 12,000-byte string",
    check(
      call(
        Builtin.equalsByteString,
        call(Builtin.sha2_256, UPLCConst.byteString(pattern(LONG))),
        UPLCConst.byteString(
          createHash("sha256").update(pattern(LONG)).digest(),
        ),
      ),
    ),
    "accepted",
  ],
  [
    "encodeUtf8 of a 12,000-byte multibyte string",
    check(
      call(
        Builtin.equalsInteger,
        call(
          Builtin.lengthOfByteString,
          call(Builtin.encodeUtf8, UPLCConst.str(LONG_TEXT)),
        ),
        UPLCConst.int(BigInt(LONG)),
      ),
    ),
    "accepted",
  ],
  [
    "addInteger onto an integer of 12,000 magnitude bytes",
    check(
      call(
        Builtin.equalsInteger,
        call(
          Builtin.addInteger,
          UPLCConst.int(LONG_INTEGER),
          UPLCConst.int(1n),
        ),
        UPLCConst.int(LONG_INTEGER + 1n),
      ),
    ),
    "accepted",
  ],
];

describe("a constant past the direct payload bound consumed in a run", () => {
  it.each(RUNS)(
    "proves %s with the PV11 evaluator's verdict and budget",
    (_, body, kind) => {
      const verdict = run(body, "verdict");
      const proof = run(body, "proof");
      expect(verdict.result.kind).toBe(kind);
      // A failure's detail names the evaluator; its kind is the verdict.
      if (kind === "accepted") expect(proof.result).toEqual(verdict.result);
      else expect(proof.result.kind).toBe(kind);
      expect(proof.execution?.steps.length ?? 0).toBeGreaterThan(0);
      expect(everyStepVerifies(proof)).toBe(true);
    },
  );
});
