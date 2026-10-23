import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import { DataB } from "@harmoniclabs/plutus-data";
import { CEKConst } from "@harmoniclabs/plutus-machine";
import {
  Builtin,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import * as UPLC from "@lucid-evolution/uplc";
import { ed25519 } from "@noble/curves/ed25519.js";
import { schnorr } from "@noble/curves/secp256k1.js";
import { describe, expect, it } from "vitest";

import type { MidgardCekDirectValueWitness } from "../src/cek-builtin.argument-kinds.js";
import {
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-reference-builtin.js";
import { midgardCekDirectBuiltinBudget } from "../src/cek-builtin.selected-control-result.js";
import type { MidgardCekBuiltinBudget } from "../src/cek-cost.js";
import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import type { MidgardCekStagedOperand } from "../src/cek-executor.staged-failure.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { errorSuccessor } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import type { MidgardUplcEvalProgram } from "../src/plutus-script-evaluator.js";
import {
  builtinState,
  bytesOperand,
  presented,
} from "./helpers/cek-staged-failure-steps.js";
import { built, call, run, staged } from "./helpers/cek-staged-route-terms.js";
import {
  discard,
  lam,
  MIDGARD_CEK_TX_BUDGET,
} from "./helpers/cek-uplc-terms.js";

/**
 * verifyEd25519Signature and verifySchnorrSecp256k1Signature over a message
 * past the direct carriage bound. Protocol version 11 bounds the message
 * alone, at 65,536 bytes, and fails the builtin on a message past it before
 * costing it; a key or signature of the wrong length, or a BIP-340 key off
 * the curve, beside a message in bound fails the builtin after costing it.
 * The proof-mode run (the structural executor, every step checked by the
 * consensus verifier) reaches the PV11 evaluator's verdict and budget, and
 * each step has exactly one provable result.
 */

const ED25519 = 21n;
const SCHNORR = 53n;

const BUILTINS = [
  ["verifyEd25519Signature", ED25519, Builtin.verifyEd25519Signature],
  [
    "verifySchnorrSecp256k1Signature",
    SCHNORR,
    Builtin.verifySchnorrSecp256k1Signature,
  ],
] as const;

const PIECE = 8_000;

/** The bytes `built(length, salt)` builds at run time. */
const builtBytes = (length: number, salt = 0): Buffer =>
  Buffer.concat(
    Array.from({ length: Math.ceil(length / PIECE) }, (_, piece) =>
      Uint8Array.from(
        { length: Math.min(PIECE, length - piece * PIECE) },
        (_, index) => (index * 7 + salt + piece) & 0xff,
      ),
    ),
  );

const ED25519_SECRET = Buffer.alloc(32, 7);
const SCHNORR_SECRET = Buffer.alloc(32, 9);

const publicKey = (tag: bigint): Buffer =>
  Buffer.from(
    tag === ED25519
      ? ed25519.getPublicKey(ED25519_SECRET)
      : schnorr.getPublicKey(SCHNORR_SECRET),
  );

const sign = (tag: bigint, message: Uint8Array): Buffer =>
  Buffer.from(
    tag === ED25519
      ? ed25519.sign(message, ED25519_SECRET)
      : schnorr.sign(message, SCHNORR_SECRET, Buffer.alloc(32, 1)),
  );

const constant = (bytes: Uint8Array): UPLCTerm =>
  UPLCConst.byteString(new DataB(bytes).bytes);

/** A 32-byte x coordinate with no point on secp256k1. */
const OFF_CURVE_KEY = Buffer.alloc(32, 0xff);

const SIZES = [9_215, 9_216, 48_999, 49_000, 49_001, 65_536] as const;

const UNCHARGED: MidgardCekBuiltinBudget = { cpu: 0n, memory: 0n };

const CONTEXT = Buffer.from("d87980", "hex");

/** The PV11 evaluator's start-up charge, spent before the first step. */
const STARTUP = { cpu: 100n, memory: 100n };

/**
 * What the pinned PV11 evaluator charges the failing builtin of `body`, whose
 * arguments are program constants. It charges machine steps in batches, so a
 * run that fails within its first batch has spent only the start-up charge
 * and the builtin's charge.
 */
const evaluatorCharge = (body: UPLCTerm): MidgardCekBuiltinBudget => {
  const evaluate = UPLC.eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluate(
    Buffer.from(
      UPLCEncoder.compile(new UPLCProgram([1, 1, 0], lam(discard(body)))),
    ),
    [CONTEXT],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    MIDGARD_CEK_TX_BUDGET.cpu,
    MIDGARD_CEK_TX_BUDGET.memory,
    false,
  );
  try {
    expect(result.ok).toBe(false);
    return {
      cpu: BigInt(result.cpu) - STARTUP.cpu,
      memory: BigInt(result.mem) - STARTUP.memory,
    };
  } finally {
    result.free();
  }
};

const steps = (
  outcome: ReturnType<typeof run>,
  kind: MidgardCekCoreStepWitness["kind"],
): MidgardCekExecutionStep[] =>
  (outcome.execution?.steps ?? []).filter((step) => step.witness.kind === kind);

const committedBool = (value: boolean) =>
  semanticizeMidgardCekDirectConstant(
    referenceConstantToDirectWitness(CEKConst.bool(value), true),
  );

describe("a signature over a staged message", () => {
  for (const [name, tag, builtin] of BUILTINS) {
    it.each(SIZES)(
      `${name} over %i bytes reaches the PV11 evaluator's verdict and budget`,
      (size) => {
        const message = builtBytes(size, 3);
        const signature = sign(tag, message);
        const flipped = Buffer.from(signature);
        flipped[0]! ^= 1;
        for (const [claim, verified] of [
          [signature, true],
          [flipped, false],
        ] as const) {
          const body = call(
            builtin,
            constant(publicKey(tag)),
            built(size, 3),
            constant(claim),
          );
          const verdict = run(body, "verdict");
          const proof = run(body, "proof");
          expect(verdict.result.kind).toBe("accepted");
          expect(proof.result).toEqual(verdict.result);
          const stagedSteps = steps(proof, "executeBuiltinStaged");
          const last = stagedSteps.at(-1)?.witness;
          if (last?.kind !== "executeBuiltinStaged") throw new Error("none");
          expect(last.tag).toBe(tag);
          expect(semanticizeMidgardCekDirectConstant(last.result)).toEqual(
            committedBool(verified),
          );
          for (const step of stagedSteps) {
            expect(
              verifyMidgardCekCoreStep(step.pre, step.post, staged(step, 9)),
            ).toBe(true);
          }
        }
      },
    );
  }
});

// Each failing case: its label, the builtin, its key, message and signature
// as run-time terms and as program constants, and whether the builtin is
// charged.
type Failing = readonly [
  string,
  bigint,
  Builtin,
  readonly [UPLCTerm, UPLCTerm, UPLCTerm],
  readonly [Uint8Array, Uint8Array, Uint8Array],
  boolean,
];

const failing: readonly Failing[] = BUILTINS.flatMap(
  ([name, tag, builtin]): Failing[] => {
    const key = publicKey(tag);
    const row = (
      label: string,
      [keyLength, keyBytes]: readonly [number | null, Uint8Array],
      messageLength: number,
      signatureBytes: Uint8Array,
      charged: boolean,
    ): Failing => {
      const message = builtBytes(messageLength, 3);
      const longKey = keyLength !== null;
      return [
        `${name}: ${label}`,
        tag,
        builtin,
        [
          longKey ? built(keyLength, 5) : constant(keyBytes),
          messageLength <= 8_000 ? constant(message) : built(messageLength, 3),
          constant(signatureBytes),
        ],
        [
          longKey ? builtBytes(keyLength, 5) : keyBytes,
          message,
          signatureBytes,
        ],
        charged,
      ];
    };
    const good = (size: number) => sign(tag, builtBytes(size, 3));
    const rows = [
      row(
        "a message of 65,537 bytes",
        [null, key],
        65_537,
        good(65_536),
        false,
      ),
      row(
        "a key of 31 bytes beside a message of 65,536 bytes",
        [null, key.subarray(1)],
        65_536,
        good(65_536),
        true,
      ),
      row(
        "a key of 31 bytes beside a message of 9,215 bytes",
        [null, key.subarray(1)],
        9_215,
        good(9_215),
        true,
      ),
      row(
        "a key of 31 bytes beside a message of 9,216 bytes",
        [null, key.subarray(1)],
        9_216,
        good(9_216),
        true,
      ),
      row(
        "a signature of 65 bytes beside a message of 9,216 bytes",
        [null, key],
        9_216,
        Buffer.concat([good(9_216), Buffer.from([0])]),
        true,
      ),
      row(
        "a signature of 63 bytes beside a message of 49,000 bytes",
        [null, key],
        49_000,
        good(49_000).subarray(1),
        true,
      ),
      row(
        "a key of 65,537 bytes beside a message of 1 byte",
        [65_537, new Uint8Array()],
        1,
        good(1),
        true,
      ),
    ];
    return tag === SCHNORR
      ? [
          ...rows,
          row(
            "a key off the curve beside a message of 65,536 bytes",
            [null, OFF_CURVE_KEY],
            65_536,
            good(65_536),
            true,
          ),
        ]
      : rows;
  },
);

describe("a signature builtin that fails beside a staged operand", () => {
  it.each(failing)(
    "%s is proved with the PV11 evaluator's charge",
    (_, tag, builtin, terms, constants, charged) => {
      expect(run(call(builtin, ...terms), "verdict").result.kind).toBe(
        "script_invalid",
      );
      const proof = run(call(builtin, ...terms), "proof");
      expect(proof.result.kind).toBe("script_invalid");
      const last = proof.execution!.steps.at(-1)!;
      expect(last.witness.kind).toBe("executeBuiltinStagedFailure");
      expect("tag" in last.witness ? last.witness.tag : null).toBe(tag);
      expect(last.post.mode).toBe("haltError");
      const charge = {
        cpu: last.post.cpu - last.pre.cpu,
        memory: last.post.memory - last.pre.memory,
      };
      expect(charge).toEqual(
        evaluatorCharge(call(builtin, ...constants.map(constant))),
      );
      expect(charge.cpu > 0n).toBe(charged);
      expect(
        verifyMidgardCekCoreStep(last.pre, last.post, staged(last, 9)),
      ).toBe(true);
    },
  );
});

const failureVerifies = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  witness: MidgardCekCoreStepWitness,
  charge: MidgardCekBuiltinBudget,
): boolean => {
  const pre = builtinState(tag, arguments_);
  return verifyMidgardCekCoreStep(
    pre,
    errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure, charge),
    witness,
  );
};

/** The step's arguments, the direct failure arm's witness, which carries
 * every operand inline, and the staged failure hop's, which reads every
 * operand's length from its leaf. */
const witnesses = (
  tag: bigint,
  operands: readonly MidgardCekStagedOperand[],
) => {
  const arguments_ = operands.map(presented);
  const committed = arguments_.map(semanticizeMidgardCekDirectConstant);
  return {
    arguments_,
    charge: midgardCekDirectBuiltinBudget(tag, committed),
    direct: {
      kind: "executeBuiltinFailure",
      tag,
      arguments: arguments_,
    } as MidgardCekCoreStepWitness,
    staged: {
      kind: "executeBuiltinStagedFailure",
      tag,
      arguments: committed,
      material: {
        runs: { refs: [], operands: [] },
        sources: operands.map((operand) => operand.leaf),
      },
      stagedOperands: [],
    } as MidgardCekCoreStepWitness,
  };
};

describe("a failing signature step has one provable result", () => {
  for (const [name, tag] of BUILTINS) {
    it(`${name}: both failure arms prove a short key beside a message within the direct bound, with the same charge`, () => {
      // A message the direct arm carries inline.
      const message = builtBytes(4_000, 3);
      const { arguments_, charge, direct, staged } = witnesses(tag, [
        bytesOperand(publicKey(tag).subarray(1)),
        bytesOperand(message),
        bytesOperand(sign(tag, message)),
      ]);
      expect(charge.cpu > 0n).toBe(true);
      expect(failureVerifies(tag, arguments_, direct, charge)).toBe(true);
      expect(failureVerifies(tag, arguments_, staged, charge)).toBe(true);
      expect(failureVerifies(tag, arguments_, direct, UNCHARGED)).toBe(false);
      expect(failureVerifies(tag, arguments_, staged, UNCHARGED)).toBe(false);
    });

    it(`${name}: only the staged failure hop proves a short key beside a message past the direct bound`, () => {
      const message = builtBytes(9_216, 3);
      const { arguments_, charge, direct, staged } = witnesses(tag, [
        bytesOperand(publicKey(tag).subarray(1)),
        bytesOperand(message),
        bytesOperand(sign(tag, message)),
      ]);
      expect(failureVerifies(tag, arguments_, direct, charge)).toBe(false);
      expect(failureVerifies(tag, arguments_, staged, charge)).toBe(true);
      expect(failureVerifies(tag, arguments_, staged, UNCHARGED)).toBe(false);
    });

    it(`${name}: no failure arm proves a well-formed signature over a message in bound`, () => {
      const message = builtBytes(9_216, 3);
      const { arguments_, charge, direct, staged } = witnesses(tag, [
        bytesOperand(publicKey(tag)),
        bytesOperand(message),
        bytesOperand(sign(tag, message)),
      ]);
      for (const budget of [charge, UNCHARGED]) {
        expect(failureVerifies(tag, arguments_, direct, budget)).toBe(false);
        expect(failureVerifies(tag, arguments_, staged, budget)).toBe(false);
      }
    });

    it(`${name}: only the uncharged staged failure proves a message of 65,537 bytes`, () => {
      const message = builtBytes(65_537, 3);
      const { arguments_, charge, direct, staged } = witnesses(tag, [
        bytesOperand(publicKey(tag).subarray(1)),
        bytesOperand(message),
        bytesOperand(sign(tag, message.subarray(1))),
      ]);
      expect(failureVerifies(tag, arguments_, staged, UNCHARGED)).toBe(true);
      expect(failureVerifies(tag, arguments_, staged, charge)).toBe(false);
      expect(failureVerifies(tag, arguments_, direct, UNCHARGED)).toBe(false);
      expect(failureVerifies(tag, arguments_, direct, charge)).toBe(false);
    });
  }
});
