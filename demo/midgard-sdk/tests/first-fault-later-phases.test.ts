import { describe, expect, it } from "vitest";

import {
  laterFaultPhase,
  type LaterFaultPosition,
  leafAdmitsLaterFault,
} from "../src/fraud-proof/first-fault-later-phases.js";
import { acceptedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";
import {
  admits,
  context,
  evaluation,
  everyLaterPosition,
  inputAsset,
  integrity,
  mintAsset,
  outputAsset,
  purpose,
  receive,
  redeemer,
  rejected,
  run,
  scan,
  witness,
} from "./first-fault-later-phases.make-fixture.js";

describe("first-fault later phases", () => {
  it("ranks the later phases in the machine's order", () => {
    expect(everyLaterPosition.map(laterFaultPhase)).toStrictEqual([
      8, 8, 9, 9, 10, 11, 11, 12, 12, 12,
    ]);
  });

  it("admits every later fault behind an earlier-phase leaf", () => {
    for (const observed of everyLaterPosition)
      expect(
        admits({ FieldPreimageLengthMismatch: { field_index: 0n } }, observed),
      ).toBe(true);
  });

  describe("unused script witness (arm 33)", () => {
    it("follows every earlier ScriptSources fault", () => {
      expect(
        admits({ ScriptSourceMissing: purpose(3n, 9n) }, witness(0n)),
      ).toBe(true);
      expect(admits({ RedeemerMissing: purpose(0n, 0n) }, witness(0n))).toBe(
        true,
      );
      expect(
        admits({ MintItemMalformed: { policy_index: 0n } }, witness(0n)),
      ).toBe(true);
      expect(admits("NetworkIdMismatch", witness(0n))).toBe(true);
    });

    it("precedes the redeemer audit and every later phase", () => {
      expect(
        admits({ UnusedRedeemer: { redeemer_index: 0n } }, witness(7n)),
      ).toBe(false);
      expect(
        admits(
          { ExecutionNativeScriptMalformed: purpose(0n, 0n) },
          witness(7n),
        ),
      ).toBe(false);
      expect(admits("ValueNotPreserved", witness(7n))).toBe(false);
    });

    it("breaks ties by field-6 ordinal", () => {
      const at = (script_index: bigint) => ({
        UnusedScriptWitness: { script_index },
      });
      expect(admits(at(3n), witness(2n))).toBe(false);
      expect(admits(at(2n), witness(2n))).toBe(true);
      expect(admits(at(1n), witness(2n))).toBe(true);
    });
  });

  describe("unused redeemer (arm 34)", () => {
    it("follows every unused witness and every purpose fault", () => {
      expect(
        admits({ UnusedScriptWitness: { script_index: 9n } }, redeemer(0n)),
      ).toBe(true);
      expect(admits({ RedeemerMissing: purpose(3n, 9n) }, redeemer(0n))).toBe(
        true,
      );
    });

    it("precedes native scripts and Cek", () => {
      expect(
        admits({ ExecutionNativeScriptFalse: purpose(0n, 0n) }, redeemer(9n)),
      ).toBe(false);
      expect(
        admits({ PlutusExecutionFailed: purpose(0n, 0n) }, redeemer(9n)),
      ).toBe(false);
    });

    it("breaks ties by field-8 ordinal", () => {
      const at = (redeemer_index: bigint) => ({
        UnusedRedeemer: { redeemer_index },
      });
      expect(admits(at(4n), redeemer(3n))).toBe(false);
      expect(admits(at(3n), redeemer(3n))).toBe(true);
    });
  });

  describe("native scripts (arms 35-38)", () => {
    it("follow the unused audits and precede script integrity", () => {
      expect(
        admits({ UnusedRedeemer: { redeemer_index: 9n } }, scan(0n, 0n)),
      ).toBe(true);
      expect(
        admits(
          { UnusedScriptWitness: { script_index: 9n } },
          evaluation(0n, 0n),
        ),
      ).toBe(true);
      expect(admits("ScriptIntegrityHashMismatch", scan(3n, 9n))).toBe(false);
      expect(admits("ScriptIntegrityHashMismatch", evaluation(3n, 9n))).toBe(
        false,
      );
    });

    it("rank one execution's scan before its evaluation", () => {
      expect(
        admits({ ExecutionNativeScriptFalse: purpose(1n, 2n) }, scan(1n, 2n)),
      ).toBe(false);
      expect(
        admits(
          { ExecutionNativeScriptMalformed: purpose(1n, 2n) },
          evaluation(1n, 2n),
        ),
      ).toBe(true);
      expect(
        admits(
          { ExecutionNativeScriptFalse: purpose(1n, 2n) },
          evaluation(1n, 2n),
        ),
      ).toBe(true);
    });

    it("give the three scan arms one rank", () => {
      for (const reason of [
        { ExecutionNativeScriptMalformed: purpose(1n, 2n) },
        { ExecutionNativeScriptNodeLimit: purpose(1n, 2n) },
        { ExecutionNativeScriptDepthLimit: purpose(1n, 2n) },
      ])
        expect(admits(reason, scan(1n, 2n))).toBe(true);
    });

    it("order executions by kind, then index", () => {
      expect(
        admits(
          { ExecutionNativeScriptNodeLimit: purpose(1n, 3n) },
          evaluation(1n, 2n),
        ),
      ).toBe(false);
      expect(
        admits({ ExecutionNativeScriptFalse: purpose(1n, 1n) }, scan(1n, 2n)),
      ).toBe(true);
      expect(
        admits(
          { ExecutionNativeScriptDepthLimit: purpose(1n, 0n) },
          scan(0n, 9n),
        ),
      ).toBe(false);
      expect(
        admits(
          { ExecutionNativeScriptDepthLimit: purpose(0n, 9n) },
          scan(1n, 0n),
        ),
      ).toBe(true);
    });
  });

  describe("script integrity (arm 39)", () => {
    it("follows native scripts and precedes Cek and value", () => {
      expect(
        admits({ ExecutionNativeScriptFalse: purpose(3n, 9n) }, integrity),
      ).toBe(true);
      expect(admits("ScriptIntegrityHashMismatch", integrity)).toBe(true);
      expect(
        admits({ ReceivePurposePlutusV3Forbidden: purpose(3n, 0n) }, integrity),
      ).toBe(false);
      expect(
        admits({ PlutusExecutionFailed: purpose(0n, 0n) }, integrity),
      ).toBe(false);
      expect(
        admits(
          { InputAssetAccumulationLimit: { input_index: 0n, asset_index: 0n } },
          integrity,
        ),
      ).toBe(false);
    });
  });

  describe("Cek (arms 40-41)", () => {
    it("follows script integrity and precedes value", () => {
      expect(admits("ScriptIntegrityHashMismatch", receive(3n, 0n))).toBe(true);
      expect(admits("ScriptIntegrityHashMismatch", run(0n, 0n))).toBe(true);
      expect(
        admits(
          { MintAssetAccumulationLimit: { mint_index: 0n } },
          receive(3n, 9n),
        ),
      ).toBe(false);
      expect(
        admits({ OutputBelowMinAda: { output_index: 0n } }, run(3n, 9n)),
      ).toBe(false);
    });

    it("ranks one execution's receive-language check before its run", () => {
      expect(
        admits({ PlutusExecutionFailed: purpose(3n, 1n) }, receive(3n, 1n)),
      ).toBe(false);
      expect(
        admits(
          { ReceivePurposePlutusV3Forbidden: purpose(3n, 1n) },
          run(3n, 1n),
        ),
      ).toBe(true);
      expect(
        admits(
          { ReceivePurposePlutusV3Forbidden: purpose(3n, 1n) },
          receive(3n, 1n),
        ),
      ).toBe(true);
      expect(
        admits({ PlutusExecutionFailed: purpose(0n, 4n) }, run(0n, 4n)),
      ).toBe(true);
    });

    it("orders executions by kind, then index", () => {
      expect(
        admits({ PlutusExecutionFailed: purpose(3n, 2n) }, receive(3n, 1n)),
      ).toBe(false);
      expect(
        admits({ PlutusExecutionFailed: purpose(0n, 5n) }, receive(3n, 1n)),
      ).toBe(true);
      expect(
        admits({ PlutusExecutionFailed: purpose(2n, 0n) }, run(1n, 7n)),
      ).toBe(false);
    });
  });

  describe("value and mint (arms 42-44)", () => {
    const input = (input_index: bigint, asset_index: bigint) => ({
      InputAssetAccumulationLimit: { input_index, asset_index },
    });
    const output = (output_index: bigint, asset_index: bigint) => ({
      OutputAssetAccumulationLimit: { output_index, asset_index },
    });
    const floor = (output_index: bigint) => ({
      OutputBelowMinAda: { output_index },
    });
    const mint = (mint_index: bigint) => ({
      MintAssetAccumulationLimit: { mint_index },
    });

    it("follows Cek", () => {
      expect(
        admits({ PlutusExecutionFailed: purpose(3n, 9n) }, inputAsset(0n, 0n)),
      ).toBe(true);
      expect(
        admits(
          { ReceivePurposePlutusV3Forbidden: purpose(3n, 9n) },
          mintAsset(0n),
        ),
      ).toBe(true);
    });

    it("orders input assets by input, then asset", () => {
      expect(admits(input(1n, 4n), inputAsset(1n, 4n))).toBe(true);
      expect(admits(input(1n, 5n), inputAsset(1n, 4n))).toBe(false);
      expect(admits(input(2n, 0n), inputAsset(1n, 4n))).toBe(false);
      expect(admits(input(0n, 9n), inputAsset(1n, 4n))).toBe(true);
    });

    it("folds every input before any output, mint or preservation", () => {
      for (const later of [
        floor(0n),
        output(0n, 0n),
        mint(0n),
        "ValueNotPreserved" as const,
      ])
        expect(admits(later, inputAsset(9n, 9n))).toBe(false);
      expect(admits(input(9n, 9n), outputAsset(2n, 3n))).toBe(true);
    });

    it("checks an output's minimum Ada before its assets", () => {
      expect(admits(floor(2n), outputAsset(2n, 0n))).toBe(true);
      expect(admits(floor(3n), outputAsset(2n, 3n))).toBe(false);
    });

    it("orders output assets by output, then asset", () => {
      expect(admits(output(2n, 4n), outputAsset(2n, 3n))).toBe(false);
      expect(admits(output(2n, 3n), outputAsset(2n, 3n))).toBe(true);
      expect(admits(output(1n, 9n), outputAsset(2n, 3n))).toBe(true);
    });

    it("folds every output before mint and preservation", () => {
      expect(admits(mint(0n), outputAsset(9n, 9n))).toBe(false);
      expect(admits("ValueNotPreserved", outputAsset(9n, 9n))).toBe(false);
      expect(admits(output(99n, 99n), mintAsset(4n))).toBe(true);
      expect(admits(floor(99n), mintAsset(4n))).toBe(true);
    });

    it("orders mint assets by index, before preservation", () => {
      expect(admits(mint(5n), mintAsset(4n))).toBe(false);
      expect(admits(mint(4n), mintAsset(4n))).toBe(true);
      expect(admits(mint(3n), mintAsset(4n))).toBe(true);
      expect(admits("ValueNotPreserved", mintAsset(4n))).toBe(false);
    });
  });

  describe("negate-to-convict contract", () => {
    it("admits no later fault behind an accepted leaf", () => {
      for (const observed of everyLaterPosition) {
        expect(
          leafAdmitsLaterFault(
            acceptedVerdictSubject("11".repeat(32)),
            context,
            observed,
          ),
        ).toBe(false);
        expect(leafAdmitsLaterFault(rejected(null), context, observed)).toBe(
          false,
        );
      }
    });

    it("refuses a noncanonical subject or a missing context", () => {
      expect(() =>
        leafAdmitsLaterFault(
          { ...rejected("ValueNotPreserved"), transaction_id: "00" },
          context,
          mintAsset(0n),
        ),
      ).toThrow("noncanonical subject");
      expect(() =>
        leafAdmitsLaterFault(
          rejected("ValueNotPreserved"),
          null as never,
          mintAsset(0n),
        ),
      ).toThrow("network context");
    });

    it("refuses negative or out-of-domain coordinates on either side", () => {
      const refusals: readonly [RejectionReason, LaterFaultPosition][] = [
        [{ UnusedScriptWitness: { script_index: 0n } }, witness(-1n)],
        [{ UnusedRedeemer: { redeemer_index: -1n } }, redeemer(0n)],
        [{ UnusedRedeemer: { redeemer_index: -1n } }, witness(0n)],
        [{ ExecutionNativeScriptFalse: purpose(0n, 0n) }, scan(4n, 0n)],
        [{ ExecutionNativeScriptFalse: purpose(0n, -1n) }, scan(0n, 0n)],
        [{ PlutusExecutionFailed: purpose(0n, 0n) }, run(0n, -1n)],
        [
          { InputAssetAccumulationLimit: { input_index: 0n, asset_index: 0n } },
          inputAsset(0n, -1n),
        ],
        [{ OutputBelowMinAda: { output_index: -1n } }, outputAsset(0n, 0n)],
        ["ValueNotPreserved", mintAsset(-1n)],
      ];
      for (const [reason, observed] of refusals)
        expect(() => admits(reason, observed)).toThrow();
    });
  });
});
