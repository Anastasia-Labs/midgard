import { describe, expect, it } from "vitest";

import type { LaterFaultPosition } from "../src/fraud-proof/first-fault-later-phases.js";
import type { RejectionReason } from "../src/rejection-reason.js";
import {
  admits,
  evaluation,
  inputAsset,
  integrity,
  mintAsset,
  outputAsset,
  purpose,
  receive,
  redeemer,
  run,
  scan,
  witness,
} from "./first-fault-later-phases.make-fixture.js";

describe("first-fault later phases twin", () => {
  // The same rows are pinned on-chain in first-fault-later-v1.test.ak
  // (twin_table); change both together.
  it("agrees with the on-chain twin table", () => {
    const rows: readonly [RejectionReason, LaterFaultPosition, boolean][] = [
      [{ RedeemerMissing: purpose(3n, 9n) }, witness(0n), true],
      [{ UnusedScriptWitness: { script_index: 3n } }, witness(2n), false],
      [{ UnusedScriptWitness: { script_index: 2n } }, witness(2n), true],
      [{ UnusedRedeemer: { redeemer_index: 0n } }, witness(7n), false],
      [{ UnusedScriptWitness: { script_index: 9n } }, redeemer(0n), true],
      [{ UnusedRedeemer: { redeemer_index: 4n } }, redeemer(3n), false],
      [
        { ExecutionNativeScriptMalformed: purpose(0n, 0n) },
        redeemer(9n),
        false,
      ],
      [{ UnusedRedeemer: { redeemer_index: 9n } }, scan(0n, 0n), true],
      [{ ExecutionNativeScriptFalse: purpose(1n, 2n) }, scan(1n, 2n), false],
      [{ ExecutionNativeScriptNodeLimit: purpose(1n, 2n) }, scan(1n, 2n), true],
      [
        { ExecutionNativeScriptDepthLimit: purpose(1n, 0n) },
        scan(0n, 9n),
        false,
      ],
      [
        { ExecutionNativeScriptMalformed: purpose(1n, 2n) },
        evaluation(1n, 2n),
        true,
      ],
      ["ScriptIntegrityHashMismatch", evaluation(3n, 9n), false],
      [{ ExecutionNativeScriptFalse: purpose(3n, 9n) }, integrity, true],
      ["ScriptIntegrityHashMismatch", integrity, true],
      [{ PlutusExecutionFailed: purpose(0n, 0n) }, integrity, false],
      ["ScriptIntegrityHashMismatch", receive(3n, 0n), true],
      [{ PlutusExecutionFailed: purpose(3n, 1n) }, receive(3n, 1n), false],
      [{ ReceivePurposePlutusV3Forbidden: purpose(3n, 1n) }, run(3n, 1n), true],
      [{ PlutusExecutionFailed: purpose(0n, 5n) }, receive(3n, 1n), true],
      [{ OutputBelowMinAda: { output_index: 0n } }, run(3n, 9n), false],
      [{ PlutusExecutionFailed: purpose(3n, 9n) }, inputAsset(0n, 0n), true],
      [
        { InputAssetAccumulationLimit: { input_index: 1n, asset_index: 5n } },
        inputAsset(1n, 4n),
        false,
      ],
      [
        { InputAssetAccumulationLimit: { input_index: 0n, asset_index: 9n } },
        inputAsset(1n, 4n),
        true,
      ],
      [{ OutputBelowMinAda: { output_index: 0n } }, inputAsset(9n, 9n), false],
      [{ OutputBelowMinAda: { output_index: 2n } }, outputAsset(2n, 0n), true],
      [{ OutputBelowMinAda: { output_index: 3n } }, outputAsset(2n, 3n), false],
      [
        { OutputAssetAccumulationLimit: { output_index: 2n, asset_index: 4n } },
        outputAsset(2n, 3n),
        false,
      ],
      [
        { MintAssetAccumulationLimit: { mint_index: 0n } },
        outputAsset(9n, 9n),
        false,
      ],
      [{ OutputBelowMinAda: { output_index: 99n } }, mintAsset(4n), true],
      [
        { MintAssetAccumulationLimit: { mint_index: 5n } },
        mintAsset(4n),
        false,
      ],
      [{ MintAssetAccumulationLimit: { mint_index: 4n } }, mintAsset(4n), true],
      ["ValueNotPreserved", mintAsset(4n), false],
    ];
    expect(
      rows.map(([reason, observed]) => admits(reason, observed)),
    ).toStrictEqual(rows.map(([, , expected]) => expected));
  });
});
