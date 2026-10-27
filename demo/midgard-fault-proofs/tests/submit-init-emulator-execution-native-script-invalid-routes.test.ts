/**
 * `executionNativeScriptInvalid` routes from a genuine Init. A forced
 * rejection recording `ExecutionNativeScriptFalse` convicts at each purpose
 * kind whose native script holds (a native-locked spend, a native mint
 * policy, a native observer, a native-protected output), and wherever no
 * native script executes at the recorded coordinate: an invalid coordinate,
 * an observer, spend, mint or receive ordinal past its field, a key-locked
 * spend, and an observer whose source is not native.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardVersionedScript,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { ExecutionNativeScriptInvalidEntry } from "../src/execution-native-script-invalid/index.js";
import type { LedgerEntry } from "../src/ledger-at.js";
import {
  driveExecutionNativeScriptInvalidThread,
  EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS,
  makeExecutionNativeScriptInvalidHarness,
  publishExecutionNativeScriptInvalidRoles,
  stepTable,
} from "./support/execution-native-script-invalid-emulator.js";
import {
  admissibleNativeObservers,
  buildNativeSubject,
  keyLockedEntries,
  native,
  nativeFalseAt,
  type NativeSubjectShape,
  PAYER,
  setupNativeScenario,
  sig,
} from "./support/execution-native-script-invalid-emulator.subject.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  plutusScript,
  rejectedFor,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

/** A native script that holds, distinct per `depth`. */
const holding = (depth: number): MidgardVersionedScript => {
  let script: Parameters<typeof native>[0] = { type: "all", scripts: [] };
  for (let level = 0; level < depth; level += 1)
    script = { type: "all", scripts: [script] };
  return native(script);
};

const admitted = (shape: NativeSubjectShape): NativeSubjectShape => {
  expect(screenMidgardForcedTxAdmission(buildNativeSubject(shape))).toBe(null);
  return shape;
};

/** One observer whose native script the payer satisfies. */
const observed = () => admissibleNativeObservers([native(sig(PAYER))]);

/** A committed output locked by `script`'s enterprise script address. */
const scriptLocked = (script: MidgardVersionedScript): LedgerEntry[] => [
  {
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x41),
      outputIndex: 0,
    }),
    output: encodeMidgardTxOutput({
      address: scriptAddress(script),
      value: { lovelace: 5_000_000n, assets: new Map() },
    }),
  },
];

type Row = {
  readonly name: string;
  readonly shape: () => NativeSubjectShape;
  readonly recorded: SDK.DirectPurposeCoordinate;
  readonly entry: ExecutionNativeScriptInvalidEntry;
  readonly fact: SDK.ExecutionNativeScriptInvalidFinalState["fact"];
};

const at = (kind: bigint, index: bigint) => ({ kind, index });

const ROWS: readonly Row[] = [
  {
    name: "an observer whose native script holds",
    shape: observed,
    recorded: at(2n, 0n),
    entry: { Observe: at(2n, 0n) },
    fact: { Evaluated: { coordinate: at(2n, 0n), satisfied: true } },
  },
  {
    name: "an invalid coordinate",
    shape: observed,
    recorded: at(9n, 0n),
    entry: "Contradict",
    fact: { NoExecution: { coordinate: at(9n, 0n) } },
  },
  {
    name: "an observer ordinal past field 3",
    shape: observed,
    recorded: at(2n, 1n),
    entry: "ObserveAbsent",
    fact: { NoExecution: { coordinate: at(2n, 1n) } },
  },
  {
    name: "a spend ordinal past field 0",
    shape: observed,
    recorded: at(0n, 1n),
    entry: "SpendAbsent",
    fact: { NoExecution: { coordinate: at(0n, 1n) } },
  },
  {
    name: "a key-locked spend",
    shape: observed,
    recorded: at(0n, 0n),
    entry: "SpendKey",
    fact: { NoExecution: { coordinate: at(0n, 0n) } },
  },
  {
    name: "a mint ordinal past field 5",
    shape: observed,
    recorded: at(1n, 0n),
    entry: { Observe: at(1n, 0n) },
    fact: { NoExecution: { coordinate: at(1n, 0n) } },
  },
  {
    name: "a receive ordinal past the protected outputs",
    shape: observed,
    recorded: at(3n, 0n),
    entry: { Observe: at(3n, 0n) },
    fact: { NoExecution: { coordinate: at(3n, 0n) } },
  },
  {
    name: "an observer whose source is a Plutus script",
    shape: () =>
      admitted({
        observers: [
          { script: plutusScript(5), witness: false, redeemer: false },
        ],
        spend: keyLockedEntries(0x21, PAYER),
        reference: keyLockedEntries(0x31, PAYER, [plutusScript(5)]),
        signers: [PAYER],
      }),
    recorded: at(2n, 0n),
    entry: { Observe: at(2n, 0n) },
    fact: { NoExecution: { coordinate: at(2n, 0n) } },
  },
  {
    name: "a spend locked by a native script that holds",
    shape: () =>
      admitted({
        spend: scriptLocked(holding(1)),
        witnesses: [holding(1)],
        signers: [PAYER],
      }),
    recorded: at(0n, 0n),
    entry: { Observe: at(0n, 0n) },
    fact: { Evaluated: { coordinate: at(0n, 0n), satisfied: true } },
  },
  {
    name: "a native mint policy that holds",
    shape: () =>
      admitted({
        spend: keyLockedEntries(0x21, PAYER),
        mints: [{ script: holding(2), redeemer: false }],
        signers: [PAYER],
      }),
    recorded: at(1n, 0n),
    entry: { Observe: at(1n, 0n) },
    fact: { Evaluated: { coordinate: at(1n, 0n), satisfied: true } },
  },
  {
    name: "a native-protected output whose script holds",
    shape: () =>
      admitted({
        spend: keyLockedEntries(0x21, PAYER),
        receives: [{ script: holding(3), redeemer: false }],
        signers: [PAYER],
      }),
    recorded: at(3n, 0n),
    entry: { Observe: at(3n, 0n) },
    fact: { Evaluated: { coordinate: at(3n, 0n), satisfied: true } },
  },
];

describe("execution-native-script-invalid routes from a genuine Init", () => {
  it.each(ROWS)(
    "R-F: convicts ExecutionNativeScriptFalse recorded at $name",
    async (row) => {
      const harness = await makeExecutionNativeScriptInvalidHarness();
      const scenario = await setupNativeScenario({
        harness,
        shape: row.shape(),
        source: rejectedFor(
          nativeFalseAt(row.recorded.kind, row.recorded.index),
        ),
      });
      const references =
        await publishExecutionNativeScriptInvalidRoles(harness);
      const run = await driveExecutionNativeScriptInvalidThread({
        harness,
        scenario,
        references,
      });
      expect(run.prepared.entry).toStrictEqual(row.entry);
      expect(run.prepared.final.fact).toStrictEqual(row.fact);
      expect(run.prepared.convicts).toBe(true);
      const outcome = await run.terminal();
      console.log(
        `ENSI route ${row.name} steps`,
        JSON.stringify(stepTable(run.steps)),
      );
      for (const { label, measurement } of run.steps) {
        expect(
          measurement.executionMemory,
          `${label} memory`,
        ).toBeLessThanOrEqual(
          EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.memory,
        );
        expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
          EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.steps,
        );
        expect(
          measurement.completeSignedBytes,
          `${label} bytes`,
        ).toBeLessThanOrEqual(
          EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.bytes,
        );
      }
      expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
      await expectSingleUtxoWithUnit(
        harness.proverLucid,
        harness.executionNativeScriptInvalid.fraudProof.spendingScriptAddress,
        outcome.fraudProofUnit,
      );
    },
    1_200_000,
  );
});
