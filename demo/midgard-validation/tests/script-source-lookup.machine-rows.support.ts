/**
 * The trace harness and the Aiken renderer of
 * `script-source-lookup.machine-rows.test.ts`: one transaction runs through
 * Phase B and through the trace builder, and the steps it exports are written
 * as the whole Aiken module `script-source-lookup-rows.test.ak`.
 */

import { fileURLToPath } from "node:url";

import { encodeCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  RejectCodes,
  runPhaseBValidationWithPatch,
  validationMachineLedgerRoot,
} from "../src/index.js";
import {
  buildValidationOneStepArgument,
  validationMachineStateData,
} from "../src/validation-machine-data.js";
import {
  makeNativeTx,
  makePhaseBCandidate,
  nativeScriptWitness,
  outRefFromTxId,
} from "./validation-fixtures.js";

// A fixed key: the shared fixtures' key is fresh on every run, and these rows
// are exported as checked-in vectors.
export const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x5a));

/** The block an event's trace is built in. */
const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

/** One transaction, its spent inputs and the ledger it is validated against. */
export type TraceCase = {
  readonly spent: readonly Buffer[];
  readonly state: readonly (readonly [Buffer, Buffer])[];
  readonly options: {
    readonly outputs: readonly [Buffer];
    readonly mintPreimageCbor?: Buffer;
    readonly referenceInputs?: readonly Buffer[];
    readonly scriptWitnesses?: readonly ReturnType<
      typeof nativeScriptWitness
    >[];
  };
  readonly rejectionCode: (typeof RejectCodes)[keyof typeof RejectCodes] | null;
};

const txOptions = (traceCase: TraceCase) => ({
  privateKey: KEY,
  ...traceCase.options,
  outputs: [...traceCase.options.outputs],
  referenceInputs: [...(traceCase.options.referenceInputs ?? [])],
  ...(traceCase.options.scriptWitnesses === undefined
    ? {}
    : { scriptWitnesses: [...traceCase.options.scriptWitnesses] }),
});

export const phaseB = async (traceCase: TraceCase) => {
  const result = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [
        makePhaseBCandidate({
          spent: [...traceCase.spent],
          ...txOptions(traceCase),
        }),
      ],
      new Map(
        traceCase.state.map(([key, output]) => [key.toString("hex"), output]),
      ),
      {
        nowCardanoSlotNo: 100n,
        bucketConcurrency: 1,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      },
    ),
  );
  return result.rejected.length === 0
    ? { accepted: result.accepted.length, rejected: null }
    : { accepted: result.accepted.length, rejected: result.rejected[0]! };
};

export const machineTrace = async (traceCase: TraceCase) => {
  const transaction = makeNativeTx({
    spendInputs: [...traceCase.spent],
    ...txOptions(traceCase),
  });
  const entries = traceCase.state.map(([outRef, output]) => ({
    outRef,
    output,
  }));
  const accepted = traceCase.rejectionCode === null;
  const expectedLedgerOps = accepted
    ? [
        ...traceCase.spent.map((key) => ({ type: "delete" as const, key })),
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: traceCase.options.outputs[0],
        }),
      ]
    : [];
  const ledgerMutationSteps = accepted
    ? await buildValidationMachineLedgerMutationSteps({
        initialEntries: entries,
        operations: expectedLedgerOps,
      })
    : [];
  // A rejected trace leaves the ledger unchanged; its stage-3 replays still
  // open each resolved input against this root.
  const unchangedRoot = (await validationMachineLedgerRoot(entries)).toString(
    "hex",
  );
  return await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      ...context,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: accepted
        ? ledgerMutationSteps[0]!.preRoot.toString("hex")
        : unchangedRoot,
      postUtxosRoot: accepted
        ? ledgerMutationSteps.at(-1)!.postRoot.toString("hex")
        : unchangedRoot,
      ledgerWitnessEntries: entries,
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: accepted ? "accepted" : "rejected",
      expectedRejectionCode: traceCase.rejectionCode,
    }),
  );
};

/** A step of the trace, as the input of the on-chain one-step verifier. */
export type OneStepVector = {
  readonly stage: number;
  readonly pre: string;
  readonly transition: string;
  readonly auxiliary: string;
};

export const oneStepVector = (
  trace: DeterministicValidationMachineTrace,
  stateIndex: number,
  stage: number,
): OneStepVector => {
  const argument = buildValidationOneStepArgument({ trace, stateIndex });
  return {
    stage,
    pre: Data.to(
      validationMachineStateData(trace.states[stateIndex]!) as never,
    ),
    transition: argument.transitionCbor.toString("hex"),
    auxiliary: argument.auxiliaryCbor.toString("hex"),
  };
};

/**
 * A native-scripts scan control's stage, NativeScripts continuation and
 * purpose kind (control items 5, 17 and 18). The control chunks long byte
 * strings, so it is read as Plutus data.
 */
const nativeScanControl = (cbor: Buffer) => {
  const items = Data.from(
    cbor.toString("hex"),
  ) as unknown as readonly unknown[];
  const [, continuation] = items[17] as readonly [unknown, string];
  return {
    stage: Number(items[5] as bigint),
    continuation,
    purposeKind: items[18] as bigint,
  };
};

/**
 * Every execution scan step of the trace: a native-scripts scan step that
 * carries the NativeScripts continuation it returns to.
 */
export const executionScanVectors = (
  trace: DeterministicValidationMachineTrace,
) =>
  trace.witnesses.flatMap((witness, stateIndex) => {
    if (witness.phase !== "phaseANativeScripts") return [];
    const control = nativeScanControl(witness.cbor);
    if (control.continuation.length === 0) return [];
    return [
      {
        ...oneStepVector(trace, stateIndex, control.stage),
        purposeKind: control.purposeKind,
      },
    ];
  });

export const AIKEN_ROWS_PATH = fileURLToPath(
  new URL(
    "../../../onchain/aiken/lib/midgard/validation-machine-tests/script-source-lookup-rows.test.ak",
    import.meta.url,
  ),
);

/**
 * Rows whose stage-9 match step is also checked against the skip that moves
 * past the matching source; the skip must be refused.
 */
const SKIP_THE_MATCH_ROWS = new Set(["first_of_two_references"]);

export const renderAikenRows = (
  rows: readonly {
    readonly name: string;
    readonly rejection: null | { readonly reason: unknown };
    readonly vectors: readonly OneStepVector[];
  }[],
  nativeRows: readonly {
    readonly name: string;
    readonly purposeKind: bigint;
    readonly falseAtIndex: bigint | null;
    readonly vectors: ReturnType<typeof executionScanVectors>;
  }[],
) => {
  const lines = [
    "// Generated by demo/midgard-validation/tests/script-source-lookup.machine-rows.test.ts",
    "// (MIDGARD_WRITE_SCRIPT_SOURCE_LOOKUP_ROWS=1). Do not edit by hand.",
    "",
    "use midgard/rejection_reason_v1",
    "use midgard/validation_machine_tests/script_source_lookup_rows_support.{",
    "  OneStepVector, every_step_dispatches, refuses_skipping_the_match, rejects_with,",
    "  verifies_every_step,",
    "}",
    "",
  ];
  const pushSteps = (name: string, vectors: readonly OneStepVector[]) => {
    lines.push(`const ${name}_steps: List<OneStepVector> =`);
    lines.push("  [");
    for (const vector of vectors)
      lines.push(
        "    OneStepVector {",
        `      stage: ${vector.stage},`,
        `      pre: #"${vector.pre}",`,
        `      transition: #"${vector.transition}",`,
        `      auxiliary: #"${vector.auxiliary}",`,
        "    },",
      );
    lines.push("  ]", "");
  };
  for (const row of rows) {
    pushSteps(row.name, row.vectors);
    lines.push(`test ${row.name}_steps_verify_on_chain() {`);
    const reason = row.rejection?.reason as
      | { UnusedScriptWitness: { script_index: bigint } }
      | { ScriptSourceMissing: { purpose_kind: bigint; purpose_index: bigint } }
      | undefined;
    if (reason === undefined) {
      lines.push(`  verifies_every_step(${row.name}_steps)`);
    } else {
      // Laid out as `aiken fmt` lays it out.
      const aikenReason =
        "UnusedScriptWitness" in reason
          ? [
              `      rejection_reason_v1.UnusedScriptWitness { script_index: ${reason.UnusedScriptWitness.script_index} },`,
            ]
          : [
              "      rejection_reason_v1.ScriptSourceMissing {",
              `        purpose_kind: ${reason.ScriptSourceMissing.purpose_kind},`,
              `        purpose_index: ${reason.ScriptSourceMissing.purpose_index},`,
              "      },",
            ];
      lines.push(
        "  and {",
        `    verifies_every_step(${row.name}_steps),`,
        "    rejects_with(",
        `      ${row.name}_steps,`,
        ...aikenReason,
        "    ),",
        "  }",
      );
    }
    lines.push("}", "");
    if (SKIP_THE_MATCH_ROWS.has(row.name))
      lines.push(
        `test ${row.name}_refuses_skipping_the_match() {`,
        `  refuses_skipping_the_match(${row.name}_steps)`,
        "}",
        "",
      );
  }
  for (const row of nativeRows) {
    pushSteps(row.name, row.vectors);
    lines.push(`test ${row.name}_steps_verify_on_chain() {`);
    if (row.falseAtIndex === null)
      lines.push(`  every_step_dispatches(${row.name}_steps)`);
    else
      lines.push(
        "  and {",
        `    every_step_dispatches(${row.name}_steps),`,
        "    rejects_with(",
        `      ${row.name}_steps,`,
        "      rejection_reason_v1.ExecutionNativeScriptFalse {",
        `        purpose_kind: ${row.purposeKind},`,
        `        purpose_index: ${row.falseAtIndex},`,
        "      },",
        "    ),",
        "  }",
      );
    lines.push("}", "");
  }
  return lines.join("\n");
};
