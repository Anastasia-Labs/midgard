import {
  computeHash28,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardNativeScript,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  retainedValidationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../midgard-validation/tests/validation-fixtures.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  detectExecutionSourceScriptDecodingCanonicalViolations,
  prepareExecutionSourceScriptDecodingArtifact,
} from "../src/execution-source-script-decoding/authenticated-replay.js";
import { buildExecutionSourceMachineAuthenticationFromRetainedDa } from "../src/execution-source-script-decoding/retained-witness.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { malformedInlinePhaseAReplay } from "./support/execution-source-phase-a-dominance.js";

const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

describe("executionSourceScriptDecoding retained-DA production replay", () => {
  it("reconstructs a valid accepted source from its exact retained execution witness and refuses tampering", async () => {
    const spent = outRefFromByte(0x7a);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const nativePayload = encodeMidgardNativeScript({
      type: "all",
      scripts: [],
    });
    const nativeItem = encodeCbor([0n, nativePayload]);
    const policyId = computeHash28(
      Buffer.concat([Buffer.from([0]), nativePayload]),
    );
    const assetName = Buffer.from("31", "hex");
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE,
      undefined,
      new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 1n]])],
      ]),
    );
    const baseline = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [nativeScriptWitness({ type: "all", scripts: [] })],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[policyId, new Map([[assetName, 1n]])]]),
      ),
    });
    const transaction = encodeRecomputedNativeTx({
      ...baseline.tx,
      witnessSet: {
        ...baseline.tx.witnessSet,
        scriptTxWitsPreimageCbor: encodeCbor([nativeItem]),
      },
    });
    const acceptedOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const mutations = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: acceptedOps,
    });
    const eventKey = {
      L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") },
    } as const;
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        eventKeyCbor: Buffer.from(
          Data.to(eventKey as never, SDK.EventKeySchema),
          "hex",
        ),
        sourceKind: "normal",
        blockEndTimeMs: 1_750_000_000_000,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
        postUtxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: acceptedOps,
        ledgerMutationSteps: mutations,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );
    const stateIndex = trace.witnesses.findIndex(
      ({ phase, auxiliary }) =>
        phase === "nativeScripts" &&
        auxiliary?.kind === "nativeExecutionDescriptor",
    );
    expect(stateIndex).toBeGreaterThanOrEqual(0);
    const witness = trace.witnesses[stateIndex]!;
    if (witness.auxiliary?.kind !== "nativeExecutionDescriptor")
      throw new Error("fixture omitted native execution descriptor");

    const descriptor: SDK.ValidationTraceDescriptor = {
      schema_version: BigInt(trace.tree.descriptor.schemaVersion),
      machine_version: BigInt(trace.tree.descriptor.machineVersion),
      trace_root: trace.tree.descriptor.traceRoot.toString("hex"),
      step_count: BigInt(trace.tree.descriptor.stepCount),
      initial_state_hash:
        trace.tree.descriptor.initialStateHash.toString("hex"),
      terminal_state_hash:
        trace.tree.descriptor.terminalStateHash.toString("hex"),
      verdict: "Accepted",
      rejection_code_hash:
        trace.tree.descriptor.rejectionCodeHash.toString("hex"),
    };
    const descriptorEntry = entry(
      Buffer.from(Data.to(eventKey as never, SDK.EventKeySchema), "hex"),
      Buffer.from(
        Data.to(descriptor as never, SDK.ValidationTraceDescriptorSchema),
        "hex",
      ),
    );
    const auxiliary = Data.from(
      Data.to(
        retainedValidationAuxiliaryWitnessData(witness.auxiliary) as never,
      ),
      SDK.RetainedValidationAuxiliaryWitnessSchema,
    ) as unknown as SDK.RetainedValidationAuxiliaryWitness;
    const retainedKey: SDK.RetainedValidationWitnessKey = {
      event_key: eventKey,
      execution_index: BigInt(witness.auxiliary.executionIndex),
    };
    const retainedValue: SDK.RetainedValidationWitness = {
      machine_state: SDK.validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      ),
      trace_proof: SDK.validationTraceProofDataFromCore(
        trace.tree.proofs[stateIndex]!,
      ),
      phase: 9n,
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary,
    };
    const retainedEntry = entry(
      SDK.encodeRetainedValidationWitnessKey(retainedKey),
      SDK.encodeRetainedValidationWitness(retainedValue),
    );
    const validationRoot = await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      [
        {
          key: Buffer.from(descriptorEntry[0], "hex"),
          value: Buffer.from(descriptorEntry[1], "hex"),
        },
      ],
    );
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      transaction.txCbor,
    );
    const sourceValue: SDK.L2TransactionSource = {
      tx_id: transaction.txId.toString("hex"),
      source: {
        compact_cbor: material.proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    const sourceValueCbor = Data.to(
      sourceValue as never,
      SDK.L2TransactionSourceSchema,
    );
    const retainedInput = {
      eventKey,
      executionIndex: 0,
      authenticatedValidationTraceEntries: [
        {
          key: Buffer.from(descriptorEntry[0], "hex"),
          value: Buffer.from(descriptorEntry[1], "hex"),
        },
      ],
      retainedValidationWitnessEntries: [
        {
          key: Buffer.from(retainedEntry[0], "hex"),
          value: Buffer.from(retainedEntry[1], "hex"),
        },
      ],
      expectedValidationTracesRoot: validationRoot.root,
    } as const;
    const changedWorkWitness = Buffer.from(retainedValue.witness_cbor, "hex");
    changedWorkWitness[changedWorkWitness.length - 1] =
      changedWorkWitness[changedWorkWitness.length - 1]! ^ 1;
    await expect(
      buildExecutionSourceMachineAuthenticationFromRetainedDa({
        ...retainedInput,
        retainedValidationWitnessEntries: [
          {
            ...retainedInput.retainedValidationWitnessEntries[0]!,
            value: SDK.encodeRetainedValidationWitness({
              ...retainedValue,
              witness_cbor: changedWorkWitness.toString("hex"),
            }),
          },
        ],
      }),
    ).rejects.toThrow(/state\/proof\/work witness is invalid/u);
    if (
      typeof retainedValue.auxiliary !== "object" ||
      !("NativeExecutionDescriptorWitness" in retainedValue.auxiliary)
    )
      throw new Error("fixture retained the wrong auxiliary kind");
    await expect(
      buildExecutionSourceMachineAuthenticationFromRetainedDa({
        ...retainedInput,
        retainedValidationWitnessEntries: [
          {
            ...retainedInput.retainedValidationWitnessEntries[0]!,
            value: SDK.encodeRetainedValidationWitness({
              ...retainedValue,
              auxiliary: {
                NativeExecutionDescriptorWitness: {
                  ...retainedValue.auxiliary.NativeExecutionDescriptorWitness,
                  script_hash: "ff".repeat(28),
                },
              },
            }),
          },
        ],
      }),
    ).rejects.toThrow(/membership is invalid/u);
    await expect(
      buildExecutionSourceMachineAuthenticationFromRetainedDa({
        ...retainedInput,
        retainedValidationWitnessEntries: [
          {
            ...retainedInput.retainedValidationWitnessEntries[0]!,
            value: SDK.encodeRetainedValidationWitness({
              ...retainedValue,
              auxiliary: {
                NativeExecutionDescriptorWitness: {
                  ...retainedValue.auxiliary.NativeExecutionDescriptorWitness,
                  signer_peaks: [
                    ...retainedValue.auxiliary.NativeExecutionDescriptorWitness
                      .signer_peaks,
                    { height: 0n, hash: "ee".repeat(32) },
                  ],
                },
              },
            }),
          },
        ],
      }),
    ).rejects.toThrow(/signer frontier is invalid/u);
    await expect(
      buildExecutionSourceMachineAuthenticationFromRetainedDa({
        ...retainedInput,
        retainedValidationWitnessEntries: [
          {
            key: SDK.encodeRetainedValidationWitnessKey({
              ...retainedKey,
              execution_index: 1n,
            }),
            value: Buffer.from(retainedEntry[1], "hex"),
          },
        ],
      }),
    ).rejects.toThrow(/coordinate is absent/u);
    const block = {
      headerHash: "ab".repeat(28),
      header: {
        validationTracesRoot: validationRoot.root,
      },
      reconstruction: {
        forcedTransactions: [],
        payload: {
          block_body: {
            validation_traces: [descriptorEntry],
            validation_trace_witnesses: [retainedEntry],
          },
        },
      },
      transactions: [
        {
          nodeTxId: transaction.txId.toString("hex"),
          txCbor: transaction.txCbor.toString("hex"),
          l2TransactionSourceCbor: sourceValueCbor,
        },
      ],
    } as unknown as CanonicalBlockEvidence;
    const detections =
      await detectExecutionSourceScriptDecodingCanonicalViolations(block);
    expect(detections).toEqual([]);
    const { authentication } =
      await buildExecutionSourceMachineAuthenticationFromRetainedDa(
        retainedInput,
      );
    expect(authentication.machine_state).toStrictEqual(
      SDK.validationMachineStateDataFromCore(trace.states[stateIndex]!),
    );
    expect(authentication.trace_proof.state_hash).toBe(
      trace.tree.proofs[stateIndex]!.stateHash.toString("hex"),
    );
    expect(authentication.script_hash).toBe(policyId.toString("hex"));
    await expect(
      prepareExecutionSourceScriptDecodingArtifact(block),
    ).rejects.toThrow(
      "executionSourceScriptDecoding retained replay yielded no contradiction",
    );
  });
  it("retains a malformed-inline Phase A trace without inventing an execution descriptor", async () => {
    const replay = await malformedInlinePhaseAReplay(
      Buffer.from("820043820700", "hex"),
    );
    expect(
      replay.canonical.trace.witnesses.some(
        ({ auxiliary }) => auxiliary?.kind === "nativeScriptToken",
      ),
    ).toBe(true);
  });
});
