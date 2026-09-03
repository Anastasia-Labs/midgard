import {
  buildMidgardValidationTraceTree,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  MidgardRedeemerTag,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "../../midgard-validation/tests/validation-fixtures.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence-v1.js";
import {
  prepareReceivePurposeLanguageEvidence,
  receivePurposeLanguageEvidenceCloses,
} from "../src/receive-purpose-language/family-v1.js";
import {
  detectReceivePurposeLanguageCanonicalViolations,
  prepareReceivePurposeLanguageArtifact,
} from "../src/receive-purpose-language/production-replay-v1.js";
import { buildReceivePurposeLanguageAuthenticationFromRetainedDa } from "../src/receive-purpose-language/retained-witness-v1.js";
import { receivePurposeLanguageDescriptorFromAuthentication } from "../src/receive-purpose-language/retained-witness-v1.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";

describe("receivePurposeLanguage retained DA", () => {
  it("reconstructs the forbidden receive purpose and PlutusV3 language without callback evidence", async () => {
    const spent = outRefFromByte(0x73);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const script = plutusV3ScriptWitness(
      Buffer.from(
        "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
        "hex",
      ),
    );
    const output = makeProtectedScriptOutput(
      hashScriptWitness(script),
      FUNDED_OUTPUT_LOVELACE,
    );
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Receiving,
          index: 0n,
          exUnits: [1_000_000n, 1_000_000n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    const mutations = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: [
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ],
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
        programMaterialSidecarCbor: Buffer.from(
          "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
          "hex",
        ),
        priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
        postUtxosRoot: mutations[0]!.preRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: [],
        ledgerMutationSteps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: "E_PLUTUS_SCRIPT_INVALID",
      }),
    );
    const stateIndex = trace.witnesses.findIndex(
      ({ phase, auxiliary }) =>
        phase === "nativeScripts" &&
        auxiliary?.kind === "nativeExecutionDescriptor" &&
        auxiliary.purpose.purposeKind === 3 &&
        auxiliary.languageTag === 3,
    );
    expect(stateIndex).toBeGreaterThanOrEqual(0);
    const witness = trace.witnesses[stateIndex]!;
    if (witness.auxiliary?.kind !== "nativeExecutionDescriptor")
      throw new Error("fixture omitted receive execution descriptor");
    const claimedAcceptedTree = buildMidgardValidationTraceTree(
      trace.states.map(hashMidgardValidationMachineState),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    );
    const descriptor: SDK.ValidationTraceDescriptor = {
      schema_version: BigInt(claimedAcceptedTree.descriptor.schemaVersion),
      machine_version: BigInt(claimedAcceptedTree.descriptor.machineVersion),
      trace_root: claimedAcceptedTree.descriptor.traceRoot.toString("hex"),
      step_count: BigInt(claimedAcceptedTree.descriptor.stepCount),
      initial_state_hash:
        claimedAcceptedTree.descriptor.initialStateHash.toString("hex"),
      terminal_state_hash:
        claimedAcceptedTree.descriptor.terminalStateHash.toString("hex"),
      verdict: "Accepted",
      rejection_code_hash:
        claimedAcceptedTree.descriptor.rejectionCodeHash.toString("hex"),
    };
    const eventKeyCbor = Buffer.from(
      Data.to(eventKey as never, SDK.EventKeySchema),
      "hex",
    );
    const descriptorCbor = Buffer.from(
      Data.to(descriptor as never, SDK.ValidationTraceDescriptorSchema),
      "hex",
    );
    const root = await buildCountedRoot(SDK.ROOT_DOMAINS.validationTraces, [
      { key: eventKeyCbor, value: descriptorCbor },
    ]);
    const auxiliary = Data.from(
      Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
      SDK.ValidationAuxiliaryWitnessSchema,
    ) as unknown as SDK.ValidationAuxiliaryWitness;
    const retainedValue: SDK.RetainedValidationWitness = {
      machine_state: SDK.validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      ),
      trace_proof: SDK.validationTraceProofDataFromCore(
        claimedAcceptedTree.proofs[stateIndex]!,
      ),
      phase: 9n,
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary,
    };
    const retainedKey: SDK.RetainedValidationWitnessKey = {
      event_key: eventKey,
      execution_index: BigInt(witness.auxiliary.executionIndex),
    };
    const input = {
      eventKey,
      executionIndex: Number(witness.auxiliary.executionIndex),
      authenticatedValidationTraceEntries: [
        { key: eventKeyCbor, value: descriptorCbor },
      ],
      retainedValidationWitnessEntries: [
        {
          key: SDK.encodeRetainedValidationWitnessKey(retainedKey),
          value: SDK.encodeRetainedValidationWitness(retainedValue),
        },
      ],
      expectedValidationTracesRoot: root.root,
      expectedLanguageTag: 3 as const,
    };
    const rebuilt =
      await buildReceivePurposeLanguageAuthenticationFromRetainedDa(input);
    expect(rebuilt.authentication).toMatchObject({
      purpose_index: 0n,
      language_tag: 3n,
    });
    const evidence = prepareReceivePurposeLanguageEvidence({
      finding: {
        subject: SDK.acceptedVerdictSubject(transaction.txId.toString("hex")),
        executionIndex: 0,
      },
      descriptor: receivePurposeLanguageDescriptorFromAuthentication(
        rebuilt.authentication,
        0,
      ),
    });
    expect(receivePurposeLanguageEvidenceCloses(evidence)).toBe(true);
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      transaction.txCbor,
    );
    const l2Source = Data.to(
      {
        tx_id: transaction.txId.toString("hex"),
        source: {
          compact_cbor: material.proofSource.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            material.proofSource.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
        },
      } as never,
      SDK.L2TransactionSourceSchema,
    );
    const encodedRetainedKey =
      SDK.encodeRetainedValidationWitnessKey(retainedKey);
    const encodedRetainedValue =
      SDK.encodeRetainedValidationWitness(retainedValue);
    const block = {
      headerHash: "ab".repeat(28),
      header: { validationTracesRoot: root.root },
      reconstruction: {
        forcedTransactions: [],
        payload: {
          block_body: {
            validation_traces: [
              [eventKeyCbor.toString("hex"), descriptorCbor.toString("hex")],
            ],
            validation_trace_witnesses: [
              [
                encodedRetainedKey.toString("hex"),
                encodedRetainedValue.toString("hex"),
              ],
            ],
          },
        },
      },
      transactions: [
        {
          nodeTxId: transaction.txId.toString("hex"),
          txCbor: transaction.txCbor.toString("hex"),
          l2TransactionSourceCbor: l2Source,
        },
      ],
    } as unknown as CanonicalBlockEvidence;
    expect(
      await detectReceivePurposeLanguageCanonicalViolations(block),
    ).toMatchObject([
      { violationId: "receive-purpose-plutus-v3-forbidden", position: 0n },
    ]);
    expect(
      (await prepareReceivePurposeLanguageArtifact(block)).authentication
        .language_tag,
    ).toBe(3n);
    await expect(
      buildReceivePurposeLanguageAuthenticationFromRetainedDa({
        ...input,
        expectedLanguageTag: 128,
      }),
    ).rejects.toThrow(/different execution source/u);
    if (
      typeof retainedValue.auxiliary !== "object" ||
      !("NativeExecutionDescriptorWitness" in retainedValue.auxiliary)
    )
      throw new Error("fixture retained wrong auxiliary kind");
    await expect(
      buildReceivePurposeLanguageAuthenticationFromRetainedDa({
        ...input,
        retainedValidationWitnessEntries: [
          {
            key: SDK.encodeRetainedValidationWitnessKey(retainedKey),
            value: SDK.encodeRetainedValidationWitness({
              ...retainedValue,
              auxiliary: {
                NativeExecutionDescriptorWitness: {
                  ...retainedValue.auxiliary.NativeExecutionDescriptorWitness,
                  subject: "ff",
                },
              },
            }),
          },
        ],
      }),
    ).rejects.toThrow(/membership is invalid/u);
    expect(encodeCbor([3n]).length).toBeGreaterThan(0);
  }, 60_000);
});
