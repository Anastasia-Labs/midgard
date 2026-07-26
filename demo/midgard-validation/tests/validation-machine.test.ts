import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1,
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_VALIDATION_DISPUTE_V1_VERSION,
  verifyMidgardValidationTraceProofV1,
} from "@al-ft/midgard-core";
import { protectMidgardAddress } from "@al-ft/midgard-core/codec";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { parseExactAikenDataCbor } from "../../midgard-fault-proofs/src/aiken-blueprint-data.js";
import { encodeValidationSemanticResolutionRedeemerV1 } from "../../midgard-fault-proofs/src/validation-dispute/submit.js";
import {
  advanceMidgardResolvedInputsAccumulatorV1,
  buildDeterministicValidationMachineTrace,
  buildMidgardCanonicalCekProgramV1,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgumentV1,
  type DeterministicValidationMachineTrace,
  encodeValidationBoundaryEvidenceCborV1,
  initialMidgardResolvedInputsAccumulatorV1,
  MidgardRedeemerTag,
  RejectCodes,
  validationSemanticResolverIndexV1,
} from "../src/index.js";
import {
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
  TEST_ADDRESS_BYTES,
  TEST_SIGNER_HASH,
} from "./validation-fixtures.js";

const root = (byte: number): string => Buffer.alloc(32, byte).toString("hex");
const validationDisputeBlueprint = JSON.parse(
  readFileSync(
    resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
    "utf8",
  ),
) as unknown;
const semanticResolverDefinitionsV1 = [
  "canonical_decode_empty_semantic_v1",
  "canonical_decode_chunk_semantic_v1",
  "compact_binding_semantic_v1",
  "static_ledger_rules_semantic_v1",
  "input_sets_empty_semantic_v1",
  "input_sets_item_semantic_v1",
  "signatures_advance_semantic_v1",
  "signatures_address_item_semantic_v1",
  "signatures_required_item_semantic_v1",
  "signatures_handoff_semantic_v1",
  "phase_a_native_scripts_advance_semantic_v1",
  "phase_a_native_scripts_item_semantic_v1",
  "phase_a_native_scripts_token_head_semantic_v1",
  "phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1",
  "phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1",
  "phase_a_native_scripts_at_least_container_frame_payload_semantic_v1",
  "phase_a_native_scripts_at_least_empty_container_payload_semantic_v1",
  "phase_a_native_scripts_timelock_payload_semantic_v1",
  "phase_a_native_scripts_signature_membership_payload_semantic_v1",
  "phase_a_native_scripts_signature_empty_payload_semantic_v1",
  "phase_a_native_scripts_signature_below_first_payload_semantic_v1",
  "phase_a_native_scripts_signature_above_last_payload_semantic_v1",
  "phase_a_native_scripts_signature_between_payload_semantic_v1",
  "phase_a_native_scripts_frame_semantic_v1",
  "phase_a_script_preconditions_semantic_v1",
  "phase_a_script_preconditions_item_semantic_v1",
  "resolve_inputs_initial_semantic_v1",
  "resolve_inputs_finish_semantic_v1",
  "resolve_inputs_membership_begin_semantic_v1",
  "resolve_inputs_membership_step_semantic_v1",
  "resolve_inputs_membership_finalize_semantic_v1",
  "resolve_inputs_non_membership_semantic_v1",
  "script_sources_non_output_semantic_v1",
  "script_sources_output_proof_begin_semantic_v1",
  "script_sources_output_proof_step_semantic_v1",
  "script_sources_output_proof_finalize_semantic_v1",
  "script_sources_output_proof_finish_semantic_v1",
  "script_sources_stage_zero_begin_semantic_v1",
  "script_sources_stage_zero_finish_semantic_v1",
  "script_sources_stage_zero_hash_block_semantic_v1",
  "script_sources_stage_zero_hash_advance_semantic_v1",
  "script_sources_stage_zero_hash_terminal_semantic_v1",
  "script_sources_stage_nine_mismatch_semantic_v1",
  "script_sources_stage_nine_native_match_semantic_v1",
  "script_sources_stage_nine_effectful_match_semantic_v1",
  "script_sources_stage_nine_missing_semantic_v1",
  "native_scripts_terminal_semantic_v1",
  "native_scripts_native_semantic_v1",
  "native_scripts_effectful_semantic_v1",
  "script_integrity_authentication_semantic_v1",
  "script_integrity_compact_semantic_v1",
  "script_integrity_witness_set_semantic_v1",
  "script_integrity_finalize_semantic_v1",
  "ledger_delta_operation_semantic_v1",
  "ledger_delta_replay_semantic_v1",
  "ledger_delta_replay_finish_semantic_v1",
  "ledger_delta_output_semantic_v1",
  "ledger_delta_output_finish_semantic_v1",
  "ledger_delta_proof_frame_semantic_v1",
  "ledger_delta_finalize_semantic_v1",
  "ledger_delta_terminal_semantic_v1",
] as const;
const semanticResolverOffsetsV1 = [
  0, 2, 3, 4, 6, 10, 24, 26, 32, 46, 49, -1, -1, 53,
] as const;

const validateBoundaryAbiAndCollectAuxiliaryKinds = (
  trace: DeterministicValidationMachineTrace,
): {
  readonly kinds: ReadonlySet<string>;
  readonly maxArgumentsBytes: number;
} => {
  const validated = new Set<string>();
  let maxArgumentsBytes = 0;
  for (let lowIndex = 0; lowIndex < trace.states.length - 1; lowIndex += 1) {
    const auxiliaryKind = trace.witnesses[lowIndex]!.auxiliary?.kind ?? "none";
    const highIndex = lowIndex + 1;
    const challengerStates = trace.states.map((state, index) => {
      if (index !== highIndex && index !== trace.states.length - 1) {
        return state;
      }
      const workRoot = Buffer.from(state.workRoot);
      workRoot[0] = workRoot[0]! ^ 0x01;
      return { ...state, workRoot };
    });
    const challengerTree = buildMidgardValidationTraceTree(
      challengerStates.map(hashMidgardValidationMachineStateV1),
      trace.verdict,
      trace.tree.descriptor.rejectionCodeHash,
    );
    const argumentsCbor = encodeValidationBoundaryEvidenceCborV1({
      dispute: {
        version: MIDGARD_VALIDATION_DISPUTE_V1_VERSION,
        operatorDescriptor: trace.tree.descriptor,
        challengerDescriptor: challengerTree.descriptor,
        lowIndex,
        highIndex,
        agreedLowHash: hashMidgardValidationMachineStateV1(
          trace.states[lowIndex]!,
        ),
        operatorHighHash: trace.tree.proofs[highIndex]!.stateHash,
        challengerHighHash: challengerTree.proofs[highIndex]!.stateHash,
        round: 1,
        responseDeadline: 1_800_000_000_000,
        turn: { type: "readyForOneStep" },
      },
      operatorTrace: trace,
      challengerTrace: {
        ...trace,
        states: challengerStates,
        tree: challengerTree,
      },
    });
    parseExactAikenDataCbor({
      blueprint: validationDisputeBlueprint,
      definitionName:
        "midgard/validation_resolution_v1/ValidationBoundaryEvidenceV1",
      cbor: argumentsCbor.toString("hex"),
      maxBytes: 16 * 1024 - 1,
    });
    const oneStepArgument = buildValidationOneStepArgumentV1({
      trace,
      stateIndex: lowIndex,
    });
    for (const [definitionName, cbor] of [
      [
        "midgard/validation_machine_v1/ValidationOneStepWitnessV1",
        oneStepArgument.transitionCbor,
      ],
      [
        "midgard/validation_machine_v1/ValidationAuxiliaryWitnessV1",
        oneStepArgument.auxiliaryCbor,
      ],
      [
        "midgard/validation_machine_v1/ValidationOneStepEvidenceV1",
        oneStepArgument.evidenceCbor,
      ],
    ] as const) {
      parseExactAikenDataCbor({
        blueprint: validationDisputeBlueprint,
        definitionName,
        cbor: cbor.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      });
      maxArgumentsBytes = Math.max(maxArgumentsBytes, cbor.length);
    }
    if (oneStepArgument.semanticResolverIndex !== null) {
      const globalIndex =
        semanticResolverOffsetsV1[oneStepArgument.resolverIndex]! +
        oneStepArgument.semanticResolverIndex;
      const moduleName = semanticResolverDefinitionsV1[globalIndex];
      if (moduleName === undefined) {
        throw new Error(
          `semantic resolver ${globalIndex.toString()} has no ABI definition`,
        );
      }
      const semanticRedeemer = encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument,
        inputIndex: 0n,
        outputIndex: 0n,
      });
      parseExactAikenDataCbor({
        blueprint: validationDisputeBlueprint,
        definitionName: `fraud_proofs/validation_trace/${moduleName}/SpendRedeemer`,
        cbor: semanticRedeemer.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      });
      maxArgumentsBytes = Math.max(maxArgumentsBytes, semanticRedeemer.length);
    }
    maxArgumentsBytes = Math.max(maxArgumentsBytes, argumentsCbor.length);
    validated.add(auxiliaryKind);
  }
  return { kinds: validated, maxArgumentsBytes };
};

const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
  ledgerMutationSteps: [],
};

const buildAcceptingIdentityProgram = () =>
  buildMidgardCanonicalCekProgramV1(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
      ).toBuffer().buffer,
    ),
  );

describe("deterministic validation machine", () => {
  it("replays an accepted transaction through bounded field-reveal instructions", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    expect(trace.states.map((state) => state.phase)).toEqual([
      ...Array<string>(9).fill("canonicalDecode"),
      "compactBinding",
      "staticLedgerRules",
      "inputSets",
      "signatures",
      "signatures",
      "signatures",
      "phaseANativeScripts",
      "phaseAScriptPreconditions",
      ...Array<string>(11).fill("resolveInputs"),
      ...Array<string>(22).fill("scriptSources"),
      "nativeScripts",
      ...Array<string>(4).fill("scriptIntegrity"),
      "cek",
      ...Array<string>(8).fill("valueAndMint"),
      ...Array<string>(9).fill("ledgerDelta"),
      "terminal",
    ]);
    const canonicalWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "canonicalDecode",
    );
    expect(canonicalWitnesses).toHaveLength(9);
    expect(
      canonicalWitnesses.every((witness) => {
        if (witness.auxiliary === null) return witness.cbor.length < 16 * 1024;
        return (
          witness.auxiliary.kind === "transactionFieldChunk" &&
          witness.auxiliary.chunkProof.chunk.length <= 4_095 &&
          witness.cbor.length + witness.auxiliary.chunkProof.chunk.length <
            16 * 1024
        );
      }),
    ).toBe(true);
    const scriptSourceWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "scriptSources",
    );
    expect(scriptSourceWitnesses).toHaveLength(22);
    expect(scriptSourceWitnesses[0]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[1]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[2]?.auxiliary?.kind).toBe(
      "transactionFieldPairPreimage",
    );
    expect(scriptSourceWitnesses[3]?.auxiliary?.kind).toBe(
      "resolvedInputReplay",
    );
    expect(scriptSourceWitnesses[4]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[5]?.auxiliary?.kind).toBe(
      "transactionFieldItem",
    );
    expect(scriptSourceWitnesses[6]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[7]?.auxiliary?.kind).toBe(
      "ledgerOutputProofBegin",
    );
    expect(
      scriptSourceWitnesses
        .slice(8, 15)
        .every(
          (witness) =>
            witness.auxiliary?.kind === "ledgerOutputProofStep",
        ),
    ).toBe(true);
    expect(scriptSourceWitnesses[15]?.auxiliary?.kind).toBe(
      "ledgerOutputProofFinalize",
    );
    expect(scriptSourceWitnesses[16]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[17]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[18]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[19]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[20]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[21]?.auxiliary).toBeNull();
    expect(
      scriptSourceWitnesses.map(validationSemanticResolverIndexV1),
    ).toEqual([
      6, 0, 0, 0, 0, 0, 0,
      1,
      2, 2, 2, 2, 2, 2, 2,
      3,
      4,
      0, 0, 0, 0, 0,
    ]);
    expect(() =>
      validationSemanticResolverIndexV1({
        ...scriptSourceWitnesses[7]!,
        auxiliary: scriptSourceWitnesses[2]!.auxiliary,
      }),
    ).toThrow("has no semantic resolver");
    expect(
      canonicalWitnesses.every((witness) => {
        if (witness.cbor.includes(transaction.txCbor)) return false;
        return (
          witness.auxiliary === null ||
          (witness.auxiliary.kind === "transactionFieldChunk" &&
            !witness.auxiliary.chunkProof.chunk.includes(transaction.txCbor))
        );
      }),
    ).toBe(true);
    const compactBindingWitness = trace.witnesses.find(
      (witness) => witness.phase === "compactBinding",
    );
    expect(compactBindingWitness).toBeDefined();
    expect(compactBindingWitness!.cbor.includes(transaction.txCbor)).toBe(
      false,
    );
    const staticRulesWitness = trace.witnesses.find(
      (witness) => witness.phase === "staticLedgerRules",
    );
    expect(staticRulesWitness).toBeDefined();
    expect(staticRulesWitness!.cbor.includes(transaction.txCbor)).toBe(false);
    expect(trace.tree.descriptor.verdict).toBe("accepted");
    expect(trace.states[0]!.transactionCommitment).toEqual(
      computeMidgardNativeTxProofCommitmentV1(
        deriveMidgardNativeTxProofSourceV1(transaction.tx),
      ),
    );
    expect(
      trace.tree.proofs.every((proof) =>
        verifyMidgardValidationTraceProofV1({
          descriptor: trace.tree.descriptor,
          proof,
        }),
      ),
    ).toBe(true);
    const oneStepAbi = validateBoundaryAbiAndCollectAuxiliaryKinds(trace);
    expect(oneStepAbi.kinds.size).toBeGreaterThanOrEqual(8);
    expect(oneStepAbi.maxArgumentsBytes).toBeLessThan(16 * 1024);
  });

  it("matches the L1 resolved-input accumulator vector", () => {
    const initial = initialMidgardResolvedInputsAccumulatorV1();
    expect(initial.toString("hex")).toBe(
      "07eb401e2f7e5de17444414ec48a5d9dca455dea72f4675cc2b08bf5b4e39979",
    );
    expect(
      advanceMidgardResolvedInputsAccumulatorV1({
        accumulator: initial,
        sourceKind: "spend",
        key: Buffer.from("010203", "hex"),
        value: Buffer.from("040506", "hex"),
      }).toString("hex"),
    ).toBe("97e2dbdabf1ac8b5046e02f46c8d081ade2d81296b174bf77b9b8c69bd59c9c0");
  });

  it("emits the exact incremental context and CEK trace for PlutusV3", async () => {
    const spent = outRefFromByte(0x1d);
    const program = buildAcceptingIdentityProgram();
    const script = plutusV3ScriptWitness(program.envelopeCbor);
    const scriptHash = hashScriptWitness(script);
    const spentOutput = makeProtectedScriptOutput(scriptHash, 10n);
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [1_000_000_000n, 1_000_000_000n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output: spentOutput }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
          ...program.material.values(),
        ]),
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    const cekWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "cek",
    );
    const scriptSourceWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "scriptSources",
    );
    expect(scriptSourceWitnesses[0]?.auxiliary).toMatchObject({
      kind: "transactionFieldChunk",
      collectionProof: {
        fieldIndex: 7,
        itemCount: 1,
        itemIndex: 0,
      },
      chunkProof: {
        fieldIndex: 7,
        itemIndex: 0,
        chunkIndex: 0,
      },
    });
    const sourceHashBlocks = scriptSourceWitnesses.filter(
      (witness) =>
        witness.auxiliary?.kind === "scriptSourceHashBlock",
    );
    expect(sourceHashBlocks).toHaveLength(1);
    expect(sourceHashBlocks[0]?.auxiliary).toMatchObject({
      kind: "scriptSourceHashBlock",
      chunkProof: {
        fieldIndex: 7,
        itemIndex: 0,
        chunkIndex: 0,
      },
      nextChunkProof: null,
    });
    const redeemerSourceWitness = scriptSourceWitnesses.find(
      (witness) =>
        witness.auxiliary?.kind === "transactionRedeemerItem",
    );
    expect(redeemerSourceWitness?.auxiliary).toMatchObject({
      kind: "transactionRedeemerItem",
      collectionProof: {
        fieldIndex: 8,
        itemCount: 1,
        itemIndex: 0,
      },
    });
    expect(cekWitnesses.map((witness) => witness.auxiliary?.kind)).toEqual(
      expect.arrayContaining([
        "nativeExecutionScan",
        "redeemerScan",
        "cekResolvedContextItem",
        "cekOutputContextItem",
        "cekSignerContextItem",
        "cekRedeemerContextSelect",
        "cekDataScanStep",
        "cekContextFinalizeSpend",
        "cekContextAssemble",
        "cekTxInfoFinalize",
        "cekContextSeed",
        "cekCoreStep",
      ]),
    );
    const cekStates = trace.states.filter((state) => state.phase === "cek");
    expect(cekStates.at(-1)!.executionCpu).toBeGreaterThan(0n);
    expect(cekStates.at(-1)!.executionMemory).toBeGreaterThan(0n);
    expect(trace.verdict).toBe("accepted");
    const oneStepAbi = validateBoundaryAbiAndCollectAuxiliaryKinds(trace);
    expect(oneStepAbi.kinds.size).toBeGreaterThan(15);
    expect(oneStepAbi.maxArgumentsBytes).toBeLessThan(16 * 1024);
  });

  it("executes an authenticated PlutusV3 reference script from a reference input", async () => {
    const spent = outRefFromByte(0x1e);
    const reference = outRefFromByte(0x1f);
    const program = buildAcceptingIdentityProgram();
    const script = plutusV3ScriptWitness(program.envelopeCbor);
    const scriptHash = hashScriptWitness(script);
    const spentOutput = makeProtectedScriptOutput(scriptHash, 10n);
    const referenceOutput = encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: 1n, assets: new Map() },
      script_ref: script,
    });
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      referenceInputs: [reference],
      outputs: [output],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [1_000_000_000n, 1_000_000_000n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [
          { outRef: spent, output: spentOutput },
          { outRef: reference, output: referenceOutput },
        ],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
          ...program.material.values(),
        ]),
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [
          { outRef: spent, output: spentOutput },
          { outRef: reference, output: referenceOutput },
        ],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    const referenceSource = trace.witnesses.find(
      (witness) =>
        witness.auxiliary?.kind === "scriptSourceScan" &&
        witness.auxiliary.originKind === "reference",
    );
    expect(referenceSource?.auxiliary).toMatchObject({
      kind: "scriptSourceScan",
      originKind: "reference",
      scriptLanguageTag: 3,
      scriptHash: Buffer.from(scriptHash, "hex"),
    });
    if (referenceSource?.auxiliary?.kind !== "scriptSourceScan") {
      throw new Error("expected a compact reference-script source witness");
    }
    expect(referenceSource.auxiliary.scriptTotalLength).toBeGreaterThan(0);
    expect(referenceSource.auxiliary.scriptItemCommitment).toHaveLength(32);
    expect("script" in referenceSource.auxiliary).toBe(false);
    expect(validationSemanticResolverIndexV1(referenceSource)).toBe(12);
    expect(
      trace.witnesses
        .filter((witness) => witness.phase === "inputSets")
        .map((witness) =>
          witness.auxiliary?.kind === "transactionFieldChunk"
            ? witness.auxiliary.collectionProof.fieldIndex
            : null,
        ),
    ).toEqual([1, 0]);
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "cekResolvedContextItem" &&
          witness.auxiliary.sourceKind === "reference",
      ),
    ).toBe(true);
    expect(
      trace.witnesses.some(
        (witness) => witness.auxiliary?.kind === "cekCoreStep",
      ),
    ).toBe(true);
    expect(trace.verdict).toBe("accepted");
    expect([
      ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
    ]).toEqual(
      expect.arrayContaining([
        "scriptSourceScan",
        "cekResolvedContextItem",
        "cekCoreStep",
      ]),
    );
  });

  it.each([
    { operation: "mint", quantity: 5n },
    { operation: "burn", quantity: -5n },
  ])(
    "executes scripted $operation through the exact mint context and CEK trace",
    async ({ quantity }) => {
      const spent = outRefFromByte(quantity > 0n ? 0x31 : 0x32);
      const program = buildAcceptingIdentityProgram();
      const script = plutusV3ScriptWitness(program.envelopeCbor);
      const policyId = Buffer.from(hashScriptWitness(script), "hex");
      const assetName = Buffer.from("aced", "hex");
      const assets = new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 5n]])],
      ]);
      const spentOutput =
        quantity > 0n ? makeOutput(10n) : makeOutput(10n, undefined, assets);
      const output =
        quantity > 0n ? makeOutput(10n, undefined, assets) : makeOutput(10n);
      const transaction = makeNativeTx({
        version: 1n,
        spendInputs: [spent],
        outputs: [output],
        scriptWitnesses: [script],
        mintPreimageCbor: encodeCbor(
          new Map([[policyId, new Map([[assetName, quantity]])]]),
        ),
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          {
            tag: MidgardRedeemerTag.Mint,
            index: 0n,
            exUnits: [1_000_000_000n, 1_000_000_000n],
          },
        ]),
        scriptLanguages: ["PlutusV3"],
      });
      const expectedLedgerOps = [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOpV1({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ];
      const ledgerMutationSteps =
        await buildValidationMachineLedgerMutationSteps({
          initialEntries: [{ outRef: spent, output: spentOutput }],
          operations: expectedLedgerOps,
        });
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...context,
          transactionId: transaction.txId,
          canonicalTransactionCbor: transaction.txCbor,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
            ...program.material.values(),
          ]),
          priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
          postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
          ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
          expectedLedgerOps,
          ledgerMutationSteps,
          expectedVerdict: "accepted",
          expectedRejectionCode: null,
        }),
      );

      expect(
        trace.witnesses.find(
          (witness) => witness.auxiliary?.kind === "cekMintContextItem",
        )?.auxiliary,
      ).toMatchObject({
        kind: "cekMintContextItem",
        quantity,
      });
      expect(
        trace.witnesses.some(
          (witness) => witness.auxiliary?.kind === "cekCoreStep",
        ),
      ).toBe(true);
      expect(trace.verdict).toBe("accepted");
      expect([
        ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
      ]).toEqual(
        expect.arrayContaining([
          "cekMintContextItem",
          "valueMintAsset",
          "ledgerDeltaReplay",
          "ledgerDeltaOutput",
        ]),
      );
    },
  );

  it("executes a MidgardV1 protected-output receiving script", async () => {
    const spent = outRefFromByte(0x33);
    const spentOutput = makeOutput(10n);
    const program = buildAcceptingIdentityProgram();
    const script = {
      language: "MidgardV1" as const,
      scriptBytes: program.envelopeCbor,
    };
    const scriptHash = hashScriptWitness(script);
    const output = makeProtectedScriptOutput(scriptHash, 10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Receiving,
          index: 0n,
          exUnits: [1_000_000_000n, 1_000_000_000n],
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output: spentOutput }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
          ...program.material.values(),
        ]),
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    expect(
      trace.witnesses.find(
        (witness) =>
          witness.auxiliary?.kind === "nativeExecutionScan" &&
          witness.auxiliary.purpose.purposeKind === 3,
      )?.auxiliary,
    ).toMatchObject({
      kind: "nativeExecutionScan",
      languageTag: 128,
      purpose: { purposeKind: 3 },
    });
    expect(
      trace.witnesses.some(
        (witness) => witness.auxiliary?.kind === "cekContextFinalize",
      ),
    ).toBe(true);
    expect(trace.verdict).toBe("accepted");
    expect([
      ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
    ]).toEqual(
      expect.arrayContaining([
        "nativeExecutionScan",
        "cekOutputContextItem",
        "cekContextFinalize",
        "ledgerDeltaOutput",
      ]),
    );
  });

  it("executes an authenticated PlutusV3 observer", async () => {
    const spent = outRefFromByte(0x34);
    const spentOutput = makeOutput(10n);
    const output = makeOutput(10n);
    const program = buildAcceptingIdentityProgram();
    const script = plutusV3ScriptWitness(program.envelopeCbor);
    const observerHash = Buffer.from(hashScriptWitness(script), "hex");
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      requiredObserverItems: [observerHash],
      networkId: 0n,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Reward,
          index: 0n,
          exUnits: [1_000_000_000n, 1_000_000_000n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output: spentOutput }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
          ...program.material.values(),
        ]),
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    expect(
      trace.witnesses.find(
        (witness) =>
          witness.auxiliary?.kind === "nativeExecutionScan" &&
          witness.auxiliary.purpose.purposeKind === 2,
      )?.auxiliary,
    ).toMatchObject({
      kind: "nativeExecutionScan",
      languageTag: 3,
      purpose: { purposeKind: 2 },
    });
    expect(
      trace.witnesses.some(
        (witness) => witness.auxiliary?.kind === "cekCoreStep",
      ),
    ).toBe(true);
    const preconditionWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "phaseAScriptPreconditions",
    );
    expect(
      preconditionWitnesses.map((witness) => witness.auxiliary?.kind ?? "none"),
    ).toEqual(["transactionFieldChunk", "none"]);
    expect(
      preconditionWitnesses.map(validationSemanticResolverIndexV1),
    ).toEqual([1, 0]);
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.phase === "scriptSources" &&
          witness.auxiliary?.kind === "transactionFieldChunk" &&
          witness.auxiliary.collectionProof.fieldIndex === 3,
      ),
    ).toBe(true);
    expect(trace.verdict).toBe("accepted");
    expect([
      ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
    ]).toEqual(
      expect.arrayContaining([
        "nativeExecutionScan",
        "cekRedeemerContextSelect",
        "cekCoreStep",
      ]),
    );
  });

  it("proves duplicate observers at the second authenticated item", async () => {
    const spent = outRefFromByte(0x35);
    const observerHash = Buffer.alloc(28, 0x71);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [makeOutput(10n)],
      requiredObserverItems: [observerHash, observerHash],
    });
    const unchangedRoot = root(0x35);
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: unchangedRoot,
        postUtxosRoot: unchangedRoot,
        ledgerWitnessEntries: [],
        expectedLedgerOps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.InvalidFieldType,
      }),
    );

    const preconditionWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "phaseAScriptPreconditions",
    );
    expect(preconditionWitnesses).toHaveLength(2);
    expect(
      preconditionWitnesses.map((witness) => witness.auxiliary?.kind),
    ).toEqual(["transactionFieldChunk", "transactionFieldChunk"]);
    expect(
      preconditionWitnesses.map(validationSemanticResolverIndexV1),
    ).toEqual([1, 1]);
    expect(trace.states.at(-1)).toMatchObject({
      phase: "terminal",
      verdict: "rejected",
    });
    expect(validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds).toContain(
      "transactionFieldChunk",
    );
  });

  it("replays signed mint through an authenticated mint leaf", async () => {
    const spent = outRefFromByte(0x21);
    const spentOutput = makeOutput(10n);
    const script = nativeScriptWitness({
      type: "all",
      scripts: [
        {
          type: "sig",
          keyHash: Buffer.from(TEST_SIGNER_HASH, "hex"),
        },
      ],
    });
    const policyId = Buffer.from(hashScriptWitness(script), "hex");
    const assetName = Buffer.from("cafe", "hex");
    const mintedOutput = makeOutput(
      10n,
      undefined,
      new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 5n]])],
      ]),
    );
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [mintedOutput],
      scriptWitnesses: [script],
      mintPreimageCbor: encodeCbor(
        new Map([[policyId, new Map([[assetName, 5n]])]]),
      ),
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: mintedOutput,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output: spentOutput }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    const mintFoldWitnesses = trace.witnesses
      .filter((witness) => witness.phase === "scriptSources")
      .flatMap((witness) => {
        const auxiliary = witness.auxiliary;
        if (
          auxiliary?.kind === "transactionFieldChunk" &&
          auxiliary.collectionProof.fieldIndex === 5
        ) {
          return [auxiliary];
        }
        return auxiliary?.kind === "mintFoldAsset" ? [auxiliary] : [];
      });
    expect(mintFoldWitnesses.map(({ kind }) => kind)).toEqual([
      "transactionFieldChunk",
      "mintFoldAsset",
    ]);
    expect(
      mintFoldWitnesses.every((witness) => {
        if (witness.kind === "transactionFieldChunk") {
          return witness.chunkProof.chunk.length <= 4_095;
        }
        return (
          witness.chunkProof.chunk.length <= 4_095 &&
          (witness.nextChunkProof?.chunk.length ?? 0) <= 4_095
        );
      }),
    ).toBe(true);
    expect(
      trace.witnesses.filter(
        (witness) => witness.auxiliary?.kind === "valueMintAsset",
      ),
    ).toHaveLength(1);
    expect(
      trace.witnesses
        .filter((witness) => witness.phase === "phaseANativeScripts")
        .map((witness) => witness.auxiliary?.kind ?? null),
    ).toEqual([
      "transactionFieldChunk",
      "nativeScriptToken",
      "nativeScriptToken",
      "nativeScriptToken",
      "nativeScriptToken",
      "nativeScriptFrame",
      null,
      "nativeScriptToken",
      "nativeScriptToken",
      "nativeScriptToken",
      "nativeScriptToken",
      "nativeScriptFrame",
      null,
    ]);
    expect(
      trace.witnesses
        .filter((witness) => witness.phase === "phaseANativeScripts")
        .map(validationSemanticResolverIndexV1),
    ).toEqual([1, 2, 3, 2, 8, 13, 0, 2, 3, 2, 8, 13, 0]);
    const nativeSource = trace.witnesses.find(
      (witness) =>
        witness.auxiliary?.kind === "scriptSourceScan" &&
        witness.auxiliary.scriptLanguageTag === 0,
    );
    expect(nativeSource).toBeDefined();
    expect(validationSemanticResolverIndexV1(nativeSource!)).toBe(11);
    expect(trace.verdict).toBe("accepted");
    expect([
      ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
    ]).toEqual(
      expect.arrayContaining([
        "valueOutputAsset",
        "valueMintAsset",
        "ledgerDeltaReplay",
        "ledgerDeltaOutput",
      ]),
    );
  });

  it("replays signed burn through the same authenticated mint leaf path", async () => {
    const spent = outRefFromByte(0x22);
    const script = nativeScriptWitness({ type: "all", scripts: [] });
    const policyId = Buffer.from(hashScriptWitness(script), "hex");
    const assetName = Buffer.from("beef", "hex");
    const spentOutput = makeOutput(
      10n,
      undefined,
      new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 5n]])],
      ]),
    );
    const burnedOutput = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [burnedOutput],
      scriptWitnesses: [script],
      mintPreimageCbor: encodeCbor(
        new Map([[policyId, new Map([[assetName, -5n]])]]),
      ),
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: burnedOutput,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output: spentOutput }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    const burnFoldWitnesses = trace.witnesses
      .filter((witness) => witness.phase === "scriptSources")
      .flatMap((witness) => {
        const auxiliary = witness.auxiliary;
        if (
          auxiliary?.kind === "transactionFieldChunk" &&
          auxiliary.collectionProof.fieldIndex === 5
        ) {
          return [auxiliary];
        }
        return auxiliary?.kind === "mintFoldAsset" ? [auxiliary] : [];
      });
    expect(burnFoldWitnesses.map(({ kind }) => kind)).toEqual([
      "transactionFieldChunk",
      "mintFoldAsset",
    ]);
    expect(
      trace.witnesses.find(
        (witness) => witness.auxiliary?.kind === "valueMintAsset",
      )?.auxiliary,
    ).toMatchObject({ kind: "valueMintAsset", quantity: -5n });
    expect(trace.verdict).toBe("accepted");
    expect([
      ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
    ]).toEqual(
      expect.arrayContaining([
        "valueInputAsset",
        "valueMintAsset",
        "ledgerDeltaReplay",
        "ledgerDeltaOutput",
      ]),
    );
  });

  it("constructs bounded mint proofs across an authenticated chunk boundary", async () => {
    const spent = outRefFromByte(0x23);
    const spentOutput = makeOutput(10n);
    const policyId = Buffer.alloc(28, 0xaa);
    const assets = new Map<Buffer, bigint>();
    for (let assetIndex = 0; assetIndex < 128; assetIndex += 1) {
      const assetName = Buffer.alloc(32);
      assetName.writeUInt32BE(assetIndex, 28);
      assets.set(assetName, 1n);
    }
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [makeOutput(10n)],
      mintPreimageCbor: encodeCbor(new Map([[policyId, assets]])),
    });
    const unchangedRoot = root(0x23);
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: unchangedRoot,
        postUtxosRoot: unchangedRoot,
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: [],
        ledgerMutationSteps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.MissingRequiredWitness,
      }),
    );

    const mintFoldWitnesses = trace.witnesses
      .filter((witness) => witness.phase === "scriptSources")
      .flatMap((witness) => {
        const auxiliary = witness.auxiliary;
        if (
          auxiliary?.kind === "transactionFieldChunk" &&
          auxiliary.collectionProof.fieldIndex === 5
        ) {
          return [auxiliary];
        }
        return auxiliary?.kind === "mintFoldAsset" ? [auxiliary] : [];
      });
    expect(mintFoldWitnesses).toHaveLength(129);
    const crossingWitness = mintFoldWitnesses[117];
    expect(crossingWitness).toMatchObject({
      kind: "mintFoldAsset",
      chunkProof: { chunkIndex: 0 },
      nextChunkProof: { chunkIndex: 1 },
    });
    expect(
      mintFoldWitnesses.every((witness) => {
        if (witness.kind === "transactionFieldChunk") {
          return witness.chunkProof.chunk.length <= 4_095;
        }
        return (
          witness.chunkProof.chunk.length <= 4_095 &&
          (witness.nextChunkProof?.chunk.length ?? 0) <= 4_095
        );
      }),
    ).toBe(true);
    expect(trace.states.at(-1)).toMatchObject({
      phase: "terminal",
      verdict: "rejected",
    });
  }, 15_000);

  it("commits an invalid forced transaction as a proved no-op", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      invalidVkeyWitness: true,
      spendInputs: [spent],
      outputs: [output],
    });
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        sourceKind: "forced",
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: root(3),
        postUtxosRoot: root(3),
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.InvalidSignature,
      }),
    );

    expect(trace.tree.descriptor.verdict).toBe("rejected");
    expect(trace.states.at(-1)).toMatchObject({
      phase: "terminal",
      verdict: "rejected",
    });
    expect(trace.states.some((state) => state.phase === "ledgerDelta")).toBe(
      false,
    );
    expect(
      trace.witnesses
        .filter((witness) => witness.phase === "signatures")
        .map((witness) => witness.auxiliary?.kind ?? null),
    ).toEqual(["transactionFieldChunk", null]);
    expect(
      validateBoundaryAbiAndCollectAuxiliaryKinds(trace).maxArgumentsBytes,
    ).toBeLessThan(16 * 1024);
  });

  it("authenticates a required signer against the streamed signer frontier", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      requiredSignerItems: [Buffer.from(TEST_SIGNER_HASH, "hex")],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );

    const signatureWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "signatures",
    );
    expect(
      signatureWitnesses.map((witness) => witness.auxiliary?.kind ?? null),
    ).toEqual(["transactionFieldChunk", "requiredSignerItem", null]);
    expect(
      signatureWitnesses[1]?.auxiliary?.kind === "requiredSignerItem"
        ? signatureWitnesses[1].auxiliary.signerProof.kind
        : null,
    ).toBe("membership");
    expect(trace.verdict).toBe("accepted");
    expect(
      validateBoundaryAbiAndCollectAuxiliaryKinds(trace).maxArgumentsBytes,
    ).toBeLessThan(16 * 1024);
  });

  it("proves a missing required signer before an invalid-signature rejection", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      invalidVkeyWitness: true,
      spendInputs: [spent],
      outputs: [output],
      requiredSignerItems: [Buffer.alloc(28, 0xa7)],
    });
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        sourceKind: "forced",
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: root(3),
        postUtxosRoot: root(3),
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.MissingRequiredWitness,
      }),
    );

    const signatureWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "signatures",
    );
    expect(
      signatureWitnesses.map((witness) => witness.auxiliary?.kind ?? null),
    ).toEqual(["transactionFieldChunk", "requiredSignerItem"]);
    expect(
      signatureWitnesses[1]?.auxiliary?.kind === "requiredSignerItem"
        ? signatureWitnesses[1].auxiliary.signerProof.kind
        : "membership",
    ).not.toBe("membership");
    expect(trace.tree.descriptor.rejectionCodeHash).toEqual(
      trace.states.at(-1)!.rejectionCodeHash,
    );
    expect(
      validateBoundaryAbiAndCollectAuxiliaryKinds(trace).maxArgumentsBytes,
    ).toBeLessThan(16 * 1024);
  });

  it.each([
    {
      name: "empty spend set",
      transaction: () =>
        makeNativeTx({
          version: 1n,
          spendInputs: [],
          outputs: [makeOutput(10n)],
        }),
      rejectionCode: RejectCodes.EmptyInputs,
      expectedInputSteps: 1,
      expectedInputFieldIndexes: [null],
    },
    {
      name: "spend/reference overlap",
      transaction: () => {
        const input = outRefFromByte(0x21);
        return makeNativeTx({
          version: 1n,
          spendInputs: [input],
          referenceInputs: [input],
          outputs: [makeOutput(10n)],
        });
      },
      rejectionCode: RejectCodes.DuplicateInputInTx,
      expectedInputSteps: 2,
      expectedInputFieldIndexes: [1, 0],
    },
    {
      name: "malformed validity interval",
      transaction: () =>
        makeNativeTx({
          version: 1n,
          spendInputs: [outRefFromByte(0x22)],
          outputs: [makeOutput(10n)],
          validityIntervalStart: 10n,
          validityIntervalEnd: 9n,
        }),
      rejectionCode: RejectCodes.InvalidValidityIntervalFormat,
      expectedInputSteps: 1,
      expectedInputFieldIndexes: [0],
    },
  ])(
    "proves $name at the bounded input-set step",
    async ({
      transaction: makeTransaction,
      rejectionCode,
      expectedInputSteps,
      expectedInputFieldIndexes,
    }) => {
      const transaction = makeTransaction();
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...context,
          sourceKind: "forced",
          transactionId: transaction.txId,
          canonicalTransactionCbor: transaction.txCbor,
          priorUtxosRoot: root(3),
          postUtxosRoot: root(3),
          ledgerWitnessEntries: [],
          expectedLedgerOps: [],
          expectedVerdict: "rejected",
          expectedRejectionCode: rejectionCode,
        }),
      );

      const inputWitnesses = trace.witnesses.filter(
        (witness) => witness.phase === "inputSets",
      );
      expect(inputWitnesses).toHaveLength(expectedInputSteps);
      expect(
        inputWitnesses.map((witness) =>
          witness.auxiliary?.kind === "transactionFieldChunk"
            ? witness.auxiliary.collectionProof.fieldIndex
            : null,
        ),
      ).toEqual(expectedInputFieldIndexes);
      expect(trace.states.at(-1)).toMatchObject({
        phase: "terminal",
        verdict: "rejected",
      });
      expect(
        validateBoundaryAbiAndCollectAuxiliaryKinds(trace).maxArgumentsBytes,
      ).toBeLessThan(16 * 1024);
    },
  );

  it("streams an aggregate field above 8 KiB as ordered L1-sized item proofs", async () => {
    const spent = outRefFromByte(0x12);
    const spentOutput = makeOutput(10n);
    const protectedRecipient = Buffer.from(TEST_ADDRESS_BYTES);
    protectedRecipient[1] = protectedRecipient[1]! ^ 0x01;
    const outputs = [
      ...Array.from({ length: 299 }, (_, index) =>
        makeOutput(BigInt(index + 1)),
      ),
      makeOutput(300n, protectMidgardAddress(protectedRecipient)),
    ];
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs,
    });
    expect(transaction.tx.body.outputsPreimageCbor.length).toBeGreaterThan(
      8 * 1024,
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        sourceKind: "forced",
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: root(3),
        postUtxosRoot: root(3),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.MissingRequiredWitness,
      }),
    );

    const outputChunks = trace.witnesses
      .filter((witness) => witness.phase === "canonicalDecode")
      .flatMap((witness) =>
        witness.auxiliary?.kind === "transactionFieldChunk" &&
        witness.auxiliary.chunkProof.fieldIndex === 2
          ? [witness.auxiliary]
          : [],
      );
    expect(outputChunks).toHaveLength(outputs.length);
    expect(
      outputChunks.every(
        ({ collectionProof, chunkProof }, itemIndex) =>
          collectionProof.itemCount === outputs.length &&
          collectionProof.itemIndex === itemIndex &&
          chunkProof.itemIndex === itemIndex &&
          chunkProof.chunkIndex === 0 &&
          chunkProof.chunk.length <= 4_095,
      ),
    ).toBe(true);
    const outputItems = trace.witnesses
      .filter((witness) => witness.phase === "scriptSources")
      .flatMap((witness) =>
        witness.auxiliary?.kind === "transactionFieldItem"
          ? [witness.auxiliary.collectionProof]
          : [],
      );
    expect(outputItems).toHaveLength(outputs.length);
    expect(
      outputItems.every(
        (proof, itemIndex) =>
          proof.itemCount === outputs.length &&
          proof.itemIndex === itemIndex &&
          proof.itemLength < 16 * 1024,
      ),
    ).toBe(true);
    expect(trace.verdict).toBe("rejected");
  }, 15_000);

  it("fails closed before proving a malformed persisted ledger output", async () => {
    const spent = outRefFromByte(0x11);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [makeOutput(10n)],
    });
    const unchangedRoot = root(6);
    await expect(
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...context,
          sourceKind: "forced",
          transactionId: transaction.txId,
          canonicalTransactionCbor: transaction.txCbor,
          priorUtxosRoot: unchangedRoot,
          postUtxosRoot: unchangedRoot,
          ledgerWitnessEntries: [
            { outRef: spent, output: Buffer.from("8200", "hex") },
          ],
          expectedLedgerOps: [],
          expectedVerdict: "rejected",
          expectedRejectionCode: RejectCodes.InvalidOutput,
        }),
      ),
    ).rejects.toThrow(
      "cannot produce an exact V1 descriptor",
    );
  });

  it("fails closed when the claimed verdict or delta disagrees with replay", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
    });
    const base = {
      ...context,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: root(4),
      postUtxosRoot: root(5),
      ledgerWitnessEntries: [{ outRef: spent, output }],
    };

    await expect(
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...base,
          expectedLedgerOps: [],
          expectedVerdict: "rejected",
          expectedRejectionCode: RejectCodes.InvalidSignature,
        }),
      ),
    ).rejects.toThrow(/disagrees with operator classification/u);

    await expect(
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...base,
          expectedLedgerOps: [],
          expectedVerdict: "accepted",
          expectedRejectionCode: null,
        }),
      ),
    ).rejects.toThrow(/ledger delta differs/u);
  });
});
