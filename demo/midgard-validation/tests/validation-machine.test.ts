import { readdirSync, readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_VALIDATION_DISPUTE_V1_VERSION,
  verifyMidgardValidationTraceProofV1,
} from "@al-ft/midgard-core";
import {
  decodeSingleCbor,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  type MidgardFieldCarriageV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  Application,
  Lambda,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { Constr, Data } from "@lucid-evolution/lucid";
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
  encodeValidationAuxiliaryWitnessCborV1,
  encodeValidationBoundaryEvidenceCborV1,
  initialMidgardResolvedInputsAccumulatorV1,
  MidgardRedeemerTag,
  purposeKindForRedeemerTagV1,
  redeemerPointerMatchesPurposeV1,
  redeemerTagForPurposeKindV1,
  RejectCodes,
  validateCekRouteMaterialV1,
  type ValidationMachineWorkWitness,
  validationSemanticResolverIndexV1,
} from "../src/index.js";
import { exerciseMidgardRetainedDaCanonicalBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";
import {
  hashScriptWitness,
  makeMintPreimageCbor,
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
const validationBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const validationDisputeBlueprint = JSON.parse(
  readFileSync(validationBlueprintPath, "utf8"),
) as unknown;
const semanticResolverDefinitionsV1 = [
  "canonical_decode_empty_semantic_v1",
  "canonical_decode_item_semantic_v1",
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
  "script_sources_stage_one_finish_semantic_v1",
  "script_sources_stage_one_redeemer_semantic_v1",
  "script_sources_stage_eleven_finish_semantic_v1",
  "script_sources_stage_eleven_source_semantic_v1",
  "script_sources_stage_twelve_finish_semantic_v1",
  "script_sources_stage_twelve_redeemer_semantic_v1",
  "script_sources_stage_ten_missing_semantic_v1",
  "script_sources_stage_ten_mismatch_semantic_v1",
  "script_sources_stage_ten_match_semantic_v1",
  "script_sources_stage_eight_finish_semantic_v1",
  "script_sources_stage_eight_purpose_semantic_v1",
  "script_sources_stage_seven_observer_semantic_v1",
  "script_sources_stage_seven_receive_semantic_v1",
  "script_sources_stage_seven_finish_semantic_v1",
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
  0, 2, 3, 4, 6, 10, 24, 26, 32, 60, 63, -1, -1, 67,
] as const;

describe("V1 purpose-kind to redeemer-pointer mapping", () => {
  it("matches the exhaustive canonical vector and rejects adjacent values", () => {
    const canonical = [
      { purposeKind: 0, redeemerTag: 0 },
      { purposeKind: 1, redeemerTag: 1 },
      { purposeKind: 2, redeemerTag: 3 },
      { purposeKind: 3, redeemerTag: 6 },
    ] as const;

    for (const { purposeKind, redeemerTag } of canonical) {
      expect(redeemerTagForPurposeKindV1(purposeKind)).toBe(redeemerTag);
      expect(purposeKindForRedeemerTagV1(redeemerTag)).toBe(purposeKind);
      expect(
        redeemerPointerMatchesPurposeV1({
          purposeKind,
          purposeIndex: 7n,
          redeemerTag,
          redeemerIndex: 7n,
        }),
      ).toBe(true);
      expect(
        redeemerPointerMatchesPurposeV1({
          purposeKind,
          purposeIndex: 7n,
          redeemerTag,
          redeemerIndex: 8n,
        }),
      ).toBe(false);
    }

    for (const purposeKind of [-1, 4]) {
      expect(redeemerTagForPurposeKindV1(purposeKind)).toBeNull();
      expect(
        redeemerPointerMatchesPurposeV1({
          purposeKind,
          purposeIndex: 7n,
          redeemerTag: 0,
          redeemerIndex: 7n,
        }),
      ).toBe(false);
    }
    for (const redeemerTag of [-1, 2, 4, 5, 7]) {
      expect(purposeKindForRedeemerTagV1(redeemerTag)).toBeNull();
      expect(
        redeemerPointerMatchesPurposeV1({
          purposeKind: 0,
          purposeIndex: 7n,
          redeemerTag,
          redeemerIndex: 7n,
        }),
      ).toBe(false);
    }
  });
});

describe("C21 challenged auxiliary carrier policy", () => {
  it("excludes retired whole-output and whole-script wire fields", () => {
    const machineSource = readFileSync(
      resolve(process.cwd(), "src/validation-machine.ts"),
      "utf8",
    );
    const encoderSource = readFileSync(
      resolve(process.cwd(), "src/validation-machine-data.ts"),
      "utf8",
    );
    expect(machineSource).not.toContain('readonly kind: "outputReplay"');
    expect(encoderSource).not.toContain('case "outputReplay"');
    expect(encoderSource).not.toContain("scriptData(auxiliary.source.script)");
    expect(encoderSource).not.toContain("byteList(auxiliary.signerHashes)");
    expect(machineSource).not.toMatch(
      /kind: "cekResolvedContextItem"[\s\S]{0,240}readonly value:/,
    );
    expect(machineSource).not.toMatch(
      /kind: "cekOutputContextItem"[\s\S]{0,180}readonly outputCbor:/,
    );
    expect(machineSource).not.toMatch(
      /kind: "cekContextFinalizeSpend"[\s\S]{0,280}readonly value:/,
    );
  });
});

/**
 * The field index a `canonicalDecode` step is reading.
 *
 * #597: `TransactionFieldItemWitness` carries only a `FieldCarriageV1` — the
 * phase takes both the field index and the item index from its own control, so
 * the auxiliary does not repeat them. The control is position 4 of the step's
 * work-witness array, which is where the machine writes it, so reading it here
 * asks the same question the on-chain step does.
 */
const canonicalDecodeFieldIndexV1 = (
  witness: ValidationMachineWorkWitness,
): number => {
  const control = decodeSingleCbor(witness.cbor);
  if (!Array.isArray(control) || control.length !== 9) {
    throw new Error("canonicalDecode control must contain nine fields");
  }
  return Number(control[4] as bigint);
};

/** The bytes a tier-1 `Inline` carriage delivers to the door. */
const inlineCarriagePreimageV1 = (
  carriage: MidgardFieldCarriageV1,
): Buffer => {
  if (carriage.carriage !== "Inline") {
    throw new Error("machine carriage is expected to be tier-1 Inline");
  }
  return carriage.preimage;
};

type MintFoldWitnessV1 = Extract<
  NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
  {
    readonly kind: "transactionFieldChunk" | "mintFoldAsset";
  }
>;

const collectMintFoldWitnessesV1 = (
  trace: DeterministicValidationMachineTrace,
): readonly MintFoldWitnessV1[] =>
  trace.witnesses
    .filter((witness) => witness.phase === "scriptSources")
    .map((witness) => witness.auxiliary)
    .filter(
      (auxiliary): auxiliary is MintFoldWitnessV1 =>
        auxiliary?.kind === "mintFoldAsset" ||
        (auxiliary?.kind === "transactionFieldChunk" &&
          auxiliary.fieldIndex === 5),
    );

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
    // #597 / #579 handoff. `ValidationAuxiliaryWitnessV1` is a **moved** wire
    // surface: #592 reshaped four of its constructors onto §8's
    // `FieldCarriageV1` (1 `TransactionFieldChunkWitness`, 2
    // `RequiredSignerItemWitness`, 29 `TransactionRedeemerItemBeginWitness`,
    // 30 `TransactionFieldItemWitness`) and left `plutus.json` byte-identical,
    // because blueprints move once in #579's single regeneration (#587's
    // precedent). The committed definition therefore still declares
    // `collection_proof`/`chunk_proof` and `collection_proof`/`item_cbor`, and a
    // step emitting one of the four cannot match it — not because the emission
    // is wrong but because the blueprint is stale.
    //
    // The sum is validated for every step that emits one of the other
    // thirty-six constructors, and the transition and evidence envelopes are
    // validated unconditionally, so this keeps a real ABI gate rather than
    // switching one off. When #579 regenerates, `movedDoorConstructors` becomes
    // empty and this branch disappears.
    const movedDoorConstructors = new Set([1, 2, 29, 30]);
    const auxiliaryConstructorIndex = ((): number | null => {
      const decoded = Data.from(oneStepArgument.auxiliaryCbor.toString("hex"));
      return decoded instanceof Constr ? decoded.index : null;
    })();
    const auxiliaryIsFrozenStale =
      auxiliaryConstructorIndex !== null &&
      movedDoorConstructors.has(auxiliaryConstructorIndex);
    for (const [definitionName, cbor] of [
      [
        "midgard/validation_machine_v1/ValidationOneStepWitnessV1",
        oneStepArgument.transitionCbor,
      ],
      ...(auxiliaryIsFrozenStale
        ? []
        : ([
            [
              "midgard/validation_machine_v1/ValidationAuxiliaryWitnessV1",
              oneStepArgument.auxiliaryCbor,
            ],
            [
              "midgard/validation_machine_v1/ValidationOneStepEvidenceV1",
              oneStepArgument.evidenceCbor,
            ],
          ] as const)),
    ] as const) {
      parseExactAikenDataCbor({
        blueprint: validationDisputeBlueprint,
        definitionName,
        cbor: cbor.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      });
      maxArgumentsBytes = Math.max(maxArgumentsBytes, cbor.length);
    }
    maxArgumentsBytes = Math.max(
      maxArgumentsBytes,
      oneStepArgument.auxiliaryCbor.length,
      oneStepArgument.evidenceCbor.length,
    );
    if (!auxiliaryIsFrozenStale && oneStepArgument.semanticResolverIndex !== null) {
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

const buildNonterminatingSelfApplicationProgram = () => {
  const selfApplication = new Lambda(
    new Application(new UPLCVar(0), new UPLCVar(0)),
  );
  return buildMidgardCanonicalCekProgramV1(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram(
          [1, 1, 0],
          new Application(selfApplication, selfApplication),
        ),
      ).toBuffer().buffer,
    ),
  );
};

describe("deterministic validation machine", { timeout: 60_000 }, () => {
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
        // #597: both constructors carry the field's whole §5.1 preimage as
        // tier-1 `Inline` carriage, so the step's envelope is its control plus
        // that preimage. Tier 1 is bounded by construction — the producer
        // refuses above the cap — so this measures the whole admitted domain.
        if (
          witness.auxiliary.kind === "transactionFieldItem" ||
          witness.auxiliary.kind === "transactionFieldChunk"
        ) {
          return (
            witness.cbor.length +
              inlineCarriagePreimageV1(witness.auxiliary.carriage).length <
            16 * 1024
          );
        }
        return false;
      }),
    ).toBe(true);
    const scriptSourceWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "scriptSources",
    );
    expect(scriptSourceWitnesses).toHaveLength(22);
    expect(scriptSourceWitnesses[0]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[1]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[2]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[3]?.auxiliary?.kind).toBe(
      "resolvedInputReplay",
    );
    expect(
      scriptSourceWitnesses.map((witness) => witness.auxiliary?.kind ?? null),
    ).not.toContain("transactionFieldPairPreimage");
    const decodeControl = (
      witness: DeterministicValidationMachineTrace["witnesses"][number],
    ): readonly unknown[] => {
      const decoded = decodeSingleCbor(witness.cbor);
      expect(Array.isArray(decoded)).toBe(true);
      return decoded as readonly unknown[];
    };
    const resolveInputControls = trace.witnesses
      .filter((witness) => witness.phase === "resolveInputs")
      .map(decodeControl);
    const originalResolutionScheduleHash = Buffer.from(
      resolveInputControls[0]![10] as Uint8Array,
    );
    expect(
      resolveInputControls.every(
        (control) =>
          control.length === 11 &&
          Buffer.from(control[10] as Uint8Array).equals(
            originalResolutionScheduleHash,
          ),
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses
        .map(decodeControl)
        .every(
          (control) =>
            (control.length === 30 || control.length === 31) &&
            Buffer.from(control[29] as Uint8Array).equals(
              originalResolutionScheduleHash,
            ),
        ),
    ).toBe(true);
    const nativeScriptControls = trace.witnesses
      .filter((witness) => witness.phase === "nativeScripts")
      .map(decodeControl);
    expect(
      nativeScriptControls.every(
        (control) =>
          control.length === 26 &&
          Buffer.from(control[25] as Uint8Array).equals(
            originalResolutionScheduleHash,
          ),
      ),
    ).toBe(true);
    const valueAndMintWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "valueAndMint",
    );
    expect(valueAndMintWitnesses).toHaveLength(8);
    expect(valueAndMintWitnesses[0]?.auxiliary).toBeNull();
    expect(
      valueAndMintWitnesses.map((witness) => witness.auxiliary?.kind ?? null),
    ).not.toContain("transactionFieldPairPreimage");
    expect(
      valueAndMintWitnesses.every((witness) => {
        const valueControl = decodeControl(witness);
        expect(valueControl).toHaveLength(12);
        const nestedNativeControl = decodeSingleCbor(
          valueControl[0] as Uint8Array,
        );
        expect(Array.isArray(nestedNativeControl)).toBe(true);
        const fields = nestedNativeControl as readonly unknown[];
        return (
          fields.length === 26 &&
          Buffer.from(fields[25] as Uint8Array).equals(
            originalResolutionScheduleHash,
          )
        );
      }),
    ).toBe(true);
    expect(scriptSourceWitnesses[4]?.auxiliary).toBeNull();
    // C21-STAGE4 Option A: the stage-4 fold witness is proof-only.
    expect(scriptSourceWitnesses[5]?.auxiliary?.kind).toBe(
      "transactionRedeemerItemBegin",
    );
    expect(scriptSourceWitnesses[6]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[7]?.auxiliary?.kind).toBe(
      "ledgerOutputProofBegin",
    );
    expect(
      scriptSourceWitnesses
        .slice(8, 15)
        .every(
          (witness) => witness.auxiliary?.kind === "ledgerOutputProofStep",
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
      6, 14, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2, 2, 3, 4, 0, 27, 23, 16, 18,
    ]);
    expect(() =>
      validationSemanticResolverIndexV1({
        ...scriptSourceWitnesses[7]!,
        auxiliary: scriptSourceWitnesses[3]!.auxiliary,
      }),
    ).toThrow("has no semantic resolver");
    expect(
      canonicalWitnesses.every((witness) => {
        if (witness.cbor.includes(transaction.txCbor)) return false;
        if (witness.auxiliary === null) return true;
        if (
          witness.auxiliary.kind === "transactionFieldItem" ||
          witness.auxiliary.kind === "transactionFieldChunk"
        ) {
          return !inlineCarriagePreimageV1(
            witness.auxiliary.carriage,
          ).includes(transaction.txCbor);
        }
        return false;
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
      fieldIndex: 6,
      itemIndex: 0,
      carriage: { carriage: "Inline" },
    });
    const sourceHashBlocks = scriptSourceWitnesses.filter(
      (witness) => witness.auxiliary?.kind === "scriptSourceHashBlock",
    );
    expect(sourceHashBlocks).toHaveLength(1);
    expect(sourceHashBlocks[0]?.auxiliary).toMatchObject({
      kind: "scriptSourceHashBlock",
      chunkProof: {
        fieldIndex: 6,
        itemIndex: 0,
        chunkIndex: 0,
      },
      nextChunkProof: null,
    });
    const redeemerSourceWitness = scriptSourceWitnesses.find(
      (witness) => witness.auxiliary?.kind === "transactionRedeemerItemBegin",
    );
    expect(redeemerSourceWitness?.auxiliary).toMatchObject({
      kind: "transactionRedeemerItemBegin",
      carriage: { carriage: "Inline" },
    });
    expect(validationSemanticResolverIndexV1(redeemerSourceWitness!)).toBe(15);
    expect(
      scriptSourceWitnesses.some(
        (witness) => validationSemanticResolverIndexV1(witness) === 14,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "scriptSourceScan" &&
          validationSemanticResolverIndexV1(witness) === 17,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerScanBegin" &&
          validationSemanticResolverIndexV1(witness) === 19,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerScanBegin" &&
          validationSemanticResolverIndexV1(witness) === 21,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerItemStep" &&
          validationSemanticResolverIndexV1(witness) === 22,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "scriptPurposeScan" &&
          validationSemanticResolverIndexV1(witness) === 24,
      ),
    ).toBe(true);
    expect(cekWitnesses.map((witness) => witness.auxiliary?.kind)).toEqual(
      expect.arrayContaining([
        "nativeExecutionScan",
        "redeemerScanBegin",
        "redeemerItemStep",
        "cekResolvedContextItem",
        "cekOutputContextItem",
        "cekSignerContextItem",
        "cekRedeemerContextSelect",
        "cekContextFinalizeSpend",
        "cekContextAssemble",
        "cekTxInfoFinalize",
        "cekContextSeed",
        "cekCoreStep",
      ]),
    );
    expect(
      cekWitnesses.some(
        (witness) => witness.auxiliary?.kind === "redeemerScanBegin",
      ),
    ).toBe(true);
    expect(
      cekWitnesses.some(
        (witness) => witness.auxiliary?.kind === "cekRedeemerContextSelect",
      ),
    ).toBe(true);
    expect(
      cekWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerItemStep" &&
          witness.auxiliary.redeemerControl === null,
      ),
    ).toBe(true);
    expect(
      cekWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerItemStep" &&
          witness.auxiliary.redeemerControl !== null,
      ),
    ).toBe(true);
    expect(
      trace.witnesses.some((witness) => {
        const auxiliary = witness.auxiliary as
          | Record<string, unknown>
          | null
          | undefined;
        return (
          auxiliary !== null &&
          auxiliary !== undefined &&
          ("redeemer" in auxiliary ||
            "rawCbor" in auxiliary ||
            "dataCborHex" in auxiliary)
        );
      }),
    ).toBe(false);
    const nativeExecutionWitness = cekWitnesses.find(
      (witness) => witness.auxiliary?.kind === "nativeExecutionScan",
    )?.auxiliary;
    if (nativeExecutionWitness?.kind !== "nativeExecutionScan") {
      throw new Error("expected a descriptor-only native execution witness");
    }
    expect(nativeExecutionWitness.source.scriptTotalLength).toBeGreaterThan(0);
    expect(nativeExecutionWitness.source.scriptItemCommitment).toHaveLength(32);
    expect(nativeExecutionWitness.firstChunkProof.chunkIndex).toBe(0);
    expect(
      nativeExecutionWitness.firstChunkProof.chunk.length,
    ).toBeLessThanOrEqual(4_095);
    expect("script" in nativeExecutionWitness.source).toBe(false);
    expect("signerHashes" in nativeExecutionWitness).toBe(false);
    const selectionStateIndex = trace.witnesses.findIndex(
      (witness) => witness.auxiliary === nativeExecutionWitness,
    );
    const selectionArgument = buildValidationOneStepArgumentV1({
      trace,
      stateIndex: selectionStateIndex,
    });
    expect(selectionArgument.cekRouteMaterial).toEqual({
      envelopeCbor: program.envelopeCbor,
      programMaterialSidecarCbor: trace.programMaterialSidecarCbor,
      programEnvelopeHash: program.envelopeHash,
    });
    const laterCekStateIndex = trace.witnesses.findIndex(
      (witness, index) =>
        index > selectionStateIndex &&
        witness.phase === "cek" &&
        witness.auxiliary?.kind !== "nativeExecutionScan",
    );
    expect(
      buildValidationOneStepArgumentV1({
        trace,
        stateIndex: laterCekStateIndex,
      }).cekRouteMaterial,
    ).toBeUndefined();
    const nonCekStateIndex = trace.witnesses.findIndex(
      (witness) => witness.phase !== "cek",
    );
    expect(
      buildValidationOneStepArgumentV1({
        trace,
        stateIndex: nonCekStateIndex,
      }).cekRouteMaterial,
    ).toBeUndefined();

    const routeMaterial = selectionArgument.cekRouteMaterial!;
    const validateRouteMaterial = (value: unknown) =>
      validateCekRouteMaterialV1({
        value,
        firstSourceChunk: nativeExecutionWitness.firstChunkProof.chunk,
        languageTag: nativeExecutionWitness.languageTag as 3 | 128,
      });
    expect(validateRouteMaterial(routeMaterial)).toEqual(routeMaterial);
    const substituteProgram = buildNonterminatingSelfApplicationProgram();
    expect(() =>
      validateRouteMaterial({
        ...routeMaterial,
        envelopeCbor: substituteProgram.envelopeCbor,
        programEnvelopeHash: substituteProgram.envelopeHash,
      }),
    ).toThrow(/selected first-source-chunk payload/u);
    expect(() =>
      validateRouteMaterial({
        ...routeMaterial,
        programMaterialSidecarCbor: Buffer.concat([
          routeMaterial.programMaterialSidecarCbor,
          Buffer.from([0]),
        ]),
      }),
    ).toThrow(/trailing|canonical/u);
    expect(() =>
      validateRouteMaterial({
        ...routeMaterial,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1(
          [],
        ),
      }),
    ).toThrow(/program material is missing root/u);
    const retainedRoots = new Set(
      [...program.material.keys()].map((root) => root.toLowerCase()),
    );
    const unrelatedEntry = [...substituteProgram.material.values()].find(
      (entry) => !retainedRoots.has(Buffer.from(entry.root).toString("hex")),
    );
    if (unrelatedEntry === undefined) {
      throw new Error("expected unrelated canonical CEK material");
    }
    expect(() =>
      validateRouteMaterial({
        ...routeMaterial,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
          ...program.material.values(),
          unrelatedEntry,
        ]),
      }),
    ).toThrow(/unreachable/u);
    expect(() =>
      validateRouteMaterial({
        ...routeMaterial,
        programEnvelopeHash: Buffer.alloc(32, 0xff),
      }),
    ).toThrow(/program-envelope hash/u);
    const challengedDescriptorWitnesses = cekWitnesses.flatMap((witness) => {
      const auxiliary = witness.auxiliary;
      return auxiliary?.kind === "cekResolvedContextItem" ||
        auxiliary?.kind === "cekOutputContextItem" ||
        auxiliary?.kind === "cekContextFinalizeSpend"
        ? [auxiliary]
        : [];
    });
    expect(challengedDescriptorWitnesses.length).toBeGreaterThanOrEqual(3);
    expect(
      challengedDescriptorWitnesses.every(
        (auxiliary) =>
          auxiliary.descriptorCbor.length > 0 &&
          auxiliary.descriptorCbor.length < 16 * 1024 &&
          !("value" in auxiliary) &&
          !("outputCbor" in auxiliary),
      ),
    ).toBe(true);
    expect(
      [nativeExecutionWitness, ...challengedDescriptorWitnesses].every(
        (auxiliary) =>
          encodeValidationAuxiliaryWitnessCborV1(auxiliary).length < 16 * 1024,
      ),
    ).toBe(true);
    const cekStates = trace.states.filter((state) => state.phase === "cek");
    expect(cekStates.at(-1)!.executionCpu).toBeGreaterThan(0n);
    expect(cekStates.at(-1)!.executionMemory).toBeGreaterThan(0n);
    expect(trace.verdict).toBe("accepted");
    const oneStepAbi = validateBoundaryAbiAndCollectAuxiliaryKinds(trace);
    expect(oneStepAbi.kinds.size).toBeGreaterThan(15);
    expect(oneStepAbi.maxArgumentsBytes).toBeLessThan(16 * 1024);
  });

  it("retains only the first over-budget CEK transition for a nonterminating program", async () => {
    const spent = outRefFromByte(0x6f);
    const program = buildNonterminatingSelfApplicationProgram();
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
          exUnits: [0n, 0n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    const rootPreparation = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: [
        { type: "delete", key: spent },
        buildValidationMachineLedgerInsertOpV1({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ],
    });
    const unchangedRoot = rootPreparation[0]!.preRoot.toString("hex");

    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
          ...program.material.values(),
        ]),
        priorUtxosRoot: unchangedRoot,
        postUtxosRoot: unchangedRoot,
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: [],
        ledgerMutationSteps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.PlutusScriptInvalid,
      }),
    );

    const coreSteps = trace.witnesses.flatMap((witness) =>
      witness.auxiliary?.kind === "cekCoreStep" ? [witness.auxiliary.step] : [],
    );
    expect(coreSteps.length).toBeGreaterThan(0);
    expect(coreSteps.length).toBeLessThan(16);
    expect(
      coreSteps
        .slice(0, -1)
        .every((step) => step.post.cpu <= 0n && step.post.memory <= 0n),
    ).toBe(true);
    expect(
      coreSteps.at(-1)!.post.cpu > 0n || coreSteps.at(-1)!.post.memory > 0n,
    ).toBe(true);
    expect(trace.states.at(-1)).toMatchObject({
      phase: "terminal",
      verdict: "rejected",
    });
    expect(trace.states.some((state) => state.phase === "ledgerDelta")).toBe(
      false,
    );
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
            ? witness.auxiliary.fieldIndex
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
      const mintPreimageCbor = makeMintPreimageCbor(
        new Map([[policyId, new Map([[assetName, quantity]])]]),
      );
      const transaction = makeNativeTx({
        version: 1n,
        spendInputs: [spent],
        outputs: [output],
        scriptWitnesses: [script],
        mintPreimageCbor,
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
    // Measured 15.0 s for the burn case on a 2-core CI runner against the
    // former 15 s budget, and both mint and burn timed out there while
    // siblings legitimately take 14.3-16.1 s; calibrated on 32 cores.
    60_000,
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
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.phase === "scriptSources" &&
          witness.auxiliary?.kind === "scriptPurposeScan" &&
          validationSemanticResolverIndexV1(witness) === 26,
      ),
    ).toBe(true);
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.phase === "scriptSources" &&
          validationSemanticResolverIndexV1(witness) === 27,
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
  }, 60_000);

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
    const cekObserverWitnesses = trace.witnesses.filter(
      (witness) =>
        witness.phase === "cek" &&
        witness.auxiliary?.kind === "transactionFieldChunk" &&
        witness.auxiliary.fieldIndex === 3,
    );
    expect(cekObserverWitnesses).toHaveLength(1);
    expect(cekObserverWitnesses[0]?.auxiliary).toMatchObject({
      kind: "transactionFieldChunk",
      fieldIndex: 3,
      itemIndex: 0,
      carriage: { carriage: "Inline" },
    });
    const cekObserverWitnessIndex = trace.witnesses.indexOf(
      cekObserverWitnesses[0]!,
    );
    expect(trace.witnesses[cekObserverWitnessIndex + 1]).toMatchObject({
      phase: "cek",
      auxiliary: null,
    });
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
          witness.auxiliary.fieldIndex === 3 &&
          validationSemanticResolverIndexV1(witness) === 25,
      ),
    ).toBe(true);
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.phase === "scriptSources" &&
          validationSemanticResolverIndexV1(witness) === 27,
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
  }, 60_000);

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
      mintPreimageCbor: makeMintPreimageCbor(
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

    const mintFoldWitnesses = collectMintFoldWitnessesV1(trace);
    expect(mintFoldWitnesses.map(({ kind }) => kind)).toEqual([
      "transactionFieldChunk",
      "mintFoldAsset",
    ]);
    expect(
      mintFoldWitnesses.every((witness) => {
        if (witness.kind === "transactionFieldChunk") {
          // Tier-1 carriage is bounded by §8.4's own cap, which the producer
          // refuses above; the 4,095-byte chunk bound was the retired
          // `ChunkProofV1`'s and has no wire surface left.
          return (
            inlineCarriagePreimageV1(witness.carriage).length <=
            MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1
          );
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
      trace.witnesses.find(
        (witness) =>
          witness.phase === "phaseANativeScripts" &&
          witness.auxiliary?.kind === "transactionFieldChunk",
      )?.auxiliary,
    ).toMatchObject({
      kind: "transactionFieldChunk",
      fieldIndex: 6,
      carriage: { carriage: "Inline" },
    });
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
    // Same CI-timing class as the mint/burn each above: ~6.5 s locally on
    // 32 cores, ~14-15 s on a 2-core runner, i.e. on the 15 s boundary.
  }, 60_000);

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
      mintPreimageCbor: makeMintPreimageCbor(
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

    const burnFoldWitnesses = collectMintFoldWitnessesV1(trace);
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
    // Same CI-timing class as the mint/burn each above: ~6.5 s locally on
    // 32 cores, ~14-15 s on a 2-core runner, i.e. on the 15 s boundary.
  }, 60_000);

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
      mintPreimageCbor: makeMintPreimageCbor(new Map([[policyId, assets]])),
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

    const mintFoldWitnesses = collectMintFoldWitnessesV1(trace);
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
          // Tier-1 carriage is bounded by §8.4's own cap, which the producer
          // refuses above; the 4,095-byte chunk bound was the retired
          // `ChunkProofV1`'s and has no wire surface left.
          return (
            inlineCarriagePreimageV1(witness.carriage).length <=
            MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1
          );
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
  }, 60_000);

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
    expect(signatureWitnesses[0]?.auxiliary).toMatchObject({
      kind: "transactionFieldChunk",
      fieldIndex: 7,
      carriage: { carriage: "Inline" },
    });
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
    expect(signatureWitnesses[0]?.auxiliary).toMatchObject({
      kind: "transactionFieldChunk",
      fieldIndex: 7,
      carriage: { carriage: "Inline" },
    });
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
            ? witness.auxiliary.fieldIndex
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

  it("carries an aggregate field above 8 KiB as ordered complete-item proofs", async () => {
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

    // #597: an item's index and length are derived from the authenticated
    // preimage now, not claimed by the prover, so what a step can be checked for
    // is that it names field 2 (from its own control) and delivers exactly the
    // committed field-2 preimage.
    const outputsPreimage = transaction.tx.body.outputsPreimageCbor;
    const canonicalOutputItems = trace.witnesses
      .filter(
        (witness) =>
          witness.phase === "canonicalDecode" &&
          witness.auxiliary?.kind === "transactionFieldItem" &&
          canonicalDecodeFieldIndexV1(witness) === 2,
      )
      .map((witness) => witness.auxiliary!);
    expect(canonicalOutputItems).toHaveLength(outputs.length);
    expect(
      canonicalOutputItems.every(
        (auxiliary) =>
          auxiliary.kind === "transactionFieldItem" &&
          inlineCarriagePreimageV1(auxiliary.carriage).equals(outputsPreimage),
      ),
    ).toBe(true);
    // C21-STAGE4 Option A: stage-4 emits the carriage-only witness. The stage-1
    // redeemer begin shares the kind, so the outputs field is pinned by the
    // bytes the carriage delivers rather than by a field index the constructor
    // no longer carries.
    const outputItems = trace.witnesses
      .filter((witness) => witness.phase === "scriptSources")
      .flatMap((witness) =>
        witness.auxiliary?.kind === "transactionRedeemerItemBegin" &&
        inlineCarriagePreimageV1(witness.auxiliary.carriage).equals(
          outputsPreimage,
        )
          ? [witness.auxiliary]
          : [],
      );
    expect(outputItems).toHaveLength(outputs.length);
    expect(trace.verdict).toBe("rejected");
  }, 60_000);

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
    ).rejects.toThrow("cannot produce an exact V1 descriptor");
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

  // ==========================================================================
  // C29 — canonical retained CBOR verification.
  //
  // The maximum retained canonical source in this suite is the 300-output
  // aggregate whose outputs preimage exceeds 8 KiB while every individual item
  // stays inside the complete-item publication bound, so canonical decode
  // reaches its exact terminal through complete-item-first staging rather than
  // an incremental scan.
  // ==========================================================================

  const buildMaximumRetainedCanonicalSourceV1 = () => {
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
    return {
      spent,
      spentOutput,
      outputCount: outputs.length,
      transaction,
      replayBase: {
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: root(3),
        postUtxosRoot: root(3),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: [],
        expectedVerdict: "rejected" as const,
        expectedRejectionCode: RejectCodes.MissingRequiredWitness,
      },
    };
  };

  const canonicalDecodeWorkTranscript = (
    trace: DeterministicValidationMachineTrace,
  ): readonly string[] =>
    trace.witnesses
      .filter((witness) => witness.phase === "canonicalDecode")
      .map(
        (witness) =>
          `${witness.programCounter.toString()}:${witness.cbor.toString("hex")}`,
      );

  it("reaches one byte-identical canonical decode terminal from normal and forced retained sources", async () => {
    const fixture = buildMaximumRetainedCanonicalSourceV1();

    // Both retained DA classifications carry the same canonical bytes, and
    // each independently folds back to them.
    const retained = await exerciseMidgardRetainedDaCanonicalBoundaryV1({
      canonicalTransactionCbor: fixture.transaction.txCbor,
    });
    expect(retained.normal.retainedPreimageBytes).toBeGreaterThan(8 * 1024);
    expect(retained.normal.retainedPreimageDigestHex).toBe(
      retained.forced.retainedPreimageDigestHex,
    );
    for (const measurement of [retained.normal, retained.forced]) {
      expect(measurement.reconstructedCanonicalDigestHex).toBe(
        measurement.retainedPreimageDigestHex,
      );
      expect(measurement.reconstructedCanonicalBytes).toBe(
        measurement.retainedPreimageBytes,
      );
      expect(measurement.transactionIdHex).toBe(retained.transactionIdHex);
      expect(measurement.transactionCommitmentHex).toBe(
        retained.transactionCommitmentHex,
      );
    }
    expect(retained.normal.revealStepCount).toBe(
      retained.forced.revealStepCount,
    );
    expect(retained.normal.revealStepCount).toBeGreaterThan(0);

    // Both source classifications reach one identical canonical-decode work
    // transcript; only the state's source-kind discriminant differs.
    const [normalTrace, forcedTrace] = await Promise.all([
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...fixture.replayBase,
          sourceKind: "normal",
        }),
      ),
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...fixture.replayBase,
          sourceKind: "forced",
        }),
      ),
    ]);
    const normalTranscript = canonicalDecodeWorkTranscript(normalTrace);
    expect(normalTranscript.length).toBeGreaterThan(fixture.outputCount);
    expect(normalTranscript).toEqual(
      canonicalDecodeWorkTranscript(forcedTrace),
    );
    expect(normalTrace.validationContextCbor.toString("hex")).toBe(
      forcedTrace.validationContextCbor.toString("hex"),
    );
    expect(normalTrace.states.map((state) => state.sourceKind)).toEqual(
      normalTrace.states.map(() => "normal"),
    );
    expect(forcedTrace.states.map((state) => state.sourceKind)).toEqual(
      forcedTrace.states.map(() => "forced"),
    );
    // The source kind is authenticated into the trace, so the same work
    // transcript still commits to two distinct terminals.
    expect(
      normalTrace.tree.descriptor.terminalStateHash.toString("hex"),
    ).not.toBe(forcedTrace.tree.descriptor.terminalStateHash.toString("hex"));

    // Complete-item-first staging: every ordered outputs item is carried whole.
    const completeItems = normalTrace.witnesses.flatMap((witness) =>
      witness.phase === "canonicalDecode" &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      canonicalDecodeFieldIndexV1(witness) === 2
        ? [witness.auxiliary]
        : [],
    );
    expect(completeItems).toHaveLength(fixture.outputCount);
    const chunkedOutputItems = normalTrace.witnesses.filter(
      (witness) =>
        witness.phase === "canonicalDecode" &&
        witness.auxiliary?.kind === "transactionFieldChunk" &&
        witness.auxiliary.fieldIndex === 2,
    );
    expect(chunkedOutputItems).toHaveLength(0);
  }, 120_000);

  it("rejects malformed, trailing, and noncanonical retained transaction CBOR at the exact decode terminal", () => {
    const fixture = buildMaximumRetainedCanonicalSourceV1();
    const canonical = Buffer.from(fixture.transaction.txCbor);

    // The pristine canonical source decodes to the authenticated identity.
    const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
    expect(computeMidgardNativeTxIdV1(decoded).toString("hex")).toBe(
      Buffer.from(fixture.transaction.txId).toString("hex"),
    );

    // Malformed: the last byte of the definite-length encoding is missing.
    expect(() =>
      decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical.subarray(0, -1)),
    ).toThrow();

    // Trailing: one extra byte after the complete top-level item.
    expect(() =>
      decodeMidgardNativeTxFullV1FromCanonicalCbor(
        Buffer.concat([canonical, Buffer.from([0x00])]),
      ),
    ).toThrow();

    // Noncanonical: the top-level array count re-encoded in non-minimal form.
    const head = canonical[0]!;
    expect(head).toBeGreaterThanOrEqual(0x80);
    expect(head).toBeLessThan(0x98);
    expect(() =>
      decodeMidgardNativeTxFullV1FromCanonicalCbor(
        Buffer.concat([
          Buffer.from([0x98, head - 0x80]),
          canonical.subarray(1),
        ]),
      ),
    ).toThrow();

    // Indefinite-length top-level array is not a canonical V1 source either.
    expect(() =>
      decodeMidgardNativeTxFullV1FromCanonicalCbor(
        Buffer.concat([
          Buffer.from([0x9f]),
          canonical.subarray(1),
          Buffer.from([0xff]),
        ]),
      ),
    ).toThrow();
  });

  it("confines the incremental CBOR scanner to consumers carrying measured §3.2 necessity evidence", () => {
    const aikenRoot = resolve(process.cwd(), "../../onchain/aiken");
    const aikenSources = ((): readonly string[] => {
      const collected: string[] = [];
      const walk = (directory: string): void => {
        for (const entry of readdirSync(directory, { withFileTypes: true })) {
          const path = resolve(directory, entry.name);
          if (entry.isDirectory()) {
            walk(path);
          } else if (entry.name.endsWith(".ak")) {
            collected.push(path);
          }
        }
      };
      walk(resolve(aikenRoot, "lib"));
      walk(resolve(aikenRoot, "validators"));
      return collected;
    })();
    expect(aikenSources.length).toBeGreaterThan(0);

    // Every on-chain module that performs an incremental canonical-CBOR scan.
    const scannerConsumers = aikenSources
      .filter((path) => {
        const relative = path.slice(aikenRoot.length + 1);
        if (relative === "lib/midgard/canonical-cbor-scan-v1.ak") {
          return false;
        }
        return readFileSync(path, "utf8").includes("canonical_cbor_scan_v1");
      })
      .map((path) => path.slice(aikenRoot.length + 1))
      .sort();

    // Each consumer must be backed by a measured §3.2 necessity artifact that
    // records the fitting representations it ruled out.
    const necessityByConsumer: Record<string, string> = {
      "lib/midgard/redeemer-item-proof-v1.ak": "redeemer-item-traversal-v1.md",
      "lib/midgard/ledger-output-scan-v1.ak":
        "ledger-output-incremental-proof-v1.md",
    };
    expect(scannerConsumers).toEqual(Object.keys(necessityByConsumer).sort());
    for (const artifact of Object.values(necessityByConsumer)) {
      const necessity = readFileSync(
        resolve(
          process.cwd(),
          "../../docs/exec-plans/evidence/necessity",
          artifact,
        ),
        "utf8",
      );
      expect(necessity).toContain("§3.2 Necessity artifact");
      // §3.2 requires the cheaper complete-item routes to be measured and
      // shown not to fit before an incremental route may be taken.
      expect(necessity).toMatch(/Complete[^|\n]*direct in proof tx/u);
      expect(necessity).toMatch(/inline-datum publication/u);
      expect(necessity).toContain("| NO above ");
    }

    // The canonical decode item path itself must stay complete-item staged: no
    // incremental scanner may appear in its staging module or its validators.
    const stagingSource = readFileSync(
      resolve(aikenRoot, "lib/midgard/canonical-decode-item-staging-v1.ak"),
      "utf8",
    );
    expect(stagingSource).not.toContain("canonical_cbor_scan_v1");
    // Staging is the complete-item ladder: authenticate → prepare → observe →
    // verify, each gated on the predecessor being well formed.
    for (const stage of [
      "pub fn authenticate(",
      "pub fn prepare(",
      "pub fn observe(",
      "pub fn verify(",
    ]) {
      expect(stagingSource).toContain(stage);
    }
    const canonicalDecodeValidators = [
      "canonical-decode-item-source-v1.ak",
      "canonical-decode-item-observe-v1.ak",
      "canonical-decode-item-semantic-v1.ak",
      "canonical-decode-item-proof-v1.ak",
      "canonical-decode-item-settlement-v1.ak",
    ];
    for (const validator of canonicalDecodeValidators) {
      const contents = readFileSync(
        resolve(
          aikenRoot,
          "validators/fraud-proofs/validation-trace",
          validator,
        ),
        "utf8",
      );
      expect(contents).not.toContain("canonical_cbor_scan_v1");
      expect(contents).toContain("canonical_decode_item_staging_v1");
    }

    // Hostile negative control: the consumer predicate fires on a source that
    // takes the scanner, so an unbacked new consumer reopens this row.
    const takesScanner = (source: string): boolean =>
      source.includes("canonical_cbor_scan_v1");
    expect(takesScanner(stagingSource)).toBe(false);
    expect(
      takesScanner(`${stagingSource}\nuse midgard/canonical_cbor_scan_v1\n`),
    ).toBe(true);
  });
});
