import { readdirSync, readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardTxOutput,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import {
  decodeSingleCbor,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import {
  computeMidgardForcedTxProofCommitment,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardResolvedInputsAccumulator,
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
  encodeValidationAuxiliaryWitnessCbor,
  initialMidgardResolvedInputsAccumulator,
  MidgardRedeemerTag,
  RejectCodes,
  validateCekRouteMaterial,
  validationSemanticResolverIndex,
  valueAndMintKind,
} from "../src/index.js";
import { exerciseMidgardRetainedDaCanonicalBoundary } from "./helpers/retained-da-boundary.js";
import {
  FUNDED_OUTPUT_LOVELACE,
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
import {
  expectCekAndValueAndMintTotality,
  root,
} from "./validation-machine.semantic-resolver-definitions.js";
import {
  canonicalDecodeFieldIndex,
  collectMintFoldWitnesses,
  expectedFieldPlanInput,
  stepFieldPreimage,
} from "./validation-machine.v1-purpose-kind-to-redeemer-pointer-mapping.js";
import {
  buildAcceptingIdentityProgram,
  buildNonterminatingSelfApplicationProgram,
  context,
  validateBoundaryAbiAndCollectAuxiliaryKinds,
} from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

describe("deterministic validation machine", { timeout: 60_000 }, () => {
  it("replays an accepted transaction through bounded field-reveal instructions", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOp({
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
      // Each family's output-proof finalize is now three descriptor
      // fact-attach steps plus a thin terminal, so both membership phases
      // grew by three steps (this trace's span window is empty, so no
      // span-attach step appears).
      ...Array<string>(14).fill("resolveInputs"),
      ...Array<string>(25).fill("scriptSources"),
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
            witness.cbor.length + stepFieldPreimage(witness.auxiliary).length <
            16 * 1024
          );
        }
        return false;
      }),
    ).toBe(true);
    const scriptSourceWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "scriptSources",
    );
    expect(scriptSourceWitnesses).toHaveLength(25);
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
    // A native-only transaction still walks the cek index: the single
    // finish step hands off to ValueAndMint, which then runs every stage.
    expect(expectCekAndValueAndMintTotality(trace)).toEqual({
      cekKinds: ["finish"],
      valueAndMintKinds: [
        "begin",
        "replayBegin",
        "replayInput",
        "replayFinish",
        "outputDescriptor",
        "outputFinish",
        "mintFinish",
        "finalize",
      ],
    });
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
    // The output-proof finalize is now three descriptor fact-attach steps
    // followed by a thin terminal; all four ride the finalize witness shape.
    expect(
      scriptSourceWitnesses
        .slice(15, 19)
        .every(
          (witness) => witness.auxiliary?.kind === "ledgerOutputProofFinalize",
        ),
    ).toBe(true);
    expect(scriptSourceWitnesses[19]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[20]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[21]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[22]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[23]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses[24]?.auxiliary).toBeNull();
    expect(scriptSourceWitnesses.map(validationSemanticResolverIndex)).toEqual([
      6, 14, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 0, 27, 23,
      16, 18,
    ]);
    expect(() =>
      validationSemanticResolverIndex({
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
          return !stepFieldPreimage(witness.auxiliary).includes(
            transaction.txCbor,
          );
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
      computeMidgardNativeTxProofCommitment(
        deriveMidgardNativeTxProofSource(transaction.tx),
      ),
    );
    expect(
      trace.tree.proofs.every((proof) =>
        verifyMidgardValidationTraceProof({
          descriptor: trace.tree.descriptor,
          proof,
        }),
      ),
    ).toBe(true);
    const oneStepAbi = validateBoundaryAbiAndCollectAuxiliaryKinds(trace);
    expect(oneStepAbi.kinds.size).toBeGreaterThanOrEqual(8);
    expect(oneStepAbi.maxArgumentsBytes).toBeLessThan(16 * 1024);
  });

  it.each([
    { label: "foreign raw network nibble 2", rawNibble: 2 },
    { label: "protected foreign raw network nibble 15", rawNibble: 15 },
  ])(
    "rejects a $label at the authenticated output-finalize instruction",
    async ({ rawNibble }) => {
      const spent = outRefFromByte(0x7d);
      const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
      const foreignAddress = Buffer.from(TEST_ADDRESS_BYTES);
      foreignAddress[0] = (foreignAddress[0]! & 0xf0) | rawNibble;
      const output = makeOutput(FUNDED_OUTPUT_LOVELACE, foreignAddress);
      const transaction = makeNativeTx({
        version: 1n,
        spendInputs: [spent],
        outputs: [output],
      });
      const unchangedRoot = root(3);
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
          expectedRejectionCode: RejectCodes.NetworkIdMismatch,
        }),
      );

      expect(trace.verdict).toBe("rejected");
      expect(trace.rejectionCode).toBe(RejectCodes.NetworkIdMismatch);
      expect(
        trace.witnesses
          .filter((witness) => witness.phase === "scriptSources")
          .at(-1),
      ).toMatchObject({
        phase: "scriptSources",
        auxiliary: { kind: "ledgerOutputProofFinalize" },
      });
    },
    60_000,
  );

  it("matches the L1 resolved-input accumulator vector", () => {
    const initial = initialMidgardResolvedInputsAccumulator();
    expect(initial.toString("hex")).toBe(
      "07eb401e2f7e5de17444414ec48a5d9dca455dea72f4675cc2b08bf5b4e39979",
    );
    expect(
      advanceMidgardResolvedInputsAccumulator({
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
    const spentOutput = makeProtectedScriptOutput(
      scriptHash,
      FUNDED_OUTPUT_LOVELACE,
    );
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
      buildValidationMachineLedgerInsertOp({
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
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
    const totality = expectCekAndValueAndMintTotality(trace);
    expect(totality.cekKinds[0]).toBe("selection");
    expect(totality.cekKinds).toContain("context");
    expect(totality.cekKinds).toContain("core");
    // The last core step claims the ValueAndMint successor itself
    // (`verify_cek_core_step`, `next_cursor == control.execution_count`), so
    // a Plutus trace has no stand-alone `finish` step.
    expect(totality.cekKinds.at(-1)).toBe("core");
    expect(totality.cekKinds).not.toContain("finish");
    const scriptSourceWitnesses = trace.witnesses.filter(
      (witness) => witness.phase === "scriptSources",
    );
    expect(scriptSourceWitnesses[0]?.auxiliary).toMatchObject({
      kind: "transactionFieldChunk",
      itemIndex: 0,
      ...expectedFieldPlanInput(transaction.txCbor, 6),
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
      ...expectedFieldPlanInput(transaction.txCbor, 8),
    });
    expect(validationSemanticResolverIndex(redeemerSourceWitness!)).toBe(15);
    expect(
      scriptSourceWitnesses.some(
        (witness) => validationSemanticResolverIndex(witness) === 14,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "scriptSourceScan" &&
          validationSemanticResolverIndex(witness) === 17,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerScanBegin" &&
          validationSemanticResolverIndex(witness) === 19,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerScanBegin" &&
          validationSemanticResolverIndex(witness) === 21,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "redeemerItemStep" &&
          validationSemanticResolverIndex(witness) === 22,
      ),
    ).toBe(true);
    expect(
      scriptSourceWitnesses.some(
        (witness) =>
          witness.auxiliary?.kind === "scriptPurposeScan" &&
          validationSemanticResolverIndex(witness) === 24,
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
    const selectionArgument = buildValidationOneStepArgument({
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
      buildValidationOneStepArgument({
        trace,
        stateIndex: laterCekStateIndex,
      }).cekRouteMaterial,
    ).toBeUndefined();
    const nonCekStateIndex = trace.witnesses.findIndex(
      (witness) => witness.phase !== "cek",
    );
    expect(
      buildValidationOneStepArgument({
        trace,
        stateIndex: nonCekStateIndex,
      }).cekRouteMaterial,
    ).toBeUndefined();

    const routeMaterial = selectionArgument.cekRouteMaterial!;
    const validateRouteMaterial = (value: unknown) =>
      validateCekRouteMaterial({
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
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
          encodeValidationAuxiliaryWitnessCbor(auxiliary).length < 16 * 1024,
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
    const spentOutput = makeProtectedScriptOutput(
      scriptHash,
      FUNDED_OUTPUT_LOVELACE,
    );
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
        buildValidationMachineLedgerInsertOp({
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
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
    const spentOutput = makeProtectedScriptOutput(
      scriptHash,
      FUNDED_OUTPUT_LOVELACE,
    );
    const referenceOutput = encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: 1n, assets: new Map() },
      script_ref: script,
    });
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
      buildValidationMachineLedgerInsertOp({
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
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
    expect(validationSemanticResolverIndex(referenceSource)).toBe(12);
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
        quantity > 0n
          ? makeOutput(FUNDED_OUTPUT_LOVELACE)
          : makeOutput(FUNDED_OUTPUT_LOVELACE, undefined, assets);
      const output =
        quantity > 0n
          ? makeOutput(FUNDED_OUTPUT_LOVELACE, undefined, assets)
          : makeOutput(FUNDED_OUTPUT_LOVELACE);
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
        buildValidationMachineLedgerInsertOp({
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
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
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
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const program = buildAcceptingIdentityProgram();
    const script = {
      language: "MidgardV1" as const,
      scriptBytes: program.envelopeCbor,
    };
    const scriptHash = hashScriptWitness(script);
    const output = makeProtectedScriptOutput(
      scriptHash,
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
          exUnits: [1_000_000_000n, 1_000_000_000n],
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOp({
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
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
          validationSemanticResolverIndex(witness) === 26,
      ),
    ).toBe(true);
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.phase === "scriptSources" &&
          validationSemanticResolverIndex(witness) === 27,
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
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
      buildValidationMachineLedgerInsertOp({
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
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
      itemIndex: 0,
      ...expectedFieldPlanInput(transaction.txCbor, 3),
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
    expect(preconditionWitnesses.map(validationSemanticResolverIndex)).toEqual([
      1, 0,
    ]);
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.phase === "scriptSources" &&
          witness.auxiliary?.kind === "transactionFieldChunk" &&
          witness.auxiliary.fieldIndex === 3 &&
          validationSemanticResolverIndex(witness) === 25,
      ),
    ).toBe(true);
    expect(
      trace.witnesses.some(
        (witness) =>
          witness.phase === "scriptSources" &&
          validationSemanticResolverIndex(witness) === 27,
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
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
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
    expect(preconditionWitnesses.map(validationSemanticResolverIndex)).toEqual([
      1, 1,
    ]);
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
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
      FUNDED_OUTPUT_LOVELACE,
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
      buildValidationMachineLedgerInsertOp({
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

    const mintFoldWitnesses = collectMintFoldWitnesses(trace);
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
            stepFieldPreimage(witness).length <=
            MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES
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
      ...expectedFieldPlanInput(transaction.txCbor, 6),
    });
    expect(
      trace.witnesses
        .filter((witness) => witness.phase === "phaseANativeScripts")
        .map(validationSemanticResolverIndex),
    ).toEqual([1, 2, 3, 2, 8, 13, 0, 2, 3, 2, 8, 13, 0]);
    const nativeSource = trace.witnesses.find(
      (witness) =>
        witness.auxiliary?.kind === "scriptSourceScan" &&
        witness.auxiliary.scriptLanguageTag === 0,
    );
    expect(nativeSource).toBeDefined();
    expect(validationSemanticResolverIndex(nativeSource!)).toBe(11);
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
      FUNDED_OUTPUT_LOVELACE,
      undefined,
      new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 5n]])],
      ]),
    );
    const burnedOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
      buildValidationMachineLedgerInsertOp({
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

    const burnFoldWitnesses = collectMintFoldWitnesses(trace);
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
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
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

    const mintFoldWitnesses = collectMintFoldWitnesses(trace);
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
            stepFieldPreimage(witness).length <=
            MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES
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
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
        canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
          materializeMidgardForcedTxFromCanonical(transaction.tx),
        ),
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
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      requiredSignerItems: [Buffer.from(TEST_SIGNER_HASH, "hex")],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOp({
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
      ...expectedFieldPlanInput(transaction.txCbor, 7),
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
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
        canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
          materializeMidgardForcedTxFromCanonical(transaction.tx),
        ),
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
      ...expectedFieldPlanInput(transaction.txCbor, 7),
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
          outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
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
          outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
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
          outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
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
          canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
            materializeMidgardForcedTxFromCanonical(transaction.tx),
          ),
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
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
        canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
          materializeMidgardForcedTxFromCanonical(transaction.tx),
        ),
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
          canonicalDecodeFieldIndex(witness) === 2,
      )
      .map((witness) => witness.auxiliary!);
    expect(canonicalOutputItems).toHaveLength(outputs.length);
    expect(
      canonicalOutputItems.every(
        (auxiliary) =>
          auxiliary.kind === "transactionFieldItem" &&
          stepFieldPreimage(auxiliary).equals(outputsPreimage),
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
        stepFieldPreimage(witness.auxiliary).equals(outputsPreimage)
          ? [witness.auxiliary]
          : [],
      );
    expect(outputItems).toHaveLength(outputs.length);
    expect(trace.verdict).toBe("rejected");
  }, 60_000);

  // E_MIN_ADA / MIN-ADA-TX (#618 ruling 1; R8 of decision 0005). The TypeScript
  // twin of the ValueAndMint stage-three output-descriptor conviction. The
  // transaction below is otherwise impeccable -- it is signed, and it preserves
  // value exactly (10 lovelace in, 10 lovelace out, zero fee) -- so the ONLY
  // thing this vector can be measuring is the minimum-Ada floor.
  //
  // The spent input is under-funded too and that is deliberate: the wiring
  // gates outputs a transaction PRODUCES, not outputs it resolves from prior
  // state. If it gated resolved inputs as well, this trace would stop at
  // stage two and the descriptor assertions below would fail.
  it("rejects an under-funded produced output with E_MIN_ADA at the ValueAndMint output-descriptor step", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(10n);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
    });
    const unchangedRoot = root(3);
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: unchangedRoot,
        postUtxosRoot: unchangedRoot,
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.MinAda,
      }),
    );

    expect(trace.verdict).toBe("rejected");
    expect(trace.rejectionCode).toBe(RejectCodes.MinAda);
    expect(trace.states.at(-1)).toMatchObject({
      phase: "terminal",
      verdict: "rejected",
    });

    // MEASURED, NOT ASSUMED: the step the rejecting terminal succeeds is the
    // output-descriptor step of output 0, and an L1 dispute of that step routes
    // to semantic resolver index 5 --
    // `value_and_mint_output_descriptor_semantic_v1`. That is what makes the
    // new rejection reachable through a real fault proof rather than only
    // through the aggregate resolver.
    const convicting = trace.witnesses.at(-2)!;
    expect(trace.witnesses.at(-1)!.phase).toBe("terminal");
    expect(convicting.phase).toBe("valueAndMint");
    expect(valueAndMintKind(convicting)).toBe("outputDescriptor");
    expect(validationSemanticResolverIndex(convicting)).toBe(5);
    expect(convicting.auxiliary).toMatchObject({
      kind: "valueOutputDescriptor",
      outputIndex: 0,
    });

    // NO FALSE CONVICTION. The floor is not a blanket refusal of this
    // transaction shape: the same shape, funded, is accepted end to end and
    // takes the very output-descriptor step that convicted above. Without this
    // leg the assertions above would also hold for a wiring that rejected
    // every output.
    const fundedOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const fundedTransaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [fundedOutput],
    });
    const fundedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(fundedTransaction.txId),
        outputCbor: fundedOutput,
      }),
    ];
    const fundedMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output: fundedOutput }],
        operations: fundedLedgerOps,
      },
    );
    const fundedTrace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...context,
        transactionId: fundedTransaction.txId,
        canonicalTransactionCbor: fundedTransaction.txCbor,
        priorUtxosRoot: fundedMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: fundedMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: fundedOutput }],
        expectedLedgerOps: fundedLedgerOps,
        ledgerMutationSteps: fundedMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );
    expect(fundedTrace.verdict).toBe("accepted");
    expect(fundedTrace.rejectionCode).toBeNull();
    expect(
      fundedTrace.witnesses.some(
        (witness) =>
          witness.phase === "valueAndMint" &&
          valueAndMintKind(witness) === "outputDescriptor",
      ),
    ).toBe(true);
  });

  it("fails closed before proving a malformed persisted ledger output", async () => {
    const spent = outRefFromByte(0x11);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    });
    const unchangedRoot = root(6);
    await expect(
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...context,
          sourceKind: "forced",
          transactionId: transaction.txId,
          canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
            materializeMidgardForcedTxFromCanonical(transaction.tx),
          ),
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
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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

  it("refuses ledger changes claimed by a rejected transaction", async () => {
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    });
    const input = {
      ...context,
      sourceKind: "forced" as const,
      transactionId: transaction.txId,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical(transaction.tx),
      ),
      priorUtxosRoot: root(3),
      postUtxosRoot: root(3),
      ledgerWitnessEntries: [],
      expectedLedgerOps: [],
      expectedVerdict: "rejected" as const,
      expectedRejectionCode: RejectCodes.EmptyInputs,
    };
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace(input),
    );
    expect(trace.verdict).toBe("rejected");
    // Definite [2, h'E_EMPTY_INPUTS', prior root, h'80']: the final bytes
    // encode an empty operation list, not an accepted-delta frontier.
    expect(trace.witnesses.at(-1)!.cbor.toString("hex")).toBe(
      "84024e455f454d5054595f494e505554535820" + "03".repeat(32) + "4180",
    );

    await expect(
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...input,
          expectedLedgerOps: [{ type: "delete", key: outRefFromByte(0x11) }],
        }),
      ),
    ).rejects.toThrow(
      "validation replay ledger delta differs from block transition",
    );
    await expect(
      Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...input,
          postUtxosRoot: root(4),
        }),
      ),
    ).rejects.toThrow(
      "validation replay ledger-mutation terminal root differs from the block transition",
    );
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

  const buildMaximumRetainedCanonicalSource = () => {
    const spent = outRefFromByte(0x12);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
    const fixture = buildMaximumRetainedCanonicalSource();

    // Each source kind retains and reconstructs its own canonical envelope.
    const retained = await exerciseMidgardRetainedDaCanonicalBoundary({
      canonicalTransactionCbor: fixture.transaction.txCbor,
    });
    expect(retained.normal.retainedPreimageBytes).toBeGreaterThan(8 * 1024);
    expect(retained.normal.retainedPreimageDigestHex).not.toBe(
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
        measurement.sourceKind === "forced"
          ? retained.forcedTransactionCommitmentHex
          : retained.transactionCommitmentHex,
      );
    }
    expect(retained.normal.revealStepCount).toBe(
      retained.forced.revealStepCount,
    );
    expect(retained.normal.revealStepCount).toBeGreaterThan(0);

    // Both sources bind the same immutable body and witnesses. Their outer
    // encodings and commitment domains differ by authenticated source kind.
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
          canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
            materializeMidgardForcedTxFromCanonical(fixture.transaction.tx),
          ),
          sourceKind: "forced",
        }),
      ),
    ]);
    const normalTranscript = canonicalDecodeWorkTranscript(normalTrace);
    const forcedTranscript = canonicalDecodeWorkTranscript(forcedTrace);
    expect(normalTranscript.length).toBeGreaterThan(fixture.outputCount);
    expect(forcedTranscript.length).toBe(normalTranscript.length);
    // The transcript authenticates each source encoding.
    expect(forcedTranscript).not.toEqual(normalTranscript);
    // The machine's WORK is nonetheless identical: same step structure, same
    // per-step positions, same verdict and rejection code, and byte-identical
    // terminal work witnesses. Only the source binding moved.
    expect(
      forcedTrace.witnesses.map((witness) => [
        witness.phase,
        witness.programCounter,
      ]),
    ).toEqual(
      normalTrace.witnesses.map((witness) => [
        witness.phase,
        witness.programCounter,
      ]),
    );
    expect(forcedTrace.verdict).toBe(normalTrace.verdict);
    expect(forcedTrace.rejectionCode).toBe(normalTrace.rejectionCode);
    expect(forcedTrace.witnesses.at(-1)!.cbor.toString("hex")).toBe(
      normalTrace.witnesses.at(-1)!.cbor.toString("hex"),
    );
    expect(normalTrace.validationContextCbor.toString("hex")).toBe(
      forcedTrace.validationContextCbor.toString("hex"),
    );
    const adjudicatedTx = materializeMidgardForcedTxFromCanonical(
      decodeMidgardNativeTxFullFromCanonicalCbor(fixture.transaction.txCbor),
    );
    // Each state binds its source-specific commitment domain.
    expect(forcedTrace.states[0]!.transactionCommitment.toString("hex")).toBe(
      computeMidgardForcedTxProofCommitment(
        deriveMidgardForcedTxProofSource(adjudicatedTx),
      ).toString("hex"),
    );
    expect(normalTrace.states[0]!.transactionCommitment.toString("hex")).toBe(
      computeMidgardNativeTxProofCommitment(
        deriveMidgardNativeTxProofSource(
          decodeMidgardNativeTxFullFromCanonicalCbor(
            fixture.transaction.txCbor,
          ),
        ),
      ).toString("hex"),
    );
    expect(
      forcedTrace.states[0]!.transactionCommitment.equals(
        normalTrace.states[0]!.transactionCommitment,
      ),
    ).toBe(false);
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
      canonicalDecodeFieldIndex(witness) === 2
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
    const fixture = buildMaximumRetainedCanonicalSource();
    const canonical = Buffer.from(fixture.transaction.txCbor);

    // The pristine canonical source decodes to the authenticated identity.
    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(canonical);
    expect(computeMidgardNativeTxId(decoded).toString("hex")).toBe(
      Buffer.from(fixture.transaction.txId).toString("hex"),
    );

    // Malformed: the last byte of the definite-length encoding is missing.
    expect(() =>
      decodeMidgardNativeTxFullFromCanonicalCbor(canonical.subarray(0, -1)),
    ).toThrow();

    // Trailing: one extra byte after the complete top-level item.
    expect(() =>
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.concat([canonical, Buffer.from([0x00])]),
      ),
    ).toThrow();

    // Noncanonical: the top-level array count re-encoded in non-minimal form.
    const head = canonical[0]!;
    expect(head).toBeGreaterThanOrEqual(0x80);
    expect(head).toBeLessThan(0x98);
    expect(() =>
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.concat([
          Buffer.from([0x98, head - 0x80]),
          canonical.subarray(1),
        ]),
      ),
    ).toThrow();

    // Indefinite-length top-level array is not a canonical V1 source either.
    expect(() =>
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.concat([
          Buffer.from([0x9f]),
          canonical.subarray(1),
          Buffer.from([0xff]),
        ]),
      ),
    ).toThrow();
  });

  it("confines the incremental CBOR scanner to the reviewed on-chain consumers", () => {
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
    // Consumption means importing the module: an Aiken source cannot call the
    // scanner without a top-level `use midgard/canonical_cbor_scan_v1` line.
    // The earlier bare-substring predicate also fired on doc-comment
    // cross-references (intra-item-bytes-v1.ak names the scanner only to
    // explain how the two readers divide §11), which is not consumption.
    const importsScanner = (source: string): boolean =>
      /^\s*use\s+midgard\/canonical_cbor_scan_v1\b/mu.test(source);
    const scannerConsumers = aikenSources
      .filter((path) => {
        const relative = path.slice(aikenRoot.length + 1);
        if (relative === "lib/midgard/canonical-cbor-scan-v1.ak") {
          return false;
        }
        return importsScanner(readFileSync(path, "utf8"));
      })
      .map((path) => path.slice(aikenRoot.length + 1))
      .sort();

    // The reviewed consumer set. Adding a scanner import anywhere else reopens
    // this row; the scanner's justification lives in the on-chain modules and
    // their emulator suites, not in a document this gate would have to parse.
    const reviewedScannerConsumers = [
      "lib/midgard/redeemer-item-proof-v1.ak",
      "lib/midgard/ledger-output-scan-v1.ak",
      // The three narrow total rules adjudicating the committed field-8
      // redeemer collection share one `head_at_v1` total header decode over
      // complete items delivered by batched §8 carriage.
      "lib/midgard/fraud-proofs/mint-item-non-canonical/field-scan.ak",
      "lib/midgard/fraud-proofs/mint-item-non-canonical/scan.ak",
      "lib/midgard/fraud-proofs/missing-redeemer/rule.ak",
      "lib/midgard/fraud-proofs/redeemer-canonicity/rule.ak",
      "lib/midgard/fraud-proofs/unused-redeemer/rule.ak",
    ];
    expect(scannerConsumers).toEqual([...reviewedScannerConsumers].sort());

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

    // Hostile negative controls: the consumer predicate fires on a source
    // that imports the scanner (plain, aliased, or with unqualified names),
    // so an unbacked new consumer reopens this row — while a doc-comment
    // mention alone stays outside it.
    expect(importsScanner(stagingSource)).toBe(false);
    expect(
      importsScanner(`${stagingSource}\nuse midgard/canonical_cbor_scan_v1\n`),
    ).toBe(true);
    expect(importsScanner("use midgard/canonical_cbor_scan_v1 as scan\n")).toBe(
      true,
    );
    expect(
      importsScanner("use midgard/canonical_cbor_scan_v1.{scan_head}\n"),
    ).toBe(true);
    expect(
      importsScanner("//// see `midgard/canonical_cbor_scan_v1` for scans\n"),
    ).toBe(false);
  });
});
