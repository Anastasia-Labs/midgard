import {
  encodeMidgardCekProgramMaterialSidecar,
  MidgardRedeemerItemProofModes,
} from "@al-ft/midgard-core";
import {
  hashMidgardRedeemerItemLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core/script-proof";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  MidgardRedeemerTag,
  type ValidationMachineWorkWitness,
} from "../src/index.js";
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
} from "./validation-fixtures.js";
import {
  buildAcceptingIdentityProgram,
  context,
  validateBoundaryAbiAndCollectAuxiliaryKinds,
} from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

// Each context's redeemer fold walks the execution frontier from the highest
// purpose down: a Plutus execution is selected, a native one is skipped, and
// the execution leaf at the frontier names the only step either may take.

type Auxiliary = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;
type Select = Extract<Auxiliary, { kind: "cekRedeemerContextSelect" }>;
type Skip = Extract<Auxiliary, { kind: "cekRedeemerContextSkip" }>;
type ItemStep = Extract<Auxiliary, { kind: "redeemerItemStep" }>;

const EX_UNITS = [1_000_000_000n, 1_000_000_000n] as const;

const acceptedTrace = async ({
  spent,
  spentOutput,
  output,
  transaction,
  sidecar,
}: {
  readonly spent: Buffer;
  readonly spentOutput: Buffer;
  readonly output: Buffer;
  readonly transaction: ReturnType<typeof makeNativeTx>;
  readonly sidecar: Buffer;
}): Promise<DeterministicValidationMachineTrace> => {
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    buildValidationMachineLedgerInsertOp({
      key: outRefFromTxId(transaction.txId),
      outputCbor: output,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...context,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      programMaterialSidecarCbor: sidecar,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

/** The fold steps of every context, in trace order. */
const foldSteps = (
  trace: DeterministicValidationMachineTrace,
): readonly { readonly index: number; readonly step: Select | Skip }[] =>
  trace.witnesses.flatMap((witness, index) =>
    witness.auxiliary?.kind === "cekRedeemerContextSelect" ||
    witness.auxiliary?.kind === "cekRedeemerContextSkip"
      ? [{ index, step: witness.auxiliary }]
      : [],
  );

const selectExecutionLeaf = (select: Select): Buffer =>
  hashMidgardScriptExecutionLeaf({
    languageTag: select.executionLanguageTag,
    purposeLeaf: hashMidgardScriptPurposeLeaf(select.purpose),
    sourceLeaf: select.sourceLeaf,
    redeemerLeaf: hashMidgardRedeemerItemLeaf({
      redeemerIndex: select.itemIndex,
      itemCommitment: select.itemCommitment,
    }),
  });

const skipExecutionLeaf = (skip: Skip): Buffer =>
  hashMidgardScriptExecutionLeaf({
    languageTag: 0,
    purposeLeaf: skip.purposeLeaf,
    sourceLeaf: skip.sourceLeaf,
  });

/** The redeemer item step that opens the item `index` selected. */
const firstItemStepAfter = (
  trace: DeterministicValidationMachineTrace,
  index: number,
): ItemStep => {
  const step = trace.witnesses[index + 1]?.auxiliary;
  if (step?.kind !== "redeemerItemStep")
    throw new Error("a redeemer select must open its item next");
  return step;
};

describe("redeemer fold over the execution frontier", () => {
  it("skips a native execution and selects the Plutus one below it", async () => {
    const spent = outRefFromByte(0x6a);
    const program = buildAcceptingIdentityProgram();
    const plutus = plutusV3ScriptWitness(program.envelopeCbor);
    const native = nativeScriptWitness({ type: "all", scripts: [] });
    const nativePolicy = Buffer.from(hashScriptWitness(native), "hex");
    const assetName = Buffer.from("aced", "hex");
    const spentOutput = makeProtectedScriptOutput(
      hashScriptWitness(plutus),
      FUNDED_OUTPUT_LOVELACE,
    );
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE,
      undefined,
      new Map([
        [
          nativePolicy.toString("hex"),
          new Map([[assetName.toString("hex"), 5n]]),
        ],
      ]),
    );
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [plutus, native],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[nativePolicy, new Map([[assetName, 5n]])]]),
      ),
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n, exUnits: EX_UNITS },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    const trace = await acceptedTrace({
      spent,
      spentOutput,
      output,
      transaction,
      sidecar: encodeMidgardCekProgramMaterialSidecar([
        ...program.material.values(),
      ]),
    });
    expect(trace.verdict).toBe("accepted");

    // One context, two executions: the native mint above the spend.
    const steps = foldSteps(trace).map(({ step }) => step);
    expect(steps.map((step) => step.kind)).toEqual([
      "cekRedeemerContextSkip",
      "cekRedeemerContextSelect",
    ]);
    expect(steps.map((step) => step.control.purposeBound)).toEqual([2, 1]);
    const [skip, select] = steps as [Skip, Select];
    expect(select.purpose.purposeKind).toBe(0);
    expect(select.executionLanguageTag).toBe(3);
    // Each step's siblings open the other execution: the frontier leaf a
    // step names is the one the other step's siblings commit.
    expect(skip.executionSiblings[0]).toEqual(selectExecutionLeaf(select));
    expect(select.executionSiblings[0]).toEqual(skipExecutionLeaf(skip));
    expect([
      ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
    ]).toEqual(
      expect.arrayContaining([
        "cekRedeemerContextSkip",
        "cekRedeemerContextSelect",
      ]),
    );
  }, 60_000);

  it("shows a MidgardV1 receive redeemer to MidgardV1 contexts only", async () => {
    const spent = outRefFromByte(0x6b);
    const program = buildAcceptingIdentityProgram();
    const plutus = plutusV3ScriptWitness(program.envelopeCbor);
    const receiving = {
      language: "MidgardV1" as const,
      scriptBytes: program.envelopeCbor,
    };
    const spentOutput = makeProtectedScriptOutput(
      hashScriptWitness(plutus),
      FUNDED_OUTPUT_LOVELACE,
    );
    const output = makeProtectedScriptOutput(
      hashScriptWitness(receiving),
      FUNDED_OUTPUT_LOVELACE,
    );
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [plutus, receiving],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Receiving, index: 0n, exUnits: EX_UNITS },
        { tag: MidgardRedeemerTag.Spend, index: 0n, exUnits: EX_UNITS },
      ]),
      scriptLanguages: ["PlutusV3", "MidgardV1"],
    });
    const trace = await acceptedTrace({
      spent,
      spentOutput,
      output,
      transaction,
      sidecar: encodeMidgardCekProgramMaterialSidecar([
        ...program.material.values(),
      ]),
    });
    expect(trace.verdict).toBe("accepted");

    // Two contexts, each folding both executions from the receive (the
    // higher purpose) down to the spend. The spend's context is PlutusV3 and
    // ends in `cekContextFinalizeSpend`; the receiving script's context is
    // MidgardV1 and ends in `cekContextFinalize`.
    const folds = foldSteps(trace).map(({ index, step }) => {
      const finalize = trace.witnesses
        .slice(index)
        .find(
          (witness) =>
            witness.auxiliary?.kind === "cekContextFinalizeSpend" ||
            witness.auxiliary?.kind === "cekContextFinalize",
        )?.auxiliary?.kind;
      if (step.kind !== "cekRedeemerContextSelect")
        throw new Error("every execution here is a Plutus execution");
      return {
        context:
          finalize === "cekContextFinalizeSpend" ? "PlutusV3" : "MidgardV1",
        purposeKind: step.purpose.purposeKind,
        languageTag: step.executionLanguageTag,
        purposeBound: step.control.purposeBound,
        mode: firstItemStepAfter(trace, index).control.mode,
      };
    });
    // A PlutusV3 context passes the receive redeemer as a descriptor, so it
    // never enters that context's redeemer map; the MidgardV1 context reads
    // it as data and keeps it.
    expect(folds).toEqual([
      {
        context: "PlutusV3",
        purposeKind: 3,
        languageTag: 128,
        purposeBound: 2,
        mode: MidgardRedeemerItemProofModes.Descriptor,
      },
      {
        context: "PlutusV3",
        purposeKind: 0,
        languageTag: 3,
        purposeBound: 1,
        mode: MidgardRedeemerItemProofModes.Data,
      },
      {
        context: "MidgardV1",
        purposeKind: 3,
        languageTag: 128,
        purposeBound: 2,
        mode: MidgardRedeemerItemProofModes.Data,
      },
      {
        context: "MidgardV1",
        purposeKind: 0,
        languageTag: 3,
        purposeBound: 1,
        mode: MidgardRedeemerItemProofModes.Data,
      },
    ]);
    expect([
      ...validateBoundaryAbiAndCollectAuxiliaryKinds(trace).kinds,
    ]).toEqual(expect.arrayContaining(["cekRedeemerContextSelect"]));
  }, 120_000);
});
