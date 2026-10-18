import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";

import { type buildMidgardCanonicalCekProgram } from "../../src/cek-program.js";
import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  MidgardRedeemerTag,
  type ValidationMachineReplayInput,
} from "../../src/index.js";
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
} from "../validation-fixtures.js";
import { context } from "../validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

/** The consensus profile's per-transaction execution-unit cap, as exUnits. */
export const MIDGARD_TX_EX_UNITS_CAP = Object.freeze({
  memory: 16_500_000n,
  steps: 10_000_000_000n,
});

/**
 * The replay input of an accepted transaction that spends one output locked
 * by `program` as a PlutusV3 script, with one Spend redeemer declaring
 * `exUnits`, and pays it to one plain output.
 */
export const plutusV3SpendInput = async (
  program: ReturnType<typeof buildMidgardCanonicalCekProgram>,
  exUnits: { readonly memory: bigint; readonly steps: bigint } = {
    memory: 5_000_000n,
    steps: 1_000_000_000n,
  },
): Promise<ValidationMachineReplayInput> => {
  const spent = outRefFromByte(0x1d);
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const spentOutput = makeProtectedScriptOutput(
    hashScriptWitness(script),
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
        exUnits: [exUnits.memory, exUnits.steps],
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
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return {
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
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
  };
};
