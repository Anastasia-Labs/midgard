import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

describe("resolved-output reference-script sources", () => {
  it("uses a spent output as the only source for an accepted native mint", async () => {
    const spent = outRefFromByte(0x67);
    const script = nativeScriptWitness({ type: "all", scripts: [] });
    const policy = Buffer.from(hashScriptWitness(script), "hex");
    const name = Buffer.from("42", "hex");
    const spentOutput = encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
      script_ref: script,
    });
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE,
      undefined,
      new Map([
        [policy.toString("hex"), new Map([[name.toString("hex"), 5n]])],
      ]),
    );
    const transaction = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[policy, new Map([[name, 5n]])]]),
      ),
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
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
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
    expect(trace.verdict).toBe("accepted");
    const sources = trace.witnesses.flatMap(({ auxiliary }) =>
      auxiliary?.kind === "scriptSourceScan" ? [auxiliary] : [],
    );
    expect(sources.length).toBeGreaterThan(0);
    expect(
      sources.every(
        (source) =>
          source.originKind === "reference" && source.sourceKey.equals(spent),
      ),
    ).toBe(true);
    expect(
      trace.witnesses.some(
        ({ auxiliary }) =>
          auxiliary?.kind === "resolvedInputReplay" &&
          auxiliary.sourceKind === "spend",
      ),
    ).toBe(true);
  });
});
