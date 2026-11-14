import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Constr } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  midgardEnvelopeDataFromCborIterative,
  MidgardRedeemerTag,
  summarizeMidgardCekLucidData,
  type ValidationMachineWorkWitness,
} from "../src/index.js";
import { buildDeterministicValidationMachineTrace } from "./helpers/trace-commit-golden.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";
import {
  buildAcceptingUnitProgram,
  context,
} from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

// Every Plutus execution's context ends in a stage-10 finalize step. The
// finalize authenticate routes the step by the staged context control:
// `route = language_tag == 128 ? 4 : purpose_kind` into the finalize
// resolvers [spend, mint, withdraw, observe, midgard]. A PlutusV3 mint and a
// PlutusV3 required observer (a Cardano withdraw purpose, redeemer tag
// Reward) each reach that step on an honest trace, routed to the mint and
// withdraw resolvers, and the script info the context assembles from is the
// Cardano purpose summary those resolvers derive on chain
// (`script_purpose_summary_v1(kind, script_hash, subject, False)`).

type Auxiliary = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;
type Assemble = Extract<Auxiliary, { kind: "cekContextAssemble" }>;

const EX_UNITS = [5_000_000n, 1_000_000_000n] as const;
const FINALIZE_ROUTES = ["spend", "mint", "withdraw", "observe", "midgard"];

type EnvelopeData = ReturnType<typeof midgardEnvelopeDataFromCborIterative>;

const listOf = (data: EnvelopeData): readonly EnvelopeData[] => {
  if (!("list" in data)) throw new Error("expected an envelope list");
  return data.list;
};
const intOf = (data: EnvelopeData | undefined): number => {
  if (data === undefined || !("int" in data))
    throw new Error("expected an envelope integer");
  return Number(data.int);
};
const bytesOf = (data: EnvelopeData | undefined): Uint8Array => {
  if (data === undefined || !("bytes" in data))
    throw new Error("expected envelope bytes");
  return data.bytes;
};

/** The staged context control a CEK context witness carries, as the
 * finalize authenticate reads it: its stage, language tag and purpose. */
const stagedContext = (witness: ValidationMachineWorkWitness) => {
  const control = listOf(
    midgardEnvelopeDataFromCborIterative(
      bytesOf(listOf(midgardEnvelopeDataFromCborIterative(witness.cbor))[1]),
    ),
  );
  return {
    stage: intOf(control[0]),
    languageTag: intOf(control[1]),
    purposeKind: intOf(control[3]),
  };
};

const acceptedTrace = async (
  spent: Buffer,
  spentOutput: Buffer,
  output: Buffer,
  transaction: ReturnType<typeof makeNativeTx>,
): Promise<DeterministicValidationMachineTrace> => {
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
};

describe("CEK context finalize routes", () => {
  it("routes a PlutusV3 mint and a PlutusV3 required observer to the mint and withdraw finalize resolvers", async () => {
    const spent = outRefFromByte(0x6c);
    const plutus = plutusV3ScriptWitness(
      buildAcceptingUnitProgram().scriptBytes,
    );
    const scriptHash = hashScriptWitness(plutus);
    const assetName = Buffer.from("f1", "hex");
    const spentOutput = makeProtectedScriptOutput(
      scriptHash,
      FUNDED_OUTPUT_LOVELACE,
    );
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE,
      undefined,
      new Map([[scriptHash, new Map([[assetName.toString("hex"), 1n]])]]),
    );
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [plutus],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[Buffer.from(scriptHash, "hex"), new Map([[assetName, 1n]])]]),
      ),
      // A transaction that requires observers names its network.
      requiredObserverItems: [Buffer.from(scriptHash, "hex")],
      networkId: 0n,
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n, exUnits: EX_UNITS },
        { tag: MidgardRedeemerTag.Mint, index: 0n, exUnits: EX_UNITS },
        { tag: MidgardRedeemerTag.Reward, index: 0n, exUnits: EX_UNITS },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    const trace = await acceptedTrace(spent, spentOutput, output, transaction);
    expect(trace.verdict).toBe("accepted");

    const finalizes = trace.witnesses.flatMap((witness, index) => {
      const kind = witness.auxiliary?.kind;
      if (kind !== "cekContextFinalize" && kind !== "cekContextFinalizeSpend")
        return [];
      const staged = stagedContext(witness);
      const assemble = trace.witnesses[index + 1]?.auxiliary as
        | Assemble
        | undefined;
      if (assemble?.kind !== "cekContextAssemble")
        throw new Error("a finalize step is followed by the context assemble");
      return [
        {
          kind,
          stage: staged.stage,
          languageTag: staged.languageTag,
          route:
            FINALIZE_ROUTES[
              staged.languageTag === 128 ? 4 : staged.purposeKind
            ],
          scriptInfo: assemble.control.scriptInfo,
        },
      ];
    });

    expect(
      finalizes.map(({ kind, stage, languageTag, route }) => ({
        kind,
        stage,
        languageTag,
        route,
      })),
    ).toEqual([
      {
        kind: "cekContextFinalizeSpend",
        stage: 10,
        languageTag: 3,
        route: "spend",
      },
      { kind: "cekContextFinalize", stage: 10, languageTag: 3, route: "mint" },
      {
        kind: "cekContextFinalize",
        stage: 10,
        languageTag: 3,
        route: "withdraw",
      },
    ]);
    // What the mint and withdraw finalize resolvers derive on chain: the
    // Cardano `MintingScript policy` and `RewardingScript (ScriptCredential
    // hash)` script infos.
    expect(finalizes[1]!.scriptInfo).toEqual(
      summarizeMidgardCekLucidData(new Constr(0, [scriptHash])),
    );
    expect(finalizes[2]!.scriptInfo).toEqual(
      summarizeMidgardCekLucidData(
        new Constr(2, [new Constr(1, [scriptHash])]),
      ),
    );
  }, 120_000);
});
