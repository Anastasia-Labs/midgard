import {
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { EventKey } from "@al-ft/midgard-sdk";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyUTxOStatePatch,
  buildMidgardCanonicalCekProgram,
  MidgardRedeemerTag,
  RejectCodes,
  replayValidationMachineEvent,
  type ValidationMachineEventReplayInput,
  type ValidationMachineLedgerEntry,
  validationMachineLedgerRoot,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  type NativeTxFixture,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const replayInput = async (
  transaction: NativeTxFixture,
  entries: readonly ValidationMachineLedgerEntry[],
): Promise<ValidationMachineEventReplayInput> => ({
  ...context,
  sourceKind: "normal",
  eventKeyCbor: Buffer.from(
    Data.to(
      { L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") } },
      EventKey,
    ),
    "hex",
  ),
  canonicalTransactionCbor: transaction.txCbor,
  ledgerWitnessEntries: entries,
  priorUtxosRoot: (await validationMachineLedgerRoot(entries)).toString("hex"),
});

const plainTransfer = () => {
  const spent = outRefFromByte(0x11);
  const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
  return {
    spent,
    output,
    entries: [{ outRef: spent, output }],
    transaction: makeNativeTx({ spendInputs: [spent], outputs: [output] }),
  };
};

describe("independent validation event replay", () => {
  it("derives a transfer verdict, transaction identity, ledger patch and post root", async () => {
    const fixture = plainTransfer();
    const result = await Effect.runPromise(
      replayValidationMachineEvent(
        await replayInput(fixture.transaction, fixture.entries),
      ),
    );
    expect(result.replayInput.transactionId).toEqual(fixture.transaction.txId);
    expect(result.replayInput.expectedVerdict).toBe("accepted");
    expect(result.trace.verdict).toBe("accepted");
    expect(result.trace.rejectionCode).toBeNull();
    expect(result.statePatch).toEqual({
      deletedOutRefs: [fixture.spent.toString("hex")],
      upsertedOutRefs: [
        [
          outRefFromTxId(fixture.transaction.txId).toString("hex"),
          fixture.output,
        ],
      ],
    });
    const state = new Map(
      fixture.entries.map(({ outRef, output }) => [
        outRef.toString("hex"),
        output,
      ]),
    );
    applyUTxOStatePatch(state, result.statePatch);
    const expectedRoot = await validationMachineLedgerRoot(
      [...state].map(([outRef, output]) => ({
        outRef: Buffer.from(outRef, "hex"),
        output,
      })),
    );
    expect(result.replayInput.postUtxosRoot).toBe(expectedRoot.toString("hex"));
    expect(result.replayInput.ledgerMutationSteps).toHaveLength(2);
    expect(result.trace.states.at(-1)?.verdict).toBe("accepted");
  });

  it("independently executes an ordinary native-script spend", async () => {
    const spent = outRefFromByte(0x12);
    const script = nativeScriptWitness({ type: "all", scripts: [] });
    const entries = [
      {
        outRef: spent,
        output: makeProtectedScriptOutput(
          hashScriptWitness(script),
          FUNDED_OUTPUT_LOVELACE,
        ),
      },
    ];
    const transaction = makeNativeTx({
      spendInputs: [spent],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      scriptWitnesses: [script],
    });
    const result = await Effect.runPromise(
      replayValidationMachineEvent(await replayInput(transaction, entries)),
    );
    expect(result.trace.verdict).toBe("accepted");
    expect(result.trace.rejectionCode).toBeNull();
    expect(
      result.trace.witnesses.some(({ phase }) => phase === "nativeScripts"),
    ).toBe(true);
    expect(result.statePatch.deletedOutRefs).toEqual([spent.toString("hex")]);
  });

  it("reconstructs the ordinary accepting Plutus V3 identity program and CEK trace", async () => {
    // Same accepting identity program as the existing validation-machine suite.
    const program = buildMidgardCanonicalCekProgram(
      Buffer.from(
        UPLCEncoder.compile(
          new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
        ).toBuffer().buffer,
      ),
    );
    const spent = outRefFromByte(0x1d);
    const script = plutusV3ScriptWitness(program.envelopeCbor);
    const entries = [
      {
        outRef: spent,
        output: makeProtectedScriptOutput(
          hashScriptWitness(script),
          FUNDED_OUTPUT_LOVELACE,
        ),
      },
    ];
    const transaction = makeNativeTx({
      spendInputs: [spent],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      scriptWitnesses: [script],
      scriptLanguages: ["PlutusV3"],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [1_000_000_000n, 1_000_000_000n],
        },
      ]),
    });
    const result = await Effect.runPromise(
      replayValidationMachineEvent({
        ...(await replayInput(transaction, entries)),
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
          ...program.material.values(),
        ]),
      }),
    );
    expect(result.trace.verdict).toBe("accepted");
    expect(result.trace.rejectionCode).toBeNull();
    expect(result.trace.witnesses.some(({ phase }) => phase === "cek")).toBe(
      true,
    );
    expect(result.replayInput.expectedLedgerOps).toHaveLength(2);
  });

  it("derives an honest forced rejection on the wrong network as an unchanged ledger", async () => {
    const fixture = plainTransfer();
    const transaction = makeNativeTx({
      spendInputs: [fixture.spent],
      outputs: [fixture.output],
      networkId: 1n,
    });
    const normal = await replayInput(transaction, fixture.entries);
    const result = await Effect.runPromise(
      replayValidationMachineEvent({
        ...normal,
        sourceKind: "forced",
        committedForcedVerdict: "rejected",
        eventKeyCbor: Buffer.from(
          Data.to(
            {
              ForcedTransactionEventKey: {
                tx_order_id: {
                  transactionId: "22".repeat(32),
                  outputIndex: 0n,
                },
              },
            },
            EventKey,
          ),
          "hex",
        ),
      }),
    );
    expect(result.replayInput.expectedVerdict).toBe("rejected");
    expect(result.replayInput.expectedRejectionCode).toBe(
      RejectCodes.NetworkIdMismatch,
    );
    expect(result.trace.rejectionCode).toBe(RejectCodes.NetworkIdMismatch);
    expect(result.trace.verdict).toBe("rejected");
    expect(result.replayInput.postUtxosRoot).toBe(normal.priorUtxosRoot);
    expect(result.replayInput.expectedLedgerOps).toEqual([]);
    expect(result.replayInput.ledgerMutationSteps).toEqual([]);
    expect(result.statePatch).toEqual({
      deletedOutRefs: [],
      upsertedOutRefs: [],
    });
  });

  it("refuses stale prior ledger context before deriving a transaction verdict", async () => {
    const fixture = plainTransfer();
    const input = await replayInput(fixture.transaction, fixture.entries);
    const staleRoot = (await validationMachineLedgerRoot([])).toString("hex");
    await expect(
      Effect.runPromise(
        replayValidationMachineEvent({ ...input, priorUtxosRoot: staleRoot }),
      ),
    ).rejects.toThrow("prior ledger root differs from authenticated context");
  });
});
