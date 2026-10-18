import { createHash } from "node:crypto";

import {
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { EventKey } from "@al-ft/midgard-sdk";
import {
  Application,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as cekExecutor from "../src/cek-executor.execute-midgard-cek-structural-program.js";
import {
  applyUTxOStatePatch,
  buildMidgardCanonicalCekProgram,
  DirectValidationTraceUnavailable,
  MidgardRedeemerTag,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type ValidationMachineLedgerEntry,
  validationMachineLedgerRoot,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeQueued,
  makeRedeemersCbor,
  nativeScriptWitness,
  type NativeTxFixture,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";
import { replayValidationMachineEvent } from "./validation-machine-event-replay.trace.js";

const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  blockTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const replayInput = async (
  transaction: NativeTxFixture,
  entries: readonly ValidationMachineLedgerEntry[],
): Promise<Parameters<typeof replayValidationMachineEvent>[0]> => ({
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
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

  it.each([1, 2])(
    "reconstructs the ordinary accepting Plutus V3 unit program and CEK trace for %i executions",
    async (executionCount) => {
      const evaluator = vi.spyOn(
        cekExecutor,
        "executeMidgardCekStructuralProgram",
      );
      evaluator.mockClear();
      // Same accepting unit program as the existing validation-machine suite.
      const program = buildMidgardCanonicalCekProgram(
        Buffer.from(
          UPLCEncoder.compile(
            new UPLCProgram(
              [1, 1, 0],
              new Lambda(
                new Application(new Lambda(UPLCConst.unit), new UPLCVar(0)),
              ),
            ),
          ),
        ),
      );
      const spentInputs = Array.from({ length: executionCount }, (_, index) =>
        outRefFromByte(0x1d + index),
      );
      const script = plutusV3ScriptWitness(program.envelopeCbor);
      const entries = spentInputs.map((spent) => ({
        outRef: spent,
        output: makeProtectedScriptOutput(
          hashScriptWitness(script),
          FUNDED_OUTPUT_LOVELACE,
        ),
      }));
      const transaction = makeNativeTx({
        spendInputs: spentInputs,
        outputs: [
          makeOutput(
            FUNDED_OUTPUT_LOVELACE * BigInt(executionCount),
            Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x4d)]),
          ),
        ],
        omitVkeyWitness: true,
        scriptWitnesses: [script],
        scriptLanguages: ["PlutusV3"],
        redeemerTxWitsPreimageCbor: makeRedeemersCbor(
          spentInputs.map((_, index) => ({
            tag: MidgardRedeemerTag.Spend,
            index: BigInt(index),
            exUnits: [5_000_000n, 1_000_000_000n],
          })),
        ),
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
      expect(result.replayInput.expectedLedgerOps).toHaveLength(
        executionCount + 1,
      );
      const traceBytes = JSON.stringify(result.trace, (_, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      );
      // Byte pin after uniform §5.1 byte-string item counting and the Cardano
      // `validRange` (an absent upper bound is an inclusive PosInf); emitted
      // Phase A steps are independently checked by the Aiken one-step predicate.
      // Re-pinned when the declared ex-units moved under the per-tx cap, and
      // again when the CEK witness began carrying its execution's purpose key,
      // when the program began returning Unit (PV 11 halt rule), when each
      // execution began with Cardano's 100 cpu / 100 memory startup cost, and
      // when the term material began carrying Cardano's 1-based variable index,
      // when the CEK witness and context control began naming the program
      // by its script hash, and when the ledger-delta phase and the state's
      // ledger-delta root were removed, so acceptance follows multi-asset
      // accounting directly.
      const expectedTraceHash =
        executionCount === 1
          ? "a0d53d48a0a7111ecf983048f701348b862ec04dd576775f1b4689f0d72913b0"
          : "0a96b179a1a18948ce77e57e9927e3482d3f8f69cc645ee7ee006ae0a3c20b34";
      expect(createHash("sha256").update(traceBytes).digest("hex")).toBe(
        expectedTraceHash,
      );
      expect(evaluator).toHaveBeenCalledTimes(executionCount);
    },
  );

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
        canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
          materializeMidgardForcedTxFromCanonical(transaction.tx),
        ),

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

  // Phase A refuses these redeemer data bytes at canonicalDecode for both
  // sources (the first spells a constructor's fields definite-length, the
  // second is not Plutus Data), so the replay owes the typed direct-proof failure, not a defect
  // from the trace re-decoding the bytes phase A refused.
  it.each([
    ["normal", "d8798101"],
    ["normal", "60"],
    ["forced", "d8798101"],
    ["forced", "60"],
  ] as const)(
    "returns the typed canonicalDecode failure for %s redeemer data %s",
    async (sourceKind, redeemerDataHex) => {
      const fixture = plainTransfer();
      const transaction = makeNativeTx({
        spendInputs: [fixture.spent],
        outputs: [fixture.output],
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: 0, index: 0n, data: Buffer.from(redeemerDataHex, "hex") },
        ]),
        scriptLanguages: ["PlutusV3"],
      });
      const normal = await replayInput(transaction, fixture.entries);
      const input: typeof normal =
        sourceKind === "normal"
          ? normal
          : {
              ...normal,
              sourceKind: "forced",
              canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
                materializeMidgardForcedTxFromCanonical(transaction.tx),
              ),
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
            };
      const exit = await Effect.runPromiseExit(
        replayValidationMachineEvent(input),
      );
      if (Exit.isSuccess(exit)) {
        throw new Error("replay accepted redeemer data phase A refuses");
      }
      expect([...Cause.defects(exit.cause)]).toEqual([]);
      const failures = [...Cause.failures(exit.cause)];
      expect(failures).toHaveLength(1);
      expect(failures[0]).toBeInstanceOf(DirectValidationTraceUnavailable);
      expect(
        (failures[0] as DirectValidationTraceUnavailable).rejectionCode,
      ).toBe(RejectCodes.InvalidFieldType);
    },
  );

  it("a forced unit-program spend agrees across intake, classification and mandatory trace", async () => {
    const dataHex = "d8799f01ff";
    const program = buildMidgardCanonicalCekProgram(
      Buffer.from(
        UPLCEncoder.compile(
          new UPLCProgram(
            [1, 1, 0],
            new Lambda(
              new Application(new Lambda(UPLCConst.unit), new UPLCVar(0)),
            ),
          ),
        ),
      ),
    );
    const script = plutusV3ScriptWitness(program.envelopeCbor);
    const spent = outRefFromByte(0x1d);
    const entries = [
      {
        outRef: spent,
        output: makeProtectedScriptOutput(
          hashScriptWitness(script),
          FUNDED_OUTPUT_LOVELACE,
        ),
      },
    ];
    const native = makeNativeTx({
      spendInputs: [spent],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      scriptWitnesses: [script],
      scriptLanguages: ["PlutusV3"],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: 0,
          index: 0n,
          data: Buffer.from(dataHex, "hex"),
          exUnits: [5_000_000n, 1_000_000_000n],
        },
      ]),
    });
    const txCbor = encodeMidgardForcedTxCanonical(
      materializeMidgardForcedTxFromCanonical(native.tx),
    );
    const material = encodeMidgardCekProgramMaterialSidecar([
      ...program.material.values(),
    ]);
    const phaseA = await Effect.runPromise(
      runPhaseAValidation(
        [
          {
            ...makeQueued(native.txId, txCbor),
            sourceKind: "forced",
            programMaterialSidecarCbor: material,
          },
        ],
        { ...context, concurrency: 1, strictnessProfile: "phase1_midgard" },
      ),
    );
    expect(phaseA.rejected).toEqual([]);
    expect(
      phaseA.accepted[0]!.ledgerTx.redeemers[0]!.dataCbor.toString("hex"),
    ).toBe(dataHex);
    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        phaseA.accepted,
        new Map(
          entries.map(({ outRef, output }) => [outRef.toString("hex"), output]),
        ),
        {
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          nowCardanoSlotNo: 100n,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        },
      ),
    );
    expect(phaseB.accepted).toHaveLength(1);
    expect(phaseB.rejected).toEqual([]);
    const result = await Effect.runPromise(
      replayValidationMachineEvent({
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        ...context,
        sourceKind: "forced",
        canonicalTransactionCbor: txCbor,
        programMaterialSidecarCbor: material,
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
        ledgerWitnessEntries: entries,
        priorUtxosRoot: (await validationMachineLedgerRoot(entries)).toString(
          "hex",
        ),
      }),
    );
    expect(result.trace.verdict).toBe("accepted");
    expect(result.trace.rejectionCode).toBeNull();
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
