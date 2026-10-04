import { createHash } from "node:crypto";

import {
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { EventKey } from "@al-ft/midgard-sdk";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
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
  replayValidationMachineEvent,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
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
  makeQueued,
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

  it.each([1, 2])(
    "reconstructs the ordinary accepting Plutus V3 identity program and CEK trace for %i executions",
    async (executionCount) => {
      const evaluator = vi.spyOn(
        cekExecutor,
        "executeMidgardCekStructuralProgram",
      );
      evaluator.mockClear();
      // Same accepting identity program as the existing validation-machine suite.
      const program = buildMidgardCanonicalCekProgram(
        Buffer.from(
          UPLCEncoder.compile(
            new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
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
            exUnits: [1_000_000_000n, 1_000_000_000n],
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
      // Byte pin after uniform §5.1 byte-string item counting; emitted Phase A
      // steps are independently checked by the Aiken one-step predicate.
      const expectedTraceHash =
        executionCount === 1
          ? "dc65a2b492ada3851a462b60e7e7bcfbebf7b6713374a751cb3712d7bd933a3d"
          : "c5c09e5732a87312fee52c44919cf7463f00db6337e930ea71f74d25b76f2e47";
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

  // Phase A refuses these redeemer data bytes at canonicalDecode (the first
  // spells a constructor's fields definite-length, the second is not Plutus
  // Data), so the replay owes the typed direct-proof failure, not a defect
  // from the trace re-decoding the bytes phase A refused.
  it.each([
    ["normal", "d8798101"],
    ["normal", "60"],
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
      const input: ValidationMachineEventReplayInput =
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

  it.each([
    { dataHex: "d8799f01ff", priorCanonical: false },
    { dataHex: "d8798101", priorCanonical: false },
    { dataHex: "d8798101", priorCanonical: true },
  ])(
    "forced identity spend $dataHex prior canonical=$priorCanonical agrees across intake, classification and mandatory trace",
    async ({ dataHex, priorCanonical }) => {
      const program = buildMidgardCanonicalCekProgram(
        Buffer.from(
          UPLCEncoder.compile(
            new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
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
          ...(priorCanonical
            ? [
                {
                  tag: 0,
                  index: 0n,
                  data: Buffer.from("d8799f01ff", "hex"),
                  exUnits: [1_000_000_000n, 1_000_000_000n] as const,
                },
              ]
            : []),
          {
            tag: 0,
            index: 0n,
            data: Buffer.from(dataHex, "hex"),
            exUnits: [1_000_000_000n, 1_000_000_000n],
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
        phaseA.accepted[0]!.ledgerTx.redeemers[
          priorCanonical ? 1 : 0
        ]!.dataCbor.toString("hex"),
      ).toBe(dataHex);
      const phaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(
          phaseA.accepted,
          new Map(
            entries.map(({ outRef, output }) => [
              outRef.toString("hex"),
              output,
            ]),
          ),
          {
            nowCardanoSlotNo: 100n,
            bucketConcurrency: 1,
            enforceScriptBudget: true,
          },
        ),
      );
      const noncanonical = dataHex === "d8798101";
      expect(phaseB.accepted).toHaveLength(noncanonical ? 0 : 1);
      expect(phaseB.rejected).toHaveLength(noncanonical ? 1 : 0);
      if (noncanonical)
        expect(phaseB.rejected[0]).toMatchObject({
          code: RejectCodes.InvalidFieldType,
          consensusPhase: "scriptSources",
          subject: {
            arm: "RedeemerMalformed",
            index: priorCanonical ? 1n : 0n,
          },
        });
      const result = await Effect.runPromise(
        replayValidationMachineEvent({
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
      expect(result.trace.verdict).toBe(noncanonical ? "rejected" : "accepted");
      expect(result.trace.rejectionCode).toBe(
        noncanonical ? RejectCodes.InvalidFieldType : null,
      );
      if (noncanonical) {
        expect(result.statePatch).toEqual({
          deletedOutRefs: [],
          upsertedOutRefs: [],
        });
        const last = result.trace.witnesses.at(-2)!;
        expect(last.phase).toBe("scriptSources");
        expect(last.auxiliary).toMatchObject({
          kind: "redeemerItemStep",
          control: { itemIndex: priorCanonical ? 1 : 0, dataLength: 4 },
          witness: { action: { kind: "traverseData", action: null } },
        });
      }
    },
  );

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
