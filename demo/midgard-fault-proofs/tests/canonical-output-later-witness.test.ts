import {
  decodeSingleCbor,
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardForcedTxCanonical,
  MIDGARD_CONSENSUS_PROFILE,
  validateMidgardConsensusForcedTxCbor,
} from "@al-ft/midgard-core";
import {
  EventKey,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  DirectValidationTraceUnavailable,
  RejectCodes,
  replayValidationMachineEvent,
  validatePhaseASingle,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import {
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { buildDeterministicValidationTraceMembers } from "./support/node-validation-trace-export.mjs";

it.each([16384, 16385])(
  "retains canonical first fault before malformed later address witness at output size %s",
  async (size) => {
    const spent = outRefFromByte(0x63);
    const native = makeNativeTx({
      spendInputs: [spent],
      outputs: [makeMinAdaFundedExactSizeOutputItem(size)],
    });
    const malformedAddress = encodeCbor([
      Buffer.alloc(31, 0x11),
      Buffer.alloc(65, 0x22),
    ]);
    expect(malformedAddress.length).toBe(101);
    const canonicalTransactionCbor = encodeMidgardForcedTxCanonical({
      version: native.tx.version,
      body: native.tx.body,
      witnessSet: {
        ...native.tx.witnessSet,
        addrTxWitsPreimageCbor: encodeCbor([malformedAddress]),
      },
    });
    expect(
      validateMidgardConsensusForcedTxCbor(canonicalTransactionCbor),
    ).toBeNull();
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      canonicalTransactionCbor,
    );
    expect(material.canonical.witnessSet.addrTxWitsPreimageCbor).toEqual(
      encodeCbor([malformedAddress]),
    );
    const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar(
      [],
    );
    const ledgerWitnessEntries = [
      { outRef: spent, output: makeOutput(50_000_000n) },
    ];
    const priorUtxosRoot = (
      await validationMachineLedgerRoot(ledgerWitnessEntries)
    ).toString("hex");
    const eventKey = {
      ForcedTransactionEventKey: {
        tx_order_id: { transactionId: "63".repeat(32), outputIndex: 0n },
      },
    };
    const context = {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      blockEndTimeMs: 1750000000000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      sourceKind: "forced" as const,
      canonicalTransactionCbor,
      programMaterialSidecarCbor,
      ledgerWitnessEntries,
      priorUtxosRoot,
      eventKeyCbor: Buffer.from(Data.to(eventKey, EventKey), "hex"),
    };
    const phaseA = validatePhaseASingle(
      {
        txId: material.transactionId,
        txCbor: canonicalTransactionCbor,
        sourceKind: "forced",
        programMaterialSidecarCbor,
        arrivalSeq: 0n,
        createdAt: new Date(0),
      },
      { ...context, concurrency: 1, strictnessProfile: "phase1_midgard" },
    );
    expect(phaseA).toMatchObject({
      code: RejectCodes.InvalidFieldType,
      consensusPhase: "canonicalDecode",
    });
    const replay = replayValidationMachineEvent(context);
    if (size === 16384) {
      const outcome = await Effect.runPromise(Effect.either(replay));
      expect(outcome).toMatchObject({
        _tag: "Left",
        left: expect.any(DirectValidationTraceUnavailable),
      });
      return;
    }
    const { trace, replayInput, statePatch } = await Effect.runPromise(replay);
    expect(trace.rejectionCode).toBe(RejectCodes.InvalidFieldType);
    expect(trace.witnesses.at(-2)?.phase).toBe("canonicalDecode");
    expect(trace.witnesses.some((w) => w.phase === "signatures")).toBe(false);
    expect(trace.witnesses.at(-2)?.auxiliary).toMatchObject({
      kind: "transactionFieldItem",
      fieldIndex: 2,
    });
    const control = decodeSingleCbor(trace.witnesses.at(-2)!.cbor) as unknown[];
    expect(control.slice(4, 6)).toEqual([2, 0]);
    expect(replayInput.canonicalTransactionCbor).toEqual(
      canonicalTransactionCbor,
    );
    expect(statePatch).toEqual({ deletedOutRefs: [], upsertedOutRefs: [] });
    const exported = await Effect.runPromise(
      buildDeterministicValidationTraceMembers({
        consensusProfile: context.consensusProfile,
        blockEndTime: new Date(context.blockEndTimeMs),
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactions: [
          {
            ...replayInput,
            eventKey,
            ledgerOps: [],
            verdict: "rejected",
            rejectionCode: RejectCodes.InvalidFieldType,
          },
        ],
      }),
    );
    expect(exported).toHaveLength(1);
    expect(exported[0]!.value).toEqual(
      validationTraceDescriptorDataFromCore(trace.tree.descriptor),
    );
    expect(exported[0]!.witnesses.length).toBe(trace.witnesses.length + 2);
  },
  120_000,
);
