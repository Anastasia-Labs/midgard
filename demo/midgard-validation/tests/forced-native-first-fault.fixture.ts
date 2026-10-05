import {
  computeHash28,
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { encodeCbor, encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { EventKey } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildMidgardCanonicalCekProgram,
  replayValidationMachineEvent,
  validatePhaseASingle,
  validationMachineLedgerRoot,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

export const nativeFaultContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  blockEndTimeMs: 1750000000000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};
export type NativeFaultShape =
  | "present"
  | "missing"
  | "empty"
  | "mint"
  | "signature"
  | "earlierFalse"
  | "exhaustedBoundary"
  | "validKey"
  | "missingKey"
  | "invalidChildren"
  | "invalidThresholdChildren";
export async function nativeFaultFixture(
  shape: NativeFaultShape,
  now = nativeFaultContext.blockEndTimeMs - 1000,
  blockSlot = nativeFaultContext.blockSlot,
) {
  const spent = outRefFromByte(0x4d);
  const native = makeNativeTx({
    spendInputs: shape === "empty" ? [] : [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    ...(shape === "mint"
      ? {
          mintPreimageCbor: makeMintPreimageCbor(
            new Map([
              [
                Buffer.alloc(28, 0x4d),
                new Map([[Buffer.from("01", "hex"), 1n]]),
              ],
            ]),
          ),
        }
      : {}),
    ...(shape === "signature" ? { invalidVkeyWitness: true } : {}),
  });
  const originalScript = Buffer.from(
    shape === "exhaustedBoundary"
      ? "8201990552" + "820180".repeat(1361) + "8200"
      : shape === "validKey"
        ? "8200581c" + "00".repeat(28)
        : shape === "missingKey"
          ? "8200"
          : shape === "invalidChildren"
            ? "820100"
            : shape === "invalidThresholdChildren"
              ? "83030100"
              : "820700",
    "hex",
  );
  const tx = materializeMidgardForcedTxFromCanonical({
    ...native.tx,
    witnessSet: {
      ...native.tx.witnessSet,
      scriptTxWitsPreimageCbor: encodeCbor([
        ...(shape === "earlierFalse"
          ? [Buffer.from("820043820280", "hex")]
          : []),
        encodeCbor([0, originalScript]),
      ]),
    },
  });
  const canonicalTransactionCbor = encodeMidgardForcedTxCanonical(tx);
  const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar([]);
  const output = encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x70]),
      computeHash28(Buffer.concat([Buffer.from([0]), originalScript])),
    ]),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  });
  const entries =
    shape === "missing" || shape === "empty" ? [] : [{ outRef: spent, output }];
  const phaseA = validatePhaseASingle(
    {
      txId: native.txId,
      txCbor: canonicalTransactionCbor,
      sourceKind: "forced",
      programMaterialSidecarCbor,
      arrivalSeq: 0n,
      createdAt: new Date(0),
    },
    {
      ...nativeFaultContext,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
  );
  const result = await Effect.runPromise(
    replayValidationMachineEvent({
      ...nativeFaultContext,
      blockEndTimeMs: now + 1000,
      blockSlot,
      sourceKind: "forced",
      eventKeyCbor: Buffer.from(
        Data.to(
          {
            ForcedTransactionEventKey: {
              tx_order_id: {
                transactionId: "4d".repeat(32),
                outputIndex: 0n,
              },
            },
          },
          EventKey,
        ),
        "hex",
      ),
      canonicalTransactionCbor,
      programMaterialSidecarCbor,
      ledgerWitnessEntries: entries,
      priorUtxosRoot: (await validationMachineLedgerRoot(entries)).toString(
        "hex",
      ),
    }),
  );
  return {
    ...result,
    native,
    phaseA,
    canonicalTransactionCbor,
    programMaterialSidecarCbor,
    entries,
  };
}

export async function redeemerFaultFixture(missingScript: boolean) {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from("010100200101", "hex"),
  );
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const spent = outRefFromByte(0x4e);
  const native = makeNativeTx({
    spendInputs: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    scriptWitnesses: missingScript ? [] : [script],
    scriptLanguages: ["PlutusV3"],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      {
        tag: 0,
        index: 0n,
        data: Buffer.from("1801", "hex"),
        exUnits: [1000000000n, 1000000000n],
      },
    ]),
  });
  const canonicalTransactionCbor = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(native.tx),
  );
  const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar(
    missingScript ? [] : [...program.material.values()],
  );
  const entries = [
    {
      outRef: spent,
      output: makeProtectedScriptOutput(
        hashScriptWitness(script),
        FUNDED_OUTPUT_LOVELACE,
      ),
    },
  ];
  return Effect.runPromise(
    replayValidationMachineEvent({
      ...nativeFaultContext,
      sourceKind: "forced",
      eventKeyCbor: Buffer.from(
        Data.to(
          {
            ForcedTransactionEventKey: {
              tx_order_id: { transactionId: "4e".repeat(32), outputIndex: 0n },
            },
          },
          EventKey,
        ),
        "hex",
      ),
      canonicalTransactionCbor,
      programMaterialSidecarCbor,
      ledgerWitnessEntries: entries,
      priorUtxosRoot: (await validationMachineLedgerRoot(entries)).toString(
        "hex",
      ),
    }),
  );
}
