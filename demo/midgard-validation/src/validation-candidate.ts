import {
  encodeMidgardAddressText,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import { LedgerColumns, type LedgerEntry, type ProcessedTx } from "./ledger.js";
import type {
  MidgardLedgerOutput,
  MidgardLedgerTx,
  MidgardOutRef,
} from "./ledger-tx/types.js";
import type { PhaseAValidatedTx } from "./types.js";
import { mintToValueDelta, sumMidgardValues } from "./value-accounting.js";

export const midgardOutRefToCbor = (outRef: MidgardOutRef): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(outRef.txId),
      outRef.index,
    ).to_cbor_bytes(),
  );

export const midgardOutRefToCborHex = (outRef: MidgardOutRef): string =>
  midgardOutRefToCbor(outRef).toString("hex");

const ledgerOutputToTxOutput = (
  output: MidgardLedgerOutput,
): MidgardTxOutput => ({
  address: output.address,
  value: output.value,
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.scriptRef === undefined ? {} : { script_ref: output.scriptRef }),
});

export const ledgerOutputToCbor = (output: MidgardLedgerOutput): Buffer =>
  encodeMidgardTxOutput(ledgerOutputToTxOutput(output));

const hashHexes = (hashes: readonly Buffer[]): readonly string[] =>
  hashes.map((hash) => hash.toString("hex"));

const mintPolicyHashHexes = (tx: MidgardLedgerTx): readonly string[] => {
  const seen = new Set<string>();
  const policyIds: string[] = [];
  for (const asset of tx.mint.assets) {
    const policyId = asset.policyId.toString("hex");
    if (!seen.has(policyId)) {
      seen.add(policyId);
      policyIds.push(policyId);
    }
  }
  return policyIds;
};

type BuildPhaseAValidatedTxArgs = {
  readonly ledgerTx: MidgardLedgerTx;
  readonly txCbor: Buffer;
  readonly arrivalSeq: bigint;
  readonly createdAt: Date;
  readonly redeemerWitnessHash: Buffer;
};

export const buildPhaseAValidatedTx = ({
  ledgerTx,
  txCbor,
  arrivalSeq,
  createdAt,
  redeemerWitnessHash,
}: BuildPhaseAValidatedTxArgs): PhaseAValidatedTx => {
  const produced: LedgerEntry[] = ledgerTx.outputs.map((output, index) => {
    const outputCbor = ledgerOutputToCbor(output);
    return {
      [LedgerColumns.TX_ID]: Buffer.from(ledgerTx.txId),
      [LedgerColumns.OUTREF]: midgardOutRefToCbor({
        txId: ledgerTx.txId,
        index: BigInt(index),
      }),
      [LedgerColumns.OUTPUT]: outputCbor,
      [LedgerColumns.ADDRESS]: encodeMidgardAddressText(output.address),
    };
  });

  return {
    ledgerTx,
    submission: {
      txCbor: Buffer.from(txCbor),
      arrivalSeq,
      createdAt: new Date(createdAt.getTime()),
    },
    graph: {
      spentOutRefHexes: ledgerTx.spendInputs.map(midgardOutRefToCborHex),
      referenceOutRefHexes: ledgerTx.referenceInputs.map(
        midgardOutRefToCborHex,
      ),
      produced,
    },
    derived: {
      outputSum: sumMidgardValues(
        ledgerTx.outputs.map((output) => output.value),
      ),
      mintDelta: mintToValueDelta(ledgerTx.mint),
      witnessKeyHashHexes: hashHexes(ledgerTx.witnessKeyHashes),
      nativeScriptHashHexes: hashHexes(ledgerTx.nativeScriptHashes),
      plutusScriptHashHexes: hashHexes(ledgerTx.plutusScriptHashes),
      requiredObserverHashHexes: hashHexes(ledgerTx.requiredObserverHashes),
      mintPolicyHashHexes: mintPolicyHashHexes(ledgerTx),
      redeemerWitnessHash: Buffer.from(redeemerWitnessHash),
      requiresScriptEvaluation: ledgerTx.requiresPlutusEvaluation,
    },
  };
};

export const processedTxFromValidatedTx = (
  candidate: PhaseAValidatedTx,
): ProcessedTx => ({
  txId: Buffer.from(candidate.ledgerTx.txId),
  txCbor: Buffer.from(candidate.submission.txCbor),
  spent: candidate.graph.spentOutRefHexes.map((outRefHex) =>
    Buffer.from(outRefHex, "hex"),
  ),
  produced: candidate.graph.produced.map((entry) => ({
    [LedgerColumns.TX_ID]: Buffer.from(entry[LedgerColumns.TX_ID]),
    [LedgerColumns.OUTREF]: Buffer.from(entry[LedgerColumns.OUTREF]),
    [LedgerColumns.OUTPUT]: Buffer.from(entry[LedgerColumns.OUTPUT]),
    [LedgerColumns.ADDRESS]: entry[LedgerColumns.ADDRESS],
  })),
});
