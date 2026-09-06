import {
  CML,
  coreToTxOutput,
  Emulator,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "../../src/workflow/raw-l1-snapshot.js";
const point = (n: number) => {
  const value = {
    slot: String(n),
    blockNo: String(n),
    blockHash: n.toString(16).padStart(64, "0"),
  };
  return { ...value, pointId: computeFraudProofRawL1PointId(value) };
};
const raw = (
  outRef: string,
  output: CML.TransactionOutput,
): FraudProofRawL1Utxo => ({
  outRef,
  outputCbor: output.to_canonical_cbor_hex(),
  datumCbor: output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
  referenceScriptCbor: output.script_ref()?.to_canonical_cbor_hex() ?? null,
});
export const recordCrossBlockRawEmulator = () => {
  const rows: Omit<FraudProofRawL1Transaction, "confirmationDepth">[] = [];
  const signedCbors = new Map<string, string>();
  const original = Emulator.prototype.submitTx;
  const recorded: { emulator?: Emulator } = {};
  const spy = vi
    .spyOn(Emulator.prototype, "submitTx")
    .mockImplementation(async function (this: Emulator, cbor: string) {
      const tx = CML.Transaction.from_cbor_hex(cbor);
      const body = tx.body();
      const resolve = (inputs: CML.TransactionInputList | undefined) => {
        const result: FraudProofRawL1Utxo[] = [];
        for (let i = 0; i < (inputs?.len() ?? 0); i++) {
          const input = inputs!.get(i);
          const hash = input.transaction_id().to_hex();
          const index = Number(input.index());
          const key = hash + index;
          const found = this.ledger[key] ?? this.mempool[key];
          if (found === undefined)
            throw new Error("recorded input missing from emulator ledger");
          result.push(raw(`${hash}#${index}`, utxoToCore(found.utxo).output()));
        }
        return result;
      };
      const resolvedInputs = resolve(body.inputs());
      const resolvedReferenceInputs = resolve(body.reference_inputs());
      const hash = await original.call(this, cbor);
      recorded.emulator = this;
      signedCbors.set(hash, cbor);
      rows.push({
        txHash: hash,
        bodyCbor: body.to_cbor_hex(),
        witnessSetCbor: tx.witness_set().to_cbor_hex(),
        redeemersCbor:
          tx.witness_set().redeemers()?.to_canonical_cbor_hex() ?? null,
        isValid: true,
        inclusionPoint: point(rows.length + 1),
        resolvedInputs,
        resolvedReferenceInputs,
      });
      return hash;
    });
  const authority: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) => {
      const emulator = recorded.emulator;
      if (emulator === undefined)
        throw new Error("no emulator transactions captured");
      const boundary = point(rows.length + 1);
      const tip = point(rows.length + 30);
      const sourceId = "local-emulator-recorded-cardano";
      const all = rows.map((row) => ({
        ...row,
        confirmationDepth:
          Number(tip.blockNo) - Number(row.inclusionPoint.blockNo) + 1,
      }));
      const touches = (row: FraudProofRawL1Transaction, unit: string) => {
        const outputs = CML.TransactionBody.from_cbor_hex(
          row.bodyCbor,
        ).outputs();
        return (
          row.resolvedInputs.some(
            (output) =>
              (coreToTxOutput(
                CML.TransactionOutput.from_cbor_hex(output.outputCbor),
              ).assets[unit] ?? 0n) !== 0n,
          ) ||
          Array.from({ length: outputs.len() }, (_, i) => outputs.get(i)).some(
            (output) => (coreToTxOutput(output).assets[unit] ?? 0n) !== 0n,
          )
        );
      };
      const transactions = all.filter((row) =>
        request.historyUnits.some((unit) => touches(row, unit)),
      );
      const utxos = Object.values(emulator.ledger)
        .filter((row) => !row.spent)
        .map((row) => row.utxo);
      const output = (utxo: UTxO) => {
        const created = rows.find((row) => row.txHash === utxo.txHash);
        return raw(
          `${utxo.txHash}#${utxo.outputIndex}`,
          created === undefined
            ? utxoToCore(utxo).output()
            : CML.TransactionBody.from_cbor_hex(created.bodyCbor)
                .outputs()
                .get(utxo.outputIndex),
        );
      };
      return {
        schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        releaseIdentityDigest: request.releaseIdentityDigest,
        finalityPolicyDigest: request.finalityPolicyDigest,
        headerHash: request.headerHash,
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId,
          grade: "security",
          sourceMode: "local_kupo_ogmios",
          kupoCheckpoint: boundary,
          ogmiosTip: tip,
        },
        cursor: {
          point: boundary,
          tip,
          confirmationDepth: 30,
          rollbackCursor: computeFraudProofRawL1RollbackCursor({
            ...request,
            sourceId,
            pointId: boundary.pointId,
          }),
        },
        scopes: request.scopes.map((scope) => ({
          ...scope,
          utxos: utxos
            .filter((utxo) => utxo.address === scope.address)
            .map(output),
        })),
        historyUnits: request.historyUnits,
        history: request.historyUnits.map((unit) => ({
          unit,
          fromGenesis: true,
          completeThroughPointId: boundary.pointId,
          transactionHashes: transactions
            .filter((row) => touches(row, unit))
            .map((row) => row.txHash),
        })),
        transactions,
      } satisfies FraudProofRawL1Snapshot;
    },
  };
  return { authority, restore: () => spy.mockRestore(), rows, signedCbors };
};
