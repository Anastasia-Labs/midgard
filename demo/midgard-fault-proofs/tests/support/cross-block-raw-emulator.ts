import {
  layerMethod,
  type MethodLayer,
} from "@al-ft/midgard-test-support/method-layer";
import {
  CML,
  coreToTxOutput,
  Emulator,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";

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
// Emulator exposes no native header hash. This encoder belongs only to this
// mocked raw-authority test domain; height/slot come from its real public API.
export const syntheticEmulatorPoint = (blockHeight: number, slot: number) => {
  const value = {
    slot: String(slot),
    blockNo: String(blockHeight),
    blockHash: blockHeight.toString(16).padStart(64, "0"),
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
/**
 * Layers a pass-through observer over `Emulator.prototype.submitTx`. It sees
 * each transaction the real emulator accepted, after any layer installed
 * before it (such as {@link recordCrossBlockRawEmulator}'s). Restore it before
 * the layers beneath it.
 */
export const observeEmulatorSubmissions = (
  observe: (txHash: string, cbor: string) => void,
): MethodLayer =>
  layerMethod(
    Emulator.prototype,
    "submitTx",
    (next) =>
      async function (this: Emulator, cbor: string) {
        const txHash = await next.call(this, cbor);
        observe(txHash, cbor);
        return txHash;
      },
  );

export const recordCrossBlockRawEmulator = (
  options: {
    /**
     * Records only this emulator's submissions, through a layer on the
     * emulator itself that never needs restoring and that leaves its reads
     * as they are. The layer dispatches to the prototype's submitTx at each
     * call, so a prototype layer installed after it still sees every one.
     */
    readonly emulator?: Emulator;
  } = {},
) => {
  const rows: Omit<
    FraudProofRawL1Transaction,
    "confirmationDepth" | "inclusionPoint"
  >[] = [];
  const signedCbors = new Map<string, string>();
  // Submission coordinates are diagnostic metadata, separate from the legacy
  // raw rows. Actual inclusion is read from Emulator.getTransactionStatus.
  const acceptedTransactions = new Map<
    string,
    Readonly<{
      txHash: string;
      transactionCbor: string;
      slot: number;
      unixTimeMs: number;
    }>
  >();
  const recorded: { emulator?: Emulator } = {};
  // Layers, not vi.spyOn: a caller may stack its own observer on submitTx
  // (see @al-ft/midgard-test-support/method-layer), and both must see every
  // real submission.
  // Match the raw authority's canonical view when prerequisite discovery reads
  // a checkpoint through Lucid while its spending transaction is in the mempool.
  const outRefReads =
    options.emulator === undefined
      ? layerMethod(
          Emulator.prototype,
          "getUtxosByOutRef",
          () =>
            async function (this: Emulator, outRefs) {
              return outRefs.flatMap(({ txHash, outputIndex }) => {
                const entry = this.ledger[txHash + outputIndex];
                return entry === undefined ? [] : [entry.utxo];
              });
            },
        )
      : { restore: () => {} };
  const record = (next: Emulator["submitTx"]) =>
    async function (this: Emulator, cbor: string) {
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
            throw new Error(
              `recorded input ${hash}#${index} missing from emulator ledger`,
            );
          result.push(raw(`${hash}#${index}`, utxoToCore(found.utxo).output()));
        }
        return result;
      };
      const resolvedInputs = resolve(body.inputs());
      const resolvedReferenceInputs = resolve(body.reference_inputs());
      const acceptedSlot = this.slot;
      const acceptedTime = this.now();
      const hash = await next.call(this, cbor);
      recorded.emulator = this;
      signedCbors.set(hash, cbor);
      acceptedTransactions.set(hash, {
        txHash: hash,
        transactionCbor: cbor,
        slot: acceptedSlot,
        unixTimeMs: acceptedTime,
      });
      rows.push({
        txHash: hash,
        bodyCbor: body.to_cbor_hex(),
        witnessSetCbor: tx.witness_set().to_cbor_hex(),
        redeemersCbor:
          tx.witness_set().redeemers()?.to_canonical_cbor_hex() ?? null,
        isValid: true,
        resolvedInputs,
        resolvedReferenceInputs,
      });
      return hash;
    };
  const submissions =
    options.emulator === undefined
      ? layerMethod(Emulator.prototype, "submitTx", record)
      : layerMethod(options.emulator, "submitTx", () =>
          record(function (this: Emulator, cbor: string) {
            return Emulator.prototype.submitTx.call(this, cbor);
          }),
        );
  // A recorded row's bytes never change, so each is decoded once, by its
  // hash: the units its inputs and outputs carry, and its outputs as raw
  // UTxOs. Decoding them again at every capture left cardano-multiplatform-lib
  // garbage that only a later GC frees, enough over a long file to exhaust
  // the worker's reserved wasm memory (see
  // midgard-test-support/cml-memory-reserve.js), after which every capture
  // paid for full GCs.
  const decoded = new Map<
    string,
    Readonly<{ units: ReadonlySet<string>; outputs: FraudProofRawL1Utxo[] }>
  >();
  const decodedOf = (
    row: Pick<
      FraudProofRawL1Transaction,
      "txHash" | "bodyCbor" | "resolvedInputs"
    >,
  ) => {
    const known = decoded.get(row.txHash);
    if (known !== undefined) return known;
    const units = new Set<string>();
    const carry = (output: CML.TransactionOutput) => {
      for (const [unit, quantity] of Object.entries(
        coreToTxOutput(output).assets,
      ))
        if (quantity !== 0n) units.add(unit);
    };
    for (const input of row.resolvedInputs)
      carry(CML.TransactionOutput.from_cbor_hex(input.outputCbor));
    const body = CML.TransactionBody.from_cbor_hex(row.bodyCbor).outputs();
    const outputs = Array.from({ length: body.len() }, (_, index) => {
      const output = body.get(index);
      carry(output);
      return raw(`${row.txHash}#${index}`, output);
    });
    const entry = { units, outputs };
    decoded.set(row.txHash, entry);
    return entry;
  };
  const authority: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) => {
      const emulator = recorded.emulator;
      if (emulator === undefined)
        throw new Error("no emulator transactions captured");
      const tip = syntheticEmulatorPoint(emulator.blockHeight, emulator.slot);
      const sourceId = "local-emulator-recorded-cardano";
      const statuses = await Promise.all(
        rows.map((row) => emulator.getTransactionStatus(row.txHash)),
      );
      const all = rows.flatMap((row, index) => {
        const status = statuses[index]!;
        if (status.status !== "confirmed") return [];
        const { blockHeight, slot, confirmations } = status.confirmation;
        if (
          blockHeight === undefined ||
          slot === undefined ||
          confirmations === undefined
        )
          throw new Error(
            "emulator confirmation lacks actual inclusion coordinates",
          );
        return [
          {
            ...row,
            inclusionPoint: syntheticEmulatorPoint(blockHeight, slot),
            confirmationDepth: confirmations,
          },
        ];
      });
      const latest = all.reduce<(typeof all)[number] | undefined>(
        (last, row) =>
          last === undefined ||
          BigInt(row.inclusionPoint.blockNo) >
            BigInt(last.inclusionPoint.blockNo)
            ? row
            : last,
        undefined,
      );
      if (latest === undefined)
        throw new Error("no confirmed emulator transactions captured");
      const boundary = latest.inclusionPoint;
      const touches = (row: FraudProofRawL1Transaction, unit: string) =>
        decodedOf(row).units.has(unit);
      const transactions = all.filter((row) =>
        request.historyUnits.some((unit) => touches(row, unit)),
      );
      // Ledger entries are removed when their spending transaction is included.
      // A remaining `spent` flag only reserves an input for a mempool transaction;
      // it must not make an authenticated chain snapshot report a rollback.
      const utxos = Object.values(emulator.ledger).map((row) => row.utxo);
      const output = (utxo: UTxO) => {
        const created = rows.find((row) => row.txHash === utxo.txHash);
        if (created === undefined)
          return raw(
            `${utxo.txHash}#${utxo.outputIndex}`,
            utxoToCore(utxo).output(),
          );
        const recorded = decodedOf(created).outputs[utxo.outputIndex];
        if (recorded === undefined)
          throw new Error(
            `recorded transaction ${utxo.txHash} has no output ${utxo.outputIndex}`,
          );
        return { ...recorded };
      };
      return {
        schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        blueprintHash: request.blueprintHash,
        finalityPolicyDigest: request.finalityPolicyDigest,
        headerHash: request.headerHash,
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId,
          grade: "security",
          sourceMode: "local_chain_follower",
          boundaryPoint: boundary,
          tipPoint: tip,
        },
        cursor: {
          point: boundary,
          tip,
          confirmationDepth: latest.confirmationDepth,
          rollbackCursor: computeFraudProofRawL1RollbackCursor({
            ...request,
            sourceId,
            pointId: boundary.pointId,
          }),
        },
        // An out-ref scope serves only its out-refs, as the follower does.
        scopes: request.scopes.map((scope) => ({
          ...scope,
          utxos: utxos
            .filter(
              (utxo) =>
                utxo.address === scope.address &&
                (scope.outRefs === undefined ||
                  scope.outRefs.includes(
                    `${utxo.txHash}#${utxo.outputIndex.toString()}`,
                  )),
            )
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
  return {
    authority,
    restore: () => {
      submissions.restore();
      outRefReads.restore();
    },
    rows,
    signedCbors,
    acceptedTransactions,
  };
};
