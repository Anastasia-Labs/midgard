import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
} from "@lucid-evolution/lucid";

import { captureTransitionTraceL1Events } from "../../src/transition-trace/l1-events.js";
import {
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
} from "../../src/workflow/raw-l1-snapshot.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  RELEASE_L1_FINALITY_POLICY,
} from "../../src/workflow/release-finality-policy.js";
import { TRANSITION_HISTORY_FIXTURE_PARAMETERS } from "../helpers/transition-history-fixture.js";

// A fake raw L1 source over an admitted retained-fixture snapshot, and the
// chain edits the transition event tests apply to it. Every edited snapshot
// still passes the real raw-snapshot admission.

export const captureInput = (snapshot: FraudProofRawL1Snapshot) => {
  const hubScope = snapshot.scopes.find(({ role }) => role === "hub_oracle")!;
  const authority: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) => ({
      ...snapshot,
      scopes: request.scopes.map((scope) => ({
        ...scope,
        utxos: snapshot.scopes.find(({ role }) => role === scope.role)!.utxos,
      })),
      historyUnits: request.historyUnits,
      history: request.historyUnits.map(
        (unit) => snapshot.history.find((entry) => entry.unit === unit)!,
      ),
    }),
  };
  const binding = {
    deploymentFingerprint: snapshot.deploymentIdentityDigest,
    network: "Preprod",
    releaseFinality: {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: snapshot.deploymentIdentityDigest,
      blueprintHash: snapshot.blueprintHash,
      policyDigest: snapshot.finalityPolicyDigest,
      policy: { ...RELEASE_L1_FINALITY_POLICY },
    },
    resolvedContracts: {
      contracts: {
        transitionTrace: { history: TRANSITION_HISTORY_FIXTURE_PARAMETERS },
      },
      hubOraclePolicyId: getAddressDetails(hubScope.address).paymentCredential!
        .hash,
    },
    definition: { headerHash: snapshot.headerHash },
  } as Parameters<typeof captureTransitionTraceL1Events>[0]["binding"];
  return { binding, authority };
};

export const capture = (snapshot: FraudProofRawL1Snapshot) =>
  captureTransitionTraceL1Events(captureInput(snapshot));

export const advance = (
  snapshot: FraudProofRawL1Snapshot,
): FraudProofRawL1Snapshot => {
  const next = (point: typeof snapshot.cursor.point, hash: string) => {
    const value = {
      slot: (BigInt(point.slot) + 1n).toString(),
      blockNo: (BigInt(point.blockNo) + 1n).toString(),
      blockHash: hash.repeat(32),
    };
    return { ...value, pointId: computeFraudProofRawL1PointId(value) };
  };
  const point = next(snapshot.cursor.point, "81");
  const tip = next(snapshot.cursor.tip, "82");
  return {
    ...snapshot,
    provenance: {
      ...snapshot.provenance,
      boundaryPoint: point,
      tipPoint: tip,
    },
    cursor: {
      ...snapshot.cursor,
      point,
      tip,
      rollbackCursor: computeFraudProofRawL1RollbackCursor({
        ...snapshot,
        sourceId: snapshot.provenance.sourceId,
        pointId: point.pointId,
      }),
    },
    history: snapshot.history.map((entry) => ({
      ...entry,
      completeThroughPointId: point.pointId,
    })),
    transactions: snapshot.transactions.map((entry) => ({
      ...entry,
      confirmationDepth: entry.confirmationDepth + 1,
    })),
  };
};

export const replaceBody = (
  snapshot: FraudProofRawL1Snapshot,
  update: (body: CML.TransactionBody) => CML.TransactionBody,
): FraudProofRawL1Snapshot => {
  const transaction = snapshot.transactions[0]!;
  const body = update(CML.TransactionBody.from_cbor_hex(transaction.bodyCbor));
  const txHash = CML.hash_transaction(body).to_hex();
  return {
    ...snapshot,
    scopes: snapshot.scopes.map((scope) => ({
      ...scope,
      utxos: scope.utxos.map((entry) => {
        const [oldHash, index] = entry.outRef.split("#");
        if (oldHash !== transaction.txHash) return entry;
        const outputCbor = body
          .outputs()
          .get(Number(index))
          .to_canonical_cbor_hex();
        return {
          ...entry,
          outRef: `${txHash}#${index}`,
          outputCbor,
          datumCbor:
            coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor))
              .datum ?? null,
        };
      }),
    })),
    history: snapshot.history.map((entry) => ({
      ...entry,
      transactionHashes: entry.transactionHashes.map((hash) =>
        hash === transaction.txHash ? txHash : hash,
      ),
    })),
    transactions: [
      { ...transaction, txHash, bodyCbor: body.to_cbor_hex() },
      ...snapshot.transactions.slice(1),
    ],
  };
};

export const changeDatum = (
  snapshot: FraudProofRawL1Snapshot,
  index: number,
  datum: string,
) =>
  replaceBody(snapshot, (body) => {
    const outputs = body.outputs();
    const next = CML.TransactionOutputList.new();
    for (let i = 0; i < outputs.len(); i += 1) {
      const output = outputs.get(i);
      next.add(
        i === index
          ? CML.TransactionOutput.new(
              output.address(),
              output.amount(),
              CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
            )
          : output,
      );
    }
    const updated = CML.TransactionBody.new(body.inputs(), next, body.fee());
    updated.set_mint(body.mint()!);
    return updated;
  });

/** Adds a minted forced-transaction event at `inclusionTime`. */
export const withLaterEvent = (
  snapshot: FraudProofRawL1Snapshot,
  inclusionTime: bigint,
  assetName = "02",
  quantity = 1n,
): FraudProofRawL1Snapshot => {
  const scope = snapshot.scopes.find(
    (entry) => entry.role === "forced_transaction_event",
  )!;
  const original = scope.utxos[0]!;
  const datum = Data.from(original.datumCbor!, SDK.TxOrderDatum);
  const sourceTxHash =
    assetName === "02" ? "91".repeat(32) : assetName.repeat(32);
  const updated = {
    ...datum,
    event: {
      ...datum.event,
      id: { transactionId: sourceTxHash, outputIndex: 0n },
    },
    inclusion_time: inclusionTime,
  };
  const policy = getAddressDetails(scope.address).paymentCredential!.hash;
  const unit = policy + assetName;
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(policy),
    CML.AssetName.from_hex(assetName),
    quantity,
  );
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(scope.address),
    CML.Value.new(3_000_000n, assets),
    CML.DatumOption.new_datum(
      CML.PlutusData.from_cbor_hex(Data.to(updated, SDK.TxOrderDatum)),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(sourceTxHash), 0n),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(policy),
    CML.AssetName.from_hex(assetName),
    quantity,
  );
  body.set_mint(mint);
  const txHash = CML.hash_transaction(body).to_hex();
  const outputCbor = output.to_canonical_cbor_hex();
  const raw = {
    outRef: `${txHash}#0`,
    outputCbor,
    datumCbor: coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor))
      .datum!,
    referenceScriptCbor: null,
  };
  return {
    ...snapshot,
    scopes: snapshot.scopes.map((entry) =>
      entry.role === scope.role
        ? { ...entry, utxos: [...entry.utxos, raw] }
        : entry,
    ),
    historyUnits: [...snapshot.historyUnits, unit],
    history: [
      ...snapshot.history,
      {
        unit,
        fromGenesis: true,
        completeThroughPointId: snapshot.cursor.point.pointId,
        transactionHashes: [txHash],
      },
    ],
    transactions: [
      ...snapshot.transactions,
      {
        ...snapshot.transactions[0]!,
        txHash,
        bodyCbor: body.to_cbor_hex(),
        resolvedInputs: [
          {
            ...snapshot.transactions[0]!.resolvedInputs[0]!,
            outRef: sourceTxHash + "#0",
          },
        ],
      },
    ],
  };
};

/**
 * Respends the first `role` output to the same address and value, and the
 * same datum unless `datumCbor` replaces it: its outRef moves and the unit
 * history grows by the respending transaction, as a permissionless list
 * insert or a hub governance update does on L1.
 */
export const respendOutput = (
  snapshot: FraudProofRawL1Snapshot,
  role: FraudProofRawL1Snapshot["scopes"][number]["role"],
  fill: string,
  datumCbor?: string,
): FraudProofRawL1Snapshot => {
  const scope = snapshot.scopes.find((entry) => entry.role === role)!;
  const spent = scope.utxos[0]!;
  const [spentHash, spentIndex] = spent.outRef.split("#");
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(spentHash!),
      BigInt(spentIndex!),
    ),
  );
  const original = CML.TransactionOutput.from_cbor_hex(spent.outputCbor);
  const output =
    datumCbor === undefined
      ? original
      : CML.TransactionOutput.new(
          original.address(),
          original.amount(),
          CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCbor)),
        );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  // The fee only separates this respend from any other in the same fixture.
  const body = CML.TransactionBody.new(
    inputs,
    outputs,
    170_000n + BigInt(parseInt(fill.slice(0, 2), 16)),
  );
  const txHash = CML.hash_transaction(body).to_hex();
  const outputCbor = output.to_canonical_cbor_hex();
  const moved =
    datumCbor === undefined
      ? { ...spent, outRef: `${txHash}#0` }
      : {
          ...spent,
          outRef: `${txHash}#0`,
          outputCbor,
          datumCbor:
            coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor))
              .datum ?? null,
        };
  const units = Object.keys(
    coreToTxOutput(CML.TransactionOutput.from_cbor_hex(spent.outputCbor))
      .assets,
  ).filter((unit) => unit !== "lovelace");
  return {
    ...snapshot,
    scopes: snapshot.scopes.map((entry) =>
      entry.role === role
        ? {
            ...entry,
            utxos: entry.utxos.map((utxo) => (utxo === spent ? moved : utxo)),
          }
        : entry,
    ),
    history: snapshot.history.map((entry) =>
      units.includes(entry.unit)
        ? {
            ...entry,
            transactionHashes: [...entry.transactionHashes, txHash],
          }
        : entry,
    ),
    transactions: [
      ...snapshot.transactions,
      {
        ...snapshot.transactions[0]!,
        txHash,
        bodyCbor: body.to_cbor_hex(),
        resolvedInputs: [spent],
      },
    ],
  };
};
