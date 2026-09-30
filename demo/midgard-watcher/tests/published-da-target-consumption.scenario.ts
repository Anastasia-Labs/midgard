import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  toUnit,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  NativeTransactionNotIncludedError,
  type PublishedDaTransactionRecord,
} from "./support/published-da-target-consumption.js";

export const headerHash = "11".repeat(28);

export const stateQueuePolicyId = "44".repeat(28);

export const fraudProofPolicyId = "55".repeat(28);

export const stateQueueAddress = credentialToAddress("Preprod", {
  type: "Script",
  hash: "33".repeat(28),
});

export const fraudProofAddress = credentialToAddress("Preprod", {
  type: "Script",
  hash: "66".repeat(28),
});

const stateQueueAssetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;

export const stateQueueUnit = toUnit(stateQueuePolicyId, stateQueueAssetName);

export const fraudProofUnit = toUnit(
  fraudProofPolicyId,
  "0000001c" + headerHash,
);

export const header: SDK.Header = {
  prevUtxosRoot: "55".repeat(32),
  utxosRoot: "55".repeat(32),
  withdrawalsRoot: "55".repeat(32),
  forcedTransactionsRoot: "55".repeat(32),
  transactionsRoot: "55".repeat(32),
  depositsRoot: "55".repeat(32),
  transitionTraceRoot: "55".repeat(32),
  eventToStepRoot: "55".repeat(32),
  validationTracesRoot: "55".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 0n,
  endTime: 1n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 44n,
  minFeeB: 155381n,
  prevHeaderHash: "66".repeat(28),
  operatorVkey: "77".repeat(28),
  protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
};

export const outRef = (txHash: string, outputIndex: number) =>
  `${txHash}#${outputIndex.toString()}`;

const inputList = (
  refs: readonly { txHash: string; outputIndex: number }[],
) => {
  const list = CML.TransactionInputList.new();
  for (const ref of refs)
    list.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(ref.txHash),
        BigInt(ref.outputIndex),
      ),
    );
  return list;
};

export const nodeOutput = (
  attestation: SDK.StateQueueNode["da_attestation"] = SDK.NO_DA_ATTESTATION,
): UTxO => ({
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: stateQueueAddress,
  assets: { lovelace: 5_000_000n, [stateQueueUnit]: 1n },
  datum: SDK.encodeLinkedListNodeView({
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header,
      da_attestation: attestation,
    }) as SDK.LinkedListNodeView["data"],
  }),
});

export const transaction = (body: {
  inputs?: readonly { txHash: string; outputIndex: number }[];
  outputs?: readonly UTxO[];
  referenceInputs?: readonly { txHash: string; outputIndex: number }[];
  burn?: bigint;
  valid?: boolean;
}) => {
  const outputs = CML.TransactionOutputList.new();
  for (const output of body.outputs ?? [])
    outputs.add(utxoToCore(output).output());
  const core = CML.TransactionBody.new(
    inputList(body.inputs ?? []),
    outputs,
    200_000n,
  );
  if (body.referenceInputs !== undefined)
    core.set_reference_inputs(inputList(body.referenceInputs));
  if (body.burn !== undefined) {
    const mint = CML.Mint.new();
    mint.set(
      CML.ScriptHash.from_hex(stateQueuePolicyId),
      CML.AssetName.from_hex(stateQueueAssetName),
      body.burn,
    );
    core.set_mint(mint);
  }
  const tx = CML.Transaction.new(
    core,
    CML.TransactionWitnessSet.new(),
    body.valid ?? true,
  );
  return { tx, txHash: CML.hash_transaction(tx.body()).to_hex() };
};

/** A commit created the node, a fraud correction burned it, one proof token lives. */
export const scenario = (proofAssets = { [fraudProofUnit]: 1n }) => {
  const creating = transaction({ outputs: [nodeOutput()] });
  const proofTransaction = transaction({
    outputs: [
      {
        txHash: "aa".repeat(32),
        outputIndex: 0,
        address: fraudProofAddress,
        assets: { lovelace: 2_000_000n, ...proofAssets },
      },
    ],
  });
  const proof = { txHash: proofTransaction.txHash, outputIndex: 0 };
  const removal = transaction({
    inputs: [{ txHash: creating.txHash, outputIndex: 0 }],
    referenceInputs: [proof],
    burn: -1n,
  });
  const chain = new Map<string, string>([
    [proofTransaction.txHash, proofTransaction.tx.to_cbor_hex()],
    [creating.txHash, creating.tx.to_cbor_hex()],
    [removal.txHash, removal.tx.to_cbor_hex()],
  ]);
  const input = {
    headerHash,
    stateQueueAddress,
    stateQueuePolicyId,
    fraudProofPolicyId,
    consumptions: [
      {
        txHash: creating.txHash,
        outputIndex: 0,
        spentByTxHash: removal.txHash,
        spentAtSlot: 50,
      },
    ],
    attestationOutputs: [] as UTxO[],
    submitted: [] as PublishedDaTransactionRecord[],
    readConfirmedTransaction: vi.fn(async (txHash: string) => {
      const cbor = chain.get(txHash);
      if (cbor === undefined)
        throw new NativeTransactionNotIncludedError(txHash);
      return { cbor };
    }),
  };
  return { creating, removal, proof, chain, input };
};
