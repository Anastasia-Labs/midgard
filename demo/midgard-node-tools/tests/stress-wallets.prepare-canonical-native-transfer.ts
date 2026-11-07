import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import { Context } from "effect";
import { type NodeUtxo } from "midgard-node/commands/command-utils";
import {
  buildTerminalDrainTx,
  buildTransferTxWithMinFee,
} from "midgard-node/commands/submit-l2-transfer";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";

export class FanoutAcquisitionProbe extends Context.Tag(
  "FanoutAcquisitionProbe",
)<FanoutAcquisitionProbe, { readonly acquisition: number }>() {}

export const TEST_SEEDS = [
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
];

export const makeTempDir = async (): Promise<string> =>
  mkdtemp(join(tmpdir(), "midgard-stress-wallets-"));

export const seedGenerator = () => {
  let index = 0;
  return () => TEST_SEEDS[index++]!;
};

export const nodeUtxo = ({
  txHashByte,
  outputIndex = 0,
  address,
  lovelace,
}: {
  readonly txHashByte: string;
  readonly outputIndex?: number;
  readonly address: string;
  readonly lovelace: bigint;
}): NodeUtxo => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  outrefCbor: makeOutRefCbor(txHashByte.repeat(32), outputIndex),
  outputCbor: Buffer.from("00", "hex"),
  address,
  assets: { lovelace },
});

export const prepareCanonicalNativeTransfer = async ({
  sourceSeedPhrase,
  sourceAddress,
  destinationAddress,
  sourceLovelace,
  requestedLovelace,
  txHashByte,
}: {
  readonly sourceSeedPhrase: string;
  readonly sourceAddress: string;
  readonly destinationAddress: string;
  readonly sourceLovelace: bigint;
  readonly requestedLovelace: bigint;
  readonly txHashByte: string;
}) => {
  const wallet = walletFromSeed(sourceSeedPhrase, { network: "Preprod" });
  const txHash = txHashByte.repeat(32);
  const outrefCbor = makeOutRefCbor(txHash, 0);
  const outputCbor = Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(sourceAddress),
      assetsToValue({ lovelace: sourceLovelace }),
    ).to_cbor_bytes(),
  );
  const built = await buildTransferTxWithMinFee({
    senderAddress: sourceAddress,
    destinationAddress,
    signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
    availableUtxos: [
      {
        txHash,
        outputIndex: 0,
        outrefCbor,
        outputCbor,
        address: sourceAddress,
        assets: { lovelace: sourceLovelace },
      },
    ],
    requestedAssets: { lovelace: requestedLovelace },
    network: "Preprod",
    networkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
  });
  return {
    txHash: built.txIdHex,
    signedTxCbor: built.txHex,
    selectedInputs: built.selectedInputs.map(
      (input) => `${input.txHash}#${input.outputIndex.toString()}`,
    ),
  };
};

export const prepareCanonicalTerminalDrain = async ({
  sourceSeedPhrase,
  sourceAddress,
  destinationAddress,
  utxos,
}: {
  readonly sourceSeedPhrase: string;
  readonly sourceAddress: string;
  readonly destinationAddress: string;
  readonly utxos: readonly NodeUtxo[];
}) => {
  const wallet = walletFromSeed(sourceSeedPhrase, { network: "Preprod" });
  const built = await buildTerminalDrainTx({
    senderAddress: sourceAddress,
    destinationAddress,
    signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
    availableUtxos: utxos,
    network: "Preprod",
    networkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
  });
  return {
    txHash: built.txIdHex,
    signedTxCbor: built.txHex,
    selectedInputs: built.selectedInputs.map(
      (x) => x.txHash + "#" + x.outputIndex.toString(),
    ),
    requestedLovelace: built.requestedAssets.lovelace ?? 0n,
    feeLovelace: built.fee,
    signedTxBytes: built.txCbor.length,
  };
};

export const fullConsolidationReadiness = () => ({
  httpStatus: 200,
  body: {
    ready: true,
    reasons: [],
    durableAdmissionBacklog: "0",
    mempoolTxCount: "0",
    unfinishedLocalMutationJobs: "0",
    unresolvedBlockSubmissionAgeMs: 0,
    providerQueryHealthy: true,
    stateQueueMutationLease: { status: "idle", pendingFinalizations: [] },
    blockCommitmentCoordination: {
      commitWorkerActive: false,
      commitPipelinePhase: "idle",
    },
  },
});
