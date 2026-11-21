import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";

const h = (byte: string) => byte.repeat(28);
const tx = (byte: string) => byte.repeat(32);
const address = credentialToAddress("Preprod", { type: "Key", hash: h("dd") });
const utxo = (byte: string): UTxO => ({
  txHash: tx(byte),
  outputIndex: 0,
  address,
  assets: { lovelace: 20_000_000n },
});
const node = (
  byte: string,
  next?: string,
  attested = false,
): SDKStateQueue.StateQueueUTxO => ({
  assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + h(byte),
  utxo: utxo(byte),
  datum: {
    key: { Key: { key: h(byte) } },
    next: next === undefined ? "Empty" : { Key: { key: h(next) } },
    data: attested ? "attested" : "unattested",
  },
});

export { address, h, node, tx, utxo };
