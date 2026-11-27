import { createHash } from "node:crypto";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec/native";
import { type Network } from "@lucid-evolution/lucid";

import {
  asObject,
  assertExactKeys,
  requiredPositiveInteger,
  requiredString,
} from "./artifact-fields.js";
import { parseStressWalletNetwork } from "./options.js";
import { type StressWalletRecord } from "./types.js";

export type StressWalletOperationScope = {
  readonly count: number;
  readonly startIndex: number;
  readonly envPrefix: string;
  readonly network: Network;
  readonly walletSetSha256: string;
};

export const buildStressWalletOperationScope = ({
  records,
  count,
  startIndex,
  envPrefix,
  network,
}: {
  readonly records: readonly StressWalletRecord[];
  readonly count: number;
  readonly startIndex: number;
  readonly envPrefix: string;
  readonly network: Network;
}): StressWalletOperationScope => {
  const walletSet = records.map((record) => ({
    walletId: record.walletId,
    index: record.index,
    envName: record.envName,
    l2Address: record.l2Address,
    paymentKeyHash: record.paymentKeyHash,
  }));
  return {
    count,
    startIndex,
    envPrefix,
    network,
    walletSetSha256: createHash("sha256")
      .update(JSON.stringify(walletSet))
      .digest("hex"),
  };
};

export const sameStressWalletOperationScope = (
  left: StressWalletOperationScope,
  right: StressWalletOperationScope,
): boolean => JSON.stringify(left) === JSON.stringify(right);

export const computeSignedNativeTxHash = (
  signedTxCbor: string,
  walletId: string,
): string => {
  try {
    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(signedTxCbor, "hex"),
    );
    return computeMidgardNativeTxId(decoded).toString("hex");
  } catch (cause) {
    throw new Error(
      `Consolidation state signedTxCbor for ${walletId} is not canonical Midgard native transaction CBOR: ${String(cause)}`,
    );
  }
};

export const parseStressWalletOperationScope = (
  value: unknown,
): StressWalletOperationScope => {
  const raw = asObject(value, "scope");
  assertExactKeys(raw, "scope", [
    "count",
    "startIndex",
    "envPrefix",
    "network",
    "walletSetSha256",
  ]);
  const walletSetSha256 = requiredString(
    raw.walletSetSha256,
    "scope.walletSetSha256",
  );
  if (!/^[0-9a-f]{64}$/.test(walletSetSha256)) {
    throw new Error(
      "scope.walletSetSha256 must be a lowercase SHA-256 digest.",
    );
  }
  return {
    count: requiredPositiveInteger(raw.count, "scope.count"),
    startIndex: requiredPositiveInteger(raw.startIndex, "scope.startIndex"),
    envPrefix: requiredString(raw.envPrefix, "scope.envPrefix"),
    network: parseStressWalletNetwork(
      requiredString(raw.network, "scope.network"),
      {},
    ),
    walletSetSha256,
  };
};
