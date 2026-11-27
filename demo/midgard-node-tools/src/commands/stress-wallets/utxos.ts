import { type NodeUtxo } from "midgard-node/commands/command-utils";

import {
  FANOUT_UTXO_QUERY_INITIAL_RETRY_MS,
  FANOUT_UTXO_QUERY_MAX_ATTEMPTS,
  FANOUT_UTXO_QUERY_MAX_RETRY_MS,
} from "./constants.js";
import { nextFanoutPollDelayMs } from "./runtime.js";
import {
  type StressWalletFundingUtxoSnapshot,
  type StressWalletRecord,
} from "./types.js";

export const outRefKey = (utxo: NodeUtxo): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const fundingUtxos = (
  utxos: readonly NodeUtxo[],
  lovelacePerWallet: bigint,
): readonly NodeUtxo[] =>
  utxos.filter((utxo) => (utxo.assets.lovelace ?? 0n) >= lovelacePerWallet);

export const newFundingUtxos = ({
  before,
  after,
  lovelacePerWallet,
}: {
  readonly before: readonly NodeUtxo[];
  readonly after: readonly NodeUtxo[];
  readonly lovelacePerWallet: bigint;
}): readonly NodeUtxo[] => {
  const beforeOutRefs = new Set(before.map(outRefKey));
  return fundingUtxos(after, lovelacePerWallet).filter(
    (utxo) => !beforeOutRefs.has(outRefKey(utxo)),
  );
};

export const queryWalletUtxos = async ({
  records,
  nodeEndpoint,
  fetchUtxos,
}: {
  readonly records: readonly StressWalletRecord[];
  readonly nodeEndpoint: string;
  readonly fetchUtxos: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
}): Promise<Map<string, readonly NodeUtxo[]>> => {
  const entries = await Promise.all(
    records.map(
      async (record) =>
        [
          record.envName,
          await fetchUtxos(nodeEndpoint, record.l2Address),
        ] as const,
    ),
  );
  return new Map(entries);
};

export const verifiedFundingSnapshots = (
  utxos: readonly NodeUtxo[],
  lovelacePerWallet: bigint,
): readonly StressWalletFundingUtxoSnapshot[] =>
  fundingUtxos(utxos, lovelacePerWallet).map((utxo) => ({
    outref: outRefKey(utxo),
    outputCbor: utxo.outputCbor.toString("hex"),
    lovelace: (utxo.assets.lovelace ?? 0n).toString(10),
  }));

export const firstFundingUtxo = (
  utxos: readonly NodeUtxo[],
  minimumLovelace: bigint,
): NodeUtxo | undefined => fundingUtxos(utxos, minimumLovelace)[0];

export const fetchFanoutUtxosWithRetry = async ({
  nodeEndpoint,
  address,
  fetchUtxos,
  sleep: sleepImpl,
}: {
  readonly nodeEndpoint: string;
  readonly address: string;
  readonly fetchUtxos: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep: (ms: number) => Promise<void>;
}): Promise<readonly NodeUtxo[]> => {
  let lastError: unknown;
  for (
    let attempt = 0;
    attempt < FANOUT_UTXO_QUERY_MAX_ATTEMPTS;
    attempt += 1
  ) {
    try {
      return await fetchUtxos(nodeEndpoint, address);
    } catch (error) {
      lastError = error;
      if (attempt === FANOUT_UTXO_QUERY_MAX_ATTEMPTS - 1) {
        break;
      }
      await sleepImpl(
        nextFanoutPollDelayMs({
          attempt,
          initialMs: FANOUT_UTXO_QUERY_INITIAL_RETRY_MS,
          maxMs: FANOUT_UTXO_QUERY_MAX_RETRY_MS,
        }),
      );
    }
  }
  throw lastError instanceof Error ? lastError : new Error(String(lastError));
};

export const sumLovelace = (utxos: readonly NodeUtxo[]): bigint =>
  utxos.reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

export const utxoAccounting = (utxos: readonly NodeUtxo[]) => ({
  lovelace: sumLovelace(utxos).toString(10),
  utxoCount: utxos.length,
  outrefs: utxos.map(outRefKey).sort(),
});
