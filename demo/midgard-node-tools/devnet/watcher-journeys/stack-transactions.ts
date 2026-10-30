import { appendFile } from "node:fs/promises";
import { join } from "node:path";

import { CML } from "@lucid-evolution/lucid";
import {
  fetchKupoAncestorPoint,
  type L1ChainPoint,
  normalizeKupoHttpUrl,
  readOgmiosBlockTransaction,
  type WebSocketFactory,
} from "midgard-node/l1-external/kupmios-history";

/** A confirmed transaction's exact bytes and the block that carries it. */
export type JourneyConfirmedTransaction = Readonly<{
  cbor: string;
  point: Readonly<{ blockHash: string; blockNo: string; slot: string }>;
}>;

/**
 * Where a journey reads confirmed transactions and how deep the chain it
 * reads them from is: a devnet journey's native recorder, or an attached
 * stack's own Kupo and Ogmios.
 */
export type JourneyChainTransactions = Readonly<{
  /** The journey's record of what it read, kept as evidence. */
  nativeEvidencePath: string;
  transaction(txHash: string): Promise<JourneyConfirmedTransaction>;
  /** The chain's block height as last observed, if any read observed it. */
  observedBlockNo(): bigint | undefined;
}>;

const TX_HASH = /^[0-9a-f]{64}$/u;
const BLOCK_HASH = /^[0-9a-f]{64}$/u;

type KupoCreation = { slot_no?: unknown; header_hash?: unknown };

/**
 * The block Kupo indexed `txHash`'s outputs in. The stack's Kupo matches
 * every output and keeps spent ones, so any transaction with an output is
 * found however long ago it was spent.
 */
const kupoCreationPoint = async (
  kupoUrl: string,
  txHash: string,
  fetchImpl: typeof fetch,
): Promise<L1ChainPoint> => {
  const path = `/matches/*@${txHash}?resolve_hashes`;
  const response = await fetchImpl(
    `${normalizeKupoHttpUrl(kupoUrl).replace(/\/+$/u, "")}${path}`,
    { signal: AbortSignal.timeout(30_000) },
  );
  if (!response.ok)
    throw new Error(`Kupo ${path} failed with ${response.status.toString()}`);
  const matches = (await response.json()) as unknown;
  if (!Array.isArray(matches) || matches.length === 0)
    throw new Error(`Kupo indexes no output of transaction ${txHash}`);
  const points = new Set(
    matches.map((match) => {
      const created = (match as { created_at?: KupoCreation }).created_at;
      if (
        !Number.isSafeInteger(created?.slot_no) ||
        typeof created?.header_hash !== "string" ||
        !BLOCK_HASH.test(created.header_hash)
      )
        throw new Error(
          `Kupo answered a malformed creation point for ${txHash}`,
        );
      return `${String(created.slot_no)}@${created.header_hash}`;
    }),
  );
  if (points.size !== 1)
    throw new Error(
      `Kupo places transaction ${txHash}'s outputs in more than one block`,
    );
  const [slot, headerHash] = [...points][0]!.split("@");
  return { slot: Number(slot), headerHash: headerHash! };
};

/**
 * Confirmed transactions read off an attached stack's own Kupo and Ogmios,
 * which the drill does not run: Kupo names the block a transaction's outputs
 * were created in, and Ogmios chain-sync serves that block's exact bytes
 * (the stack runs it with `--include-transaction-cbor`) and the tip it was
 * read against. Every read is checked to hash to the transaction it names,
 * and appended to `chain-reads.ndjson` in `directory`.
 */
export const stackJourneyTransactions = (
  input: Readonly<{
    kupoUrl: string;
    ogmiosUrl: string;
    directory: string;
    fetchImpl?: typeof fetch;
    webSocketFactory?: WebSocketFactory;
  }>,
): JourneyChainTransactions => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const nativeEvidencePath = join(input.directory, "chain-reads.ndjson");
  let observedBlockNo: bigint | undefined;
  return Object.freeze({
    nativeEvidencePath,
    observedBlockNo: () => observedBlockNo,
    transaction: async (txHash: string) => {
      if (!TX_HASH.test(txHash))
        throw new Error(`Malformed transaction hash ${txHash}`);
      const blockPoint = await kupoCreationPoint(
        input.kupoUrl,
        txHash,
        fetchImpl,
      );
      const observed = await readOgmiosBlockTransaction({
        ogmiosUrl: input.ogmiosUrl,
        intersection: await fetchKupoAncestorPoint({
          kupoUrl: input.kupoUrl,
          slot: blockPoint.slot,
          fetchImpl,
        }),
        blockPoint,
        txHash,
        ...(input.webSocketFactory === undefined
          ? {}
          : { webSocketFactory: input.webSocketFactory }),
      });
      if (observed.transactionCbor === undefined)
        throw new Error(
          "The stack's Ogmios serves no transaction bytes; it must run with --include-transaction-cbor",
        );
      const cbor = observed.transactionCbor;
      if (
        CML.hash_transaction(
          CML.Transaction.from_cbor_hex(cbor).body(),
        ).to_hex() !== txHash
      )
        throw new Error(
          `The bytes Ogmios served for ${txHash} hash to another transaction`,
        );
      const tip = observed.selectedChainTip;
      if (tip !== undefined) {
        const height = BigInt(tip.height);
        if (observedBlockNo === undefined || height > observedBlockNo)
          observedBlockNo = height;
      }
      const point = {
        blockHash: observed.blockPoint.headerHash,
        blockNo: observed.blockPoint.blockNo.toString(),
        slot: observed.blockPoint.slot.toString(),
      };
      await appendFile(
        nativeEvidencePath,
        `${JSON.stringify({
          observedAt: new Date().toISOString(),
          txHash,
          point,
          tipBlockNo: tip?.height ?? null,
        })}\n`,
        { mode: 0o600 },
      );
      return { cbor, point };
    },
  });
};
