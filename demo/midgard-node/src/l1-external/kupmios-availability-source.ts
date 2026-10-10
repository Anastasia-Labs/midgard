/**
 * The availability command's canonical source under `--l1 kupmios`
 * (`../commands/availability-challenge-source.ts`): Kupo and Ogmios, history
 * included. Tools only; reached by a dynamic import from the command.
 *
 * - Boundary: the Ogmios tip with its height, required to be exactly Kupo's
 *   most recent checkpoint, so the index and the node agree on one point.
 * - Anchor: Kupo's checkpoint at the anchor's slot is the anchor.
 * - Inclusion: Kupo's creation point of the transaction's first output, its
 *   block read by chain-sync for the block number, counted against the
 *   boundary.
 * - Foreign spend: Kupo's exact `spent_at`, the spending transaction read by
 *   chain-sync to its exact block (verified by the SDK from its bytes).
 * - Unit history: every Kupo match (live or spent) holding the unit, with
 *   its inline datum.
 */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type {
  AvailabilityCanonicalSource,
  CanonicalBoundary,
} from "../commands/availability-challenge-source.js";
import {
  fetchKupoAncestorPoint,
  fetchKupoCreationPoint,
  fetchKupoSpend,
  type FetchLike,
  readLocalOgmiosTip,
  readOgmiosBlockTransaction,
  type WebSocketFactory,
} from "./kupmios-history.js";
import {
  DEFAULT_L1_READ_TIMEOUT_MS,
  fetchJsonWithTimeout,
  joinUrl,
  normalizeKupoHttpUrl,
} from "./kupmios-history.l1-chain-point.js";

export type KupmiosAvailabilitySourceInput = Readonly<{
  lucid: LucidEvolution;
  kupoUrl: string;
  ogmiosUrl: string;
  fetchImpl?: FetchLike;
  webSocketFactory?: WebSocketFactory;
}>;

const optional = <K extends string, V>(key: K, value: V | undefined) =>
  (value === undefined ? {} : { [key]: value }) as Partial<Record<K, V>>;

/** Kupo's most recent checkpoint, from its `/health` metrics and ETag. */
const readKupoCheckpoint = async (
  input: KupmiosAvailabilitySourceInput,
): Promise<Readonly<{ slot: number; blockHash: string | undefined }>> => {
  const response = await (input.fetchImpl ?? fetch)(
    joinUrl(normalizeKupoHttpUrl(input.kupoUrl), "/health"),
    {
      headers: { accept: "text/plain" },
      signal: AbortSignal.timeout(DEFAULT_L1_READ_TIMEOUT_MS),
    },
  );
  if (!response.ok)
    throw new Error(
      "Availability command could not read the canonical Kupo checkpoint",
    );
  const checkpoint = (await response.text()).match(
    /^kupo_most_recent_checkpoint\s+([0-9]+(?:\.[0-9]+)?)/mu,
  );
  return {
    slot: checkpoint === null ? NaN : Number(checkpoint[1]),
    blockHash: response.headers
      .get("etag")
      ?.replace(/^W\//u, "")
      .replace(/^"|"$/gu, "")
      .toLowerCase(),
  };
};

/** The datums of every Kupo match holding `policyId.assetName`, live or spent. */
export const kupoUnitHistory =
  (input: Pick<KupmiosAvailabilitySourceInput, "kupoUrl" | "fetchImpl">) =>
  async (
    unit: Readonly<{ policyId: string; assetName: string }>,
  ): Promise<readonly (string | null)[]> => {
    const url = joinUrl(
      normalizeKupoHttpUrl(input.kupoUrl),
      `/matches/${unit.policyId}.${unit.assetName === "" ? "*" : unit.assetName}?resolve_hashes`,
    );
    const body = await fetchJsonWithTimeout(
      input.fetchImpl ?? fetch,
      url,
      DEFAULT_L1_READ_TIMEOUT_MS,
    );
    if (!Array.isArray(body))
      throw new Error(`Kupo returned no match array for ${url}`);
    return body.map((match: { datum_type?: unknown; datum?: unknown }) =>
      match.datum_type === "inline" && typeof match.datum === "string"
        ? match.datum
        : null,
    );
  };

export const availabilityKupmiosSource = (
  input: KupmiosAvailabilitySourceInput,
): AvailabilityCanonicalSource => {
  const http = optional("fetchImpl", input.fetchImpl);
  const ws = optional("webSocketFactory", input.webSocketFactory);
  const readBoundary = async (): Promise<CanonicalBoundary> => {
    const tip = await readLocalOgmiosTip(input.ogmiosUrl, http);
    const checkpoint = await readKupoCheckpoint(input);
    if (
      !Number.isSafeInteger(checkpoint.slot) ||
      checkpoint.slot !== tip.slot ||
      checkpoint.blockHash !== tip.blockHash
    )
      throw new Error(
        "Availability command requires Kupo and Ogmios aligned at the same canonical tip",
      );
    return {
      pointId: `${tip.slot.toString()}:${tip.blockHash}`,
      slot: tip.slot,
      blockNo: tip.blockNo,
      blockHash: tip.blockHash,
    };
  };
  return {
    readBoundary,
    assertCanonicalAncestor: async (anchor) => {
      await readBoundary();
      const ancestor = await fetchKupoAncestorPoint({
        kupoUrl: input.kupoUrl,
        slot: anchor.slot + 1,
        ...http,
      });
      if (
        ancestor.slot !== anchor.slot ||
        ancestor.headerHash !== anchor.blockHash
      )
        throw new Error(
          "Availability command canonical generation changed; recover durable intents before new work",
        );
    },
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      resolveInclusion: async (output) => {
        const before = await readBoundary();
        const point = await fetchKupoCreationPoint({
          kupoUrl: input.kupoUrl,
          outRef: output,
          ...http,
        });
        const intersection = await fetchKupoAncestorPoint({
          kupoUrl: input.kupoUrl,
          slot: point.slot,
          ...http,
        });
        const transaction = await readOgmiosBlockTransaction({
          ogmiosUrl: input.ogmiosUrl,
          intersection,
          blockPoint: point,
          txHash: output.txHash,
          ...ws,
        });
        const after = await readBoundary();
        if (
          before.pointId !== after.pointId ||
          transaction.txHash !== output.txHash ||
          transaction.blockPoint.blockNo > after.blockNo
        )
          throw new Error(
            "Availability transaction inclusion changed during its canonical read",
          );
        return {
          slot: transaction.blockPoint.slot,
          blockHash: transaction.blockPoint.headerHash,
          depth: after.blockNo - transaction.blockPoint.blockNo,
        };
      },
      resolveForeignSpend: (outRef) =>
        SDK.resolveDaAvailabilityForeignSpend({
          outRef,
          readBoundary,
          fetchSpend: async (ref) => {
            const spend = await fetchKupoSpend({
              kupoUrl: input.kupoUrl,
              outRef: ref,
              ...http,
            });
            return spend === null
              ? undefined
              : {
                  transactionId: spend.transactionId,
                  point: {
                    slot: spend.point.slot,
                    blockHash: spend.point.headerHash,
                  },
                };
          },
          fetchAncestor: async (slot) => {
            const ancestor = await fetchKupoAncestorPoint({
              kupoUrl: input.kupoUrl,
              slot,
              ...http,
            });
            return { slot: ancestor.slot, blockHash: ancestor.headerHash };
          },
          readTransaction: async ({ ancestor, point, txHash }) => {
            const transaction = await readOgmiosBlockTransaction({
              ogmiosUrl: input.ogmiosUrl,
              intersection: {
                slot: ancestor.slot,
                headerHash: ancestor.blockHash,
              },
              blockPoint: { slot: point.slot, headerHash: point.blockHash },
              txHash,
              ...ws,
            });
            return {
              txHash: transaction.txHash,
              point: {
                slot: transaction.blockPoint.slot,
                blockHash: transaction.blockPoint.headerHash,
                blockNo: transaction.blockPoint.blockNo,
              },
              ...optional("cbor", transaction.transactionCbor),
            };
          },
        }),
    }),
    unitHistory: kupoUnitHistory(input),
  };
};
