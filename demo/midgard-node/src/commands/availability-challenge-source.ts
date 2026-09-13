import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  fetchKupoAncestorPoint,
  fetchKupoCreationPoint,
  readOgmiosBlockTransaction,
} from "../l1-tx-order-carriage.js";
import { readLocalOgmiosTip } from "../services/state-queue-correction-observer.js";

export const availabilityCommandCanonicalSource = (input: {
  readonly lucid: LucidEvolution;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
}) => {
  const readBoundary = async () => {
    const tip = await readLocalOgmiosTip(input.ogmiosUrl);
    const response = await fetch(
      `${input.kupoUrl.replace(/\/+$/u, "")}/health`,
      {
        headers: { accept: "text/plain" },
        signal: AbortSignal.timeout(20_000),
      },
    );
    if (!response.ok)
      throw new Error(
        "Availability command could not read the canonical Kupo checkpoint",
      );
    const metrics = await response.text();
    const checkpoint = metrics.match(
      /^kupo_most_recent_checkpoint\s+([0-9]+(?:\.[0-9]+)?)/mu,
    );
    const slot = checkpoint === null ? NaN : Number(checkpoint[1]);
    const blockHash = response.headers
      .get("etag")
      ?.replace(/^W\//u, "")
      .replace(/^"|"$/gu, "")
      .toLowerCase();
    if (
      !Number.isSafeInteger(slot) ||
      slot !== tip.slot ||
      blockHash !== tip.blockHash
    ) {
      throw new Error(
        "Availability command requires Kupo and Ogmios aligned at the same canonical tip",
      );
    }
    return {
      pointId: `${tip.slot}:${tip.blockHash}`,
      slot: tip.slot,
      blockNo: tip.blockNo,
      blockHash: tip.blockHash,
    };
  };
  return {
    readBoundary,
    async assertCanonicalAncestor(anchor: {
      readonly slot: number;
      readonly blockHash: string;
    }): Promise<void> {
      await readBoundary();
      const ancestor = await fetchKupoAncestorPoint({
        kupoUrl: input.kupoUrl,
        slot: anchor.slot + 1,
      });
      if (
        ancestor.slot !== anchor.slot ||
        ancestor.headerHash !== anchor.blockHash
      ) {
        throw new Error(
          "Availability command canonical generation changed; recover durable intents before new work",
        );
      }
    },
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      resolveInclusion: async (output) => {
        const before = await readBoundary();
        const point = await fetchKupoCreationPoint({
          kupoUrl: input.kupoUrl,
          outRef: output,
        });
        const intersection = await fetchKupoAncestorPoint({
          kupoUrl: input.kupoUrl,
          slot: point.slot,
        });
        const transaction = await readOgmiosBlockTransaction({
          ogmiosUrl: input.ogmiosUrl,
          intersection,
          blockPoint: point,
          txHash: output.txHash,
        });
        const after = await readBoundary();
        if (
          before.pointId !== after.pointId ||
          transaction.txHash !== output.txHash ||
          transaction.blockPoint.blockNo > after.blockNo
        ) {
          throw new Error(
            "Availability transaction inclusion changed during its canonical read",
          );
        }
        return {
          slot: transaction.blockPoint.slot,
          blockHash: transaction.blockPoint.headerHash,
          depth: after.blockNo - transaction.blockPoint.blockNo,
        };
      },
    }),
  };
};
