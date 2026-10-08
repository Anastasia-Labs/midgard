import { normalizeOgmiosHttpUrl } from "@al-ft/midgard-core/ogmios-slot";
import {
  fetchKupoAncestorPoint,
  fetchKupoCreationPoint,
  normalizeKupoHttpUrl,
  readOgmiosBlockTransaction,
  type WebSocketFactory,
} from "midgard-node/l1-kupmios";

import {
  type ChainPoint,
  fetchJson,
  type FetchLike,
  joinUrl,
  type LiveEconomicTransaction,
  type LiveTip,
  type LiveTransactionOutput,
  type LocalKupmiosStateCorrectionAuthorityConfig,
  type LocalKupmiosStateCorrectionSource,
  nonNegativeInteger,
  ogmiosResult,
  parseTipPoint,
  record,
  stableJson,
} from "./e2e-state-correction-local-authority.fetch-json.js";
import {
  openOgmiosSession,
  parseKupoOutputs,
  parseOgmiosEconomicTransaction,
} from "./e2e-state-correction-local-authority.open-ogmios-session.js";

const readEconomicOgmiosTransaction = async ({
  ogmiosUrl,
  intersection,
  includedAt,
  txHash,
  timeoutMs,
  webSocketFactory,
}: {
  readonly ogmiosUrl: string;
  readonly intersection: { readonly slot: number; readonly headerHash: string };
  readonly includedAt: ChainPoint;
  readonly txHash: string;
  readonly timeoutMs: number;
  readonly webSocketFactory?: WebSocketFactory;
}): Promise<LiveEconomicTransaction> => {
  const session = await openOgmiosSession({
    ogmiosUrl,
    timeoutMs,
    ...(webSocketFactory === undefined ? {} : { webSocketFactory }),
  });
  try {
    const found = record(
      await session.request("findIntersection", {
        points: [{ slot: intersection.slot, id: intersection.headerHash }],
      }),
      "Q57 Ogmios intersection",
    );
    if (found.intersection === undefined) {
      throw new Error("Q57 Ogmios did not accept the Kupo ancestor");
    }
    let acknowledgedIntersection = false;
    for (let scanned = 0; scanned < 1_000; scanned += 1) {
      const next = record(
        await session.request("nextBlock", {}),
        "Q57 Ogmios nextBlock",
      );
      if (next.direction === "backward") {
        if (acknowledgedIntersection) {
          throw new Error("Q57 Ogmios rolled back during economic observation");
        }
        acknowledgedIntersection = true;
        scanned -= 1;
        continue;
      }
      if (next.direction !== "forward") {
        throw new Error("Q57 Ogmios nextBlock has no direction");
      }
      const block = record(next.block, "Q57 Ogmios block");
      if (block.id !== includedAt.blockHash) {
        if (
          typeof block.slot === "number" &&
          block.slot > Number(includedAt.slot)
        ) {
          throw new Error("Q57 Ogmios passed the Kupo economic block");
        }
        continue;
      }
      if (block.slot?.toString() !== includedAt.slot) {
        throw new Error("Q57 Ogmios economic block slot disagrees with Kupo");
      }
      if (!Array.isArray(block.transactions)) {
        throw new Error("Q57 Ogmios economic block has no transactions");
      }
      const transaction = block.transactions.find(
        (value) => record(value, "Q57 Ogmios transaction").id === txHash,
      );
      if (transaction === undefined) {
        throw new Error(`Q57 Ogmios block does not contain ${txHash}`);
      }
      return parseOgmiosEconomicTransaction(
        transaction,
        txHash,
        `Q57 Ogmios transaction ${txHash}`,
      );
    }
    throw new Error("Q57 Ogmios did not reach the economic transaction");
  } finally {
    session.close();
  }
};

const outputsEqual = (
  left: readonly LiveTransactionOutput[],
  right: readonly LiveTransactionOutput[],
): boolean => stableJson(left) === stableJson(right);

const TIP_READ_ATTEMPTS = 5;

/** The height comes from queryNetwork/blockHeight and is bound to the tip only
 * when two tip reads bracketing it agree. A chain that keeps moving fails. */
const queryTip = async ({
  ogmiosUrl,
  fetchImpl,
  timeoutMs,
}: {
  readonly ogmiosUrl: string;
  readonly fetchImpl: FetchLike;
  readonly timeoutMs: number;
}): Promise<LiveTip> => {
  const query = async (method: string): Promise<unknown> =>
    await fetchJson({
      fetchImpl,
      url: normalizeOgmiosHttpUrl(ogmiosUrl),
      timeoutMs,
      init: {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          method,
          id: "midgard-q57-authority-tip",
        }),
      },
    });
  for (let attempt = 0; attempt < TIP_READ_ATTEMPTS; attempt += 1) {
    const before = parseTipPoint(
      await query("queryNetwork/tip"),
      "live Ogmios tip",
    );
    const height = nonNegativeInteger(
      ogmiosResult(
        await query("queryNetwork/blockHeight"),
        "live Ogmios block height",
      ),
      "live Ogmios block height",
    );
    const after = parseTipPoint(
      await query("queryNetwork/tip"),
      "live Ogmios tip",
    );
    if (before.slot === after.slot && before.blockHash === after.blockHash) {
      return { ...before, height };
    }
  }
  throw new Error(
    `live Ogmios tip moved during each of ${TIP_READ_ATTEMPTS.toString()} block height reads`,
  );
};

export const createLocalKupmiosStateCorrectionSource = (
  config: Omit<LocalKupmiosStateCorrectionAuthorityConfig, "source">,
): LocalKupmiosStateCorrectionSource => {
  const fetchImpl = config.fetchImpl ?? fetch;
  const timeoutMs = config.timeoutMs ?? 20_000;
  const kupoUrl = normalizeKupoHttpUrl(config.kupoUrl);
  const ogmiosUrl = config.ogmiosUrl;
  return {
    observeTransaction: async ({ txHash, outputIndex }) => {
      const kupoPoint = await fetchKupoCreationPoint({
        kupoUrl,
        outRef: { txHash, outputIndex },
        fetchImpl,
        timeoutMs,
      });
      const kupoIncludedAt = {
        slot: kupoPoint.slot.toString(),
        blockHash: kupoPoint.headerHash,
      };
      const ancestor = await fetchKupoAncestorPoint({
        kupoUrl,
        slot: kupoPoint.slot,
        fetchImpl,
        timeoutMs,
      });
      const observed = await readOgmiosBlockTransaction({
        ogmiosUrl,
        intersection: ancestor,
        blockPoint: kupoPoint,
        txHash,
        ...(config.webSocketFactory === undefined
          ? {}
          : { webSocketFactory: config.webSocketFactory }),
        timeoutMs,
      });
      const liveTip = await queryTip({ ogmiosUrl, fetchImpl, timeoutMs });
      return {
        kupoIncludedAt,
        ogmiosIncludedAt: kupoIncludedAt,
        liveTip,
        confirmationDepth: liveTip.height - observed.blockPoint.blockNo + 1,
      };
    },
    observeOutput: async ({ txHash, outputIndex }) => {
      const url = joinUrl(
        kupoUrl,
        `/matches/${outputIndex.toString()}@${txHash}?resolve_hashes`,
      );
      const outputs = parseKupoOutputs(
        await fetchJson({ fetchImpl, url, timeoutMs }),
        `live Kupo output ${txHash}#${outputIndex.toString()}`,
      ).filter(
        (output) =>
          output.txHash === txHash && output.outputIndex === outputIndex,
      );
      if (outputs.length > 1) {
        throw new Error(
          `live Kupo returned duplicate output ${txHash}#${outputIndex.toString()}`,
        );
      }
      return outputs[0] ?? null;
    },
    observeEconomicTransaction: async ({ txHash, outputIndex, includedAt }) => {
      const [kupoOutputs, ancestor] = await Promise.all([
        fetchJson({
          fetchImpl,
          url: joinUrl(kupoUrl, `/matches/*@${txHash}?resolve_hashes`),
          timeoutMs,
        }).then((value) =>
          parseKupoOutputs(value, `Q57 Kupo transaction outputs ${txHash}`),
        ),
        fetchKupoAncestorPoint({
          kupoUrl,
          slot: Number(includedAt.slot),
          fetchImpl,
          timeoutMs,
        }),
      ]);
      if (
        kupoOutputs.length === 0 ||
        kupoOutputs.some((output) => output.txHash !== txHash) ||
        !kupoOutputs.some((output) => output.outputIndex === outputIndex)
      ) {
        throw new Error(
          `Q57 Kupo returned an incomplete output set for ${txHash}`,
        );
      }
      const outputIndices = kupoOutputs
        .map(({ outputIndex: index }) => index)
        .sort((left, right) => left - right);
      if (
        outputIndices.some((index, position) => index !== position) ||
        new Set(outputIndices).size !== outputIndices.length
      ) {
        throw new Error(`Q57 Kupo output set is non-contiguous for ${txHash}`);
      }
      const ogmios = await readEconomicOgmiosTransaction({
        ogmiosUrl,
        intersection: ancestor,
        includedAt,
        txHash,
        timeoutMs,
        ...(config.webSocketFactory === undefined
          ? {}
          : { webSocketFactory: config.webSocketFactory }),
      });
      const normalizedKupoOutputs = [...kupoOutputs]
        .sort((left, right) => left.outputIndex - right.outputIndex)
        .map(({ address, lovelace, assets }) => ({
          address,
          lovelace,
          assets,
        }));
      if (!outputsEqual(normalizedKupoOutputs, ogmios.outputs)) {
        throw new Error(
          `live Kupo/Ogmios output disagreement for transaction ${txHash}`,
        );
      }
      return ogmios;
    },
    observeUnspentAddress: async ({ address }) =>
      parseKupoOutputs(
        await fetchJson({
          fetchImpl,
          url: joinUrl(kupoUrl, `/matches/${address}?unspent`),
          timeoutMs,
        }),
        `live Kupo unspent address ${address}`,
      ).filter((output) => !output.spent),
    observeStateQueue: async ({ address, policyId }) => {
      const url = joinUrl(kupoUrl, `/matches/${address}?unspent`);
      const outputs = parseKupoOutputs(
        await fetchJson({ fetchImpl, url, timeoutMs }),
        "live Kupo state queue",
      );
      const blockPrefix = `${policyId}4d424c43`;
      return {
        depth: outputs.filter((output) =>
          Object.entries(output.assets).some(
            ([unit, quantity]) =>
              unit.startsWith(blockPrefix) && quantity === "1",
          ),
        ).length,
      };
    },
    observeTip: async () => await queryTip({ ogmiosUrl, fetchImpl, timeoutMs }),
    observeDatabase: config.observeDatabase,
  };
};
