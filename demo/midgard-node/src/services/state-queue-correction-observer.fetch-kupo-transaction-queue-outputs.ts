import { type StateQueueTransitionNode } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type FetchLike,
  normalizeKupoHttpUrl,
} from "../l1-tx-order-carriage.js";
import { type HistoricalQueueOutput } from "./state-queue-correction-observer.decode-kupo-correction-lock-match.js";
import {
  HEX_28,
  parseQueue,
} from "./state-queue-correction-observer.parse-state-queue-correction-observer-state.js";

export const fetchKupoTransactionQueueOutputs = async ({
  kupoUrl,
  transactionHash,
  stateQueueAddress,
  stateQueuePolicyId,
  fetchImpl,
}: {
  readonly kupoUrl: string;
  readonly transactionHash: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly fetchImpl: FetchLike;
}): Promise<readonly HistoricalQueueOutput[]> => {
  const url = `${normalizeKupoHttpUrl(kupoUrl).replace(/\/+$/u, "")}/matches/*@${transactionHash}?resolve_hashes`;
  const response = await fetchImpl(url);
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Kupo transaction-output query failed with HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  let decodedBody: unknown;
  try {
    decodedBody = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("Kupo transaction-output query returned malformed JSON", {
      cause,
    });
  }
  if (!Array.isArray(decodedBody)) {
    throw new Error("Kupo transaction-output query did not return an array");
  }
  const queueOutputs: HistoricalQueueOutput[] = [];
  for (const [matchIndex, candidate] of decodedBody.entries()) {
    const match = candidate as {
      transaction_id?: unknown;
      output_index?: unknown;
      address?: unknown;
      datum_type?: unknown;
      datum?: unknown;
      value?: { assets?: unknown };
    };
    if (
      match.transaction_id !== transactionHash ||
      typeof match.output_index !== "number" ||
      !Number.isSafeInteger(match.output_index) ||
      match.output_index < 0 ||
      typeof match.address !== "string" ||
      typeof match.value !== "object" ||
      match.value === null ||
      typeof match.value.assets !== "object" ||
      match.value.assets === null ||
      Array.isArray(match.value.assets)
    ) {
      throw new Error(
        `Kupo transaction output ${matchIndex.toString()} is non-canonical`,
      );
    }
    const stateQueueAssets = Object.entries(
      match.value.assets as Record<string, unknown>,
    ).flatMap(([rawUnit, quantity]) => {
      const unit = rawUnit.replaceAll(".", "");
      return unit.startsWith(stateQueuePolicyId) &&
        (quantity === 1 || quantity === "1")
        ? [unit.slice(stateQueuePolicyId.length)]
        : unit.startsWith(stateQueuePolicyId)
          ? [null]
          : [];
    });
    if (stateQueueAssets.length === 0) continue;
    if (
      match.address !== stateQueueAddress ||
      stateQueueAssets.length !== 1 ||
      stateQueueAssets[0] === null ||
      match.datum_type !== "inline" ||
      typeof match.datum !== "string"
    ) {
      throw new Error(
        "State-queue policy output has a foreign address, quantity, or non-inline datum",
      );
    }
    const assetName = stateQueueAssets[0];
    const headerHash =
      assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME
        ? null
        : assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX) &&
            HEX_28.test(
              assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length),
            )
          ? assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length)
          : undefined;
    if (headerHash === undefined) {
      throw new Error(
        "State-queue policy output carries an unknown asset name",
      );
    }
    let view: SDK.LinkedListNodeView;
    try {
      view = SDK.linkedListDatumToNodeView(
        Data.from(match.datum, SDK.LinkedListDatum),
        assetName,
      );
    } catch (cause) {
      throw new Error("State-queue output carries an invalid inline datum", {
        cause,
      });
    }
    const viewHeaderHash = view.key === "Empty" ? null : view.key.Key.key;
    if (viewHeaderHash !== headerHash) {
      throw new Error(
        "State-queue output asset identity disagrees with its datum",
      );
    }
    queueOutputs.push({
      node: {
        headerHash,
        outRef: `${transactionHash}#${match.output_index.toString()}`,
      },
      nextHeaderHash: view.next === "Empty" ? null : view.next.Key.key,
    });
  }
  if (
    new Set(queueOutputs.map(({ node }) => node.headerHash)).size !==
      queueOutputs.length ||
    new Set(queueOutputs.map(({ node }) => node.outRef)).size !==
      queueOutputs.length
  ) {
    throw new Error("Kupo returned duplicate state-queue transaction outputs");
  }
  return queueOutputs;
};

export const reconstructQueueAfterTransaction = ({
  previousQueue,
  transactionHash,
  spentInputOutRefs,
  outputs,
}: {
  readonly previousQueue: readonly StateQueueTransitionNode[];
  readonly transactionHash: string;
  readonly spentInputOutRefs: readonly string[];
  readonly outputs: readonly HistoricalQueueOutput[];
}): readonly StateQueueTransitionNode[] => {
  const spent = new Set(spentInputOutRefs);
  const previousIdentities = new Set(
    previousQueue.map(({ headerHash }) => headerHash),
  );
  const outputByIdentity = new Map(
    outputs.map(({ node }) => [node.headerHash, node]),
  );
  if (
    outputs.length === 0 ||
    !previousQueue.some(({ outRef }) => spent.has(outRef)) ||
    outputs.some(
      ({ node }) =>
        previousIdentities.has(node.headerHash) &&
        !previousQueue.some(
          (prior) =>
            prior.headerHash === node.headerHash && spent.has(prior.outRef),
        ),
    )
  ) {
    throw new Error("State-queue transaction outputs do not follow its inputs");
  }
  const retained = previousQueue.flatMap((node) => {
    if (!spent.has(node.outRef)) return [node];
    const continuation = outputByIdentity.get(node.headerHash);
    return continuation === undefined ? [] : [continuation];
  });
  const introduced = outputs
    .map(({ node }) => node)
    .filter(({ headerHash }) => !previousIdentities.has(headerHash));
  if (
    introduced.length > 1 ||
    introduced.some(({ headerHash }) => headerHash === null)
  ) {
    throw new Error(
      "State-queue transaction introduced a non-canonical identity set",
    );
  }
  const nextQueue = [...retained, ...introduced];
  if (parseQueue(nextQueue) === null) {
    throw new Error("State-queue transaction reconstructed an invalid queue");
  }
  const expectedNextByIdentity = new Map(
    nextQueue.map((node, index) => [
      node.headerHash,
      nextQueue[index + 1]?.headerHash ?? null,
    ]),
  );
  if (
    outputs.some(
      ({ node, nextHeaderHash }) =>
        expectedNextByIdentity.get(node.headerHash) !== nextHeaderHash,
    )
  ) {
    throw new Error(
      "State-queue output links disagree with reconstructed order",
    );
  }
  if (
    nextQueue.every(({ outRef }) => !outRef.startsWith(`${transactionHash}#`))
  ) {
    throw new Error(
      "State-queue transaction has no authenticated continuation",
    );
  }
  return nextQueue;
};
