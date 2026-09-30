import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { L1SourceIntegrityError } from "./source-integrity.js";
import {
  HEX_28,
  HEX_32,
  type HistoricalOutput,
  httpUrl,
  json,
  openRpc,
  type Point,
  type Spend,
  type StateQueueReplayFetch,
  type StateQueueReplayWebSocketFactory,
  type Transaction,
} from "./state-queue-replay-provider.open-rpc.js";

const parseTransaction = (
  value: unknown,
  block: { blockHash: string; slot: number; blockNo: number },
  transactionIndex: number,
): Transaction => {
  const tx = value as {
    id?: unknown;
    cbor?: unknown;
    inputs?: unknown;
    references?: unknown;
    mint?: unknown;
    redeemers?: unknown;
  };
  if (typeof tx.id !== "string" || !HEX_32.test(tx.id)) {
    throw new Error("Ogmios replay transaction id is invalid");
  }
  if (!Array.isArray(tx.inputs))
    throw new Error("Ogmios replay inputs are absent");
  const spentInputOutRefs = tx.inputs.map((item) => {
    const input = item as { transaction?: { id?: unknown }; index?: unknown };
    if (
      typeof input.transaction?.id !== "string" ||
      !HEX_32.test(input.transaction.id) ||
      typeof input.index !== "number" ||
      !Number.isSafeInteger(input.index) ||
      input.index < 0
    ) {
      throw new Error("Ogmios replay input is invalid");
    }
    return `${input.transaction.id}#${input.index.toString()}`;
  });
  const references = tx.references ?? [];
  if (!Array.isArray(references)) {
    throw new Error("Ogmios replay reference inputs are invalid");
  }
  const referenceInputOutRefs = references.map((item) => {
    const input = item as { transaction?: { id?: unknown }; index?: unknown };
    if (
      typeof input.transaction?.id !== "string" ||
      !HEX_32.test(input.transaction.id) ||
      typeof input.index !== "number" ||
      !Number.isSafeInteger(input.index) ||
      input.index < 0
    ) {
      throw new Error("Ogmios replay reference input is invalid");
    }
    return `${input.transaction.id}#${input.index.toString()}`;
  });
  const mint = tx.mint ?? {};
  if (typeof mint !== "object" || mint === null || Array.isArray(mint)) {
    throw new Error("Ogmios replay mint is invalid");
  }
  const mintPolicyIds = Object.keys(mint);
  if (mintPolicyIds.some((policyId) => !HEX_28.test(policyId))) {
    throw new Error("Ogmios replay mint policy id is invalid");
  }
  mintPolicyIds.sort();
  const redeemers = tx.redeemers ?? [];
  if (!Array.isArray(redeemers))
    throw new Error("Ogmios replay redeemers are invalid");
  const parsedRedeemers = redeemers.map((item) => {
    const redeemer = item as {
      redeemer?: unknown;
      validator?: { purpose?: unknown; index?: unknown };
    };
    if (
      typeof redeemer.redeemer !== "string" ||
      typeof redeemer.validator?.purpose !== "string" ||
      typeof redeemer.validator.index !== "number" ||
      !Number.isSafeInteger(redeemer.validator.index) ||
      redeemer.validator.index < 0
    ) {
      throw new Error("Ogmios replay redeemer is invalid");
    }
    return {
      purpose: redeemer.validator.purpose,
      index: redeemer.validator.index.toString(),
      cborHex: redeemer.redeemer,
    };
  });
  return {
    transactionHash: tx.id,
    ...block,
    transactionIndex,
    mintPolicyIds,
    redeemers: parsedRedeemers,
    spentInputOutRefs,
    referenceInputOutRefs,
    ...(typeof tx.cbor === "string" ? { cbor: tx.cbor } : {}),
  };
};

export const readTransaction = async (
  ogmiosUrl: string,
  ancestor: Point,
  spend: Spend,
  factory: StateQueueReplayWebSocketFactory,
): Promise<Transaction> => {
  const rpc = await openRpc(ogmiosUrl, factory);
  try {
    const found = (await rpc.request("findIntersection", {
      points: [{ slot: ancestor.slot, id: ancestor.blockHash }],
    })) as { intersection?: unknown };
    if (found.intersection === undefined)
      throw new Error("Ogmios found no replay intersection");
    let handshakeRollback = false;
    for (let scanned = 0; scanned < 1_000; scanned += 1) {
      const next = (await rpc.request("nextBlock", {})) as {
        direction?: unknown;
        block?: unknown;
      };
      if (next.direction === "backward" && !handshakeRollback) {
        handshakeRollback = true;
        scanned -= 1;
        continue;
      }
      if (next.direction === "backward")
        throw new Error("Ogmios rolled back during replay");
      if (next.direction !== "forward")
        throw new Error("Ogmios replay direction is invalid");
      const block = next.block as {
        id?: unknown;
        slot?: unknown;
        height?: unknown;
        transactions?: unknown;
      };
      if (block.id !== spend.point.blockHash) {
        if (typeof block.slot === "number" && block.slot > spend.point.slot) {
          throw new Error("Ogmios replay passed the Kupo spend point");
        }
        continue;
      }
      if (
        typeof block.id !== "string" ||
        !HEX_32.test(block.id) ||
        typeof block.slot !== "number" ||
        !Number.isSafeInteger(block.slot) ||
        block.slot < 0 ||
        typeof block.height !== "number" ||
        !Number.isSafeInteger(block.height) ||
        block.height < 0 ||
        !Array.isArray(block.transactions)
      ) {
        throw new Error("Ogmios replay block is invalid");
      }
      const index = block.transactions.findIndex(
        (item) => (item as { id?: unknown }).id === spend.transactionHash,
      );
      if (index < 0)
        throw new L1SourceIntegrityError(
          "Kupo spend transaction is absent from Ogmios block",
        );
      return parseTransaction(
        block.transactions[index],
        { blockHash: block.id, slot: block.slot, blockNo: block.height },
        index,
      );
    }
    throw new Error("Ogmios replay exceeded its block scan bound");
  } finally {
    rpc.close();
  }
};

export const fetchOutputs = async (
  kupoUrl: string,
  transactionHash: string,
  stateQueueAddress: string,
  stateQueuePolicyId: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<readonly HistoricalOutput[]> => {
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/*@${transactionHash}?resolve_hashes`,
  );
  if (!Array.isArray(body))
    throw new Error("Kupo transaction output lookup is invalid");
  const outputs: HistoricalOutput[] = [];
  for (const item of body) {
    const output = item as {
      transaction_id?: unknown;
      output_index?: unknown;
      address?: unknown;
      datum_type?: unknown;
      datum?: unknown;
      value?: { assets?: unknown };
    };
    if (
      output.transaction_id !== transactionHash ||
      typeof output.output_index !== "number" ||
      !Number.isSafeInteger(output.output_index) ||
      output.output_index < 0 ||
      typeof output.value?.assets !== "object" ||
      output.value.assets === null ||
      Array.isArray(output.value.assets)
    ) {
      throw new Error("Kupo replay transaction output is invalid");
    }
    const assets = Object.entries(
      output.value.assets as Record<string, unknown>,
    ).flatMap(([unit, quantity]) => {
      const normalized = unit.replaceAll(".", "");
      return normalized.startsWith(stateQueuePolicyId)
        ? [{ assetName: normalized.slice(stateQueuePolicyId.length), quantity }]
        : [];
    });
    if (assets.length === 0) continue;
    if (
      output.address !== stateQueueAddress ||
      assets.length !== 1 ||
      (assets[0]!.quantity !== 1 && assets[0]!.quantity !== "1") ||
      output.datum_type !== "inline" ||
      typeof output.datum !== "string"
    ) {
      throw new L1SourceIntegrityError(
        "state-queue policy output has invalid provenance",
      );
    }
    const assetName = assets[0]!.assetName;
    const headerHash =
      assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME
        ? null
        : assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX) &&
            HEX_28.test(
              assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length),
            )
          ? assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length)
          : undefined;
    if (headerHash === undefined)
      throw new L1SourceIntegrityError("unknown state-queue asset name");
    let view: SDK.LinkedListNodeView;
    try {
      view = SDK.linkedListDatumToNodeView(
        Data.from(output.datum, SDK.LinkedListDatum),
        assetName,
      );
    } catch (cause) {
      throw new L1SourceIntegrityError("state-queue replay datum is invalid", {
        cause,
      });
    }
    const viewHeader = view.key === "Empty" ? null : view.key.Key.key;
    if (viewHeader !== headerHash)
      throw new L1SourceIntegrityError("state-queue asset and datum disagree");
    outputs.push({
      node: {
        headerHash,
        outRef: `${transactionHash}#${output.output_index.toString()}`,
      },
      nextHeaderHash: view.next === "Empty" ? null : view.next.Key.key,
    });
  }
  return outputs;
};

export type CorrectionLockOutput = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
}>;
