import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import type { SpendingValidator } from "@lucid-evolution/lucid";
import {
  credentialToAddress,
  validatorToAddress,
} from "@lucid-evolution/lucid";

import {
  parseReleaseL1FinalityPolicyV1,
  type ReleaseL1FinalityPolicyV1,
} from "@/commands/e2e-release-finality-policy-v1.js";
import type { StateCorrectionAvailabilityChallengeCapabilityV1 } from "@/commands/e2e-state-correction-acceptance.js";
import type { StateCorrectionIndependentAuthorityV1 } from "@/commands/e2e-state-correction-reconciliation.js";
import {
  type DeploymentManifestV1Value,
  parseDeploymentManifestV1Value,
} from "@/deployment-manifest-v1.js";
import {
  fetchKupoAncestorPointV1,
  fetchKupoCreationPointV1,
  normalizeKupoHttpUrl,
  normalizeOgmiosWebSocketUrl,
  readOgmiosBlockTransactionV1,
  type WebSocketFactory,
  type WebSocketLike,
} from "@/l1-tx-order-carriage-v1.js";
import { normalizeOgmiosHttpUrl } from "@/local-ledger-slot.js";

type FetchLike = (input: string, init?: RequestInit) => Promise<Response>;

type ChainPoint = {
  readonly slot: string;
  readonly blockHash: string;
};

type LiveTip = ChainPoint & { readonly height: number };

type LiveKupoOutput = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly lovelace: string;
  readonly spent: boolean;
  readonly assets: Readonly<Record<string, string>>;
};

type LiveTransactionOutput = Pick<
  LiveKupoOutput,
  "address" | "lovelace" | "assets"
>;

type LiveEconomicTransaction = {
  readonly feeLovelace: string;
  readonly inputs: readonly string[];
  readonly referenceInputs: readonly string[];
  readonly outputs: readonly LiveTransactionOutput[];
};

export interface LocalKupmiosStateCorrectionSourceV1 {
  observeTransaction(input: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly expectedIncludedAt: ChainPoint;
  }): Promise<{
    readonly kupoIncludedAt: ChainPoint;
    readonly ogmiosIncludedAt: ChainPoint | null;
    readonly liveTip: LiveTip;
    readonly confirmationDepth: number;
  }>;
  observeOutput(input: {
    readonly txHash: string;
    readonly outputIndex: number;
  }): Promise<LiveKupoOutput | null>;
  observeEconomicTransaction(input: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly includedAt: ChainPoint;
  }): Promise<LiveEconomicTransaction>;
  observeUnspentAddress(input: {
    readonly address: string;
  }): Promise<readonly LiveKupoOutput[]>;
  observeStateQueue(input: {
    readonly address: string;
    readonly policyId: string;
  }): Promise<{ readonly depth: number }>;
  observeTip(): Promise<LiveTip>;
  observeDatabase(): Promise<{
    readonly unfinishedMutationJobs: number;
    readonly pendingFinalizations: number;
  }>;
}

export type LocalKupmiosStateCorrectionAuthorityConfigV1 = {
  readonly provider: string | undefined;
  readonly providerFailover: string | undefined;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly manifestId: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly reserveAddress: string;
  readonly finalityPolicy: ReleaseL1FinalityPolicyV1;
  readonly economicsPolicy: ReleaseEconomicsPolicyV1;
  readonly observeDatabase: LocalKupmiosStateCorrectionSourceV1["observeDatabase"];
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly source?: LocalKupmiosStateCorrectionSourceV1;
};

export type LocalAuthorityDeploymentV1 = {
  readonly manifestId: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly reserveAddress: string;
  readonly finalityPolicy: ReleaseL1FinalityPolicyV1;
  readonly economicsPolicy: ReleaseEconomicsPolicyV1;
  readonly availabilityChallengeCapability: StateCorrectionAvailabilityChallengeCapabilityV1;
};

type ReleaseEconomicsPolicyV1 = {
  readonly requiredBondLovelace: string;
  readonly slashingPenaltyLovelace: string;
  readonly fraudProverRewardLovelace: string;
  readonly inactivitySlashingPenaltyLovelace: string;
  readonly proverCollateralFloorLovelace: string;
};

export const releaseEconomicsPolicyFromDeploymentManifestV1 = (
  manifest: DeploymentManifestV1Value,
): ReleaseEconomicsPolicyV1 => ({
  requiredBondLovelace: manifest.economics.requiredBondLovelace.toString(),
  slashingPenaltyLovelace:
    manifest.economics.slashingPenaltyLovelace.toString(),
  fraudProverRewardLovelace:
    manifest.economics.fraudProverRewardLovelace.toString(),
  inactivitySlashingPenaltyLovelace:
    manifest.economics.inactivitySlashingPenaltyLovelace.toString(),
  proverCollateralFloorLovelace:
    manifest.economics.proverCollateralFloorLovelace.toString(),
});

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;

const record = (value: unknown, field: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Record<string, unknown>;
};

const nonNegativeInteger = (value: unknown, field: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return value;
};

const unsignedDecimal = (value: unknown, field: string): string => {
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value.toString();
  }
  if (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${field} must be a canonical unsigned decimal`);
  }
  return value;
};

const lowerHex = (value: unknown, pattern: RegExp, field: string): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${field} is not canonical lowercase hex`);
  }
  return value;
};

const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/u, "")}/${path.replace(/^\/+/, "")}`;

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

const stableJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => compareText(left, right))
    .map(([key, child]) => `${JSON.stringify(key)}:${stableJson(child)}`)
    .join(",")}}`;
};

export const stateCorrectionValueDigestV1 = (
  value: Readonly<Record<string, string>>,
): string => {
  for (const [unit, quantity] of Object.entries(value)) {
    unsignedDecimal(quantity, `Q57 value.${unit}`);
  }
  return createHash("sha256")
    .update(
      stableJson(
        Object.fromEntries(
          Object.entries(value)
            .filter(([, quantity]) => quantity !== "0")
            .sort(([left], [right]) => compareText(left, right)),
        ),
      ),
    )
    .digest("hex");
};

const outputValue = (
  output: Pick<LiveTransactionOutput, "lovelace" | "assets">,
): Readonly<Record<string, string>> => ({
  lovelace: output.lovelace,
  ...output.assets,
});

const aggregateOutputValues = (
  outputs: readonly LiveTransactionOutput[],
): Readonly<Record<string, string>> => {
  const totals = new Map<string, bigint>();
  for (const output of outputs) {
    for (const [unit, quantity] of Object.entries(outputValue(output))) {
      totals.set(unit, (totals.get(unit) ?? 0n) + BigInt(quantity));
    }
  }
  return Object.fromEntries(
    [...totals.entries()]
      .filter(([, quantity]) => quantity !== 0n)
      .sort(([left], [right]) => compareText(left, right))
      .map(([unit, quantity]) => [unit, quantity.toString()]),
  );
};

const assertLoopbackEndpoint = (value: string, field: string): void => {
  const parsed = new URL(value);
  const hostname = parsed.hostname.toLowerCase();
  if (
    hostname !== "127.0.0.1" &&
    hostname !== "localhost" &&
    hostname !== "::1" &&
    hostname !== "[::1]"
  ) {
    throw new Error(`${field} must be a loopback local Kupmios endpoint`);
  }
};

const fetchJson = async ({
  fetchImpl,
  url,
  timeoutMs,
  init,
}: {
  readonly fetchImpl: FetchLike;
  readonly url: string;
  readonly timeoutMs: number;
  readonly init?: RequestInit;
}): Promise<unknown> => {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const response = await fetchImpl(url, {
      ...init,
      signal: controller.signal,
    });
    const text = await response.text();
    if (!response.ok) {
      throw new Error(
        `local Kupmios HTTP ${response.status.toString()} from ${url}: ${text.slice(0, 256)}`,
      );
    }
    try {
      return JSON.parse(text) as unknown;
    } catch (cause) {
      throw new Error(`local Kupmios returned malformed JSON from ${url}`, {
        cause,
      });
    }
  } finally {
    clearTimeout(timeout);
  }
};

const parseTip = (value: unknown, field: string): LiveTip => {
  const root = record(value, field);
  const result = Object.hasOwn(root, "result")
    ? record(root.result, `${field}.result`)
    : root;
  const tip = Object.hasOwn(result, "tip")
    ? record(result.tip, `${field}.tip`)
    : result;
  return {
    slot: nonNegativeInteger(tip.slot, `${field}.slot`).toString(),
    blockHash: lowerHex(tip.id, HEX_32, `${field}.id`),
    height: nonNegativeInteger(tip.height, `${field}.height`),
  };
};

const parseKupoOutputs = (
  value: unknown,
  field: string,
): readonly LiveKupoOutput[] => {
  if (!Array.isArray(value)) throw new Error(`${field} must be an array`);
  return value.map((entry, index) => {
    const itemField = `${field}[${index.toString()}]`;
    const item = record(entry, itemField);
    const valueRecord = record(item.value, `${itemField}.value`);
    const assets = record(valueRecord.assets, `${itemField}.value.assets`);
    const normalizedAssets: Record<string, string> = {};
    for (const [unit, quantity] of Object.entries(assets)) {
      const normalizedUnit = unit.replaceAll(".", "");
      if (!/^[0-9a-f]{56,120}$/u.test(normalizedUnit)) {
        throw new Error(
          `${itemField}.value.assets.${unit} is not an asset unit`,
        );
      }
      normalizedAssets[normalizedUnit] = unsignedDecimal(
        quantity,
        `${itemField}.value.assets.${unit}`,
      );
    }
    return {
      txHash: lowerHex(
        item.transaction_id,
        HEX_32,
        `${itemField}.transaction_id`,
      ),
      outputIndex: nonNegativeInteger(
        item.output_index,
        `${itemField}.output_index`,
      ),
      address:
        typeof item.address === "string" && item.address.length > 0
          ? item.address
          : (() => {
              throw new Error(`${itemField}.address is missing`);
            })(),
      lovelace: unsignedDecimal(valueRecord.coins, `${itemField}.value.coins`),
      spent: item.spent_at !== null,
      assets: normalizedAssets,
    };
  });
};

const parseOgmiosValue = (
  value: unknown,
  field: string,
): LiveTransactionOutput["assets"] & { readonly lovelace: string } => {
  const valueRecord = record(value, field);
  const ada = record(valueRecord.ada, `${field}.ada`);
  const assets: Record<string, string> = {};
  for (const [policyId, rawPolicyAssets] of Object.entries(valueRecord)) {
    if (policyId === "ada") continue;
    if (!HEX_28.test(policyId)) {
      throw new Error(`${field}.${policyId} is not a policy id`);
    }
    const policyAssets = record(rawPolicyAssets, `${field}.${policyId}`);
    for (const [assetName, quantity] of Object.entries(policyAssets)) {
      if (!/^(?:[0-9a-f]{2}){0,32}$/u.test(assetName)) {
        throw new Error(
          `${field}.${policyId}.${assetName} is not an asset name`,
        );
      }
      assets[`${policyId}${assetName}`] = unsignedDecimal(
        quantity,
        `${field}.${policyId}.${assetName}`,
      );
    }
  }
  return {
    lovelace: unsignedDecimal(ada.lovelace, `${field}.ada.lovelace`),
    ...assets,
  };
};

const parseOgmiosEconomicTransaction = (
  value: unknown,
  txHash: string,
  field: string,
): LiveEconomicTransaction => {
  const transaction = record(value, field);
  if (transaction.id !== txHash) {
    throw new Error(`${field}.id does not match ${txHash}`);
  }
  const fee = parseOgmiosValue(transaction.fee, `${field}.fee`);
  if (Object.keys(fee).some((key) => key !== "lovelace")) {
    throw new Error(`${field}.fee contains a non-ADA asset`);
  }
  if (!Array.isArray(transaction.outputs)) {
    throw new Error(`${field}.outputs must be an array`);
  }
  const outputs = transaction.outputs.map((value, index) => {
    const outputField = `${field}.outputs[${index.toString()}]`;
    const output = record(value, outputField);
    if (typeof output.address !== "string" || output.address.length === 0) {
      throw new Error(`${outputField}.address is missing`);
    }
    const parsedValue = parseOgmiosValue(output.value, `${outputField}.value`);
    const { lovelace, ...assets } = parsedValue;
    return { address: output.address, lovelace, assets };
  });
  const references =
    transaction.references === undefined
      ? []
      : Array.isArray(transaction.references)
        ? transaction.references.map((value, index) => {
            const reference = record(
              value,
              `${field}.references[${index.toString()}]`,
            );
            const referencedTransaction = record(
              reference.transaction,
              `${field}.references[${index.toString()}].transaction`,
            );
            return `${lowerHex(
              referencedTransaction.id,
              HEX_32,
              `${field}.references[${index.toString()}].transaction.id`,
            )}#${nonNegativeInteger(
              reference.index,
              `${field}.references[${index.toString()}].index`,
            ).toString()}`;
          })
        : (() => {
            throw new Error(`${field}.references must be an array`);
          })();
  const inputs = Array.isArray(transaction.inputs)
    ? transaction.inputs.map((value, index) => {
        const input = record(value, `${field}.inputs[${index.toString()}]`);
        const inputTransaction = record(
          input.transaction,
          `${field}.inputs[${index.toString()}].transaction`,
        );
        return `${lowerHex(
          inputTransaction.id,
          HEX_32,
          `${field}.inputs[${index.toString()}].transaction.id`,
        )}#${nonNegativeInteger(
          input.index,
          `${field}.inputs[${index.toString()}].index`,
        ).toString()}`;
      })
    : (() => {
        throw new Error(`${field}.inputs must be an array`);
      })();
  return {
    feeLovelace: fee.lovelace,
    inputs,
    referenceInputs: references,
    outputs,
  };
};

type OgmiosSession = {
  readonly request: (
    method: string,
    params: Readonly<Record<string, unknown>>,
  ) => Promise<unknown>;
  readonly close: () => void;
};

const openOgmiosSession = async ({
  ogmiosUrl,
  timeoutMs,
  webSocketFactory,
}: {
  readonly ogmiosUrl: string;
  readonly timeoutMs: number;
  readonly webSocketFactory?: WebSocketFactory;
}): Promise<OgmiosSession> => {
  const socket: WebSocketLike = (
    webSocketFactory ??
    ((url: string) => new WebSocket(url) as unknown as WebSocketLike)
  )(normalizeOgmiosWebSocketUrl(ogmiosUrl));
  const pending = new Map<
    number,
    {
      readonly resolve: (value: unknown) => void;
      readonly reject: (error: Error) => void;
    }
  >();
  let nextId = 0;
  let terminal: Error | null = null;
  const fail = (error: Error): void => {
    terminal ??= error;
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
  };
  socket.addEventListener("message", ((event: { readonly data: unknown }) => {
    if (typeof event.data !== "string") {
      fail(new Error("Q57 Ogmios chain-sync sent a non-text frame"));
      return;
    }
    let message: {
      readonly id?: unknown;
      readonly result?: unknown;
      readonly error?: unknown;
    };
    try {
      message = JSON.parse(event.data) as typeof message;
    } catch (cause) {
      fail(new Error("Q57 Ogmios chain-sync sent malformed JSON", { cause }));
      return;
    }
    if (typeof message.id !== "number") return;
    const waiter = pending.get(message.id);
    if (waiter === undefined) return;
    pending.delete(message.id);
    if (message.error !== undefined) {
      waiter.reject(
        new Error(
          `Q57 Ogmios chain-sync error: ${JSON.stringify(message.error)}`,
        ),
      );
    } else {
      waiter.resolve(message.result);
    }
  }) as (event: never) => void);
  socket.addEventListener("error", (() => {
    fail(new Error("Q57 Ogmios chain-sync socket failed"));
  }) as (event: never) => void);
  socket.addEventListener("close", (() => {
    fail(new Error("Q57 Ogmios chain-sync socket closed"));
  }) as (event: never) => void);
  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(() => {
      socket.close();
      reject(new Error("Q57 Ogmios chain-sync open timed out"));
    }, timeoutMs);
    socket.addEventListener(
      "open",
      (() => {
        clearTimeout(timeout);
        resolve();
      }) as (event: never) => void,
      { once: true },
    );
    socket.addEventListener(
      "error",
      (() => {
        clearTimeout(timeout);
        reject(new Error("Q57 Ogmios chain-sync failed while opening"));
      }) as (event: never) => void,
      { once: true },
    );
  });
  return {
    request: async (method, params) => {
      if (terminal !== null) throw terminal;
      const id = nextId++;
      return await new Promise<unknown>((resolve, reject) => {
        const timeout = setTimeout(() => {
          pending.delete(id);
          reject(new Error(`Q57 Ogmios ${method} timed out`));
        }, timeoutMs);
        pending.set(id, {
          resolve: (result) => {
            clearTimeout(timeout);
            resolve(result);
          },
          reject: (error) => {
            clearTimeout(timeout);
            reject(error);
          },
        });
        socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
      });
    },
    close: () => socket.close(),
  };
};

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

const queryTip = async ({
  ogmiosUrl,
  fetchImpl,
  timeoutMs,
}: {
  readonly ogmiosUrl: string;
  readonly fetchImpl: FetchLike;
  readonly timeoutMs: number;
}): Promise<LiveTip> =>
  parseTip(
    await fetchJson({
      fetchImpl,
      url: normalizeOgmiosHttpUrl(ogmiosUrl),
      timeoutMs,
      init: {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          method: "queryNetwork/tip",
          id: "midgard-q57-authority-tip",
        }),
      },
    }),
    "live Ogmios tip",
  );

export const createLocalKupmiosStateCorrectionSourceV1 = (
  config: Omit<LocalKupmiosStateCorrectionAuthorityConfigV1, "source">,
): LocalKupmiosStateCorrectionSourceV1 => {
  const fetchImpl = config.fetchImpl ?? fetch;
  const timeoutMs = config.timeoutMs ?? 20_000;
  const kupoUrl = normalizeKupoHttpUrl(config.kupoUrl);
  const ogmiosUrl = config.ogmiosUrl;
  return {
    observeTransaction: async ({ txHash, outputIndex }) => {
      const kupoPoint = await fetchKupoCreationPointV1({
        kupoUrl,
        outRef: { txHash, outputIndex },
        fetchImpl,
        timeoutMs,
      });
      const kupoIncludedAt = {
        slot: kupoPoint.slot.toString(),
        blockHash: kupoPoint.headerHash,
      };
      const ancestor = await fetchKupoAncestorPointV1({
        kupoUrl,
        slot: kupoPoint.slot,
        fetchImpl,
        timeoutMs,
      });
      const observed = await readOgmiosBlockTransactionV1({
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
        fetchKupoAncestorPointV1({
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

const assertAtOrAfter = (
  live: LiveTip,
  prior: ChainPoint,
  field: string,
): void => {
  const priorSlot = Number(prior.slot);
  const liveSlot = Number(live.slot);
  if (!Number.isSafeInteger(priorSlot) || liveSlot < priorSlot) {
    throw new Error(`${field} rolled back before the accepted observation`);
  }
  if (liveSlot === priorSlot && live.blockHash !== prior.blockHash) {
    throw new Error(`${field} disagrees at the accepted slot`);
  }
};

export const createLocalKupmiosStateCorrectionAuthorityV1 = (
  config: LocalKupmiosStateCorrectionAuthorityConfigV1,
): StateCorrectionIndependentAuthorityV1 => {
  if (config.provider !== "Kupmios") {
    throw new Error("Q57 authority requires L1_PROVIDER=Kupmios");
  }
  if (
    config.providerFailover !== undefined &&
    config.providerFailover.trim() !== "" &&
    config.providerFailover.trim().toLowerCase() !== "false"
  ) {
    throw new Error("Q57 authority forbids L1 provider failover");
  }
  assertLoopbackEndpoint(config.kupoUrl, "L1_KUPO_KEY");
  assertLoopbackEndpoint(config.ogmiosUrl, "L1_OGMIOS_KEY");
  if (!HEX_28.test(config.stateQueuePolicyId)) {
    throw new Error("Q57 authority state-queue policy id is invalid");
  }
  const finalityPolicy = parseReleaseL1FinalityPolicyV1(
    config.finalityPolicy,
    "Q57 deployment manifest l1Finality",
  );
  const source =
    config.source ?? createLocalKupmiosStateCorrectionSourceV1(config);
  return {
    authenticateTransaction: async (input) => {
      const live = await source.observeTransaction({
        txHash: input.txHash,
        outputIndex: input.kupoOutputIndex,
        expectedIncludedAt: input.includedAt,
      });
      if (
        live.kupoIncludedAt.slot !== input.includedAt.slot ||
        live.kupoIncludedAt.blockHash !== input.includedAt.blockHash
      ) {
        throw new Error(
          `live Kupo inclusion disagrees for transaction ${input.txHash}`,
        );
      }
      if (
        live.ogmiosIncludedAt === null ||
        live.ogmiosIncludedAt.slot !== live.kupoIncludedAt.slot ||
        live.ogmiosIncludedAt.blockHash !== live.kupoIncludedAt.blockHash
      ) {
        throw new Error(
          `live Kupo/Ogmios inclusion disagreement for transaction ${input.txHash}`,
        );
      }
      assertAtOrAfter(
        live.liveTip,
        input.observedAtTip,
        `transaction ${input.txHash} live tip`,
      );
      if (
        input.observedAtTip.confirmationDepth <
          finalityPolicy.confirmationDepth ||
        live.confirmationDepth < finalityPolicy.confirmationDepth
      ) {
        throw new Error(
          `transaction ${input.txHash} has confirmation depth ${live.confirmationDepth.toString()}, below release depth ${finalityPolicy.confirmationDepth.toString()}`,
        );
      }
    },
    authenticateFinalState: async (input) => {
      if (input.manifestId !== config.manifestId) {
        throw new Error("Q57 authority manifest identity mismatch");
      }
      if (
        input.observedAt.confirmationDepth < finalityPolicy.confirmationDepth
      ) {
        throw new Error(
          `final Q57 observation depth is below release depth ${finalityPolicy.confirmationDepth.toString()}`,
        );
      }
      const [
        tip,
        queue,
        database,
        tokens,
        bondInputs,
        economicTransactions,
        payoutTransaction,
        reserveOutputs,
      ] = await Promise.all([
        source.observeTip(),
        source.observeStateQueue({
          address: config.stateQueueAddress,
          policyId: config.stateQueuePolicyId,
        }),
        source.observeDatabase(),
        Promise.all(
          input.retainedProofTokens.map(async ({ outRef }) => {
            const [txHash, outputIndexText] = outRef.split("#");
            return await source.observeOutput({
              txHash: txHash!,
              outputIndex: Number(outputIndexText),
            });
          }),
        ),
        Promise.all(
          input.economics.map(async ({ operatorBondInputOutRef }) => {
            if (operatorBondInputOutRef === null) return null;
            const [txHash, outputIndexText] =
              operatorBondInputOutRef.split("#");
            return await source.observeOutput({
              txHash: txHash!,
              outputIndex: Number(outputIndexText),
            });
          }),
        ),
        Promise.all(
          input.economics.map(
            async (economics) =>
              await source.observeEconomicTransaction({
                txHash: economics.removalTxHash,
                outputIndex: economics.kupoOutputIndex,
                includedAt: economics.includedAt,
              }),
          ),
        ),
        source.observeEconomicTransaction({
          txHash: input.withdrawalReservePayout.payoutConcludeTxHash,
          outputIndex: input.withdrawalReservePayout.kupoOutputIndex,
          includedAt: input.withdrawalReservePayout.includedAt,
        }),
        source.observeUnspentAddress({ address: config.reserveAddress }),
      ]);
      assertAtOrAfter(tip, input.observedAt, "final Q57 live tip");
      if (queue.depth !== input.stateQueueDepth) {
        throw new Error(
          `live Kupo state-queue depth mismatch: expected ${input.stateQueueDepth.toString()}, found ${queue.depth.toString()}`,
        );
      }
      if (
        database.unfinishedMutationJobs !== input.unfinishedMutationJobs ||
        database.pendingFinalizations !== input.pendingFinalizations
      ) {
        throw new Error("live node database final state disagrees");
      }
      for (const [index, expected] of input.retainedProofTokens.entries()) {
        const token = tokens[index];
        const [txHash, outputIndexText] = expected.outRef.split("#");
        if (
          token === null ||
          token === undefined ||
          token.txHash !== txHash ||
          token.outputIndex !== Number(outputIndexText) ||
          token.spent ||
          token.assets[expected.unit] !== "1"
        ) {
          throw new Error(
            `live Kupo does not retain permanent proof token ${expected.unit}@${expected.outRef}`,
          );
        }
      }
      for (const [index, expected] of input.economics.entries()) {
        const transaction = economicTransactions[index]!;
        const bondInput = bondInputs[index];
        if (transaction.feeLovelace !== expected.removalFeeLovelace) {
          throw new Error(
            `live Ogmios fee does not equal the exact removal fee for ${expected.familyId}`,
          );
        }
        const requiredBond = BigInt(
          config.economicsPolicy.requiredBondLovelace,
        );
        const fullSlash = BigInt(
          config.economicsPolicy.slashingPenaltyLovelace,
        );
        const reward = BigInt(config.economicsPolicy.fraudProverRewardLovelace);
        const inactivitySlash = BigInt(
          config.economicsPolicy.inactivitySlashingPenaltyLovelace,
        );
        const observedBond = BigInt(expected.operatorBondInputLovelace);
        const observedSlash = BigInt(expected.slashedLovelace);
        const fullTranche =
          observedBond === requiredBond && observedSlash === fullSlash;
        const partiallyInactivitySlashedTranche =
          observedBond === requiredBond - inactivitySlash &&
          observedSlash === fullSlash - inactivitySlash;
        if (!fullTranche && !partiallyInactivitySlashedTranche) {
          throw new Error(
            `live bond and slash do not match a release-bound full or partially inactivity-slashed tranche for ${expected.familyId}`,
          );
        }
        if (
          expected.slashedLovelace !== expected.removalFeeLovelace ||
          BigInt(expected.proverRewardLovelace) !== reward
        ) {
          throw new Error(
            `live slash fee and prover reward do not match release economics for ${expected.familyId}`,
          );
        }
        if (
          (expected.operatorBondInputOutRef === null) !==
            (expected.operatorBondInputLovelace === "0") ||
          (expected.proverRewardOutputOutRef === null) !==
            (expected.proverRewardLovelace === "0")
        ) {
          throw new Error(
            `Q57 economic outref/amount nullability mismatch for ${expected.familyId}`,
          );
        }
        if (
          expected.operatorBondInputOutRef === null ||
          bondInput === null ||
          bondInput === undefined ||
          !transaction.inputs.includes(expected.operatorBondInputOutRef) ||
          bondInput.lovelace !== expected.operatorBondInputLovelace ||
          bondInput.address !==
            credentialToAddress("Preprod", {
              type: "Key",
              hash: expected.operatorCredential,
            })
        ) {
          throw new Error(
            `live removal omitted or substituted the exact operator bond input for ${expected.familyId}`,
          );
        }
        if (
          !transaction.referenceInputs.includes(
            expected.referencedProofTokenOutRef,
          )
        ) {
          throw new Error(
            `live removal omitted permanent proof-token reference for ${expected.familyId}`,
          );
        }
        if (BigInt(expected.proverRewardLovelace) <= 0n) {
          throw new Error(
            `Q57 requires non-zero launch economics for ${expected.familyId}`,
          );
        }
        const proverAddress = credentialToAddress("Preprod", {
          type: "Key",
          hash: expected.proverCredential,
        });
        const rewardIndex =
          expected.proverRewardOutputOutRef === null
            ? -1
            : Number(expected.proverRewardOutputOutRef.split("#")[1]);
        if (
          expected.proverRewardOutputOutRef === null ||
          !expected.proverRewardOutputOutRef.startsWith(
            `${expected.removalTxHash}#`,
          )
        ) {
          throw new Error(
            `live removal has no exact prover reward output for ${expected.familyId}`,
          );
        }
        const rewards = transaction.outputs
          .map((output, outputIndex) => ({ output, outputIndex }))
          .filter(
            ({ output }) =>
              output.address === proverAddress &&
              output.lovelace === expected.proverRewardLovelace &&
              Object.keys(output.assets).length === 0,
          );
        if (rewards.length !== 1 || rewards[0]!.outputIndex !== rewardIndex) {
          throw new Error(
            `live removal has ${rewards.length.toString()} exact prover-reward outputs for ${expected.familyId}`,
          );
        }
      }
      const payout = input.withdrawalReservePayout;
      const payoutOutputs = payoutTransaction.outputs.filter(
        (output) =>
          output.address === payout.destination &&
          stateCorrectionValueDigestV1(outputValue(output)) ===
            payout.payoutValueSha256,
      );
      if (payoutOutputs.length !== 1) {
        throw new Error(
          "live payout conclusion does not contain one exact destination/value output",
        );
      }
      const reserveDigest = stateCorrectionValueDigestV1(
        aggregateOutputValues(reserveOutputs),
      );
      if (reserveDigest !== payout.reserveValueSha256) {
        throw new Error("live Kupo reserve value does not match Q57 evidence");
      }
    },
  };
};

const requireManifestContract = (
  manifest: DeploymentManifestV1Value,
  name: string,
): DeploymentManifestV1Value["contracts"][string] => {
  const contract = manifest.contracts[name];
  if (contract === undefined) {
    throw new Error(`deployment manifest has no ${name} contract`);
  }
  return contract;
};

export const loadLocalAuthorityDeploymentV1 = async (
  manifestPath: string,
): Promise<LocalAuthorityDeploymentV1> => {
  const manifest = parseDeploymentManifestV1Value(
    JSON.parse(await readFile(manifestPath, "utf8")) as unknown,
  );
  if (manifest.network !== "Preprod") {
    throw new Error("Q57 local authority requires a Preprod manifest");
  }
  const stateQueueSpend = requireManifestContract(manifest, "stateQueueSpend");
  const stateQueueMint = requireManifestContract(manifest, "stateQueueMint");
  const reserveSpend = requireManifestContract(manifest, "reserveSpend");
  const spendingScript: SpendingValidator = {
    type: stateQueueSpend.contract.type,
    script: stateQueueSpend.contract.cborHex,
  } as SpendingValidator;
  return {
    manifestId: manifest.manifestId,
    stateQueueAddress: validatorToAddress("Preprod", spendingScript),
    stateQueuePolicyId: stateQueueMint.scriptHash,
    reserveAddress: validatorToAddress("Preprod", {
      type: reserveSpend.contract.type,
      script: reserveSpend.contract.cborHex,
    } as SpendingValidator),
    finalityPolicy: parseReleaseL1FinalityPolicyV1(manifest.l1Finality),
    economicsPolicy: releaseEconomicsPolicyFromDeploymentManifestV1(manifest),
    // Q58 is deliberately absent from the strict V1 manifest schema. This
    // loader cannot promote a caller/config assertion into release authority.
    availabilityChallengeCapability: "missing",
  };
};
