import { createHash } from "node:crypto";

import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type WebSocketFactory } from "midgard-node/l1-kupmios";

import { type ReleaseL1FinalityPolicy } from "./e2e-release-finality-policy.js";

export type FetchLike = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

export type ChainPoint = {
  readonly slot: string;
  readonly blockHash: string;
};

export type LiveTip = ChainPoint & { readonly height: number };

export type LiveKupoOutput = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly lovelace: string;
  readonly spent: boolean;
  readonly assets: Readonly<Record<string, string>>;
};

export type LiveTransactionOutput = Pick<
  LiveKupoOutput,
  "address" | "lovelace" | "assets"
>;

export type LiveEconomicTransaction = {
  readonly feeLovelace: string;
  readonly inputs: readonly string[];
  readonly referenceInputs: readonly string[];
  readonly outputs: readonly LiveTransactionOutput[];
};

export interface LocalKupmiosStateCorrectionSource {
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

export type LocalKupmiosStateCorrectionAuthorityConfig = {
  readonly providerFailover: string | undefined;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly manifestId: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly reserveAddress: string;
  readonly finalityPolicy: ReleaseL1FinalityPolicy;
  readonly economicsPolicy: ReleaseEconomicsPolicy;
  readonly observeDatabase: LocalKupmiosStateCorrectionSource["observeDatabase"];
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly source?: LocalKupmiosStateCorrectionSource;
};

export type LocalAuthorityDeployment = {
  readonly manifestId: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly reserveAddress: string;
  readonly finalityPolicy: ReleaseL1FinalityPolicy;
  readonly economicsPolicy: ReleaseEconomicsPolicy;
};

type ReleaseEconomicsPolicy = {
  readonly requiredBondLovelace: string;
  readonly slashingPenaltyLovelace: string;
  readonly fraudProverRewardLovelace: string;
  readonly inactivitySlashingPenaltyLovelace: string;
  readonly proverCollateralFloorLovelace: string;
};

export const releaseEconomicsPolicyFromDeploymentManifest = (
  manifest: DeploymentManifest,
): ReleaseEconomicsPolicy => ({
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

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const record = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Record<string, unknown>;
};

export const nonNegativeInteger = (value: unknown, field: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return value;
};

export const unsignedDecimal = (value: unknown, field: string): string => {
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value.toString();
  }
  if (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${field} must be a canonical unsigned decimal`);
  }
  return value;
};

export const lowerHex = (
  value: unknown,
  pattern: RegExp,
  field: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${field} is not canonical lowercase hex`);
  }
  return value;
};

export const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/u, "")}/${path.replace(/^\/+/, "")}`;

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

export const stableJson = (value: unknown): string => {
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

export const stateCorrectionValueDigest = (
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

export const outputValue = (
  output: Pick<LiveTransactionOutput, "lovelace" | "assets">,
): Readonly<Record<string, string>> => ({
  lovelace: output.lovelace,
  ...output.assets,
});

export const aggregateOutputValues = (
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

export const assertLoopbackEndpoint = (value: string, field: string): void => {
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

export const fetchJson = async ({
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

export const ogmiosResult = (value: unknown, field: string): unknown => {
  const root = record(value, field);
  return Object.hasOwn(root, "result") ? root.result : root;
};

/** Ogmios v6 answers queryNetwork/tip with a point only, never a height. */
export const parseTipPoint = (value: unknown, field: string): ChainPoint => {
  const tip = record(ogmiosResult(value, field), `${field}.result`);
  return {
    slot: nonNegativeInteger(tip.slot, `${field}.slot`).toString(),
    blockHash: lowerHex(tip.id, HEX_32, `${field}.id`),
  };
};
