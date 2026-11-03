/**
 * The user's own L1 indexer: Kupmios or Blockfrost, never a node's local
 * state query. The network and slot mapping come from the deployment
 * manifest. The indexed slot (through which the indexer's reads and
 * tx-status answers are complete) is Kupo's most recent checkpoint or
 * Blockfrost's latest block.
 */
import {
  blockfrostTipPointOf,
  kupoCheckpointSlotOf,
} from "@al-ft/midgard-core/l1-provider-tip";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { type UserL1View } from "@al-ft/midgard-sdk";
import {
  Blockfrost,
  Kupmios,
  Lucid,
  type Network,
} from "@lucid-evolution/lucid";

import { refuse } from "./options.js";

export type L1Access =
  | {
      readonly kind: "kupmios";
      readonly kupoUrl: string;
      readonly ogmiosUrl: string;
    }
  | {
      readonly kind: "blockfrost";
      readonly url: string;
      readonly projectId: string;
    };

export type L1AccessOptions = {
  readonly l1?: string;
  readonly kupoUrl?: string;
  readonly ogmiosUrl?: string;
  readonly blockfrostUrl?: string;
  readonly blockfrostProjectId?: string;
};

export type OpenL1ViewRequest = {
  readonly access: L1Access;
  readonly network: Network;
  readonly slotConfig: L1SlotConfig;
  /** Absent for read-only use (`status`). */
  readonly seedPhrase?: string;
};

/** One HTTP request to the indexer is bounded so a dead indexer surfaces. */
const REQUEST_BOUND_MS = 10_000;

const trimUrl = (url: string) => url.trim().replace(/\/+$/u, "");

const required = (
  value: string | undefined,
  flag: string,
  envName: string,
): string => {
  const trimmed = value?.trim() ?? "";
  return trimmed.length > 0
    ? trimmed
    : refuse(`${flag} (or ${envName}) is required for this --l1 access.`);
};

/** Flags win over the environment names the node's L1 access uses. */
export const resolveL1Access = (
  options: L1AccessOptions,
  env: Readonly<Record<string, string | undefined>>,
  network: Network,
): L1Access => {
  const kind = (options.l1 ?? env.L1_ACCESS ?? "").trim();
  if (kind === "kupmios")
    return {
      kind,
      kupoUrl: trimUrl(
        required(
          options.kupoUrl ?? env.L1_KUPO_URL,
          "--kupo-url",
          "L1_KUPO_URL",
        ),
      ),
      ogmiosUrl: required(
        options.ogmiosUrl ?? env.L1_OGMIOS_URL,
        "--ogmios-url",
        "L1_OGMIOS_URL",
      ),
    };
  if (kind === "blockfrost") {
    if (network === "Custom")
      refuse(
        "--l1 blockfrost serves named networks only; this deployment is on a Custom network.",
      );
    return {
      kind,
      url: trimUrl(
        required(
          options.blockfrostUrl ?? env.L1_BLOCKFROST_URL,
          "--blockfrost-url",
          "L1_BLOCKFROST_URL",
        ),
      ),
      projectId: required(
        options.blockfrostProjectId ?? env.L1_BLOCKFROST_PROJECT_ID,
        "--blockfrost-project-id",
        "L1_BLOCKFROST_PROJECT_ID",
      ),
    };
  }
  return refuse(
    kind.length === 0
      ? "--l1 (or L1_ACCESS) is required: kupmios or blockfrost."
      : `--l1 must be kupmios or blockfrost, not ${JSON.stringify(kind)}.`,
  );
};

const getJson = async (
  fetchImpl: typeof fetch,
  url: string,
  headers: Record<string, string>,
): Promise<unknown> => {
  const response = await fetchImpl(url, {
    headers,
    signal: AbortSignal.timeout(REQUEST_BOUND_MS),
  });
  if (!response.ok)
    throw new Error(`${url} answered HTTP ${response.status.toString()}`);
  return await response.json();
};

export const indexedSlotReader =
  (access: L1Access, fetchImpl: typeof fetch) => async (): Promise<number> => {
    if (access.kind === "blockfrost")
      return blockfrostTipPointOf(
        await getJson(fetchImpl, `${access.url}/blocks/latest`, {
          project_id: access.projectId,
        }),
      ).slot;
    const slot = kupoCheckpointSlotOf(
      await getJson(fetchImpl, `${access.kupoUrl}/health`, {
        accept: "application/json",
      }),
    );
    if (slot === null)
      throw new Error(
        `Kupo at ${access.kupoUrl} has no checkpoint yet; wait for it to sync`,
      );
    return slot;
  };

/**
 * Kupo answers `GET /patterns/{address}` with the patterns that cover that
 * address (`[]` when none does). Every address the submission reads or
 * creates outputs at must be covered, or the wallet's coins and the
 * transaction's status would look missing.
 */
export const requireKupoCoverage = async (
  fetchImpl: typeof fetch,
  kupoUrl: string,
  addresses: readonly { readonly role: string; readonly address: string }[],
): Promise<void> => {
  for (const { role, address } of addresses) {
    let patterns: unknown;
    try {
      patterns = await getJson(fetchImpl, `${kupoUrl}/patterns/${address}`, {
        accept: "application/json",
      });
    } catch (cause) {
      refuse(
        `Cannot read Kupo's patterns at ${kupoUrl}: ${cause instanceof Error ? cause.message : String(cause)}`,
      );
    }
    if (!Array.isArray(patterns) || patterns.length === 0)
      refuse(
        `Kupo at ${kupoUrl} does not index the ${role} ${address}. Run Kupo with --match "*" (or a pattern covering this address) and without --prune-utxo.`,
      );
  }
};

/** The real providers. */
export const openProviderL1View = async (
  request: OpenL1ViewRequest,
  fetchImpl: typeof fetch,
): Promise<UserL1View> => {
  const { access } = request;
  const provider =
    access.kind === "kupmios"
      ? new Kupmios(access.kupoUrl, access.ogmiosUrl)
      : new Blockfrost(access.url, access.projectId);
  const lucid = await Lucid(provider, request.network, {
    slotConfig: { ...request.slotConfig },
  });
  if (request.seedPhrase !== undefined)
    lucid.selectWallet.fromSeed(request.seedPhrase);
  return { lucid, indexedSlot: indexedSlotReader(access, fetchImpl) };
};
