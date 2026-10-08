import {
  HISTORY_GENESIS_DIGEST_ALGORITHM,
  readEventHistoryGenesisPin,
} from "../l1-event-history-source.js";
import type { WebSocketFactory } from "../l1-kupmios.js";

export const HISTORY_GENESIS_PIN_VARIABLE =
  "L1_HISTORY_GENESIS_LOSSLESS_SHA256";
const DEFAULT_TIMEOUT_MS = 30_000;

export type HistoryGenesisPin = Readonly<{
  variable: typeof HISTORY_GENESIS_PIN_VARIABLE;
  algorithm: typeof HISTORY_GENESIS_DIGEST_ALGORITHM;
  sha256: string;
}>;

/**
 * Prints the Shelley genesis pin of the chain the configured Ogmios serves.
 * The value is a proposal: the operator approves it as this deployment's chain
 * before setting L1_HISTORY_GENESIS_LOSSLESS_SHA256.
 */
export const runHistoryGenesisPin = async (input?: {
  readonly ogmiosUrl?: string;
  readonly env?: NodeJS.ProcessEnv;
  readonly timeoutMs?: number;
  readonly webSocketFactory?: WebSocketFactory;
}): Promise<HistoryGenesisPin> => {
  const env = input?.env ?? process.env;
  const ogmiosUrl = input?.ogmiosUrl?.trim() ?? env.L1_OGMIOS_KEY?.trim() ?? "";
  if (ogmiosUrl.length === 0)
    throw new Error(
      "Ogmios URL is required. Pass --ogmios-url or set L1_OGMIOS_KEY.",
    );
  const sha256 = await readEventHistoryGenesisPin({
    ogmiosUrl,
    timeoutMs: input?.timeoutMs ?? DEFAULT_TIMEOUT_MS,
    webSocketFactory: input?.webSocketFactory,
  });
  return Object.freeze({
    variable: HISTORY_GENESIS_PIN_VARIABLE,
    algorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
    sha256,
  });
};
