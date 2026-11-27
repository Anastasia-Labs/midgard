import { type Network } from "@lucid-evolution/lucid";
import { type NodeUtxo } from "midgard-node/commands/command-utils";

import { STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION } from "./constants.js";
import { sha256Bytes } from "./files.js";
import { type StressWalletOperationScope } from "./scope.js";
import { outRefKey } from "./utxos.js";

export type TerminalDrainEntry = {
  readonly walletId: string;
  readonly address: string;
  readonly beforeOutrefs: readonly string[];
  readonly beforeLovelace: string;
  readonly beforeValueSha256: string;
  readonly status: "already_empty" | "prepared" | "committed";
  readonly txHash?: string;
  readonly signedTxCbor?: string;
  readonly selectedInputs?: readonly string[];
  readonly requestedLovelace?: string;
  readonly feeLovelace?: string;
  readonly signedTxBytes?: number;
};

export type TerminalDrainState = {
  readonly schemaVersion: typeof STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION;
  readonly scope: StressWalletOperationScope;
  readonly scopeSha256: string;
  readonly nodeEndpoint: string;
  readonly network: Network;
  readonly treasuryAddress: string;
  readonly treasuryBeforeLovelace: string;
  readonly minFeeA: string;
  readonly minFeeB: string;
  readonly feeCapLovelace: string;
  readonly maxFeeIterations: number;
  readonly entries: readonly TerminalDrainEntry[];
};

export const terminalScopeHash = (scope: StressWalletOperationScope): string =>
  sha256Bytes(Buffer.from(JSON.stringify(scope), "utf8"));

export const terminalSnapshotHash = (utxos: readonly NodeUtxo[]): string =>
  sha256Bytes(
    Buffer.from(
      JSON.stringify(
        [...utxos]
          .sort((a, b) => outRefKey(a).localeCompare(outRefKey(b)))
          .map((utxo) => ({
            outref: outRefKey(utxo),
            outputCbor: utxo.outputCbor.toString("hex"),
            assets: Object.fromEntries(
              Object.entries(utxo.assets)
                .filter(([, q]) => q !== 0n)
                .sort(([a], [b]) => a.localeCompare(b))
                .map(([unit, q]) => [unit, q.toString(10)]),
            ),
          })),
      ),
      "utf8",
    ),
  );

export const terminalDecimal = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0 || value !== value.trim())
    throw new Error(label + " must be an exact non-empty string.");
  const parsed = value;
  if (!/^(0|[1-9]\d*)$/.test(parsed))
    throw new Error(label + " must be a canonical non-negative decimal.");
  return parsed;
};

export const terminalExactString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0 || value !== value.trim())
    throw new Error(label + " must be an exact non-empty string.");
  return value;
};
