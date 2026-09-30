import { readFile } from "node:fs/promises";

import { sleep } from "midgard-node/sleep";

import {
  asObject,
  assertExactKeys,
  requiredString,
} from "./artifact-fields.js";
import {
  type ConsolidationState,
  type ConsolidationStateEntry,
} from "./consolidation.assert-consolidation-transfer-intent.js";
import { STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION } from "./constants.js";
import { fileExists } from "./files.js";
import {
  consolidationPendingTxStatuses,
  nextFanoutPollDelayMs,
  rejectedTxStatuses,
} from "./runtime.js";
import {
  computeSignedNativeTxHash,
  parseStressWalletOperationScope,
} from "./scope.js";
import { type StressWalletConsolidateRuntime } from "./types.js";

export const parseStressWalletConsolidationJournal = (
  value: unknown,
  path = "<memory>",
): ConsolidationState => {
  const raw = asObject(value, "stress wallet consolidation state");
  assertExactKeys(raw, "stress wallet consolidation state", [
    "schemaVersion",
    "treasuryAddress",
    "nodeEndpoint",
    "reserveLovelace",
    "scope",
    "entries",
  ]);
  if (
    raw.schemaVersion !== STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION
  ) {
    throw new Error(
      `Unsupported consolidation state schema at ${path}; canonical V1 is required.`,
    );
  }
  if (!Array.isArray(raw.entries)) {
    throw new Error(`Consolidation state entries at ${path} must be an array.`);
  }
  const walletIds = new Set<string>();
  const txHashes = new Set<string>();
  const entries = raw.entries.map((value, index): ConsolidationStateEntry => {
    const entry = asObject(value, `entries[${index.toString()}]`);
    assertExactKeys(
      entry,
      `entries[${index.toString()}]`,
      [
        "walletId",
        "address",
        "beforeLovelace",
        "beforeOutrefs",
        "requestedLovelace",
      ],
      [
        "txHash",
        "signedTxCbor",
        "selectedInputs",
        "selectedInputLovelace",
        "acceptedStatus",
      ],
    );
    if (!Array.isArray(entry.beforeOutrefs)) {
      throw new Error(
        `Consolidation state entries[${index.toString()}].beforeOutrefs must be an array.`,
      );
    }
    if (
      entry.selectedInputs !== undefined &&
      !Array.isArray(entry.selectedInputs)
    ) {
      throw new Error(
        `Consolidation state entries[${index.toString()}].selectedInputs must be an array.`,
      );
    }
    const parsed: ConsolidationStateEntry = {
      walletId: requiredString(entry.walletId, "walletId"),
      address: requiredString(entry.address, "address"),
      beforeLovelace: requiredString(entry.beforeLovelace, "beforeLovelace"),
      beforeOutrefs: entry.beforeOutrefs.map((outref) =>
        requiredString(outref, "beforeOutref"),
      ),
      requestedLovelace: requiredString(
        entry.requestedLovelace,
        "requestedLovelace",
      ),
      ...(entry.txHash === undefined
        ? {}
        : { txHash: requiredString(entry.txHash, "txHash") }),
      ...(entry.signedTxCbor === undefined
        ? {}
        : {
            signedTxCbor: requiredString(entry.signedTxCbor, "signedTxCbor"),
          }),
      ...(entry.selectedInputs === undefined
        ? {}
        : {
            selectedInputs: entry.selectedInputs.map((input) =>
              requiredString(input, "selectedInput"),
            ),
          }),
      ...(entry.selectedInputLovelace === undefined
        ? {}
        : {
            selectedInputLovelace: requiredString(
              entry.selectedInputLovelace,
              "selectedInputLovelace",
            ),
          }),
      ...(entry.acceptedStatus === undefined
        ? {}
        : {
            acceptedStatus: requiredString(
              entry.acceptedStatus,
              "acceptedStatus",
            ),
          }),
    };
    if (walletIds.has(parsed.walletId)) {
      throw new Error(
        `Duplicate walletId ${parsed.walletId} in consolidation state at ${path}.`,
      );
    }
    walletIds.add(parsed.walletId);
    if (parsed.txHash !== undefined) {
      if (!/^[0-9a-f]{64}$/.test(parsed.txHash)) {
        throw new Error(
          `Consolidation state txHash for ${parsed.walletId} must be a lowercase 32-byte digest.`,
        );
      }
      if (txHashes.has(parsed.txHash)) {
        throw new Error(
          `Duplicate txHash ${parsed.txHash} in consolidation state at ${path}.`,
        );
      }
      txHashes.add(parsed.txHash);
    }
    if (
      parsed.signedTxCbor !== undefined &&
      (!/^[0-9a-f]+$/.test(parsed.signedTxCbor) ||
        parsed.signedTxCbor.length % 2 !== 0)
    ) {
      throw new Error(
        `Consolidation state signedTxCbor for ${parsed.walletId} must be non-empty lowercase hex.`,
      );
    }
    if (
      parsed.signedTxCbor !== undefined &&
      parsed.txHash !== undefined &&
      computeSignedNativeTxHash(parsed.signedTxCbor, parsed.walletId) !==
        parsed.txHash
    ) {
      throw new Error(
        `Consolidation state txHash/signedTxCbor mismatch for ${parsed.walletId}.`,
      );
    }
    if (parsed.signedTxCbor !== undefined && parsed.txHash === undefined) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has signedTxCbor without txHash.`,
      );
    }
    if (
      parsed.selectedInputLovelace !== undefined &&
      !/^(0|[1-9]\d*)$/.test(parsed.selectedInputLovelace)
    ) {
      throw new Error(
        `Consolidation state selectedInputLovelace for ${parsed.walletId} must be a canonical non-negative decimal.`,
      );
    }
    if (
      parsed.signedTxCbor !== undefined &&
      (parsed.selectedInputs === undefined ||
        parsed.selectedInputLovelace === undefined)
    ) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has signedTxCbor without exact selected input accounting.`,
      );
    }
    if (
      parsed.signedTxCbor === undefined &&
      parsed.selectedInputLovelace !== undefined
    ) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has selected input accounting without signedTxCbor.`,
      );
    }
    if (parsed.txHash !== undefined && parsed.signedTxCbor === undefined) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} lacks its exact signedTxCbor.`,
      );
    }
    return parsed;
  });
  return {
    schemaVersion: STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION,
    treasuryAddress: requiredString(raw.treasuryAddress, "treasuryAddress"),
    nodeEndpoint: requiredString(raw.nodeEndpoint, "nodeEndpoint"),
    reserveLovelace: requiredString(raw.reserveLovelace, "reserveLovelace"),
    scope: parseStressWalletOperationScope(raw.scope),
    entries,
  };
};

export const readConsolidationState = async (
  path: string,
): Promise<ConsolidationState | undefined> => {
  if (!(await fileExists(path))) return undefined;
  return parseStressWalletConsolidationJournal(
    JSON.parse(await readFile(path, "utf8")) as unknown,
    path,
  );
};

export const waitForConsolidationAcceptance = async ({
  nodeEndpoint,
  txHash,
  runtime,
  acceptanceTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
}: {
  readonly nodeEndpoint: string;
  readonly txHash: string;
  readonly runtime: StressWalletConsolidateRuntime;
  readonly acceptanceTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
}): Promise<string> => {
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const status = (await runtime.fetchTxStatus(nodeEndpoint, txHash))
      .trim()
      .toLowerCase();
    if (status === "committed") return status;
    if (rejectedTxStatuses.has(status)) {
      throw new Error(
        `Consolidation transfer ${txHash} reached rejected status ${status}.`,
      );
    }
    if (!consolidationPendingTxStatuses.has(status)) {
      throw new Error(
        `Consolidation transfer ${txHash} returned unknown status ${status || "<empty>"}; refusing to infer commitment/finality.`,
      );
    }
    if (monotonicNow() - startedAt >= acceptanceTimeoutMs) {
      throw new Error(
        `Timed out waiting ${acceptanceTimeoutMs.toString()}ms for consolidation transfer ${txHash}; last status ${status}.`,
      );
    }
    await sleepImpl(
      nextFanoutPollDelayMs({
        attempt,
        initialMs: pollInitialIntervalMs,
        maxMs: pollMaxIntervalMs,
      }),
    );
    attempt += 1;
  }
};
