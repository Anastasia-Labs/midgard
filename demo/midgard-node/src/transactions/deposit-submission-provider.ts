import {
  ChainSynchronization,
  createInteractionContext,
  createMempoolMonitoringClient,
  type ConnectionConfig,
  type InteractionContext,
} from "@cardano-ogmios/client";
import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type * as DepositSubmissionAttemptsDB from "@/database/depositSubmissionAttempts.js";
import { parseExactKupoConfirmationMatch } from "@/kupo-confirmation-metadata.js";
import { kupmiosKupoUrlFromLucid } from "@/kupmios.js";
import {
  queryLocalOgmiosSubmitSlotSnapshot,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "@/local-ledger-slot.js";
import { inspectSignedTxValidityInterval } from "@/transactions/utils.js";

const PROVIDER_EVIDENCE_TIMEOUT_MS = 10_000;

export type DepositTransactionDependencies = {
  readonly spend: readonly string[];
  readonly collateral: readonly string[];
  readonly reference: readonly string[];
};

export type HistoricalDepositOutputObservation =
  | {
      readonly kind: "committed";
      readonly slot: number;
      readonly blockHash: string;
      readonly kupoCheckpoint: number;
      readonly kupoCheckpointHash: string;
    }
  | {
      readonly kind: "absent";
      readonly kupoCheckpoint: number;
      readonly kupoCheckpointHash: string;
    };

export type DepositTxObservation =
  | {
      readonly kind: "committed";
      readonly slot: number;
      readonly blockHash: string;
      readonly kupoCheckpoint: number;
    }
  | {
      readonly kind: "accepted";
      readonly mempoolSlot: number;
    }
  | {
      readonly kind: "absent_safe";
      readonly mempoolSlot: number;
      readonly kupoCheckpoint: number;
      readonly currentSlot: number;
    }
  | {
      readonly kind: "expired";
      readonly mempoolSlot: number;
      readonly kupoCheckpoint: number;
      readonly currentSlot: number;
      readonly invalidHereafterSlot: number;
    }
  | {
      readonly kind: "ambiguous";
      readonly reason: string;
    };

export type DepositObservationRuntime = {
  readonly queryHistoricalOutput: (
    txHash: string,
    outputIndex: number,
  ) => Promise<HistoricalDepositOutputObservation>;
  readonly queryMempool: (
    txHash: string,
  ) => Promise<{ readonly slot: number; readonly contains: boolean }>;
  readonly queryCanonicalPoint: (point: {
    readonly slot: number;
    readonly id: string;
  }) => Promise<boolean>;
  readonly queryCurrentSlot: () => Promise<SubmitSlotSnapshot>;
  readonly queryDependencies: (
    outRefs: readonly {
      readonly txHash: string;
      readonly outputIndex: number;
    }[],
  ) => Promise<readonly UTxO[]>;
};

const outRefList = (
  list: ReturnType<CML.TransactionBody["inputs"]> | undefined,
): readonly string[] => {
  if (list === undefined) return [];
  const outRefs: string[] = [];
  for (let index = 0; index < list.len(); index += 1) {
    const input = list.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${Number(input.index()).toString()}`,
    );
  }
  return [...new Set(outRefs)].sort();
};

/** Derives every L1 dependency whose identity must survive a crash. */
export const depositDependenciesFromSignedTx = (
  tx: CML.Transaction,
): DepositTransactionDependencies => ({
  spend: outRefList(tx.body().inputs()),
  collateral: outRefList(tx.body().collateral_inputs()),
  reference: outRefList(tx.body().reference_inputs()),
});

const parseOutRef = (
  value: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const match = /^([0-9a-f]{64})#(0|[1-9]\d*)$/.exec(value);
  if (match === null)
    throw new Error(`Invalid stored Cardano outref: ${value}`);
  const outputIndex = Number(match[2]);
  if (!Number.isSafeInteger(outputIndex) || outputIndex > 0xffff) {
    throw new Error(`Stored Cardano outref index is out of range: ${value}`);
  }
  return { txHash: match[1]!, outputIndex };
};

const dependencyKeys = (
  dependencies: DepositTransactionDependencies,
): readonly string[] =>
  [...dependencies.spend, ...dependencies.collateral, ...dependencies.reference]
    .filter((value, index, all) => all.indexOf(value) === index)
    .sort();

const sameDependencies = (
  left: DepositTransactionDependencies,
  right: DepositTransactionDependencies,
): boolean =>
  JSON.stringify(left.spend) === JSON.stringify(right.spend) &&
  JSON.stringify(left.collateral) === JSON.stringify(right.collateral) &&
  JSON.stringify(left.reference) === JSON.stringify(right.reference);

const errorText = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

const withTimeout = async <A>(
  label: string,
  operation: () => Promise<A>,
  timeoutMs = PROVIDER_EVIDENCE_TIMEOUT_MS,
  onTimeout?: () => void,
): Promise<A> => {
  let timeout: NodeJS.Timeout | undefined;
  const timedOut = new Promise<never>((_, reject) => {
    timeout = setTimeout(() => {
      onTimeout?.();
      reject(new Error(`${label} timed out after ${timeoutMs.toString()}ms`));
    }, timeoutMs);
  });
  try {
    return await Promise.race([operation(), timedOut]);
  } finally {
    if (timeout !== undefined) clearTimeout(timeout);
  }
};

const checkpointPoint = (
  response: Pick<Response, "headers">,
): { readonly slot: number; readonly id: string } => {
  const raw = response.headers.get("x-most-recent-checkpoint");
  if (raw === null || !/^\d+$/.test(raw)) {
    throw new Error("Kupo response is missing X-Most-Recent-Checkpoint");
  }
  const value = Number(raw);
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("Kupo response checkpoint is invalid");
  }
  const rawEtag = response.headers.get("etag");
  const id = rawEtag
    ?.trim()
    .replace(/^W\//u, "")
    .replace(/^"|"$/gu, "")
    .toLowerCase();
  if (id === undefined || !/^[0-9a-f]{64}$/u.test(id)) {
    throw new Error("Kupo response is missing a valid checkpoint ETag");
  }
  return { slot: value, id };
};

type KupoHistoricalFetchResponse = Pick<
  Response,
  "ok" | "status" | "statusText" | "headers" | "json"
>;

type KupoHistoricalFetch = (
  input: string | URL,
  init?: RequestInit,
) => Promise<KupoHistoricalFetchResponse>;

export const queryHistoricalDepositOutput = async ({
  kupoUrl,
  txHash,
  outputIndex,
  fetchImpl = fetch,
  timeoutMs = PROVIDER_EVIDENCE_TIMEOUT_MS,
}: {
  readonly kupoUrl: string;
  readonly txHash: string;
  readonly outputIndex: number;
  readonly fetchImpl?: KupoHistoricalFetch;
  readonly timeoutMs?: number;
}): Promise<HistoricalDepositOutputObservation> => {
  const pattern = encodeURIComponent(`${outputIndex.toString()}@${txHash}`);
  const controller = new AbortController();
  return withTimeout(
    "Kupo historical deposit-output query",
    async () => {
      const response = await fetchImpl(`${kupoUrl}/matches/${pattern}`, {
        signal: controller.signal,
        headers: { accept: "application/json" },
      });
      if (!response.ok) {
        throw new Error(
          `Kupo returned HTTP ${response.status.toString()} ${response.statusText}`,
        );
      }
      const checkpoint = checkpointPoint(response);
      const body = (await response.json()) as unknown;
      if (Array.isArray(body) && body.length === 0) {
        return {
          kind: "absent",
          kupoCheckpoint: checkpoint.slot,
          kupoCheckpointHash: checkpoint.id,
        } as const;
      }
      const match = parseExactKupoConfirmationMatch({
        body,
        txHash,
        outputIndex,
      });
      return {
        kind: "committed",
        slot: match.slotNo,
        blockHash: match.blockHeaderHash,
        kupoCheckpoint: checkpoint.slot,
        kupoCheckpointHash: checkpoint.id,
      } as const;
    },
    timeoutMs,
    () => controller.abort(),
  );
};

const ogmiosConnectionConfig = (endpoint: string): ConnectionConfig => {
  const url = new URL(endpoint);
  if (
    !["http:", "https:", "ws:", "wss:"].includes(url.protocol) ||
    (url.pathname !== "" && url.pathname !== "/") ||
    url.search.length > 0 ||
    url.hash.length > 0 ||
    url.username.length > 0 ||
    url.password.length > 0
  ) {
    throw new Error("Ogmios mempool monitoring requires a root endpoint URL");
  }
  const tls = url.protocol === "https:" || url.protocol === "wss:";
  const port = url.port.length > 0 ? Number(url.port) : tls ? 443 : 80;
  if (!Number.isSafeInteger(port) || port <= 0 || port > 65_535) {
    throw new Error("Ogmios endpoint port is invalid");
  }
  return { host: url.hostname, port, tls };
};

const closeInteractionContext = (
  context: InteractionContext | undefined,
): void => {
  if (context === undefined) return;
  (context.socket as unknown as { close: () => void }).close();
};

export const queryOgmiosMempool = async ({
  ogmiosUrl,
  txHash,
  timeoutMs = PROVIDER_EVIDENCE_TIMEOUT_MS,
}: {
  readonly ogmiosUrl: string;
  readonly txHash: string;
  readonly timeoutMs?: number;
}): Promise<{ readonly slot: number; readonly contains: boolean }> => {
  let context: InteractionContext | undefined;
  return withTimeout(
    "Ogmios mempool observation",
    async () => {
      let socketFailure: Error | undefined;
      context = await createInteractionContext(
        (error) => {
          socketFailure = error;
        },
        (code, reason) => {
          if (code !== 1000) {
            socketFailure = new Error(
              `Ogmios socket closed code=${code.toString()} reason=${reason}`,
            );
          }
        },
        { connection: ogmiosConnectionConfig(ogmiosUrl) },
      );
      const client = await createMempoolMonitoringClient(context);
      let acquired = false;
      try {
        const slot = Number(await client.acquireMempool());
        acquired = true;
        if (!Number.isSafeInteger(slot) || slot < 0) {
          throw new Error("Ogmios acquired mempool slot is invalid");
        }
        const contains = await client.hasTransaction(txHash);
        if (socketFailure !== undefined) throw socketFailure;
        return { slot, contains };
      } finally {
        if (acquired) await client.releaseMempool().catch(() => undefined);
        await client.shutdown().catch(() => undefined);
      }
    },
    timeoutMs,
    () => closeInteractionContext(context),
  );
};

export const queryOgmiosCanonicalPoint = async ({
  ogmiosUrl,
  point,
  timeoutMs = PROVIDER_EVIDENCE_TIMEOUT_MS,
}: {
  readonly ogmiosUrl: string;
  readonly point: { readonly slot: number; readonly id: string };
  readonly timeoutMs?: number;
}): Promise<boolean> => {
  let context: InteractionContext | undefined;
  return withTimeout(
    "Ogmios canonical-point observation",
    async () => {
      let socketFailure: Error | undefined;
      context = await createInteractionContext(
        (error) => {
          socketFailure = error;
        },
        (code, reason) => {
          if (code !== 1000) {
            socketFailure = new Error(
              `Ogmios socket closed code=${code.toString()} reason=${reason}`,
            );
          }
        },
        { connection: ogmiosConnectionConfig(ogmiosUrl) },
      );
      try {
        const result = await ChainSynchronization.findIntersection(context, [
          point,
        ]);
        if (socketFailure !== undefined) throw socketFailure;
        return (
          result.intersection !== "origin" &&
          Number(result.intersection.slot) === point.slot &&
          result.intersection.id.toLowerCase() === point.id.toLowerCase()
        );
      } catch {
        return false;
      } finally {
        closeInteractionContext(context);
      }
    },
    timeoutMs,
    () => closeInteractionContext(context),
  );
};

/**
 * Combines historical Kupo, a frozen Ogmios mempool snapshot, authoritative
 * slot evidence, and every signed dependency. Any disagreement fails closed.
 */
export const observePreparedDeposit = async ({
  txHash,
  signedTxCbor,
  expectedDepositOutRef,
  storedDependencies,
  runtime,
}: {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly expectedDepositOutRef: string;
  readonly storedDependencies: DepositTransactionDependencies;
  readonly runtime: DepositObservationRuntime;
}): Promise<DepositTxObservation> => {
  try {
    const tx = CML.Transaction.from_cbor_hex(signedTxCbor);
    const bodyHash = CML.hash_transaction(tx.body()).to_hex();
    if (bodyHash !== txHash) {
      return {
        kind: "ambiguous",
        reason: `stored signed CBOR hash mismatch expected=${txHash} actual=${bodyHash}`,
      };
    }
    const signedDependencies = depositDependenciesFromSignedTx(tx);
    if (!sameDependencies(signedDependencies, storedDependencies)) {
      return {
        kind: "ambiguous",
        reason: "stored dependency set does not match the exact signed body",
      };
    }
    const expected = parseOutRef(expectedDepositOutRef);
    if (expected.txHash !== txHash) {
      return {
        kind: "ambiguous",
        reason: "expected deposit outref is not bound to the stored tx hash",
      };
    }
    const historical = await runtime.queryHistoricalOutput(
      expected.txHash,
      expected.outputIndex,
    );
    const checkpointCanonical = await runtime.queryCanonicalPoint({
      slot: historical.kupoCheckpoint,
      id: historical.kupoCheckpointHash,
    });
    if (!checkpointCanonical) {
      return {
        kind: "ambiguous",
        reason: "Kupo checkpoint is not on the canonical Ogmios chain",
      };
    }
    if (historical.kind === "committed") {
      const creationPointCanonical = await runtime.queryCanonicalPoint({
        slot: historical.slot,
        id: historical.blockHash,
      });
      if (!creationPointCanonical) {
        return {
          kind: "ambiguous",
          reason: "Kupo deposit match is not on the canonical Ogmios chain",
        };
      }
      return historical;
    }

    const mempool = await runtime.queryMempool(txHash);
    if (mempool.contains) {
      return { kind: "accepted", mempoolSlot: mempool.slot };
    }
    if (historical.kupoCheckpoint < mempool.slot) {
      return {
        kind: "ambiguous",
        reason: `Kupo checkpoint ${historical.kupoCheckpoint.toString()} is behind Ogmios mempool slot ${mempool.slot.toString()}`,
      };
    }
    const slot = await runtime.queryCurrentSlot();
    const chainTipSlot = slot.chainTipSlot ?? slot.health?.lastKnownTipSlot;
    if (chainTipSlot === undefined) {
      return {
        kind: "ambiguous",
        reason:
          "authoritative Ogmios chain-tip slot is unavailable for Kupo coverage proof",
      };
    }
    if (historical.kupoCheckpoint < chainTipSlot) {
      return {
        kind: "ambiguous",
        reason: `Kupo checkpoint ${historical.kupoCheckpoint.toString()} is behind authoritative Ogmios chain tip ${chainTipSlot.toString()}`,
      };
    }
    const keys = dependencyKeys(storedDependencies);
    const requested = keys.map(parseOutRef);
    const visible = await runtime.queryDependencies(requested);
    const visibleKeys = new Set(
      visible.map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`),
    );
    const missing = keys.filter((key) => !visibleKeys.has(key));
    if (missing.length > 0) {
      return {
        kind: "ambiguous",
        reason: `signed transaction dependencies are no longer all unspent: ${missing.join(",")}`,
      };
    }
    const validity = inspectSignedTxValidityInterval(signedTxCbor);
    if (
      validity.invalidHereafterSlot !== undefined &&
      slot.currentSlot + SUBMIT_SLOT_VALIDITY_BUFFER >=
        validity.invalidHereafterSlot
    ) {
      return {
        kind: "expired",
        mempoolSlot: mempool.slot,
        kupoCheckpoint: historical.kupoCheckpoint,
        currentSlot: slot.currentSlot,
        invalidHereafterSlot: validity.invalidHereafterSlot,
      };
    }
    if (
      validity.invalidBeforeSlot !== undefined &&
      slot.currentSlot <
        validity.invalidBeforeSlot + SUBMIT_SLOT_VALIDITY_BUFFER
    ) {
      return {
        kind: "ambiguous",
        reason: `authoritative slot ${slot.currentSlot.toString()} has not reached signed validity margin ${(
          validity.invalidBeforeSlot + SUBMIT_SLOT_VALIDITY_BUFFER
        ).toString()}`,
      };
    }
    return {
      kind: "absent_safe",
      mempoolSlot: mempool.slot,
      kupoCheckpoint: historical.kupoCheckpoint,
      currentSlot: slot.currentSlot,
    };
  } catch (cause) {
    return {
      kind: "ambiguous",
      reason: `provider evidence unavailable: ${errorText(cause)}`,
    };
  }
};

export const liveDepositObservationRuntime = ({
  lucid,
  ogmiosUrl,
  timeoutMs,
}: {
  readonly lucid: LucidEvolution;
  readonly ogmiosUrl: string;
  readonly timeoutMs: number;
}): DepositObservationRuntime => {
  const kupoUrl = kupmiosKupoUrlFromLucid(lucid);
  if (kupoUrl === undefined) {
    throw new Error("Durable deposit recovery requires the local Kupo URL");
  }
  return {
    queryHistoricalOutput: (txHash, outputIndex) =>
      queryHistoricalDepositOutput({
        kupoUrl,
        txHash,
        outputIndex,
        timeoutMs,
      }),
    queryMempool: (txHash) =>
      queryOgmiosMempool({ ogmiosUrl, txHash, timeoutMs }),
    queryCanonicalPoint: (point) =>
      queryOgmiosCanonicalPoint({ ogmiosUrl, point, timeoutMs }),
    queryCurrentSlot: () =>
      queryLocalOgmiosSubmitSlotSnapshot({
        ogmiosUrl,
        timeoutMs,
      }),
    queryDependencies: (outRefs) =>
      withTimeout(
        "L1 dependency lookup",
        () => lucid.utxosByOutRef([...outRefs]),
        timeoutMs,
      ),
  };
};

export const observeDepositSubmissionAttempt = async ({
  lucid,
  attempt,
  ogmiosUrl,
  timeoutMs,
}: {
  readonly lucid: LucidEvolution;
  readonly attempt: DepositSubmissionAttemptsDB.Row;
  readonly ogmiosUrl: string;
  readonly timeoutMs: number;
}): Promise<DepositTxObservation> =>
  observePreparedDeposit({
    txHash: attempt.tx_hash.toString("hex"),
    signedTxCbor: attempt.signed_tx_cbor.toString("hex"),
    expectedDepositOutRef: attempt.expected_deposit_out_ref,
    storedDependencies: attempt.dependency_out_refs,
    runtime: liveDepositObservationRuntime({ lucid, ogmiosUrl, timeoutMs }),
  });
