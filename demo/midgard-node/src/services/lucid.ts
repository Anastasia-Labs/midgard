import { Effect, Schedule } from "effect";
import { ConfigError, NodeConfig } from "./config.js";
import * as LE from "@lucid-evolution/lucid";

/**
 * Lucid-provider construction and fallback behavior for the Midgard node.
 *
 * This module centralizes provider patching because Blockfrost/Kupmios
 * limitations, rate limits, and evaluation quirks directly affect transaction
 * correctness and operational reliability.
 */
const KOIOS_BASE_URL_BY_NETWORK: Partial<Record<LE.Network, string>> = {
  Mainnet: "https://api.koios.rest/api/v1",
  Preprod: "https://preprod.koios.rest/api/v1",
  Preview: "https://preview.koios.rest/api/v1",
};

/**
 * Resolves the Koios base URL for a network when a fallback exists.
 */
const resolveKoiosBaseUrl = (network: LE.Network): string | undefined =>
  KOIOS_BASE_URL_BY_NETWORK[network];

/**
 * Converts an unknown error into a stable diagnostic string.
 */
const formatUnknownError = (error: unknown): string => {
  if (error instanceof Error) {
    return error.stack ?? error.message;
  }
  return String(error);
};

/**
 * Returns whether a Blockfrost failure is worth retrying through a fallback
 * provider.
 */
const isBlockfrostFallbackEligibleError = (error: unknown): boolean => {
  const message = formatUnknownError(error).toLowerCase();
  return (
    message.includes("blockfrost") ||
    message.includes("project over limit") ||
    message.includes("usage is over limit") ||
    message.includes("\"status_code\":402") ||
    message.includes("status_code: 402") ||
    message.includes("\"status_code\":429") ||
    message.includes("status_code: 429") ||
    message.includes("too many requests") ||
    message.includes("rate limit") ||
    message.includes("could not fetch utxos from blockfrost") ||
    message.includes("cannot convert undefined to a bigint") ||
    message.includes("fetch failed")
  );
};

/**
 * Returns whether a Blockfrost failure specifically looks like quota/rate-limit
 * exhaustion.
 */
const isBlockfrostRateLimitError = (error: unknown): boolean => {
  const message = formatUnknownError(error).toLowerCase();
  return (
    message.includes("project over limit") ||
    message.includes("usage is over limit") ||
    message.includes("\"status_code\":402") ||
    message.includes("status_code: 402") ||
    message.includes("\"status_code\":429") ||
    message.includes("status_code: 429") ||
    message.includes("too many requests") ||
    message.includes("rate limit")
  );
};

/**
 * Converts an optional script reference into Blockfrost's `additionalUtxoSet`
 * script shape.
 */
const toAdditionalScript = (
  scriptRef: LE.Script | null | undefined,
): Record<string, unknown> | undefined => {
  if (scriptRef === undefined || scriptRef === null) {
    return undefined;
  }
  switch (scriptRef.type) {
    case "PlutusV1":
      return {
        "plutus:v1": LE.applySingleCborEncoding(scriptRef.script),
      };
    case "PlutusV2":
      return {
        "plutus:v2": LE.applySingleCborEncoding(scriptRef.script),
      };
    case "PlutusV3":
      return {
        "plutus:v3": LE.applySingleCborEncoding(scriptRef.script),
      };
    default:
      return undefined;
  }
};

/**
 * Converts Lucid assets into the nested value format expected by Blockfrost's
 * evaluate-UTxOs endpoint.
 */
export const toBlockfrostAdditionalValue = (
  assets: LE.Assets,
): Record<string, unknown> => {
  const value: Record<string, unknown> = {
    coins: assets.lovelace ?? 0n,
  };
  for (const [unit, amount] of Object.entries(assets)) {
    if (unit === "lovelace") continue;
    const policyId = unit.slice(0, 56);
    const assetName = unit.slice(56);
    const policyAssets =
      (value[policyId] as Record<string, bigint> | undefined) ?? {};
    policyAssets[assetName] = amount;
    value[policyId] = policyAssets;
  }
  return value;
};

/**
 * JSON-stringifies a value while preserving bigint fields as bare JSON numbers.
 */
export const stringifyJsonWithBigIntNumbers = (value: unknown): string => {
  if (value === null) {
    return "null";
  }
  const valueType = typeof value;
  if (valueType === "bigint") {
    return (value as bigint).toString(10);
  }
  if (valueType === "string" || valueType === "boolean") {
    return JSON.stringify(value);
  }
  if (valueType === "number") {
    if (!Number.isFinite(value as number)) {
      throw new Error(`Cannot stringify non-finite JSON number: ${value}`);
    }
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map((item) => stringifyJsonWithBigIntNumbers(item)).join(",")}]`;
  }
  if (valueType === "object") {
    const entries = Object.entries(value as Record<string, unknown>)
      .filter(([, entryValue]) => entryValue !== undefined)
      .map(
        ([entryKey, entryValue]) =>
          `${JSON.stringify(entryKey)}:${stringifyJsonWithBigIntNumbers(entryValue)}`,
      );
    return `{${entries.join(",")}}`;
  }
  throw new Error(`Cannot stringify unsupported JSON value type: ${valueType}`);
};

type BlockfrostEvalRedeemerData = {
  memory: number | string;
  steps: number | string;
};

type BlockfrostEvalResponse =
  | {
      result: {
        EvaluationResult: Record<string, BlockfrostEvalRedeemerData>;
      };
    }
  | {
      result: Record<string, unknown>;
    }
  | {
      status_code?: number;
      message?: string;
      fault: unknown;
    };

/**
 * Calls Blockfrost's evaluate-with-UTxO-context endpoint directly.
 */
const evaluateTxViaBlockfrostUtxoEndpoint = async (
  provider: LE.Blockfrost,
  tx: string,
  additionalUTxOs: readonly LE.UTxO[],
): Promise<LE.EvalRedeemer[]> => {
  const blockfrostProvider = provider as unknown as {
    url: string;
    projectId: string;
  };
  const payload = {
    cbor: tx,
    additionalUtxoSet: additionalUTxOs.map((utxo) => [
      {
        txId: utxo.txHash,
        index: utxo.outputIndex,
      },
      {
        address: utxo.address,
        value: toBlockfrostAdditionalValue(utxo.assets),
        datumHash: utxo.datumHash,
        datum: utxo.datum,
        script: toAdditionalScript(utxo.scriptRef),
      },
    ]),
  };
  const res = (await fetch(
    `${blockfrostProvider.url}/utils/txs/evaluate/utxos`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        project_id: blockfrostProvider.projectId,
      },
      body: stringifyJsonWithBigIntNumbers(payload),
    },
  ).then((response) => response.json())) as BlockfrostEvalResponse;

  if ("fault" in res) {
    const message =
      res.status_code === 400
        ? res.message
        : `Could not evaluate the transaction: ${JSON.stringify(res)}. Transaction: ${tx}`;
    throw new Error(message);
  }
  if (!("EvaluationResult" in res.result)) {
    throw new Error(`EvaluateTransaction fails: ${JSON.stringify(res.result)}`);
  }
  const evaluationResult = res.result.EvaluationResult as Record<
    string,
    BlockfrostEvalRedeemerData
  >;

  const evalRedeemers: LE.EvalRedeemer[] = [];
  for (const [redeemerPointer, data] of Object.entries(evaluationResult)) {
    const [pTag, pIndex] = redeemerPointer.split(":");
    evalRedeemers.push({
      redeemer_tag:
        pTag === "certificate"
          ? "publish"
          : pTag === "withdrawal"
            ? "withdraw"
            : (pTag as LE.RedeemerTag),
      redeemer_index: Number(pIndex),
      ex_units: {
        mem: Number(data.memory),
        steps: Number(data.steps),
      },
    });
  }
  return evalRedeemers;
};

/**
 * Formats an outref as `txHash#outputIndex`.
 */
const outRefLabel = (outRef: LE.OutRef): string =>
  `${outRef.txHash}#${outRef.outputIndex}`;

/**
 * Converts a UTxO into its outref-only view.
 */
const utxoToOutRef = (utxo: LE.UTxO): LE.OutRef => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
});

/**
 * Derives the distinct input/reference-input outrefs referenced by a tx body.
 */
const deriveOutRefsFromTx = (tx: string): LE.OutRef[] => {
  const parsed = LE.CML.Transaction.from_cbor_hex(tx);
  const body = parsed.body();
  const outRefs: LE.OutRef[] = [];
  /**
   * Appends a transaction input to the ordered list of selected inputs.
   */
  const push = (input: LE.CML.TransactionInput) => {
    outRefs.push({
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    });
  };

  const inputs = body.inputs();
  for (let index = 0; index < inputs.len(); index += 1) {
    push(inputs.get(index));
  }
  const referenceInputs = body.reference_inputs();
  if (referenceInputs !== undefined) {
    for (let index = 0; index < referenceInputs.len(); index += 1) {
      push(referenceInputs.get(index));
    }
  }
  const deduped = new Map<string, LE.OutRef>();
  for (const outRef of outRefs) {
    deduped.set(outRefLabel(outRef), outRef);
  }
  return Array.from(deduped.values());
};

/**
 * Merges two UTxO context sets, preferring entries from the first list on
 * outref collisions.
 */
const mergeAdditionalUtxos = (
  preferred: LE.UTxO[],
  fallback: LE.UTxO[],
): LE.UTxO[] => {
  const merged = new Map<string, LE.UTxO>();
  for (const utxo of fallback) {
    merged.set(outRefLabel(utxoToOutRef(utxo)), utxo);
  }
  for (const utxo of preferred) {
    merged.set(outRefLabel(utxoToOutRef(utxo)), utxo);
  }
  return Array.from(merged.values());
};

/**
 * Fetches the UTxO context required by a transaction from Blockfrost.
 */
const resolveTxUtxoContext = async (
  provider: LE.Blockfrost,
  tx: string,
): Promise<LE.UTxO[]> => {
  const outRefs = deriveOutRefsFromTx(tx);
  if (outRefs.length === 0) {
    return [];
  }
  const fetched = await provider.getUtxosByOutRef(outRefs);
  return fetched.filter((utxo) => (utxo.assets.lovelace ?? 0n) >= 0n);
};

/**
 * Patches Blockfrost evaluation to retry with explicit UTxO context when the
 * default evaluation path fails.
 */
const patchBlockfrostEvaluateTx = (provider: LE.Blockfrost): LE.Blockfrost => {
  const originalEvaluateTx = provider.evaluateTx.bind(provider);
  provider.evaluateTx = async (tx, additionalUTxOs) => {
    try {
      return await originalEvaluateTx(tx, additionalUTxOs);
    } catch (originalError) {
      let resolvedAdditionalUTxOs = additionalUTxOs ?? [];
      if (resolvedAdditionalUTxOs.length === 0) {
        try {
          resolvedAdditionalUTxOs = await resolveTxUtxoContext(provider, tx);
        } catch {
          // keep empty list and rethrow original below if fallback cannot run
        }
      }
      if (resolvedAdditionalUTxOs.length === 0) {
        throw originalError;
      }
      try {
        const txContextUtxos = await resolveTxUtxoContext(provider, tx);
        return evaluateTxViaBlockfrostUtxoEndpoint(
          provider,
          tx,
          mergeAdditionalUtxos(resolvedAdditionalUTxOs, txContextUtxos),
        );
      } catch {
        throw originalError;
      }
    }
  };
  return provider;
};

/**
 * Replaces Blockfrost's `awaitTx` with a polling implementation that handles
 * quota/rate-limit responses explicitly.
 */
const patchBlockfrostAwaitTx = (provider: LE.Blockfrost): LE.Blockfrost => {
  const blockfrostProvider = provider as unknown as {
    url: string;
    projectId: string;
  };
  provider.awaitTx = async (txHash, checkInterval = 3_000) => {
    while (true) {
      const isConfirmed = await fetch(
        `${blockfrostProvider.url}/txs/${txHash}/cbor`,
        {
          headers: {
            project_id: blockfrostProvider.projectId,
            lucid: "Lucid",
          },
        },
      ).then((res) => res.json());
      const confirmationPayload = isConfirmed as
        | { readonly error?: unknown }
        | { readonly error?: unknown; readonly status_code?: number; readonly message?: string }
        | null
        | undefined;
      if (
        confirmationPayload !== undefined &&
        confirmationPayload !== null &&
        typeof confirmationPayload === "object" &&
        !("error" in confirmationPayload)
      ) {
        await new Promise<void>((resolve) => {
          setTimeout(() => resolve(), 1_000);
        });
        return true;
      }
      if (
        confirmationPayload !== undefined &&
        confirmationPayload !== null &&
        typeof confirmationPayload === "object" &&
        "error" in confirmationPayload
      ) {
        const statusCode =
          "status_code" in confirmationPayload &&
          typeof confirmationPayload.status_code === "number"
            ? confirmationPayload.status_code
            : undefined;
        const message =
          "message" in confirmationPayload &&
          typeof confirmationPayload.message === "string"
            ? confirmationPayload.message
            : "";
        const normalizedMessage = message.toLowerCase();
        if (
          statusCode === 402 ||
          statusCode === 429 ||
          statusCode === 500 ||
          normalizedMessage.includes("usage is over limit") ||
          normalizedMessage.includes("project over limit")
        ) {
          throw new Error(JSON.stringify(confirmationPayload));
        }
      }
      await new Promise<void>((resolve) => {
        setTimeout(() => resolve(), checkInterval);
      });
    }
  };
  return provider;
};

type BlockfrostProviderMethodName =
  | "getProtocolParameters"
  | "getUtxos"
  | "getUtxosWithUnit"
  | "getUtxoByUnit"
  | "getUtxosByOutRef"
  | "getDelegation"
  | "getDatum"
  | "awaitTx"
  | "submitTx"
  | "evaluateTx";

const BLOCKFROST_PROVIDER_METHODS: readonly BlockfrostProviderMethodName[] = [
  "getProtocolParameters",
  "getUtxos",
  "getUtxosWithUnit",
  "getUtxoByUnit",
  "getUtxosByOutRef",
  "getDelegation",
  "getDatum",
  "awaitTx",
  "submitTx",
  "evaluateTx",
];

/**
 * Wraps a primary Blockfrost provider with a fallback API key that is only
 * used after an explicit quota/rate-limit failure.
 *
 * This preserves the primary key as the normal path while still giving
 * operators a deterministic second chance during sustained traffic spikes.
 */
const patchBlockfrostWithApiKeyFallback = (
  primaryProvider: LE.Blockfrost,
  fallbackProvider: LE.Blockfrost,
): LE.Blockfrost => {
  const warnedMethods = new Set<BlockfrostProviderMethodName>();
  const primaryDynamic = primaryProvider as unknown as Record<string, unknown>;
  const fallbackDynamic = fallbackProvider as unknown as Record<string, unknown>;

  const wrap = (methodName: BlockfrostProviderMethodName): void => {
    const primaryMethod = primaryDynamic[methodName];
    const fallbackMethod = fallbackDynamic[methodName];
    if (
      typeof primaryMethod !== "function" ||
      typeof fallbackMethod !== "function"
    ) {
      return;
    }

    primaryDynamic[methodName] = async (...args: unknown[]) => {
      try {
        return await primaryMethod.apply(primaryProvider, args);
      } catch (primaryError) {
        if (!isBlockfrostRateLimitError(primaryError)) {
          throw primaryError;
        }
        if (!warnedMethods.has(methodName)) {
          warnedMethods.add(methodName);
          console.warn(
            [
              `[lucid] Blockfrost primary key ${methodName} hit quota/rate limit;`,
              "retrying with fallback Blockfrost key.",
              `cause=${formatUnknownError(primaryError)}`,
            ].join(" "),
          );
        }
        try {
          return await fallbackMethod.apply(fallbackProvider, args);
        } catch (fallbackError) {
          throw new Error(
            [
              `Blockfrost primary key ${methodName} failed: ${formatUnknownError(primaryError)}`,
              `Blockfrost fallback key ${methodName} failed: ${formatUnknownError(fallbackError)}`,
            ].join(" | "),
          );
        }
      }
    };
  };

  for (const methodName of BLOCKFROST_PROVIDER_METHODS) {
    wrap(methodName);
  }

  return primaryProvider;
};

/**
 * Polls Koios for transaction confirmation when Blockfrost's await endpoint is
 * unavailable or rate limited.
 *
 * The timeout is bounded so higher-level submission logic can surface a
 * concrete degraded-provider failure instead of hanging indefinitely.
 */
const awaitTxViaKoios = async (
  koiosBaseUrl: string,
  txHash: string,
  checkInterval: number,
): Promise<boolean> => {
  const startedAt = Date.now();
  const timeoutMs = 160_000;
  while (Date.now() - startedAt < timeoutMs) {
    const response = await fetch(`${koiosBaseUrl}/tx_info`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
      },
      body: JSON.stringify({
        _tx_hashes: [txHash],
      }),
    });
    const body = (await response.json()) as unknown;
    if (Array.isArray(body) && body.length > 0) {
      return true;
    }
    await new Promise<void>((resolve) => {
      setTimeout(() => resolve(), checkInterval);
    });
  }
  return false;
};

/**
 * Wraps Blockfrost methods with Koios fallbacks when the failure looks like a
 * Blockfrost quota/availability issue.
 */
const patchBlockfrostWithKoiosFallback = (
  provider: LE.Blockfrost,
  network: LE.Network,
): LE.Blockfrost => {
  const koiosBaseUrl = resolveKoiosBaseUrl(network);
  if (koiosBaseUrl === undefined) {
    return provider;
  }
  const koios = new LE.Koios(koiosBaseUrl);
  const warnedMethods = new Set<BlockfrostProviderMethodName>();
  const blockfrostDynamic = provider as unknown as Record<string, unknown>;
  const koiosDynamic = koios as unknown as Record<string, unknown>;

  const wrap = (methodName: BlockfrostProviderMethodName): void => {
    const blockfrostMethod = blockfrostDynamic[methodName];
    const koiosMethod = koiosDynamic[methodName];
    if (
      typeof blockfrostMethod !== "function" ||
      typeof koiosMethod !== "function"
    ) {
      return;
    }
    blockfrostDynamic[methodName] = async (...args: unknown[]) => {
      try {
        return await blockfrostMethod.apply(provider, args);
      } catch (blockfrostError) {
        if (!isBlockfrostFallbackEligibleError(blockfrostError)) {
          throw blockfrostError;
        }
        if (!warnedMethods.has(methodName)) {
          warnedMethods.add(methodName);
          console.warn(
            [
              `[lucid] Blockfrost ${methodName} failed;`,
              `falling back to Koios (${koiosBaseUrl}).`,
              `cause=${formatUnknownError(blockfrostError)}`,
            ].join(" "),
          );
        }
        try {
          if (methodName === "awaitTx") {
            const txHash = args[0];
            const checkInterval = args[1];
            if (typeof txHash !== "string") {
              throw new Error("Koios awaitTx fallback requires tx hash");
            }
            return await awaitTxViaKoios(
              koiosBaseUrl,
              txHash,
              typeof checkInterval === "number" ? checkInterval : 20_000,
            );
          }
          return await koiosMethod.apply(koios, args);
        } catch (koiosError) {
          throw new Error(
            [
              `Blockfrost ${methodName} failed: ${formatUnknownError(blockfrostError)}`,
              `Koios fallback failed: ${formatUnknownError(koiosError)}`,
            ].join(" | "),
          );
        }
      }
    };
  };

  for (const methodName of BLOCKFROST_PROVIDER_METHODS) {
    wrap(methodName);
  }

  return provider;
};

/**
 * Builds the Lucid service bundle used by the node, including reference-script
 * and operator-wallet specializations.
 */
const makeLucid: Effect.Effect<
  {
    api: LE.LucidEvolution;
    referenceScriptsApi: LE.LucidEvolution;
    operatorMainAddress: string;
    operatorMergeAddress: string;
    referenceScriptsAddress: string;
    switchToOperatorsMainWallet: Effect.Effect<void>;
    switchToOperatorsMergingWallet: Effect.Effect<void>;
    switchToReferenceScriptWallet: Effect.Effect<void>;
  },
  ConfigError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const operatorMainAddress = LE.walletFromSeed(
    nodeConfig.L1_OPERATOR_SEED_PHRASE,
    {
      network: nodeConfig.NETWORK,
    },
  ).address;
  const operatorMergeAddress = LE.walletFromSeed(
    nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
    {
      network: nodeConfig.NETWORK,
    },
  ).address;
  yield* Effect.logInfo("Initializing Lucid...");
  const lucid: LE.LucidEvolution = yield* Effect.tryPromise({
    try: () => {
      switch (nodeConfig.L1_PROVIDER) {
        case "Kupmios":
          return LE.Lucid(
            new LE.Kupmios(nodeConfig.L1_KUPO_KEY, nodeConfig.L1_OGMIOS_KEY),
            nodeConfig.NETWORK,
          );
        case "Blockfrost":
          const primaryBlockfrostProvider = patchBlockfrostEvaluateTx(
            patchBlockfrostAwaitTx(
              new LE.Blockfrost(
                nodeConfig.L1_BLOCKFROST_API_URL,
                nodeConfig.L1_BLOCKFROST_KEY,
              ),
            ),
          );
          const fallbackBlockfrostKey =
            nodeConfig.L1_BLOCKFROST_KEY_FALLBACK.trim();
          const withFallbackBlockfrostKey =
            fallbackBlockfrostKey.length > 0 &&
            fallbackBlockfrostKey !== nodeConfig.L1_BLOCKFROST_KEY
              ? patchBlockfrostWithApiKeyFallback(
                  primaryBlockfrostProvider,
                  patchBlockfrostEvaluateTx(
                    patchBlockfrostAwaitTx(
                      new LE.Blockfrost(
                        nodeConfig.L1_BLOCKFROST_API_URL,
                        fallbackBlockfrostKey,
                      ),
                    ),
                  ),
                )
              : primaryBlockfrostProvider;
          const blockfrostProvider = patchBlockfrostWithKoiosFallback(
            withFallbackBlockfrostKey,
            nodeConfig.NETWORK,
          );
          return LE.Lucid(
            blockfrostProvider,
            nodeConfig.NETWORK,
          );
      }
    },
    catch: (e) =>
      new ConfigError({
        message: `An error occurred on lucid initialization`,
        cause: e,
        fieldsAndValues: [
          ["L1_PROVIDER", nodeConfig.L1_PROVIDER],
          ["NETWORK", nodeConfig.NETWORK],
        ],
      }),
  }).pipe(
    Effect.tapError(Effect.logInfo),
    Effect.retry(Schedule.fixed("1000 millis")),
  );
  const referenceScriptsApi: LE.LucidEvolution = yield* Effect.tryPromise({
    try: () => LE.Lucid(lucid.config().provider, nodeConfig.NETWORK),
    catch: (e) =>
      new ConfigError({
        message: "An error occurred while initializing reference-scripts Lucid",
        cause: e,
        fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
      }),
  });
  const switchToReferenceScriptWallet = Effect.sync(() =>
    referenceScriptsApi.selectWallet.fromSeed(
      nodeConfig.L1_REFERENCE_SCRIPT_SEED_PHRASE,
    ),
  );
  yield* switchToReferenceScriptWallet;
  const referenceScriptsAddress = yield* Effect.tryPromise({
    try: () => referenceScriptsApi.wallet().address(),
    catch: (e) =>
      new ConfigError({
        message: "Failed to derive reference-scripts wallet address",
        cause: e,
        fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
      }),
  });
  if (
    nodeConfig.L1_REFERENCE_SCRIPT_ADDRESS.trim() !== referenceScriptsAddress
  ) {
    return yield* Effect.fail(
      new ConfigError({
        message:
          "Configured L1_REFERENCE_SCRIPT_ADDRESS does not match the address derived from L1_REFERENCE_SCRIPT_SEED_PHRASE",
        cause: "reference-script-address-seed-mismatch",
        fieldsAndValues: [
          ["L1_REFERENCE_SCRIPT_ADDRESS", nodeConfig.L1_REFERENCE_SCRIPT_ADDRESS],
          ["derived_address", referenceScriptsAddress],
        ],
      }),
    );
  }
  const distinctOperationalWallets = new Map<string, string[]>();
  for (const [role, address] of [
    ["operator-main", operatorMainAddress],
    ["operator-merge", operatorMergeAddress],
    ["reference-scripts", referenceScriptsAddress],
  ] as const) {
    const existingRoles = distinctOperationalWallets.get(address) ?? [];
    distinctOperationalWallets.set(address, [...existingRoles, role]);
  }
  const overlappingOperationalWalletRoles = [...distinctOperationalWallets]
    .filter(([, roles]) => roles.length > 1)
    .map(([address, roles]) => `${roles.join("+")}=${address}`);
  if (overlappingOperationalWalletRoles.length > 0) {
    return yield* Effect.fail(
      new ConfigError({
        message:
          "Operational wallet roles must use distinct L1 addresses for production-safe local wallet views",
        cause: overlappingOperationalWalletRoles.join(","),
        fieldsAndValues: [
          ["L1_OPERATOR_ADDRESS", operatorMainAddress],
          ["L1_OPERATOR_MERGE_ADDRESS", operatorMergeAddress],
          ["L1_REFERENCE_SCRIPT_ADDRESS", referenceScriptsAddress],
        ],
      }),
    );
  }
  yield* Effect.logInfo("Lucid built successfully.");
  return {
    api: lucid,
    referenceScriptsApi,
    operatorMainAddress,
    operatorMergeAddress,
    referenceScriptsAddress,
    switchToOperatorsMainWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE),
    ),
    switchToOperatorsMergingWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(
        nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
      ),
    ),
    switchToReferenceScriptWallet,
  };
});

/**
 * Service exposing the fully-initialized Lucid clients and wallet-switching
 * helpers used by the node.
 */
export class Lucid extends Effect.Service<Lucid>()("Lucid", {
  effect: makeLucid,
  dependencies: [NodeConfig.layer],
}) {}
