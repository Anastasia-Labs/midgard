import * as SDK from "@al-ft/midgard-sdk";
import { getAssetNameByPolicy, type NodeWithDatum } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data as LucidData,
  LucidEvolution,
  paymentCredentialOf,
  scriptHashToCredential,
  toUnit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { compareOutRefs } from "../tx-context.js";

export const REGISTERED_ACTIVATION_DELAY_MS = SDK.REGISTRATION_DURATION_MS;

export const ACTIVATION_VALIDITY_WINDOW_MS = 120_000n;

export const REGISTERED_SET_REFRESH_MAX_RETRIES = 12;

export const REGISTERED_SET_REFRESH_RETRY_DELAY = "2 seconds";

const NODE_SET_FETCH_MAX_RETRIES = 8;

const NODE_SET_FETCH_RETRY_DELAY = "2 seconds";

const HUB_ORACLE_FETCH_MAX_RETRIES = 6;

const HUB_ORACLE_FETCH_RETRY_DELAY = "1 second";

export const ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE = 25_000_000n;

export type ActivationTxHashes = {
  readonly registerTxHash: string | null;
  readonly activateTxHash: string | null;
};

export type DeregistrationTxHashes = {
  readonly deregisterTxHash: string | null;
};

export type RegistrationTxHashes = {
  readonly registerTxHash: string | null;
};

export type OperatorLifecycleTxHashes = {
  readonly registerTxHash: string | null;
  readonly activateTxHash: string | null;
  readonly deregisterTxHash: string | null;
};

export type OperatorLifecycleMode =
  | "register-and-activate"
  | "register-only"
  | "activate-only"
  | "deregister-only";

export const summarizeOnChainScriptFailure = (
  cause: unknown,
): string | null => {
  const message = String(cause);
  const scriptHashMatch = message.match(/ScriptHash[^0-9a-f]*([0-9a-f]{56})/i);
  const scriptInfoMatch = message.match(/ScriptInfo:\\s*([^\\\\n"]+)/i);
  const reasonMatch = message.match(/Caused by:\\s*([^\\\\n"]+)/i);
  const txIdMatch = message.match(/TxId:\\s*([0-9a-f]{64})/i);
  if (
    scriptHashMatch === null &&
    scriptInfoMatch === null &&
    reasonMatch === null &&
    txIdMatch === null
  ) {
    return null;
  }
  return [
    txIdMatch !== null ? `tx_id=${txIdMatch[1]}` : null,
    scriptHashMatch !== null ? `script_hash=${scriptHashMatch[1]}` : null,
    scriptInfoMatch !== null ? `script_info=${scriptInfoMatch[1]}` : null,
    reasonMatch !== null ? `reason=${reasonMatch[1]}` : null,
  ]
    .filter((value): value is string => value !== null)
    .join(",");
};

export const describeUnknownValue = (value: unknown): string => {
  try {
    return JSON.stringify(value, (_key, innerValue) =>
      typeof innerValue === "bigint" ? innerValue.toString() : innerValue,
    );
  } catch {
    return String(value);
  }
};

export const registeredNodeMatchesOperator = (
  node: SDK.LinkedListNodeView,
  operatorKeyHash: string,
): boolean => SDK.registeredNodeOperator(node) === operatorKeyHash;

export const linkPointsToKey = (
  node: SDK.LinkedListNodeView,
  key: SDK.NodeKey,
): boolean =>
  key !== "Empty" && node.next !== "Empty" && node.next.Key.key === key.Key.key;

export const summarizeNodeSetForDiagnostics = (
  nodes: readonly NodeWithDatum[],
): readonly unknown[] =>
  nodes.map(({ utxo, datum, assetName }) => ({
    outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
    assetName,
    key: datum.key,
    next: datum.next,
    data: datum.data,
    registeredOperator: SDK.registeredNodeOperator(datum),
  }));

export const getOperatorKeyHash = (
  lucid: LucidEvolution,
): Effect.Effect<string, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const operatorAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    const paymentCredential = paymentCredentialOf(operatorAddress);
    if (paymentCredential?.type !== "Key") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Operator wallet does not have a key payment credential",
          cause: operatorAddress,
        }),
      );
    }
    return paymentCredential.hash;
  });

export const fetchHubOracleRefInput = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to resolve Cardano network for hub-oracle witness lookup",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const hubOracleAddress = credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    );
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    let hubOracleUtxos: UTxO[] | null = null;
    let lastFetchCause: unknown = null;
    for (
      let attempt = 0;
      attempt < HUB_ORACLE_FETCH_MAX_RETRIES;
      attempt += 1
    ) {
      const withUnitAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
          catch: (cause) => cause,
        }),
      );
      if (withUnitAttempt._tag === "Right") {
        hubOracleUtxos = withUnitAttempt.right;
        break;
      }

      const atAddressAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: async () => {
            const atAddress = await lucid.utxosAt(hubOracleAddress);
            return atAddress.filter(
              (utxo) => (utxo.assets[hubOracleUnit] ?? 0n) > 0n,
            );
          },
          catch: (cause) => cause,
        }),
      );
      if (atAddressAttempt._tag === "Right") {
        hubOracleUtxos = atAddressAttempt.right;
        break;
      }

      lastFetchCause = `utxosAtWithUnit=${String(withUnitAttempt.left)};utxosAt=${String(atAddressAttempt.left)}`;
      if (attempt + 1 < HUB_ORACLE_FETCH_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Hub-oracle UTxO fetch failed (attempt ${(attempt + 1).toString()}/${HUB_ORACLE_FETCH_MAX_RETRIES.toString()}); retrying in ${HUB_ORACLE_FETCH_RETRY_DELAY}. cause=${String(lastFetchCause)}`,
        );
        yield* Effect.sleep(HUB_ORACLE_FETCH_RETRY_DELAY);
      }
    }
    if (hubOracleUtxos === null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to fetch hub-oracle UTxOs for operator registration flow",
          cause: `attempts=${HUB_ORACLE_FETCH_MAX_RETRIES.toString()},last_cause=${String(lastFetchCause)}`,
        }),
      );
    }
    if (hubOracleUtxos.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Expected exactly one hub-oracle witness UTxO",
          cause: `found=${hubOracleUtxos.length},address=${hubOracleAddress},unit=${hubOracleUnit}`,
        }),
      );
    }
    return hubOracleUtxos[0];
  });

export const fetchNodeSet = (
  lucid: LucidEvolution,
  address: string,
  policyId: string,
): Effect.Effect<NodeWithDatum[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    let utxos: UTxO[] | null = null;
    let lastFetchError: SDK.StateQueueError | null = null;
    for (let attempt = 0; attempt < NODE_SET_FETCH_MAX_RETRIES; attempt += 1) {
      const fetchResult = yield* Effect.either(
        SDK.utxosAtByNFTPolicyId(lucid, address, policyId).pipe(
          Effect.map((beaconUtxos) => beaconUtxos.map(({ utxo }) => utxo)),
          Effect.mapError(
            (cause) =>
              new SDK.StateQueueError({
                message: `Failed to fetch node set at address ${address}`,
                cause,
              }),
          ),
        ),
      );
      if (fetchResult._tag === "Right") {
        utxos = fetchResult.right;
        break;
      }
      lastFetchError = fetchResult.left;
      if (attempt + 1 < NODE_SET_FETCH_MAX_RETRIES) {
        yield* Effect.sleep(NODE_SET_FETCH_RETRY_DELAY);
      }
    }
    if (utxos === null) {
      return yield* Effect.fail(
        lastFetchError ??
          new SDK.StateQueueError({
            message: `Failed to fetch node set at address ${address}`,
            cause: "unknown",
          }),
      );
    }
    const sorted = [...utxos].sort(compareOutRefs);
    return yield* Effect.forEach(sorted, (utxo) =>
      Effect.gen(function* () {
        const datum = yield* SDK.getLinkedListNodeViewFromUTxO(utxo).pipe(
          Effect.mapError(
            (cause) =>
              new SDK.StateQueueError({
                message:
                  "Failed to decode node datum while loading operator set",
                cause,
              }),
          ),
        );
        const assetName = getAssetNameByPolicy(utxo.assets, policyId);
        if (assetName === null) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message: "Failed to resolve unique node NFT asset for policy",
              cause: `${utxo.txHash}#${utxo.outputIndex},policy=${policyId}`,
            }),
          );
        }
        return { utxo, datum, assetName };
      }),
    );
  });

export const decodeHubOracleDatum = (
  hubOracleRefInput: UTxO,
): Effect.Effect<SDK.HubOracleDatum, SDK.StateQueueError> =>
  Effect.try({
    try: () => {
      if (hubOracleRefInput.datum == null) {
        throw new Error("Hub oracle reference input is missing inline datum");
      }
      return LucidData.from(hubOracleRefInput.datum, SDK.HubOracleDatum);
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: "Failed to decode hub oracle datum from reference input",
        cause,
      }),
  });

export const toActivationResult = (
  txHashes: ActivationTxHashes,
): Effect.Effect<ActivationTxHashes, never> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `Operator onboarding result: registerTxHash=${txHashes.registerTxHash ?? "skipped"}, activateTxHash=${txHashes.activateTxHash ?? "skipped"}`,
    );
    return txHashes;
  });
