/**
 * Operator lifecycle transaction orchestration for register/activate flows.
 * This module is the main off-chain entrypoint for operator-set updates and
 * composes the shared layout and clock helpers extracted from the monolith.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  activateLayoutToLogString,
  activeAppendAnchorWitness,
  getAssetNameByPolicy,
  nodeKeyEquals,
  type NodeWithDatum,
  orderedNotMemberWitness,
  registerLayoutToLogString,
} from "@al-ft/midgard-sdk";
import {
  Constr as LucidConstr,
  credentialToAddress,
  Data as LucidData,
  LucidEvolution,
  paymentCredentialOf,
  scriptHashToCredential,
  toUnit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  configuredContractDeploymentInfoPath,
  readFinalizedDeploymentIdentity,
  verifyConfiguredDeploymentManifestProgram,
} from "@/commands/contract-deployment-info.js";
import { Lucid, MidgardContracts } from "@/services/index.js";
import {
  referenceScriptTargetsByCommand,
  resolveReferenceScriptTargetsProgram,
  resolveSpendableWalletUtxos,
  selectWalletFundingUtxos,
  utxoOutRefKey,
} from "@/transactions/reference-scripts.js";
import {
  alignUnixTimeMsToSlotBoundary,
  currentTimeMsForLucidOrEmulatorFallback,
  resolveCurrentTimeMs,
} from "@/transactions/register-active-operator/clock.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { compareOutRefs } from "@/tx-context.js";
import { alignedUnixTimeStrictlyAfter } from "@/workers/utils/commit-end-time.js";
export type { ReferenceScriptCommandName } from "@/transactions/reference-scripts.js";
export {
  deployReferenceScriptCommandProgram,
  REFERENCE_SCRIPT_COMMAND_NAMES,
} from "@/transactions/reference-scripts.js";

const REGISTERED_ACTIVATION_DELAY_MS = 30n;
const ACTIVATION_VALIDITY_WINDOW_MS = 120_000n;
const REGISTERED_SET_REFRESH_MAX_RETRIES = 12;
const REGISTERED_SET_REFRESH_RETRY_DELAY = "2 seconds";
const NODE_SET_FETCH_MAX_RETRIES = 8;
const NODE_SET_FETCH_RETRY_DELAY = "2 seconds";
const HUB_ORACLE_FETCH_MAX_RETRIES = 6;
const HUB_ORACLE_FETCH_RETRY_DELAY = "1 second";
const ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE = 25_000_000n;
const REGISTERED_OPERATOR_DATUM_AIKEN_SCHEMA = LucidData.Object({
  operator: LucidData.Bytes({ minLength: 28, maxLength: 28 }),
});

type ActivationTxHashes = {
  readonly registerTxHash: string | null;
  readonly activateTxHash: string | null;
};

type DeregistrationTxHashes = {
  readonly deregisterTxHash: string | null;
};

type RegistrationTxHashes = {
  readonly registerTxHash: string | null;
};

type OperatorLifecycleTxHashes = {
  readonly registerTxHash: string | null;
  readonly activateTxHash: string | null;
  readonly deregisterTxHash: string | null;
};

type OperatorLifecycleMode =
  | "register-and-activate"
  | "register-only"
  | "activate-only"
  | "deregister-only";

const summarizeOnChainScriptFailure = (cause: unknown): string | null => {
  const message = String(cause);
  const scriptHashMatch = message.match(/ScriptHash[^0-9a-f]*([0-9a-f]{56})/i);
  const scriptInfoMatch = message.match(/ScriptInfo:\\s*([^\\\\n\"]+)/i);
  const reasonMatch = message.match(/Caused by:\\s*([^\\\\n\"]+)/i);
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

const describeUnknownValue = (value: unknown): string => {
  try {
    return JSON.stringify(value, (_key, innerValue) =>
      typeof innerValue === "bigint" ? innerValue.toString() : innerValue,
    );
  } catch {
    return String(value);
  }
};

const isRawLucidConstr = (
  value: unknown,
): value is { readonly index: number | bigint; readonly fields: unknown[] } =>
  typeof value === "object" &&
  value !== null &&
  "index" in value &&
  "fields" in value &&
  (typeof value.index === "number" || typeof value.index === "bigint") &&
  Array.isArray(value.fields);

const normalizeLucidDataValue = (value: unknown): unknown => {
  if (value instanceof LucidConstr) {
    return value;
  }
  if (isRawLucidConstr(value)) {
    return new LucidConstr(
      Number(value.index),
      value.fields.map(normalizeLucidDataValue),
    );
  }
  if (Array.isArray(value)) {
    return value.map(normalizeLucidDataValue);
  }
  if (value instanceof Map) {
    return new Map(
      [...value.entries()].map(([key, innerValue]) => [
        normalizeLucidDataValue(key),
        normalizeLucidDataValue(innerValue),
      ]),
    );
  }
  return value;
};

const posixTimeToNodeKey = (posixTime: bigint): string => {
  if (posixTime < 0n) {
    throw new Error("Registered-operator activation time cannot be negative");
  }
  const hex = posixTime.toString(16);
  return hex.length % 2 === 0 ? hex : `0${hex}`;
};

const nodeKeyToPosixTime = (key: SDK.NodeKey): bigint | undefined => {
  if (key === "Empty") {
    return undefined;
  }
  return key.Key.key.length === 0 ? 0n : BigInt(`0x${key.Key.key}`);
};

const decodeRegisteredOperatorDatumValue = (
  value: unknown,
): SDK.RegisteredOperatorDatum | undefined => {
  if (typeof value === "object" && value !== null) {
    if ("operator" in value && typeof value.operator === "string") {
      return {
        operator: value.operator,
      };
    }
  }
  try {
    const normalizedValue = normalizeLucidDataValue(value);
    const parsed = LucidData.castFrom(
      normalizedValue as never,
      REGISTERED_OPERATOR_DATUM_AIKEN_SCHEMA as never,
    ) as SDK.RegisteredOperatorDatum;
    return parsed;
  } catch {
    return undefined;
  }
};

const registeredNodeMatchesOperator = (
  node: SDK.LinkedListNodeView,
  operatorKeyHash: string,
): boolean =>
  decodeRegisteredOperatorDatumValue(node.data)?.operator === operatorKeyHash;

const linkPointsToKey = (
  node: SDK.LinkedListNodeView,
  key: SDK.NodeKey,
): boolean =>
  key !== "Empty" && node.next !== "Empty" && node.next.Key.key === key.Key.key;

const summarizeNodeSetForDiagnostics = (
  nodes: readonly NodeWithDatum[],
): readonly unknown[] =>
  nodes.map(({ utxo, datum, assetName }) => ({
    outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
    assetName,
    key: datum.key,
    next: datum.next,
    data: datum.data,
    registeredDatum: decodeRegisteredOperatorDatumValue(datum.data) ?? null,
  }));

const getOperatorKeyHash = (
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

const fetchHubOracleRefInput = (
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

const fetchNodeSet = (
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

const decodeHubOracleDatum = (
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

const toActivationResult = (
  txHashes: ActivationTxHashes,
): Effect.Effect<ActivationTxHashes, never> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `Operator onboarding result: registerTxHash=${txHashes.registerTxHash ?? "skipped"}, activateTxHash=${txHashes.activateTxHash ?? "skipped"}`,
    );
    return txHashes;
  });

const toLifecycleResult = (
  txHashes: OperatorLifecycleTxHashes,
): Effect.Effect<OperatorLifecycleTxHashes, never> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `Operator lifecycle result: registerTxHash=${txHashes.registerTxHash ?? "skipped"}, activateTxHash=${txHashes.activateTxHash ?? "skipped"}, deregisterTxHash=${txHashes.deregisterTxHash ?? "skipped"}`,
    );
    return txHashes;
  });

const operatorLifecycleProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  mode: OperatorLifecycleMode,
  referenceScriptsLucid: LucidEvolution = lucid,
  referenceScriptsAddress?: string,
): Effect.Effect<
  OperatorLifecycleTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const usesWallClockTime = lucid.config().network === "Custom";
    const hubOracleRefInput = yield* fetchHubOracleRefInput(lucid, contracts);
    const hubOracleDatum = yield* decodeHubOracleDatum(hubOracleRefInput);
    yield* Effect.logInfo(
      `Hub oracle policies: registered=${hubOracleDatum.registered_operators},active=${hubOracleDatum.active_operators},retired=${hubOracleDatum.retired_operators}`,
    );
    const policyMismatches: string[] = [];
    if (
      hubOracleDatum.registered_operators !==
      contracts.registeredOperators.policyId
    ) {
      policyMismatches.push(
        `registered(hub=${hubOracleDatum.registered_operators},contracts=${contracts.registeredOperators.policyId})`,
      );
    }
    if (
      hubOracleDatum.active_operators !== contracts.activeOperators.policyId
    ) {
      policyMismatches.push(
        `active(hub=${hubOracleDatum.active_operators},contracts=${contracts.activeOperators.policyId})`,
      );
    }
    if (
      hubOracleDatum.retired_operators !== contracts.retiredOperators.policyId
    ) {
      policyMismatches.push(
        `retired(hub=${hubOracleDatum.retired_operators},contracts=${contracts.retiredOperators.policyId})`,
      );
    }
    if (policyMismatches.length > 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Hub oracle policy ids do not match configured contract policy ids; activation would be invalid with mismatched policies",
          cause: policyMismatches.join(","),
        }),
      );
    }
    // `activate-only` must reuse the same published active/registered reference
    // scripts that `register-only` established, otherwise activation can build
    // against a different reference-input layout than registration prepared.
    const operatorReferenceTargets = [
      ...referenceScriptTargetsByCommand(contracts)["registered-operators"],
      ...referenceScriptTargetsByCommand(contracts)["active-operators"],
    ] as const;
    const operatorReferenceScriptRefs =
      mode === "deregister-only"
        ? yield* resolveReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "registered-operators",
            referenceScriptTargetsByCommand(contracts)["registered-operators"],
            contracts.referenceScriptAuth,
            referenceScriptsAddress,
          )
        : yield* resolveReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "operator lifecycle",
            operatorReferenceTargets,
            contracts.referenceScriptAuth,
            referenceScriptsAddress,
          );
    const registeredOperatorScriptRefs = operatorReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("registered-operators "),
    );
    const activeOperatorScriptRefs = operatorReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("active-operators "),
    );
    const lifecycleScriptRefs = [
      ...registeredOperatorScriptRefs,
      ...activeOperatorScriptRefs,
    ];
    const lifecycleScriptRefOutRefs = new Set(
      lifecycleScriptRefs.map(({ utxo }) => utxoOutRefKey(utxo)),
    );

    const registeredNodes = yield* fetchNodeSet(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      contracts.registeredOperators.policyId,
    );
    const activeNodes = yield* fetchNodeSet(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
    );
    const retiredNodes = yield* fetchNodeSet(
      lucid,
      contracts.retiredOperators.spendingScriptAddress,
      contracts.retiredOperators.policyId,
    );

    yield* Effect.logInfo(
      `Operator set snapshot: registered(policy=${contracts.registeredOperators.policyId},address=${contracts.registeredOperators.spendingScriptAddress},count=${registeredNodes.length.toString()}),active(policy=${contracts.activeOperators.policyId},address=${contracts.activeOperators.spendingScriptAddress},count=${activeNodes.length.toString()}),retired(policy=${contracts.retiredOperators.policyId},address=${contracts.retiredOperators.spendingScriptAddress},count=${retiredNodes.length.toString()})`,
    );
    yield* Effect.logInfo(
      `Retired set keys: ${retiredNodes
        .map(({ datum }) =>
          datum.key === "Empty" ? "Empty" : datum.key.Key.key,
        )
        .join(",")}`,
    );

    const existingActiveNodes = activeNodes.filter(({ datum }) =>
      nodeKeyEquals(datum, operatorKeyHash),
    );
    if (existingActiveNodes.length > 0) {
      yield* Effect.logInfo(
        `Operator ${operatorKeyHash} is already active; skipping operator lifecycle step for mode=${mode}.`,
      );
      return yield* toLifecycleResult({
        registerTxHash: null,
        activateTxHash: null,
        deregisterTxHash: null,
      });
    }

    let currentRegisteredNodes = registeredNodes;
    let registerTxHash: string | null = null;
    let activateTxHash: string | null = null;
    let deregisterTxHash: string | null = null;

    const existingRegisteredNodes = registeredNodes.filter(({ datum }) =>
      registeredNodeMatchesOperator(datum, operatorKeyHash),
    );
    if (existingRegisteredNodes.length > 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Found multiple registered-operator nodes for the current operator key",
          cause: operatorKeyHash,
        }),
      );
    }
    const resumeActivationFromExistingRegistration =
      existingRegisteredNodes.length === 1 && mode === "register-and-activate";

    if (existingRegisteredNodes.length === 1) {
      if (mode === "register-only") {
        yield* Effect.logInfo(
          `Operator ${operatorKeyHash} is already registered; skipping register-only flow.`,
        );
        return yield* toLifecycleResult({
          registerTxHash: null,
          activateTxHash: null,
          deregisterTxHash: null,
        });
      }

      if (mode === "register-and-activate") {
        yield* Effect.logInfo(
          `Operator ${operatorKeyHash} is already registered but not active; resuming with activation without deregistration.`,
        );
      } else if (mode === "deregister-only") {
        const existingRegisteredNode = existingRegisteredNodes[0];
        const existingRegisteredAnchor = currentRegisteredNodes.find(
          ({ datum }) =>
            linkPointsToKey(datum, existingRegisteredNode.datum.key),
        );
        if (existingRegisteredAnchor === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Registered-operators anchor node for existing registration was not found",
              cause: operatorKeyHash,
            }),
          );
        }
        yield* Effect.logWarning(
          `Operator ${operatorKeyHash} is registered; executing deregister-only lifecycle step.`,
        );

        const registeredNodeUnit = toUnit(
          contracts.registeredOperators.policyId,
          existingRegisteredNode.assetName,
        );
        const updatedRegisteredAnchorDatumAfterDeregister: SDK.LinkedListNodeView =
          {
            ...existingRegisteredAnchor.datum,
            next: existingRegisteredNode.datum.next,
          };
        const spendableWalletUtxosForDeregister =
          yield* resolveSpendableWalletUtxos(lucid, lifecycleScriptRefOutRefs);
        if (spendableWalletUtxosForDeregister.length === 0) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "No wallet funding UTxOs available for deregistration transaction",
              cause: operatorKeyHash,
            }),
          );
        }
        const deregisterUnsignedTx = yield* Effect.tryPromise({
          try: () =>
            SDK.buildDeregisterRegisteredOperatorTx({
              lucid,
              contracts,
              operatorKeyHash,
              registeredOperatorScriptRefs,
              registeredNode: existingRegisteredNode,
              registeredAnchor: existingRegisteredAnchor,
              registeredNodeUnit,
              updatedRegisteredAnchorDatum:
                updatedRegisteredAnchorDatumAfterDeregister,
            }).complete({
              localUPLCEval: true,
              presetWalletInputs: [...spendableWalletUtxosForDeregister],
            }),
          catch: (cause) =>
            new SDK.LucidError({
              message:
                "Failed to build operator deregistration transaction during operator lifecycle flow",
              cause,
            }),
        });
        const deregisterSubmitResult = yield* Effect.either(
          handleSignSubmit(lucid, deregisterUnsignedTx),
        );
        if (deregisterSubmitResult._tag === "Left") {
          return yield* Effect.fail(deregisterSubmitResult.left);
        }
        deregisterTxHash = deregisterSubmitResult.right;
        let registrationCleared = false;
        for (
          let attempt = 0;
          attempt < REGISTERED_SET_REFRESH_MAX_RETRIES;
          attempt += 1
        ) {
          currentRegisteredNodes = yield* fetchNodeSet(
            lucid,
            contracts.registeredOperators.spendingScriptAddress,
            contracts.registeredOperators.policyId,
          );
          const remainingRegistrationNodes = currentRegisteredNodes.filter(
            ({ datum }) =>
              registeredNodeMatchesOperator(datum, operatorKeyHash),
          );
          if (remainingRegistrationNodes.length === 0) {
            registrationCleared = true;
            break;
          }
          if (remainingRegistrationNodes.length > 1) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  "Found multiple registered-operator nodes after deregistration step",
                cause: operatorKeyHash,
              }),
            );
          }
          if (attempt + 1 < REGISTERED_SET_REFRESH_MAX_RETRIES) {
            yield* Effect.sleep(REGISTERED_SET_REFRESH_RETRY_DELAY);
          }
        }
        if (!registrationCleared) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Deregistration step did not clear the operator node from registered set",
              cause: operatorKeyHash,
            }),
          );
        }
      }
    }

    const postRefreshRegisteredNodes = currentRegisteredNodes.filter(
      ({ datum }) => registeredNodeMatchesOperator(datum, operatorKeyHash),
    );
    if (postRefreshRegisteredNodes.length > 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Found multiple registered-operator nodes after registration refresh",
          cause: operatorKeyHash,
        }),
      );
    }
    if (mode === "deregister-only") {
      if (postRefreshRegisteredNodes.length === 0) {
        return yield* toLifecycleResult({
          registerTxHash: null,
          activateTxHash: null,
          deregisterTxHash,
        });
      }
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Deregister-only flow expected the operator node to be removed from registered set",
          cause: operatorKeyHash,
        }),
      );
    }

    if (mode === "activate-only" || resumeActivationFromExistingRegistration) {
      if (postRefreshRegisteredNodes.length === 1) {
        yield* Effect.logInfo(
          `${mode} flow found pre-existing registration node for operator ${operatorKeyHash}.`,
        );
      } else {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Activate-only flow requires an existing registered operator node",
            cause: operatorKeyHash,
          }),
        );
      }
    } else if (postRefreshRegisteredNodes.length === 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Registration refresh did not clear existing operator node from registered set",
          cause: operatorKeyHash,
        }),
      );
    }

    if (
      postRefreshRegisteredNodes.length === 0 &&
      mode !== "activate-only" &&
      !resumeActivationFromExistingRegistration
    ) {
      const registeredRootNode = currentRegisteredNodes.find(
        ({ datum }) => datum.key === "Empty",
      );
      if (registeredRootNode === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Registered-operators root node is missing",
            cause: `policy=${contracts.registeredOperators.policyId}`,
          }),
        );
      }

      const activeNotMemberWitness = activeNodes.find(({ datum }) =>
        orderedNotMemberWitness(datum, operatorKeyHash),
      );
      if (activeNotMemberWitness === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to find active-operators witness node proving non-membership for registration",
            cause: operatorKeyHash,
          }),
        );
      }
      const retiredNotMemberWitness = retiredNodes.find(({ datum }) =>
        orderedNotMemberWitness(datum, operatorKeyHash),
      );
      if (retiredNotMemberWitness === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to find retired-operators witness node proving non-membership for registration",
            cause: operatorKeyHash,
          }),
        );
      }

      const registerBuildTime = yield* resolveCurrentTimeMs(lucid);
      const registerValidTo = alignUnixTimeMsToSlotBoundary(
        lucid,
        registerBuildTime + ACTIVATION_VALIDITY_WINDOW_MS,
      );
      const registrationTime =
        registerValidTo - 1n + REGISTERED_ACTIVATION_DELAY_MS;
      const registrationNodeKey = posixTimeToNodeKey(registrationTime);
      const prependedNodeDatum: SDK.LinkedListNodeView = {
        key: { Key: { key: registrationNodeKey } },
        next: registeredRootNode.datum.next,
        data: SDK.encodeRegisteredOperatorDatumValue(
          operatorKeyHash,
        ) as SDK.LinkedListNodeView["data"],
      };
      const updatedRegisteredRootDatum: SDK.LinkedListNodeView = {
        ...registeredRootNode.datum,
        next: { Key: { key: registrationNodeKey } },
      };

      const registeredNodeUnit = toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + registrationNodeKey,
      );
      const registerMintAssets = {
        [registeredNodeUnit]: 1n,
      };
      const spendableWalletUtxosForRegister =
        yield* resolveSpendableWalletUtxos(lucid, lifecycleScriptRefOutRefs);
      if (spendableWalletUtxosForRegister.length === 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "No wallet funding UTxOs available for registration transaction",
            cause: operatorKeyHash,
          }),
        );
      }
      const registerFundingInputs = selectWalletFundingUtxos(
        spendableWalletUtxosForRegister,
        requiredBondLovelace + ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
      );
      if (registerFundingInputs.length === 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to select wallet funding UTxOs for registration transaction",
            cause: operatorKeyHash,
          }),
        );
      }
      const prependedNodeAssets = {
        lovelace: requiredBondLovelace,
        [registeredNodeUnit]: 1n,
      };
      /**
       * Builds the registration transaction for an active operator.
       */
      let registerLayout: SDK.RegisterRedeemerLayout | undefined;
      const registerTxConfigBase = {
        lucid,
        contracts,
        operatorKeyHash,
        registeredOperatorScriptRefs,
        hubOracleRefInput,
        activeNotMemberWitness,
        retiredNotMemberWitness,
        registeredRootNode,
        registerFundingInputs,
        registerMintAssets,
        prependedNodeDatum,
        prependedNodeAssets,
        updatedRegisteredRootDatum,
        registerValidTo,
      };
      const mkRegisterTx = (layout?: SDK.RegisterRedeemerLayout) =>
        SDK.buildRegisterOperatorTx({
          ...registerTxConfigBase,
          layout,
          onLayout: (resolvedLayout) => {
            registerLayout = resolvedLayout;
          },
        });

      yield* Effect.logInfo(
        [
          "Register witnesses:",
          `hub=${hubOracleRefInput.txHash}#${hubOracleRefInput.outputIndex.toString()}`,
          `active=${activeNotMemberWitness.utxo.txHash}#${activeNotMemberWitness.utxo.outputIndex.toString()}:${activeNotMemberWitness.assetName}`,
          `retired=${retiredNotMemberWitness.utxo.txHash}#${retiredNotMemberWitness.utxo.outputIndex.toString()}:${retiredNotMemberWitness.assetName}`,
          `valid_to=${registerValidTo.toString()}`,
          `registration_time=${registrationTime.toString()}`,
          `prepended_node_datum=${SDK.encodeLinkedListNodeView(prependedNodeDatum)}`,
        ].join(" "),
      );
      yield* Effect.tryPromise({
        try: () =>
          mkRegisterTx().complete({
            localUPLCEval: true,
            presetWalletInputs: [...registerFundingInputs],
          }),
        catch: (cause) =>
          new SDK.LucidError({
            message: [
              "Failed to build operator registration transaction with final redeemer context.",
              `cause=${String(cause)}`,
            ].join(" "),
            cause,
          }),
      });
      if (registerLayout === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "BuildTxWithRedeemer did not resolve operator register layout",
            cause: operatorKeyHash,
          }),
        );
      }
      const resolvedRegisterLayout = registerLayout;
      yield* Effect.logInfo(
        `Using register redeemer layout: ${registerLayoutToLogString(resolvedRegisterLayout)}`,
      );
      const registerUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          mkRegisterTx(resolvedRegisterLayout).complete({
            localUPLCEval: true,
            presetWalletInputs: [...registerFundingInputs],
          }),
        catch: (cause) =>
          new SDK.LucidError({
            message: [
              "Failed to rebuild operator registration transaction with resolved redeemer context.",
              `cause=${String(cause)}`,
            ].join(" "),
            cause,
          }),
      });
      registerTxHash = yield* handleSignSubmit(lucid, registerUnsignedTx);
      let refreshedRegisteredNodeSet = false;
      for (
        let attempt = 0;
        attempt < REGISTERED_SET_REFRESH_MAX_RETRIES;
        attempt += 1
      ) {
        currentRegisteredNodes = yield* fetchNodeSet(
          lucid,
          contracts.registeredOperators.spendingScriptAddress,
          contracts.registeredOperators.policyId,
        );
        const operatorNodeVisible = currentRegisteredNodes.some(({ datum }) =>
          registeredNodeMatchesOperator(datum, operatorKeyHash),
        );
        if (operatorNodeVisible) {
          refreshedRegisteredNodeSet = true;
          break;
        }
        if (attempt + 1 < REGISTERED_SET_REFRESH_MAX_RETRIES) {
          yield* Effect.sleep(REGISTERED_SET_REFRESH_RETRY_DELAY);
        }
      }
      if (!refreshedRegisteredNodeSet) {
        const diagnostics = {
          operatorKeyHash,
          nodeCount: currentRegisteredNodes.length,
          nodes: summarizeNodeSetForDiagnostics(currentRegisteredNodes),
        };
        yield* Effect.logWarning(
          `Registered set refresh diagnostics: ${describeUnknownValue(diagnostics)}`,
        );
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Registered set refresh did not expose the operator node after successful registration",
            cause: describeUnknownValue(diagnostics),
          }),
        );
      }
    }

    if (mode === "register-only") {
      return yield* toLifecycleResult({
        registerTxHash,
        activateTxHash: null,
        deregisterTxHash,
      });
    }

    const registeredNode = currentRegisteredNodes.find(({ datum }) =>
      registeredNodeMatchesOperator(datum, operatorKeyHash),
    );
    if (registeredNode === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Operator registration node was not found after registration step",
          cause: operatorKeyHash,
        }),
      );
    }
    const registeredAnchor = currentRegisteredNodes.find(({ datum }) =>
      linkPointsToKey(datum, registeredNode.datum.key),
    );
    if (registeredAnchor === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Registered-operators anchor node for activation was not found",
          cause: operatorKeyHash,
        }),
      );
    }

    const activeAppendAnchor = activeNodes.find(({ datum }) =>
      activeAppendAnchorWitness(datum, operatorKeyHash),
    );
    if (activeAppendAnchor === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to find active-operators append anchor for activation",
          cause:
            "Current operator key must be lexicographically greater than the active-operators tail key",
        }),
      );
    }
    const retiredNotMemberWitnessForActivate = retiredNodes.find(({ datum }) =>
      orderedNotMemberWitness(datum, operatorKeyHash),
    );
    if (retiredNotMemberWitnessForActivate === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to find retired-operators witness node proving non-membership for activation",
          cause: operatorKeyHash,
        }),
      );
    }

    const activationTime = nodeKeyToPosixTime(registeredNode.datum.key);
    if (activationTime === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to decode registered-operator activation time from registration node key",
          cause: describeUnknownValue(registeredNode.datum.key),
        }),
      );
    }
    const initialNow = yield* resolveCurrentTimeMs(lucid);
    if (!usesWallClockTime && initialNow < activationTime) {
      const waitMs = activationTime - initialNow + 1_000n;
      yield* Effect.logInfo(
        `Waiting ${waitMs.toString()}ms until operator activation time (ledger_now=${initialNow.toString()},activation_time=${activationTime.toString()})`,
      );
      yield* Effect.sleep(Number(waitMs));
    }
    const resolveActivationValidityWindow = (): {
      readonly validFrom: bigint;
      readonly validTo?: bigint;
    } => {
      const currentTime = currentTimeMsForLucidOrEmulatorFallback(lucid);
      const lowerBoundTarget = activationTime;
      // Keep a finite lower bound at/after the required activation lower-bound:
      // canonical registered-operator activation requires the whole interval to
      // be after the activation time encoded in the registered node key.
      // On custom networks (emulator), avoid an upper bound because wall-clock
      // slot estimation can drift from the emulator ledger tip during long
      // candidate retries and cause false "slot range" submit failures.
      const alignedActivationTime = alignUnixTimeMsToSlotBoundary(
        lucid,
        lowerBoundTarget,
      );
      const validFrom =
        alignedActivationTime >= lowerBoundTarget
          ? alignedActivationTime
          : BigInt(
              alignedUnixTimeStrictlyAfter(lucid, Number(lowerBoundTarget)),
            );
      if (usesWallClockTime) {
        return { validFrom };
      }
      const upperBoundBase = currentTime + ACTIVATION_VALIDITY_WINDOW_MS;
      const validTo = alignUnixTimeMsToSlotBoundary(
        lucid,
        upperBoundBase > validFrom
          ? upperBoundBase
          : validFrom + ACTIVATION_VALIDITY_WINDOW_MS,
      );
      return { validFrom, validTo };
    };

    const registeredNodeUnit = toUnit(
      contracts.registeredOperators.policyId,
      registeredNode.assetName,
    );
    const activeNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );
    const transferredOperatorAssets = {
      ...registeredNode.utxo.assets,
      [registeredNodeUnit]: 0n,
      [activeNodeUnit]: 1n,
    };
    delete transferredOperatorAssets[registeredNodeUnit];

    const spendableWalletUtxosForActivation =
      yield* resolveSpendableWalletUtxos(lucid, lifecycleScriptRefOutRefs);
    if (spendableWalletUtxosForActivation.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "No wallet funding UTxOs available for activation transaction",
          cause: operatorKeyHash,
        }),
      );
    }
    const activationFundingInputs = selectWalletFundingUtxos(
      spendableWalletUtxosForActivation,
      ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
    );
    if (activationFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to select wallet funding UTxOs for activation transaction",
          cause: operatorKeyHash,
        }),
      );
    }
    const activationCompleteOptions = (options: {
      readonly localUPLCEval: boolean;
    }) => ({
      ...options,
      presetWalletInputs: [...activationFundingInputs],
    });

    const updatedRegisteredAnchorDatum: SDK.LinkedListNodeView = {
      ...registeredAnchor.datum,
      next: registeredNode.datum.next,
    };
    /**
     * Builds the activation transaction for a registered operator.
     */
    let activateLayout: SDK.ActivateRedeemerLayout | undefined;
    const mkActivateTx = (layout?: SDK.ActivateRedeemerLayout) => {
      const { validFrom, validTo } = resolveActivationValidityWindow();
      return SDK.buildActivateOperatorTx({
        lucid,
        contracts,
        operatorKeyHash,
        registeredOperatorScriptRefs,
        activeOperatorScriptRefs,
        hubOracleRefInput,
        retiredNotMemberWitness: retiredNotMemberWitnessForActivate,
        registeredNode,
        registeredAnchor,
        activeAppendAnchor,
        activationFundingInputs,
        validFrom,
        validTo,
        registeredNodeUnit,
        activeNodeUnit,
        transferredOperatorAssets,
        updatedRegisteredAnchorDatum,
        layout,
        onLayout: (layout) => {
          activateLayout = layout;
        },
      });
    };

    yield* Effect.tryPromise({
      try: () =>
        mkActivateTx().complete(
          activationCompleteOptions({ localUPLCEval: true }),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build activation transaction with final redeemer context: ${String(cause)}`,
          cause,
        }),
    });
    if (activateLayout === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "BuildTxWithRedeemer did not resolve operator activate layout",
          cause: operatorKeyHash,
        }),
      );
    }
    const resolvedActivateLayout = activateLayout;
    yield* Effect.logInfo(
      `Using activate redeemer layout: ${activateLayoutToLogString(resolvedActivateLayout)}`,
    );
    const activationUnsignedTx = yield* Effect.tryPromise({
      try: () =>
        mkActivateTx(resolvedActivateLayout).complete(
          activationCompleteOptions({ localUPLCEval: true }),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to rebuild activation transaction with resolved redeemer context: ${String(cause)}`,
          cause,
        }),
    });
    const activateSubmitResult = yield* Effect.either(
      handleSignSubmit(lucid, activationUnsignedTx),
    );
    if (activateSubmitResult._tag === "Left") {
      const onChainFailureSummary = summarizeOnChainScriptFailure(
        activateSubmitResult.left.cause,
      );
      if (onChainFailureSummary !== null) {
        yield* Effect.logWarning(
          `Activation submission on-chain failure summary: ${onChainFailureSummary}`,
        );
      }
      return yield* Effect.fail(activateSubmitResult.left);
    }
    activateTxHash = activateSubmitResult.right;

    return yield* toLifecycleResult({
      registerTxHash,
      activateTxHash,
      deregisterTxHash,
    });
  });

export const registerAndActivateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  ActivationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "register-and-activate",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.andThen(({ registerTxHash, activateTxHash }) =>
      toActivationResult({
        registerTxHash,
        activateTxHash,
      }),
    ),
  );

export const registerOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  RegistrationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "register-only",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.map(({ registerTxHash }) => ({
      registerTxHash,
    })),
  );

export const activateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  ActivationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "activate-only",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.map(({ registerTxHash, activateTxHash }) => ({
      registerTxHash,
      activateTxHash,
    })),
  );

export const deregisterOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  DeregistrationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "deregister-only",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.map(({ deregisterTxHash }) => ({
      deregisterTxHash,
    })),
  );

const configuredReleaseRequiredBondProgram = Effect.gen(function* () {
  const verification = yield* verifyConfiguredDeploymentManifestProgram;
  if (!verification.ok) {
    return yield* Effect.fail(
      new Error(
        `Operator lifecycle refused deployment manifest drift: ${verification.mismatches.join("; ")}`,
      ),
    );
  }
  const identity = yield* Effect.try({
    try: () =>
      readFinalizedDeploymentIdentity(configuredContractDeploymentInfoPath()),
    catch: (cause) =>
      new Error(
        `Failed to load finalized deployment economics: ${String(cause)}`,
      ),
  });
  return BigInt(identity.manifest.economics.requiredBondLovelace);
});

export const program = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const requiredBondLovelace = yield* configuredReleaseRequiredBondProgram;
  yield* lucidService.switchToOperatorsMainWallet;
  return yield* registerAndActivateOperatorProgram(
    lucidService.api,
    contracts,
    requiredBondLovelace,
    lucidService.referenceScriptsApi,
    lucidService.referenceScriptsAddress,
  );
});

export const activateProgram = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const requiredBondLovelace = yield* configuredReleaseRequiredBondProgram;
  yield* lucidService.switchToOperatorsMainWallet;
  return yield* activateOperatorProgram(
    lucidService.api,
    contracts,
    requiredBondLovelace,
    lucidService.referenceScriptsApi,
    lucidService.referenceScriptsAddress,
  );
});
