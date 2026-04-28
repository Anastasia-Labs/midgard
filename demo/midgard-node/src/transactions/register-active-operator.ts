/**
 * Operator lifecycle transaction orchestration for register/activate flows.
 * This module is the main off-chain entrypoint for operator-set updates and
 * composes the shared layout and clock helpers extracted from the monolith.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr as LucidConstr,
  Data as LucidData,
  type Data as LucidDatum,
  LucidEvolution,
  type RedeemerBuilder,
  UTxO,
  credentialToAddress,
  paymentCredentialOf,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Lucid, MidgardContracts, NodeConfig } from "@/services/index.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
  handleSignSubmit,
} from "@/transactions/utils.js";
import { compareOutRefs } from "@/tx-context.js";
import {
  alignUnixTimeToSlotBoundary,
  alignedUnixTimeStrictlyAfter,
} from "@/workers/utils/commit-end-time.js";
import {
  alignUnixTimeMsToSlotBoundary,
  currentTimeMsForLucidOrEmulatorFallback,
  resolveCurrentTimeMs,
} from "@/transactions/register-active-operator/clock.js";
import {
  type ActivateRedeemerLayout,
  activeAppendAnchorWitness,
  type NodeWithDatum,
  type ReferenceScriptPublication,
  type RegisterRedeemerLayout,
  type RetireRedeemerLayout,
  activateLayoutToLogString,
  activateLayoutsEqual,
  deriveActivateRedeemerLayout,
  deriveRegisterRedeemerLayout,
  deriveRetireRedeemerLayout,
  getAssetNameByPolicy,
  nodeKeyEquals,
  orderedNotMemberWitness,
  registerLayoutToLogString,
  registerLayoutsEqual,
  resolveInitialActivateRedeemerLayout,
  resolveInitialRegisterRedeemerLayout,
  resolveInitialRetireRedeemerLayout,
  retireLayoutToLogString,
  retireLayoutsEqual,
} from "@/transactions/register-active-operator/lifecycle-layout.js";
import {
  withStubbedProviderEvaluation as withSharedStubbedProviderEvaluation,
} from "@/cml-redeemers.js";
import {
  referenceScriptTargetsByCommand,
  resolveReferenceScriptTargetsProgram,
  resolveSpendableWalletUtxos,
  selectWalletFundingUtxos,
  utxoOutRefKey,
} from "@/transactions/reference-scripts.js";
export {
  deployReferenceScriptCommandProgram,
  REFERENCE_SCRIPT_COMMAND_NAMES,
} from "@/transactions/reference-scripts.js";
export type { ReferenceScriptCommandName } from "@/transactions/reference-scripts.js";

const REGISTERED_ACTIVATION_DELAY_MS = 30n;
const ACTIVATION_VALIDITY_WINDOW_MS = 120_000n;
const DRAFT_REDEEMER_EX_UNITS = {
  mem: 1_000_000,
  steps: 1_000_000,
} as const;
const REGISTERED_SET_REFRESH_MAX_RETRIES = 12;
const REGISTERED_SET_REFRESH_RETRY_DELAY = "2 seconds";
const NODE_SET_FETCH_MAX_RETRIES = 8;
const NODE_SET_FETCH_RETRY_DELAY = "2 seconds";
const HUB_ORACLE_FETCH_MAX_RETRIES = 6;
const HUB_ORACLE_FETCH_RETRY_DELAY = "1 second";
const SUBMISSION_EX_UNITS_HEADROOM_NUMERATOR = 8n;
const SUBMISSION_EX_UNITS_HEADROOM_DENOMINATOR = 10n;
const ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE = 25_000_000n;
const ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER = LucidData.to(
  "ListStateTransition",
  SDK.ActiveOperatorSpendRedeemer,
);
const REGISTERED_OPERATOR_DATUM_AIKEN_SCHEMA = LucidData.Object({
  operator: LucidData.Bytes({ minLength: 28, maxLength: 28 }),
  bond_unlock_time: LucidData.Nullable(LucidData.Integer()),
});
const ACTIVATION_SELECTED_REGISTERED_INPUTS_COUNT = 2;

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
  readonly retireTxHash: string | null;
  readonly reregisterTxHash: string | null;
};

type RegisterActivateRetireReregisterTxHashes = {
  readonly registerTxHash: string | null;
  readonly activateTxHash: string | null;
  readonly retireTxHash: string | null;
  readonly reregisterTxHash: string | null;
};

type OperatorLifecycleMode =
  | "register-and-activate"
  | "register-only"
  | "activate-only"
  | "deregister-only"
  | "register-activate-retire-reregister";

type StubEvaluationMode = "draft" | "submission";

type RegisterOperatorOrigin =
  | {
      readonly tag: "NewOperator";
      readonly retiredNotMemberWitness: NodeWithDatum;
      readonly bondUnlockTime: null;
    }
  | {
      readonly tag: "ReturningOperator";
      readonly retiredOperatorNode: NodeWithDatum;
      readonly retiredOperatorAnchor: NodeWithDatum;
      readonly retiredNodeUnit: string;
      readonly retiredAnchorNodeUnit: string;
      readonly bondUnlockTime: bigint | null;
    };

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

const encodeActiveOperatorDatumValue = (
  bondUnlockTime: bigint | null,
): LucidDatum =>
  SDK.castActiveOperatorDatumToData({
    bond_unlock_time: bondUnlockTime,
    inactivity_strikes: 0n,
  }) as LucidDatum;

const encodeRetiredOperatorDatumValue = (
  bondUnlockTime: bigint | null,
): LucidDatum =>
  SDK.castRetiredOperatorDatumToData({
    bond_unlock_time: bondUnlockTime,
  }) as LucidDatum;

const encodeLinkedListNodeView = (nodeView: SDK.LinkedListNodeView): string =>
  SDK.encodeLinkedListNodeView(nodeView);

const decodeOptionalBigIntValue = (
  value: unknown,
): bigint | null | undefined => {
  if (value === null || value === "None") {
    return null;
  }
  if (typeof value === "bigint") {
    return value;
  }
  if (typeof value === "number" && Number.isInteger(value)) {
    return BigInt(value);
  }
  if (
    typeof value === "object" &&
    value !== null &&
    "Some" in value &&
    Array.isArray(value.Some) &&
    value.Some.length === 1
  ) {
    const someValue = value.Some[0];
    if (typeof someValue === "bigint") {
      return someValue;
    }
    if (typeof someValue === "number" && Number.isInteger(someValue)) {
      return BigInt(someValue);
    }
  }
  return undefined;
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

const describePolicyAssets = (assets: Record<string, bigint>): string => {
  const policyAssets = Object.entries(assets).filter(
    ([unit, quantity]) => unit !== "lovelace" && quantity > 0n,
  );
  if (policyAssets.length === 0) {
    return "<none>";
  }
  return policyAssets
    .map(
      ([unit, quantity]) =>
        `${unit.slice(0, 56)}.${unit.slice(56)}=${quantity.toString()}`,
    )
    .join("|");
};

const policyIdsFromAssets = (
  assets: Record<string, bigint>,
): readonly string[] =>
  Object.entries(assets)
    .filter(([unit, quantity]) => unit !== "lovelace" && quantity > 0n)
    .map(([unit]) => unit.slice(0, 56));

const describeReferenceInputOrder = (
  referenceInputs: readonly UTxO[],
  labelsByOutRef: ReadonlyMap<string, string>,
): string =>
  referenceInputs
    .map((utxo, index) => {
      const outRef = utxoOutRefKey(utxo);
      return [
        `${index.toString()}:${labelsByOutRef.get(outRef) ?? "unknown"}`,
        outRef,
        `assets=${describePolicyAssets(utxo.assets)}`,
        `datum=${utxo.datum ?? "none"}`,
      ].join(":");
    })
    .join(",");

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

const advanceCustomEmulatorPastUnixTime = (
  lucid: LucidEvolution,
  targetUnixTimeMs: bigint,
): boolean => {
  if (lucid.config().network !== "Custom") {
    return false;
  }
  const provider = lucid.config().provider as {
    readonly time?: number;
    readonly awaitSlot?: (slot: number) => void;
  };
  if (
    typeof provider.time !== "number" ||
    typeof provider.awaitSlot !== "function"
  ) {
    return false;
  }
  const currentTimeMs = BigInt(provider.time);
  if (currentTimeMs > targetUnixTimeMs) {
    return true;
  }
  const remainingMs = targetUnixTimeMs - currentTimeMs;
  const slotDelta = Number((remainingMs + 999n) / 1_000n) + 1;
  provider.awaitSlot(lucid.currentSlot() + slotDelta);
  return true;
};

const encodeRegisteredOperatorDatumValue = (
  operatorKeyHash: string,
  bondUnlockTime: bigint | null,
): LucidDatum =>
  SDK.castRegisteredOperatorDatumToData({
    operator: operatorKeyHash,
    bond_unlock_time: bondUnlockTime,
  }) as LucidDatum;

const decodeRegisteredOperatorDatumValue = (
  value: unknown,
): SDK.RegisteredOperatorDatum | undefined => {
  if (typeof value === "object" && value !== null) {
    if ("operator" in value && typeof value.operator === "string") {
      return {
        operator: value.operator,
        bond_unlock_time:
          "bond_unlock_time" in value
            ? (decodeOptionalBigIntValue(value.bond_unlock_time) ?? null)
            : null,
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

const decodeActiveOperatorDatumValue = (
  value: unknown,
): SDK.ActiveOperatorDatum | undefined => {
  if (
    typeof value === "object" &&
    value !== null &&
    ("bond_unlock_time" in value || "inactivity_strikes" in value)
  ) {
    const bondUnlockTime =
      "bond_unlock_time" in value
        ? decodeOptionalBigIntValue(value.bond_unlock_time)
        : null;
    const inactivityStrikes =
      "inactivity_strikes" in value &&
      typeof value.inactivity_strikes === "bigint"
        ? value.inactivity_strikes
        : "inactivity_strikes" in value &&
            typeof value.inactivity_strikes === "number" &&
            Number.isInteger(value.inactivity_strikes)
          ? BigInt(value.inactivity_strikes)
          : undefined;
    if (bondUnlockTime === undefined || inactivityStrikes === undefined) {
      return undefined;
    }
    return {
      bond_unlock_time: bondUnlockTime,
      inactivity_strikes: inactivityStrikes,
    };
  }
  try {
    const normalizedValue = normalizeLucidDataValue(value);
    return LucidData.castFrom(
      normalizedValue as never,
      SDK.ActiveOperatorDatum as never,
    ) as SDK.ActiveOperatorDatum;
  } catch {
    return undefined;
  }
};

const decodeRetiredOperatorDatumValue = (
  value: unknown,
): SDK.RetiredOperatorDatum | undefined => {
  if (typeof value === "object" && value !== null) {
    const bondUnlockTime =
      "bond_unlock_time" in value
        ? decodeOptionalBigIntValue(value.bond_unlock_time)
        : null;
    if (bondUnlockTime === undefined) {
      return undefined;
    }
    return {
      bond_unlock_time: bondUnlockTime,
    };
  }
  try {
    const normalizedValue = normalizeLucidDataValue(value);
    return LucidData.castFrom(
      normalizedValue as never,
      SDK.RetiredOperatorDatum as never,
    ) as SDK.RetiredOperatorDatum;
  } catch {
    return undefined;
  }
};

const linkPointsToKey = (
  node: SDK.LinkedListNodeView,
  key: SDK.NodeKey,
): boolean =>
  key !== "Empty" &&
  node.next !== "Empty" &&
  node.next.Key.key === key.Key.key;

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

const decodeRegisteredOperatorActivationTime = (
  node: SDK.LinkedListNodeView,
): bigint | undefined => nodeKeyToPosixTime(node.key);

const mkRegisteredActivateRedeemer = ({
  operatorKeyHash,
  layout,
  retiredOperatorAssetName,
}: {
  readonly operatorKeyHash: string;
  readonly layout: ActivateRedeemerLayout;
  readonly retiredOperatorAssetName: string;
}): string => {
  return LucidData.to(
    {
      ActivateOperator: {
        activating_operator: operatorKeyHash,
        anchor_element_input_index:
          layout.registeredOperatorsAnchorNodeInputIndex,
        removed_node_input_index:
          layout.registeredOperatorsRemovedNodeInputIndex,
        anchor_element_output_index:
          layout.registeredOperatorsAnchorNodeOutputIndex,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        retired_operators_element_ref_input_index:
          layout.retiredOperatorRefInputIndex,
        active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
      },
    },
    SDK.RegisteredOperatorMintRedeemer,
  );
};

const mkRegisterOperatorOriginRedeemer = (
  operatorOrigin: RegisterRedeemerLayout["operatorOrigin"],
) =>
  operatorOrigin.tag === "NewOperator"
    ? {
        NewOperator: {
          retired_operators_element_ref_input_index:
            operatorOrigin.retiredOperatorRefInputIndex,
        },
      }
    : {
        ReturningOperator: {
          retired_operators_redeemer_index:
            operatorOrigin.retiredOperatorsRedeemerIndex,
        },
      };

const mkRetiredReregisterRedeemer = ({
  operatorKeyHash,
  bondUnlockTime,
  layout,
}: {
  readonly operatorKeyHash: string;
  readonly bondUnlockTime: bigint | null;
  readonly layout: RegisterRedeemerLayout;
}): string => {
  if (layout.operatorOrigin.tag !== "ReturningOperator") {
    throw new Error("Returning-operator register layout is required");
  }
  return LucidData.to(
    {
      ReregisterOperator: {
        retired_operator_key: operatorKeyHash,
        retired_operator_bond_unlock_time: bondUnlockTime,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        retired_operator_anchor_element_input_index:
          layout.operatorOrigin.retiredOperatorAnchorNodeInputIndex,
        retired_operator_removed_node_input_index:
          layout.operatorOrigin.retiredOperatorRemovedNodeInputIndex,
        retired_operator_anchor_element_output_index:
          layout.operatorOrigin.retiredOperatorAnchorNodeOutputIndex,
        registered_operators_redeemer_index:
          layout.operatorOrigin.registeredOperatorsRedeemerIndex,
      },
    },
    SDK.RetiredOperatorMintRedeemer,
  );
};

const mkActiveRetireRedeemer = ({
  operatorKeyHash,
  layout,
}: {
  readonly operatorKeyHash: string;
  readonly layout: RetireRedeemerLayout;
}): string =>
  LucidData.to(
    {
      RetireOperator: {
        active_operator_key: operatorKeyHash,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        active_operator_anchor_element_input_index:
          layout.activeOperatorsAnchorNodeInputIndex,
        active_operator_removed_node_input_index:
          layout.activeOperatorsRemovedNodeInputIndex,
        active_operator_anchor_element_output_index:
          layout.activeOperatorsAnchorNodeOutputIndex,
        retired_operators_redeemer_index:
          layout.retiredOperatorsRedeemerIndex,
        penalize_for_inactivity: false,
        operator_removal_scheduler_sync: {
          ShowOperatorIsInactive: {
            scheduler_ref_input_index: layout.schedulerRefInputIndex,
          },
        },
      },
    },
    SDK.ActiveOperatorMintRedeemer,
  );

const mkRetiredRetireRedeemer = ({
  operatorKeyHash,
  bondUnlockTime,
  layout,
}: {
  readonly operatorKeyHash: string;
  readonly bondUnlockTime: bigint | null;
  readonly layout: RetireRedeemerLayout;
}): string =>
  LucidData.to(
    {
      RetireOperator: {
        new_retired_operator_key: operatorKeyHash,
        bond_unlock_time: bondUnlockTime,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        retired_operator_anchor_element_input_index:
          layout.retiredOperatorsAnchorNodeInputIndex,
        retired_operator_anchor_element_output_index:
          layout.retiredOperatorsAnchorNodeOutputIndex,
        retired_operator_inserted_node_output_index:
          layout.retiredOperatorsInsertedNodeOutputIndex,
        active_operators_redeemer_index:
          layout.activeOperatorsRedeemerIndex,
      },
    },
    SDK.RetiredOperatorMintRedeemer,
  );

const mkRegisteredActivateRedeemerBuilder = ({
  operatorKeyHash,
  layout,
  retiredOperatorAssetName,
  registeredNode,
  registeredAnchor,
}: {
  readonly operatorKeyHash: string;
  readonly layout: ActivateRedeemerLayout;
  readonly retiredOperatorAssetName: string;
  readonly registeredNode: UTxO;
  readonly registeredAnchor: UTxO;
}): RedeemerBuilder => ({
  kind: "selected",
  inputs: [registeredNode, registeredAnchor],
  makeRedeemer: (inputIndices) => {
    if (
      inputIndices.length !== ACTIVATION_SELECTED_REGISTERED_INPUTS_COUNT
    ) {
      throw new Error(
        `Activation redeemer builder expected ${ACTIVATION_SELECTED_REGISTERED_INPUTS_COUNT.toString()} registered inputs, got ${inputIndices.length.toString()}`,
      );
    }
    const [removedNodeInputIndex, anchorNodeInputIndex] = inputIndices;
    if (
      removedNodeInputIndex === undefined ||
      anchorNodeInputIndex === undefined
    ) {
      throw new Error("Activation redeemer builder input index missing");
    }
    return mkRegisteredActivateRedeemer({
      operatorKeyHash,
      layout: {
        ...layout,
        registeredOperatorsRemovedNodeInputIndex: removedNodeInputIndex,
        registeredOperatorsAnchorNodeInputIndex: anchorNodeInputIndex,
      },
      retiredOperatorAssetName,
    });
  },
});

const completeWithLocalEvaluation = async <A>(
  build: (options: { localUPLCEval: boolean }) => Promise<A>,
): Promise<A> => {
  return await build({ localUPLCEval: true });
};

const withRegisteredOperatorStubbedProviderEvaluation = async <A>(
  lucid: LucidEvolution,
  run: () => Promise<A>,
  mode: StubEvaluationMode = "draft",
): Promise<A> => {
  const provider = lucid.config().provider as {
    getProtocolParameters?: () => Promise<{
      maxTxExMem: bigint;
      maxTxExSteps: bigint;
    }>;
  };
  const resolveStubExUnits = async (
    redeemerPointers: readonly {
      readonly tag: number;
      readonly index: bigint;
    }[],
  ): Promise<{ mem: number; steps: number }> => {
    const redeemerCount = redeemerPointers.length;
    if (mode === "draft") {
      return DRAFT_REDEEMER_EX_UNITS;
    }
    if (typeof provider.getProtocolParameters !== "function") {
      return DRAFT_REDEEMER_EX_UNITS;
    }

    const protocolParameters = await provider.getProtocolParameters();
    const safeRedeemerCount = BigInt(Math.max(redeemerCount, 1));
    const memShare =
      protocolParameters.maxTxExMem / safeRedeemerCount;
    const stepsShare =
      protocolParameters.maxTxExSteps / safeRedeemerCount;
    const mem = Number(
      (memShare * SUBMISSION_EX_UNITS_HEADROOM_NUMERATOR) /
        SUBMISSION_EX_UNITS_HEADROOM_DENOMINATOR,
    );
    const steps = Number(
      (stepsShare * SUBMISSION_EX_UNITS_HEADROOM_NUMERATOR) /
        SUBMISSION_EX_UNITS_HEADROOM_DENOMINATOR,
    );
    if (!Number.isFinite(mem) || !Number.isFinite(steps) || mem <= 0 || steps <= 0) {
      return DRAFT_REDEEMER_EX_UNITS;
    }
    return { mem, steps };
  };
  return withSharedStubbedProviderEvaluation(
    lucid,
    run,
    resolveStubExUnits,
  );
};

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
    for (let attempt = 0; attempt < HUB_ORACLE_FETCH_MAX_RETRIES; attempt += 1) {
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
          Effect.map((beaconUtxos) =>
            beaconUtxos.map(({ utxo }) => utxo),
          ),
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
      if (hubOracleRefInput.datum === undefined || hubOracleRefInput.datum === null) {
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
      `Operator lifecycle result: registerTxHash=${txHashes.registerTxHash ?? "skipped"}, activateTxHash=${txHashes.activateTxHash ?? "skipped"}, deregisterTxHash=${txHashes.deregisterTxHash ?? "skipped"}, retireTxHash=${txHashes.retireTxHash ?? "skipped"}, reregisterTxHash=${txHashes.reregisterTxHash ?? "skipped"}`,
    );
    return txHashes;
  });

const retireActiveOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
  referenceScriptsLucid: LucidEvolution = lucid,
): Effect.Effect<
  string,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError
> =>
  Effect.gen(function* () {
    const retireReferenceTargets = [
      ...referenceScriptTargetsByCommand(contracts)["active-operators"],
      ...referenceScriptTargetsByCommand(contracts)["retired-operators"],
    ] as const;
    const retireReferenceScriptRefs =
      yield* resolveReferenceScriptTargetsProgram(
        referenceScriptsLucid,
        "operator retire",
        retireReferenceTargets,
      );
    const activeOperatorScriptRefs = retireReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("active-operators "),
    );
    const retiredOperatorScriptRefs = retireReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("retired-operators "),
    );
    const lifecycleScriptRefOutRefs = new Set(
      retireReferenceScriptRefs.map(({ utxo }) => utxoOutRefKey(utxo)),
    );

    const hubOracleRefInput = yield* fetchHubOracleRefInput(lucid, contracts);
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
    const activeOperatorNodes = activeNodes.filter(({ datum }) =>
      nodeKeyEquals(datum, operatorKeyHash),
    );
    if (activeOperatorNodes.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Expected exactly one active-operator node for retirement",
          cause: `operator=${operatorKeyHash},count=${activeOperatorNodes.length.toString()}`,
        }),
      );
    }
    const activeOperatorNode = activeOperatorNodes[0];
    const activeOperatorAnchor = activeNodes.find(({ datum }) =>
      linkPointsToKey(datum, activeOperatorNode.datum.key),
    );
    if (activeOperatorAnchor === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Active-operators anchor node for retirement was not found",
          cause: operatorKeyHash,
        }),
      );
    }
    const existingRetiredOperatorNodes = retiredNodes.filter(({ datum }) =>
      nodeKeyEquals(datum, operatorKeyHash),
    );
    if (existingRetiredOperatorNodes.length > 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Cannot retire active operator because the retired set already contains the operator key",
          cause: operatorKeyHash,
        }),
      );
    }
    const retiredAppendAnchor = retiredNodes.find(({ datum }) =>
      orderedNotMemberWitness(datum, operatorKeyHash),
    );
    if (retiredAppendAnchor === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to find retired-operators insertion anchor for retirement",
          cause: operatorKeyHash,
        }),
      );
    }
    const activeOperatorDatum = decodeActiveOperatorDatumValue(
      activeOperatorNode.datum.data,
    );
    if (activeOperatorDatum === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to decode active-operator datum for retirement",
          cause: describeUnknownValue(activeOperatorNode.datum.data),
        }),
      );
    }
    const schedulerRef = yield* SDK.fetchSchedulerUTxOProgram(lucid, {
      schedulerAddress: contracts.scheduler.spendingScriptAddress,
      schedulerPolicyId: contracts.scheduler.policyId,
    }).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch scheduler UTxO for operator retirement",
            cause,
          }),
      ),
    );
    const schedulerRefInput = schedulerRef.utxo;
    let hubOracleSchedulerPolicyId: string | null = null;
    let hubOracleDatumStatus = "missing";
    if (hubOracleRefInput.datum !== undefined) {
      try {
        const hubOracleDatum = LucidData.from(
          hubOracleRefInput.datum,
          SDK.HubOracleDatum,
        ) as SDK.HubOracleDatum;
        hubOracleSchedulerPolicyId = hubOracleDatum.scheduler;
        hubOracleDatumStatus = "decoded";
      } catch (cause) {
        hubOracleDatumStatus = `decode_error=${String(cause)}`;
      }
    }
    const schedulerRefPolicyIds = policyIdsFromAssets(schedulerRefInput.assets);
    const schedulerPolicyDiagnostics = [
      "Retirement scheduler policy check:",
      `hub_scheduler_policy=${hubOracleSchedulerPolicyId ?? "<unavailable>"}`,
      `contracts_scheduler_policy=${contracts.scheduler.policyId}`,
      `scheduler_ref_assets=${describePolicyAssets(schedulerRefInput.assets)}`,
      `hub_matches_contracts=${(
        hubOracleSchedulerPolicyId === contracts.scheduler.policyId
      ).toString()}`,
      `scheduler_ref_has_hub_policy=${(
        hubOracleSchedulerPolicyId !== null &&
        schedulerRefPolicyIds.includes(hubOracleSchedulerPolicyId)
      ).toString()}`,
      `hub_datum_status=${hubOracleDatumStatus}`,
      `hub_ref=${hubOracleRefInput.txHash}#${hubOracleRefInput.outputIndex.toString()}`,
      `scheduler_ref=${schedulerRefInput.txHash}#${schedulerRefInput.outputIndex.toString()}`,
    ].join(" ");
    yield* Effect.logWarning(schedulerPolicyDiagnostics);

    const activeNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      activeOperatorNode.assetName,
    );
    const activeAnchorNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      activeOperatorAnchor.assetName,
    );
    const retiredNodeUnit = toUnit(
      contracts.retiredOperators.policyId,
      SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );
    const retiredAnchorNodeUnit = toUnit(
      contracts.retiredOperators.policyId,
      retiredAppendAnchor.assetName,
    );
    const retiredNodeDatum: SDK.LinkedListNodeView = {
      key: { Key: { key: operatorKeyHash } },
      next: retiredAppendAnchor.datum.next,
      data: encodeRetiredOperatorDatumValue(
        activeOperatorDatum.bond_unlock_time,
      ),
    };
    const updatedRetiredAnchorDatum: SDK.LinkedListNodeView = {
      ...retiredAppendAnchor.datum,
      next: { Key: { key: operatorKeyHash } },
    };
    const updatedActiveAnchorDatum: SDK.LinkedListNodeView = {
      ...activeOperatorAnchor.datum,
      next: activeOperatorNode.datum.next,
    };
    const retiredNodeAssets: Record<string, bigint> = {
      ...activeOperatorNode.utxo.assets,
      [retiredNodeUnit]: 1n,
    };
    delete retiredNodeAssets[activeNodeUnit];

    const spendableWalletUtxosForRetirement =
      yield* resolveSpendableWalletUtxos(
        lucid,
        lifecycleScriptRefOutRefs,
      );
    if (spendableWalletUtxosForRetirement.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "No wallet funding UTxOs available for retirement transaction",
          cause: operatorKeyHash,
        }),
      );
    }
    const retirementFundingInputs = [
      ...selectWalletFundingUtxos(
        spendableWalletUtxosForRetirement,
        ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
      ),
    ];
    if (retirementFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to select wallet funding UTxOs for retirement transaction",
          cause: operatorKeyHash,
        }),
      );
    }
    const retirementCompleteOptions = (
      options: { readonly localUPLCEval: boolean },
    ) => ({
      ...options,
      presetWalletInputs: [...retirementFundingInputs],
    });
    const retirementReferenceInputs = [
      ...activeOperatorScriptRefs.map(({ utxo }) => utxo),
      ...retiredOperatorScriptRefs.map(({ utxo }) => utxo),
      hubOracleRefInput,
      schedulerRefInput,
    ].sort(compareOutRefs);
    const retirementReferenceInputLabels = new Map<string, string>([
      ...activeOperatorScriptRefs.map(
        ({ name, utxo }) => [utxoOutRefKey(utxo), name] as const,
      ),
      ...retiredOperatorScriptRefs.map(
        ({ name, utxo }) => [utxoOutRefKey(utxo), name] as const,
      ),
      [utxoOutRefKey(hubOracleRefInput), "hub oracle"] as const,
      [utxoOutRefKey(schedulerRefInput), "scheduler"] as const,
    ]);
    const retirementReferenceInputDiagnostics =
      `retire_reference_inputs=${describeReferenceInputOrder(
        retirementReferenceInputs,
        retirementReferenceInputLabels,
      )}`;
    yield* Effect.logWarning(retirementReferenceInputDiagnostics);
    const mkRetireTx = (layout: RetireRedeemerLayout) => {
      const activeRetireRedeemer = mkActiveRetireRedeemer({
        operatorKeyHash,
        layout,
      });
      const retiredRetireRedeemer = mkRetiredRetireRedeemer({
        operatorKeyHash,
        bondUnlockTime: activeOperatorDatum.bond_unlock_time,
        layout,
      });
      return lucid
        .newTx()
        .collectFrom(retirementFundingInputs)
        .collectFrom(
          [activeOperatorNode.utxo, activeOperatorAnchor.utxo],
          ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER,
        )
        .collectFrom([retiredAppendAnchor.utxo], LucidData.void())
        .readFrom(retirementReferenceInputs)
        .mintAssets({ [activeNodeUnit]: -1n }, activeRetireRedeemer)
        .mintAssets({ [retiredNodeUnit]: 1n }, retiredRetireRedeemer)
        .pay.ToContract(
          contracts.retiredOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: encodeLinkedListNodeView(retiredNodeDatum),
          },
          retiredNodeAssets,
        )
        .pay.ToContract(
          contracts.retiredOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: encodeLinkedListNodeView(updatedRetiredAnchorDatum),
          },
          retiredAppendAnchor.utxo.assets,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: encodeLinkedListNodeView(updatedActiveAnchorDatum),
          },
          activeOperatorAnchor.utxo.assets,
        )
        .addSignerKey(operatorKeyHash);
    };

    const retireLayoutParams = {
      hubOracleRefInput,
      schedulerRefInput,
      activeOperatorNode,
      activeOperatorAnchor,
      retiredAppendAnchor,
      activeOperatorsPolicyId: contracts.activeOperators.policyId,
      activeOperatorsAddress: contracts.activeOperators.spendingScriptAddress,
      activeNodeUnit,
      activeAnchorNodeUnit,
      retiredOperatorsPolicyId: contracts.retiredOperators.policyId,
      retiredOperatorsAddress: contracts.retiredOperators.spendingScriptAddress,
      retiredNodeUnit,
      retiredAnchorNodeUnit,
      contracts,
    } as const;
    let retireLayout = resolveInitialRetireRedeemerLayout({
      activeOperatorScriptRefs,
      retiredOperatorScriptRefs,
      hubOracleRefInput,
      schedulerRefInput,
      activeOperatorNode,
      activeOperatorAnchor,
      retiredAppendAnchor,
      contracts,
      fundingInputs: retirementFundingInputs,
    });
    const initialRetireLayoutDiagnostics =
      `computed_retire_layout=${retireLayoutToLogString(retireLayout)}`;
    yield* Effect.logInfo(
      `Initial retire redeemer layout: ${retireLayoutToLogString(retireLayout)}`,
    );
    yield* Effect.logWarning(initialRetireLayoutDiagnostics);
    const retireDraft = yield* Effect.tryPromise({
      try: () =>
        withRegisteredOperatorStubbedProviderEvaluation(lucid, () =>
          mkRetireTx(retireLayout).complete({
            localUPLCEval: true,
            presetWalletInputs: [...retirementFundingInputs],
          }),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build retirement draft transaction: ${String(cause)} layout=${retireLayoutToLogString(retireLayout)} ${retirementReferenceInputDiagnostics} ${initialRetireLayoutDiagnostics} ${schedulerPolicyDiagnostics}`,
          cause,
        }),
    });
    const derivedDraftLayout = yield* deriveRetireRedeemerLayout(
      retireDraft.toTransaction(),
      retireLayoutParams,
    );
    if (!retireLayoutsEqual(retireLayout, derivedDraftLayout)) {
      yield* Effect.logWarning(
        [
          "Retirement draft layout differed from initial indexes; using tx-derived retirement layout.",
          `initial=${retireLayoutToLogString(retireLayout)}`,
          `derived=${retireLayoutToLogString(derivedDraftLayout)}`,
        ].join(" "),
      );
      retireLayout = derivedDraftLayout;
    }
    yield* Effect.logInfo(
      `Resolved retire redeemer layout: ${retireLayoutToLogString(retireLayout)}`,
    );
    let retireUnsignedTx = yield* Effect.tryPromise({
      try: () =>
        completeWithLocalEvaluation((options) =>
          mkRetireTx(retireLayout).complete(retirementCompleteOptions(options)),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build retirement transaction: ${String(cause)}`,
          cause,
        }),
    });
    for (let iteration = 0; iteration < 2; iteration += 1) {
      const derivedSubmitLayout = yield* deriveRetireRedeemerLayout(
        retireUnsignedTx.toTransaction(),
        retireLayoutParams,
      );
      if (retireLayoutsEqual(retireLayout, derivedSubmitLayout)) {
        break;
      }
      if (iteration === 1) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Retirement transaction layout did not converge after deterministic rebuild",
            cause: JSON.stringify({
              authoredLayout: retireLayoutToLogString(retireLayout),
              derivedLayout: retireLayoutToLogString(derivedSubmitLayout),
            }),
          }),
        );
      }
      yield* Effect.logWarning(
        [
          "Retirement layout drift detected after balancing; rebuilding with tx-derived indexes.",
          `authored=${retireLayoutToLogString(retireLayout)}`,
          `derived=${retireLayoutToLogString(derivedSubmitLayout)}`,
        ].join(" "),
      );
      retireLayout = derivedSubmitLayout;
      retireUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          completeWithLocalEvaluation((options) =>
            mkRetireTx(retireLayout).complete(
              retirementCompleteOptions(options),
            ),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: `Failed to rebuild retirement transaction with tx-derived layout: ${String(cause)}`,
            cause,
          }),
      });
    }
    const retireSubmitResult = yield* Effect.either(
      handleSignSubmit(lucid, retireUnsignedTx),
    );
    if (retireSubmitResult._tag === "Left") {
      const onChainFailureSummary = summarizeOnChainScriptFailure(
        retireSubmitResult.left.cause,
      );
      if (onChainFailureSummary !== null) {
        yield* Effect.logWarning(
          `Retirement submission on-chain failure summary: ${onChainFailureSummary}`,
        );
      }
      return yield* Effect.fail(retireSubmitResult.left);
    }

    let refreshedOperatorSets = false;
    for (
      let attempt = 0;
      attempt < REGISTERED_SET_REFRESH_MAX_RETRIES;
      attempt += 1
    ) {
      const refreshedActiveNodes = yield* fetchNodeSet(
        lucid,
        contracts.activeOperators.spendingScriptAddress,
        contracts.activeOperators.policyId,
      );
      const refreshedRetiredNodes = yield* fetchNodeSet(
        lucid,
        contracts.retiredOperators.spendingScriptAddress,
        contracts.retiredOperators.policyId,
      );
      const remainingActiveOperatorNodes = refreshedActiveNodes.filter(
        ({ datum }) => nodeKeyEquals(datum, operatorKeyHash),
      );
      const visibleRetiredOperatorNodes = refreshedRetiredNodes.filter(
        ({ datum }) => nodeKeyEquals(datum, operatorKeyHash),
      );
      if (
        remainingActiveOperatorNodes.length === 0 &&
        visibleRetiredOperatorNodes.length === 1
      ) {
        refreshedOperatorSets = true;
        break;
      }
      if (
        remainingActiveOperatorNodes.length > 1 ||
        visibleRetiredOperatorNodes.length > 1
      ) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Operator set refresh found duplicate operator nodes after retirement",
            cause: `active=${remainingActiveOperatorNodes.length.toString()},retired=${visibleRetiredOperatorNodes.length.toString()},operator=${operatorKeyHash}`,
          }),
        );
      }
      if (attempt + 1 < REGISTERED_SET_REFRESH_MAX_RETRIES) {
        yield* Effect.sleep(REGISTERED_SET_REFRESH_RETRY_DELAY);
      }
    }
    if (!refreshedOperatorSets) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Operator set refresh did not move the operator from active to retired set",
          cause: operatorKeyHash,
        }),
      );
    }
    return retireSubmitResult.right;
  });

const operatorLifecycleProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  mode: OperatorLifecycleMode,
  referenceScriptsLucid: LucidEvolution = lucid,
): Effect.Effect<
  OperatorLifecycleTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError
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
    if (hubOracleDatum.registered_operators !== contracts.registeredOperators.policyId) {
      policyMismatches.push(
        `registered(hub=${hubOracleDatum.registered_operators},contracts=${contracts.registeredOperators.policyId})`,
      );
    }
    if (hubOracleDatum.active_operators !== contracts.activeOperators.policyId) {
      policyMismatches.push(
        `active(hub=${hubOracleDatum.active_operators},contracts=${contracts.activeOperators.policyId})`,
      );
    }
    if (hubOracleDatum.retired_operators !== contracts.retiredOperators.policyId) {
      policyMismatches.push(
        `retired(hub=${hubOracleDatum.retired_operators},contracts=${contracts.retiredOperators.policyId})`,
      );
    }
    if (hubOracleDatum.scheduler !== contracts.scheduler.policyId) {
      policyMismatches.push(
        `scheduler(hub=${hubOracleDatum.scheduler},contracts=${contracts.scheduler.policyId})`,
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
    // scripts that `register-only` established, otherwise later balancing can
    // introduce a different reference-input layout than the draft indexed.
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
          )
        : yield* resolveReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "operator lifecycle",
            operatorReferenceTargets,
          );
    const registeredOperatorScriptRefs = operatorReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("registered-operators "),
    );
    const activeOperatorScriptRefs = operatorReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("active-operators "),
    );
    let retiredOperatorScriptRefs: readonly ReferenceScriptPublication[] = [];
    let lifecycleScriptRefs: readonly ReferenceScriptPublication[] = [
      ...registeredOperatorScriptRefs,
      ...activeOperatorScriptRefs,
    ];
    let lifecycleScriptRefOutRefs = new Set(
      lifecycleScriptRefs.map(({ utxo }) => utxoOutRefKey(utxo)),
    );
    const includeRetiredOperatorScriptRefs = (
      refs: readonly ReferenceScriptPublication[],
    ) => {
      retiredOperatorScriptRefs = refs;
      lifecycleScriptRefs = [
        ...registeredOperatorScriptRefs,
        ...activeOperatorScriptRefs,
        ...retiredOperatorScriptRefs,
      ];
      lifecycleScriptRefOutRefs = new Set(
        lifecycleScriptRefs.map(({ utxo }) => utxoOutRefKey(utxo)),
      );
    };

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
        retireTxHash: null,
        reregisterTxHash: null,
      });
    }

    let currentRegisteredNodes = registeredNodes;
    let currentRetiredNodes = retiredNodes;
    let registerTxHash: string | null = null;
    let activateTxHash: string | null = null;
    let deregisterTxHash: string | null = null;
    let retireTxHash: string | null = null;
    let reregisterTxHash: string | null = null;

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

    if (existingRegisteredNodes.length === 1) {
      if (mode === "register-only") {
        yield* Effect.logInfo(
          `Operator ${operatorKeyHash} is already registered; skipping register-only flow.`,
        );
        return yield* toLifecycleResult({
          registerTxHash: null,
          activateTxHash: null,
          deregisterTxHash: null,
          retireTxHash: null,
          reregisterTxHash: null,
        });
      }

      if (mode !== "activate-only") {
        const existingRegisteredNode = existingRegisteredNodes[0];
        const existingRegisteredAnchor = currentRegisteredNodes.find(({ datum }) =>
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
          mode === "deregister-only"
            ? `Operator ${operatorKeyHash} is registered; executing deregister-only lifecycle step.`
            : `Operator ${operatorKeyHash} is already registered but not active; refreshing the registration node before activation.`,
        );

        const registeredNodeUnit = toUnit(
          contracts.registeredOperators.policyId,
          existingRegisteredNode.assetName,
        );
        const updatedRegisteredAnchorDatumAfterDeregister: SDK.LinkedListNodeView = {
          ...existingRegisteredAnchor.datum,
          next: existingRegisteredNode.datum.next,
        };
        const deregisterLayouts = [
          { removedNodeInputIndex: 0n, anchorNodeInputIndex: 1n },
          { removedNodeInputIndex: 1n, anchorNodeInputIndex: 0n },
        ] as const;
        const spendableWalletUtxosForDeregister =
          yield* resolveSpendableWalletUtxos(
            lucid,
            lifecycleScriptRefOutRefs,
          );
        if (spendableWalletUtxosForDeregister.length === 0) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "No wallet funding UTxOs available for deregistration transaction",
              cause: operatorKeyHash,
            }),
          );
        }
        let deregistered = false;
        let lastDeregisterFailure:
          | SDK.LucidError
          | TxSignError
          | TxSubmitError
          | TxConfirmError
          | null = null;
        for (const deregisterLayout of deregisterLayouts) {
          const deregisterRedeemer = LucidData.to(
            {
              DeregisterOperator: {
                deregistering_operator: operatorKeyHash,
                removed_node_input_index:
                  deregisterLayout.removedNodeInputIndex,
                anchor_element_input_index:
                  deregisterLayout.anchorNodeInputIndex,
                anchor_element_output_index: 0n,
              },
            },
            SDK.RegisteredOperatorMintRedeemer,
          );
          const deregisterUnsignedTxResult = yield* Effect.either(
            Effect.tryPromise({
              try: () =>
                completeWithLocalEvaluation((options) =>
                  lucid
                    .newTx()
                    .collectFrom(
                      [existingRegisteredNode.utxo, existingRegisteredAnchor.utxo],
                      LucidData.void(),
                    )
                    .readFrom(
                      registeredOperatorScriptRefs.map(({ utxo }) => utxo),
                    )
                    .mintAssets({ [registeredNodeUnit]: -1n }, deregisterRedeemer)
                    .pay.ToContract(
                      contracts.registeredOperators.spendingScriptAddress,
                      {
                        kind: "inline",
                        value: encodeLinkedListNodeView(
                          updatedRegisteredAnchorDatumAfterDeregister,
                        ),
                      },
                      existingRegisteredAnchor.utxo.assets,
                    )
                    .addSignerKey(operatorKeyHash)
                    .complete({
                      ...options,
                      presetWalletInputs: [...spendableWalletUtxosForDeregister],
                    }),
                ),
              catch: (cause) =>
                new SDK.LucidError({
                  message:
                    "Failed to build operator deregistration transaction during operator lifecycle flow",
                  cause,
                }),
            }),
          );
          if (deregisterUnsignedTxResult._tag === "Left") {
            lastDeregisterFailure = deregisterUnsignedTxResult.left;
            continue;
          }
          const deregisterSubmitResult = yield* Effect.either(
            handleSignSubmit(lucid, deregisterUnsignedTxResult.right),
          );
          if (deregisterSubmitResult._tag === "Right") {
            deregistered = true;
            deregisterTxHash = deregisterSubmitResult.right;
            break;
          }
          lastDeregisterFailure = deregisterSubmitResult.left;
        }
        if (!deregistered) {
          if (lastDeregisterFailure !== null) {
            return yield* Effect.fail(lastDeregisterFailure);
          }
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Failed to execute operator deregistration transaction in lifecycle flow",
              cause: operatorKeyHash,
            }),
          );
        }
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
            ({ datum }) => registeredNodeMatchesOperator(datum, operatorKeyHash),
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

    const postRefreshRegisteredNodes = currentRegisteredNodes.filter(({ datum }) =>
      registeredNodeMatchesOperator(datum, operatorKeyHash),
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
          retireTxHash: null,
          reregisterTxHash: null,
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

    if (mode === "activate-only") {
      if (postRefreshRegisteredNodes.length === 1) {
        yield* Effect.logInfo(
          `Activate-only flow found pre-existing registration node for operator ${operatorKeyHash}.`,
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

    if (postRefreshRegisteredNodes.length === 0 && mode !== "activate-only") {
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
      const returningRetiredNodes = currentRetiredNodes.filter(({ datum }) =>
        nodeKeyEquals(datum, operatorKeyHash),
      );
      if (returningRetiredNodes.length > 1) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Found multiple retired-operator nodes for the current operator key",
            cause: operatorKeyHash,
          }),
        );
      }

      let registerOperatorOrigin: RegisterOperatorOrigin;
      if (returningRetiredNodes.length === 1) {
        const retiredOperatorNode = returningRetiredNodes[0];
        const retiredOperatorAnchor = currentRetiredNodes.find(({ datum }) =>
          linkPointsToKey(datum, retiredOperatorNode.datum.key),
        );
        if (retiredOperatorAnchor === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Retired-operators anchor node for returning registration was not found",
              cause: operatorKeyHash,
            }),
          );
        }
        const retiredOperatorDatum = decodeRetiredOperatorDatumValue(
          retiredOperatorNode.datum.data,
        );
        if (retiredOperatorDatum === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Failed to decode retired-operator datum for returning registration",
              cause: describeUnknownValue(retiredOperatorNode.datum.data),
            }),
          );
        }
        const retiredOperatorLovelace =
          retiredOperatorNode.utxo.assets.lovelace ?? 0n;
        if (retiredOperatorLovelace < requiredBondLovelace) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Retired operator node does not carry enough lovelace to satisfy registered-operator bond",
              cause: `operator=${operatorKeyHash},lovelace=${retiredOperatorLovelace.toString()},required=${requiredBondLovelace.toString()}`,
            }),
          );
        }
        includeRetiredOperatorScriptRefs(
          yield* resolveReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "retired-operators",
            referenceScriptTargetsByCommand(contracts)["retired-operators"],
          ),
        );
        registerOperatorOrigin = {
          tag: "ReturningOperator",
          retiredOperatorNode,
          retiredOperatorAnchor,
          retiredNodeUnit: toUnit(
            contracts.retiredOperators.policyId,
            retiredOperatorNode.assetName,
          ),
          retiredAnchorNodeUnit: toUnit(
            contracts.retiredOperators.policyId,
            retiredOperatorAnchor.assetName,
          ),
          bondUnlockTime: retiredOperatorDatum.bond_unlock_time,
        };
      } else {
        const retiredNotMemberWitness = currentRetiredNodes.find(({ datum }) =>
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
        registerOperatorOrigin = {
          tag: "NewOperator",
          retiredNotMemberWitness,
          bondUnlockTime: null,
        };
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
        data: encodeRegisteredOperatorDatumValue(
          operatorKeyHash,
          registerOperatorOrigin.bondUnlockTime,
        ),
      };
      const updatedRegisteredRootDatum: SDK.LinkedListNodeView = {
        ...registeredRootNode.datum,
        next: { Key: { key: registrationNodeKey } },
      };

      const registeredNodeUnit = toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + registrationNodeKey,
      );
      const registeredRootNodeUnit = toUnit(
        contracts.registeredOperators.policyId,
        registeredRootNode.assetName,
      );
      const spendableWalletUtxosForRegister =
        yield* resolveSpendableWalletUtxos(
          lucid,
          lifecycleScriptRefOutRefs,
        );
      if (spendableWalletUtxosForRegister.length === 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
              message:
                "No wallet funding UTxOs available for registration transaction",
              cause: operatorKeyHash,
            }),
          );
      }
      const registerFundingInputs = [
        ...selectWalletFundingUtxos(
          spendableWalletUtxosForRegister,
          (registerOperatorOrigin.tag === "NewOperator"
            ? requiredBondLovelace
            : 0n) + ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
        ),
      ];
      if (registerFundingInputs.length === 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to select wallet funding UTxOs for registration transaction",
            cause: operatorKeyHash,
          }),
        );
      }
      const prependedNodeAssets: Record<string, bigint> =
        registerOperatorOrigin.tag === "ReturningOperator"
          ? {
              ...registerOperatorOrigin.retiredOperatorNode.utxo.assets,
              [registeredNodeUnit]: 1n,
            }
          : {
              lovelace: requiredBondLovelace,
              [registeredNodeUnit]: 1n,
            };
      if (registerOperatorOrigin.tag === "ReturningOperator") {
        delete prependedNodeAssets[registerOperatorOrigin.retiredNodeUnit];
      }
      const updatedRetiredAnchorDatum: SDK.LinkedListNodeView | null =
        registerOperatorOrigin.tag === "ReturningOperator"
          ? {
              ...registerOperatorOrigin.retiredOperatorAnchor.datum,
              next: registerOperatorOrigin.retiredOperatorNode.datum.next,
            }
          : null;
      /**
       * Builds the registration transaction for an active operator.
       */
      const mkRegisterTx = (layout: RegisterRedeemerLayout) => {
        if (layout.operatorOrigin.tag !== registerOperatorOrigin.tag) {
          throw new Error(
            `Register layout origin ${layout.operatorOrigin.tag} does not match selected origin ${registerOperatorOrigin.tag}`,
          );
        }
        const registerRedeemer = LucidData.to(
          {
            RegisterOperator: {
              registering_operator: operatorKeyHash,
              root_input_index: layout.rootInputIndex,
              root_output_index: layout.anchorNodeOutputIndex,
              registered_node_output_index: layout.prependedNodeOutputIndex,
              hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
              active_operators_element_ref_input_index:
                layout.activeOperatorRefInputIndex,
              operator_origin: mkRegisterOperatorOriginRedeemer(
                layout.operatorOrigin,
              ),
            },
          },
          SDK.RegisteredOperatorMintRedeemer,
        );
        let tx = lucid
          .newTx()
          .collectFrom([registeredRootNode.utxo], LucidData.void());
        if (registerOperatorOrigin.tag === "ReturningOperator") {
          tx = tx.collectFrom(
            [
              registerOperatorOrigin.retiredOperatorNode.utxo,
              registerOperatorOrigin.retiredOperatorAnchor.utxo,
            ],
            LucidData.void(),
          );
        }
        tx = tx
          .readFrom([
            ...registeredOperatorScriptRefs.map(({ utxo }) => utxo),
            ...(registerOperatorOrigin.tag === "ReturningOperator"
              ? retiredOperatorScriptRefs.map(({ utxo }) => utxo)
              : []),
            hubOracleRefInput,
            activeNotMemberWitness.utxo,
            ...(registerOperatorOrigin.tag === "NewOperator"
              ? [registerOperatorOrigin.retiredNotMemberWitness.utxo]
              : []),
          ])
          .mintAssets({ [registeredNodeUnit]: 1n }, registerRedeemer);
        if (registerOperatorOrigin.tag === "ReturningOperator") {
          tx = tx.mintAssets(
            { [registerOperatorOrigin.retiredNodeUnit]: -1n },
            mkRetiredReregisterRedeemer({
              operatorKeyHash,
              bondUnlockTime: registerOperatorOrigin.bondUnlockTime,
              layout,
            }),
          );
        }
        tx = tx.pay
          .ToContract(
            contracts.registeredOperators.spendingScriptAddress,
            {
              kind: "inline",
              value: encodeLinkedListNodeView(prependedNodeDatum),
            },
            prependedNodeAssets,
          )
          .pay.ToContract(
            contracts.registeredOperators.spendingScriptAddress,
            {
              kind: "inline",
              value: encodeLinkedListNodeView(updatedRegisteredRootDatum),
            },
            registeredRootNode.utxo.assets,
          );
        if (
          registerOperatorOrigin.tag === "ReturningOperator" &&
          updatedRetiredAnchorDatum !== null
        ) {
          tx = tx.pay.ToContract(
            contracts.retiredOperators.spendingScriptAddress,
            {
              kind: "inline",
              value: encodeLinkedListNodeView(updatedRetiredAnchorDatum),
            },
            registerOperatorOrigin.retiredOperatorAnchor.utxo.assets,
          );
        }
        tx = tx.addSignerKey(operatorKeyHash);
        tx = tx.validTo(Number(registerValidTo));
        return tx;
      };

      const registerLayoutOperatorOriginParams =
        registerOperatorOrigin.tag === "NewOperator"
          ? {
              tag: "NewOperator" as const,
              retiredNotMemberWitness:
                registerOperatorOrigin.retiredNotMemberWitness,
            }
          : {
              tag: "ReturningOperator" as const,
              retiredOperatorNode: registerOperatorOrigin.retiredOperatorNode,
              retiredOperatorAnchor:
                registerOperatorOrigin.retiredOperatorAnchor,
              retiredOperatorsPolicyId: contracts.retiredOperators.policyId,
              retiredOperatorsAddress:
                contracts.retiredOperators.spendingScriptAddress,
              retiredAnchorNodeUnit:
                registerOperatorOrigin.retiredAnchorNodeUnit,
              contracts,
            };
      const layoutDerivationParams = {
        hubOracleRefInput,
        activeNotMemberWitness,
        registeredRootNode,
        registeredOperatorsPolicyId: contracts.registeredOperators.policyId,
        registeredOperatorsAddress:
          contracts.registeredOperators.spendingScriptAddress,
        registeredNodeUnit,
        registeredRootNodeUnit,
        operatorOrigin: registerLayoutOperatorOriginParams,
      } as const;
      const initialRegisterOperatorOrigin =
        registerOperatorOrigin.tag === "NewOperator"
          ? {
              tag: "NewOperator" as const,
              retiredNotMemberWitness:
                registerOperatorOrigin.retiredNotMemberWitness,
            }
          : {
              tag: "ReturningOperator" as const,
              retiredOperatorNode: registerOperatorOrigin.retiredOperatorNode,
              retiredOperatorAnchor:
                registerOperatorOrigin.retiredOperatorAnchor,
              contracts,
            };
      const draftRegisterLayout = resolveInitialRegisterRedeemerLayout({
        registeredOperatorScriptRefs,
        retiredOperatorScriptRefs,
        hubOracleRefInput,
        activeNotMemberWitness,
        registeredRootNode,
        fundingInputs: registerFundingInputs,
        operatorOrigin: initialRegisterOperatorOrigin,
      });
      const draftRegisterUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          withRegisteredOperatorStubbedProviderEvaluation(lucid, () =>
            mkRegisterTx(draftRegisterLayout).complete({
              localUPLCEval: true,
              presetWalletInputs: [...registerFundingInputs],
            }),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: `Failed to build draft operator registration transaction: ${String(cause)}`,
            cause,
          }),
      });
      const registerDraftTx = draftRegisterUnsignedTx.toTransaction();
      let registerLayout = yield* deriveRegisterRedeemerLayout(
        registerDraftTx,
        layoutDerivationParams,
      );
      yield* Effect.logInfo(
        `Resolved register redeemer layout: ${registerLayoutToLogString(registerLayout)}`,
      );
      yield* Effect.logInfo(
        [
          "Register witnesses:",
          `origin=${registerOperatorOrigin.tag}`,
          `hub=${hubOracleRefInput.txHash}#${hubOracleRefInput.outputIndex.toString()}`,
          `active=${activeNotMemberWitness.utxo.txHash}#${activeNotMemberWitness.utxo.outputIndex.toString()}:${activeNotMemberWitness.assetName}`,
          registerOperatorOrigin.tag === "NewOperator"
            ? `retired_nonmember=${registerOperatorOrigin.retiredNotMemberWitness.utxo.txHash}#${registerOperatorOrigin.retiredNotMemberWitness.utxo.outputIndex.toString()}:${registerOperatorOrigin.retiredNotMemberWitness.assetName}`
            : `retired_removed=${registerOperatorOrigin.retiredOperatorNode.utxo.txHash}#${registerOperatorOrigin.retiredOperatorNode.utxo.outputIndex.toString()}:${registerOperatorOrigin.retiredOperatorNode.assetName}`,
          registerOperatorOrigin.tag === "ReturningOperator"
            ? `retired_anchor=${registerOperatorOrigin.retiredOperatorAnchor.utxo.txHash}#${registerOperatorOrigin.retiredOperatorAnchor.utxo.outputIndex.toString()}:${registerOperatorOrigin.retiredOperatorAnchor.assetName}`
            : null,
          `valid_to=${registerValidTo.toString()}`,
          `registration_time=${registrationTime.toString()}`,
          `prepended_node_datum=${encodeLinkedListNodeView(prependedNodeDatum)}`,
        ]
          .filter((part): part is string => part !== null)
          .join(" "),
      );
      let registerUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          completeWithLocalEvaluation((options) =>
            mkRegisterTx(registerLayout).complete({
              ...options,
              presetWalletInputs: [...registerFundingInputs],
            }),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: [
              "Failed to build operator registration transaction with resolved redeemer layout.",
              `cause=${String(cause)}`,
              `layout=${registerLayoutToLogString(registerLayout)}`,
            ].join(" "),
            cause,
          }),
      });
      for (let iteration = 0; iteration < 2; iteration += 1) {
        const derivedSubmitLayout = yield* deriveRegisterRedeemerLayout(
          registerUnsignedTx.toTransaction(),
          layoutDerivationParams,
        );
        if (registerLayoutsEqual(registerLayout, derivedSubmitLayout)) {
          break;
        }
        if (iteration === 1) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Register transaction layout did not converge after deterministic rebuild",
              cause: JSON.stringify({
                authoredLayout: registerLayoutToLogString(registerLayout),
                derivedLayout: registerLayoutToLogString(derivedSubmitLayout),
              }),
            }),
          );
        }
        yield* Effect.logWarning(
          [
            "Register layout drift detected after balancing; rebuilding with tx-derived indexes.",
            `authored=${registerLayoutToLogString(registerLayout)}`,
            `derived=${registerLayoutToLogString(derivedSubmitLayout)}`,
          ].join(" "),
        );
        registerLayout = derivedSubmitLayout;
        registerUnsignedTx = yield* Effect.tryPromise({
          try: () =>
            completeWithLocalEvaluation((options) =>
              mkRegisterTx(registerLayout).complete({
                ...options,
                presetWalletInputs: [...registerFundingInputs],
              }),
            ),
          catch: (cause) =>
            new SDK.LucidError({
              message: `Failed to rebuild operator registration transaction with tx-derived redeemer layout: ${String(cause)}`,
              cause,
            }),
        });
      }
      yield* Effect.logInfo(
        `Using register redeemer layout: ${registerLayoutToLogString(registerLayout)}`,
      );
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
      if (registerOperatorOrigin.tag === "ReturningOperator") {
        let refreshedRetiredNodeSet = false;
        for (
          let attempt = 0;
          attempt < REGISTERED_SET_REFRESH_MAX_RETRIES;
          attempt += 1
        ) {
          currentRetiredNodes = yield* fetchNodeSet(
            lucid,
            contracts.retiredOperators.spendingScriptAddress,
            contracts.retiredOperators.policyId,
          );
          const remainingRetiredOperatorNodes = currentRetiredNodes.filter(
            ({ datum }) => nodeKeyEquals(datum, operatorKeyHash),
          );
          if (remainingRetiredOperatorNodes.length === 0) {
            refreshedRetiredNodeSet = true;
            break;
          }
          if (remainingRetiredOperatorNodes.length > 1) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  "Found multiple retired-operator nodes after returning registration",
                cause: operatorKeyHash,
              }),
            );
          }
          if (attempt + 1 < REGISTERED_SET_REFRESH_MAX_RETRIES) {
            yield* Effect.sleep(REGISTERED_SET_REFRESH_RETRY_DELAY);
          }
        }
        if (!refreshedRetiredNodeSet) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Retired set refresh did not remove the operator node after successful returning registration",
              cause: operatorKeyHash,
            }),
          );
        }
      }
    }

    if (mode === "register-only") {
      return yield* toLifecycleResult({
        registerTxHash,
        activateTxHash: null,
        deregisterTxHash,
        retireTxHash: null,
        reregisterTxHash: null,
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
    const registeredOperatorDatum = decodeRegisteredOperatorDatumValue(
      registeredNode.datum.data,
    );
    if (registeredOperatorDatum === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to decode registered-operator datum for activation",
          cause: describeUnknownValue(registeredNode.datum.data),
        }),
      );
    }
    if (registeredOperatorDatum.operator !== operatorKeyHash) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Registered-operator datum operator does not match active wallet key",
          cause: `operator=${operatorKeyHash},datum_operator=${registeredOperatorDatum.operator}`,
        }),
      );
    }
    const activeBondUnlockTime = registeredOperatorDatum.bond_unlock_time;

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
    const retiredNotMemberWitnessForActivate = currentRetiredNodes.find(
      ({ datum }) => orderedNotMemberWitness(datum, operatorKeyHash),
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

    const activationTime = decodeRegisteredOperatorActivationTime(
      registeredNode.datum,
    );
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
    } else if (
      usesWallClockTime &&
      initialNow < activationTime &&
      advanceCustomEmulatorPastUnixTime(lucid, activationTime + 1_000n)
    ) {
      yield* Effect.logInfo(
        `Advanced custom emulator to operator activation time (ledger_now=${initialNow.toString()},activation_time=${activationTime.toString()})`,
      );
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
          : BigInt(alignedUnixTimeStrictlyAfter(lucid, Number(lowerBoundTarget)));
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
    const activeAnchorNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      activeAppendAnchor.assetName,
    );
    const registeredAnchorNodeUnit = toUnit(
      contracts.registeredOperators.policyId,
      registeredAnchor.assetName,
    );
    const transferredOperatorAssets = {
      ...registeredNode.utxo.assets,
      [registeredNodeUnit]: 0n,
      [activeNodeUnit]: 1n,
    };
    delete transferredOperatorAssets[registeredNodeUnit];

    const spendableWalletUtxosForActivation =
      yield* resolveSpendableWalletUtxos(
        lucid,
        lifecycleScriptRefOutRefs,
      );
    if (spendableWalletUtxosForActivation.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
              message:
                "No wallet funding UTxOs available for activation transaction",
              cause: operatorKeyHash,
            }),
          );
    }
    const activationFundingInputs = [
      ...selectWalletFundingUtxos(
        spendableWalletUtxosForActivation,
        ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
      ),
    ];
    if (activationFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to select wallet funding UTxOs for activation transaction",
          cause: operatorKeyHash,
        }),
      );
    }
    const activationCompleteOptions = (
      options: { readonly localUPLCEval: boolean },
    ) => ({
      ...options,
      presetWalletInputs: [...activationFundingInputs],
    });
    const activationDraftCompleteOptions = {
      presetWalletInputs: [...activationFundingInputs],
    };

    const updatedRegisteredAnchorDatum: SDK.LinkedListNodeView = {
      ...registeredAnchor.datum,
      next: registeredNode.datum.next,
    };
    /**
     * Builds the activation transaction for a registered operator.
     */
    const mkActivateTx = (layout: ActivateRedeemerLayout) => {
      const { validFrom, validTo } = resolveActivationValidityWindow();
      const activatedNodeDatum: SDK.LinkedListNodeView = {
        key: { Key: { key: operatorKeyHash } },
        next: activeAppendAnchor.datum.next,
        data: encodeActiveOperatorDatumValue(activeBondUnlockTime),
      };
      const updatedActiveAnchorDatum: SDK.LinkedListNodeView = {
        ...activeAppendAnchor.datum,
        next: { Key: { key: operatorKeyHash } },
      };
      const registeredActivateRedeemerBuilder =
        mkRegisteredActivateRedeemerBuilder({
          operatorKeyHash,
          layout,
          retiredOperatorAssetName: retiredNotMemberWitnessForActivate.assetName,
          registeredNode: registeredNode.utxo,
          registeredAnchor: registeredAnchor.utxo,
        });
      const activeActivateRedeemer = LucidData.to(
        {
          ActivateOperator: {
            new_active_operator_key: operatorKeyHash,
            new_active_operator_bond_unlock_time: activeBondUnlockTime,
            active_operator_anchor_element_input_index:
              layout.activeOperatorsAnchorNodeInputIndex,
            active_operator_anchor_element_output_index:
              layout.activeOperatorsAnchorNodeOutputIndex,
            active_operator_inserted_node_output_index:
              layout.activeOperatorsInsertedNodeOutputIndex,
            registered_operators_redeemer_index:
              layout.registeredOperatorsRedeemerIndex,
          },
        },
        SDK.ActiveOperatorMintRedeemer,
      );

      let tx = lucid
        .newTx()
        .validFrom(Number(validFrom))
        .collectFrom(activationFundingInputs)
        .collectFrom(
          [registeredNode.utxo, registeredAnchor.utxo],
          LucidData.void(),
        )
        .collectFrom(
          [activeAppendAnchor.utxo],
          ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER,
        )
        .readFrom(
          [
            ...registeredOperatorScriptRefs.map(({ utxo }) => utxo),
            ...activeOperatorScriptRefs.map(({ utxo }) => utxo),
            hubOracleRefInput,
            retiredNotMemberWitnessForActivate.utxo,
          ],
        )
        .mintAssets(
          { [registeredNodeUnit]: -1n },
          registeredActivateRedeemerBuilder,
        )
        .mintAssets({ [activeNodeUnit]: 1n }, activeActivateRedeemer);
      if (validTo !== undefined) {
        tx = tx.validTo(Number(validTo));
      }

      return tx
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: encodeLinkedListNodeView(activatedNodeDatum),
          },
          transferredOperatorAssets,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: encodeLinkedListNodeView(updatedActiveAnchorDatum),
          },
          activeAppendAnchor.utxo.assets,
        )
        .pay.ToContract(
          contracts.registeredOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: encodeLinkedListNodeView(updatedRegisteredAnchorDatum),
          },
          registeredAnchor.utxo.assets,
        )
        .addSignerKey(operatorKeyHash);
    };

    const activateLayoutParams = {
      hubOracleRefInput,
      retiredNotMemberWitnessForActivate,
      operatorKeyHash,
      registeredNode,
      registeredAnchor,
      activeAppendAnchor,
      registeredOperatorsPolicyId: contracts.registeredOperators.policyId,
      registeredOperatorsAddress: contracts.registeredOperators.spendingScriptAddress,
      registeredAnchorNodeUnit,
      activeOperatorsPolicyId: contracts.activeOperators.policyId,
      activeOperatorsAddress: contracts.activeOperators.spendingScriptAddress,
      activeNodeUnit,
      activeAnchorNodeUnit,
      contracts,
    } as const;
    let activateLayout = resolveInitialActivateRedeemerLayout({
      registeredOperatorScriptRefs,
      activeOperatorScriptRefs,
      hubOracleRefInput,
      retiredNotMemberWitnessForActivate,
      registeredNode,
      registeredAnchor,
      activeAppendAnchor,
      contracts,
      fundingInputs: activationFundingInputs,
    });
    const activationDraft = yield* Effect.tryPromise({
      try: () =>
        withRegisteredOperatorStubbedProviderEvaluation(lucid, () =>
          mkActivateTx(activateLayout).complete({
            localUPLCEval: true,
            ...activationDraftCompleteOptions,
          }),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build activation draft transaction: ${String(cause)}`,
          cause,
        }),
    });
    const activationDraftTx = activationDraft.toTransaction();
    const derivedDraftLayout = yield* deriveActivateRedeemerLayout(
      activationDraftTx,
      activateLayoutParams,
    );
    if (!activateLayoutsEqual(activateLayout, derivedDraftLayout)) {
      yield* Effect.logWarning(
        [
          "Activation draft layout differed from initial indexes; using tx-derived activation layout.",
          `initial=${activateLayoutToLogString(activateLayout)}`,
          `derived=${activateLayoutToLogString(derivedDraftLayout)}`,
        ].join(" "),
      );
      activateLayout = derivedDraftLayout;
    }
    yield* Effect.logInfo(
      `Resolved activate redeemer layout: ${activateLayoutToLogString(activateLayout)}`,
    );
    let activationUnsignedTx = yield* Effect.tryPromise({
      try: () =>
        completeWithLocalEvaluation((options) =>
          mkActivateTx(activateLayout).complete(activationCompleteOptions(options)),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build activation transaction: ${String(cause)}`,
          cause,
        }),
    });
    for (let iteration = 0; iteration < 2; iteration += 1) {
      const derivedSubmitLayout = yield* deriveActivateRedeemerLayout(
        activationUnsignedTx.toTransaction(),
        activateLayoutParams,
      );
      if (activateLayoutsEqual(activateLayout, derivedSubmitLayout)) {
        break;
      }
      if (iteration === 1) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Activation transaction layout did not converge after deterministic rebuild",
            cause: JSON.stringify({
              authoredLayout: activateLayoutToLogString(activateLayout),
              derivedLayout: activateLayoutToLogString(derivedSubmitLayout),
            }),
          }),
        );
      }
      yield* Effect.logWarning(
        [
          "Activation layout drift detected after balancing; rebuilding with tx-derived indexes.",
          `authored=${activateLayoutToLogString(activateLayout)}`,
          `derived=${activateLayoutToLogString(derivedSubmitLayout)}`,
        ].join(" "),
      );
      activateLayout = derivedSubmitLayout;
      activationUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          completeWithLocalEvaluation((options) =>
            mkActivateTx(activateLayout).complete(activationCompleteOptions(options)),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: `Failed to rebuild activation transaction with tx-derived layout: ${String(cause)}`,
            cause,
        }),
      });
    }
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

    if (mode === "register-activate-retire-reregister") {
      retireTxHash = yield* retireActiveOperatorProgram(
        lucid,
        contracts,
        operatorKeyHash,
        referenceScriptsLucid,
      );
      const reregisterResult = yield* operatorLifecycleProgram(
        lucid,
        contracts,
        requiredBondLovelace,
        "register-only",
        referenceScriptsLucid,
      );
      reregisterTxHash = reregisterResult.registerTxHash;
      if (reregisterTxHash === null) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Returning-operator registration was skipped after retirement",
            cause: operatorKeyHash,
          }),
        );
      }
    }

    return yield* toLifecycleResult({
      registerTxHash,
      activateTxHash,
      deregisterTxHash,
      retireTxHash,
      reregisterTxHash,
    });
  });

export const registerAndActivateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
): Effect.Effect<
  ActivationTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "register-and-activate",
    referenceScriptsLucid,
  ).pipe(
    Effect.andThen(({ registerTxHash, activateTxHash }) =>
      toActivationResult({
        registerTxHash,
        activateTxHash,
      }),
    ),
  );

export const registerActivateRetireReregisterOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
): Effect.Effect<
  RegisterActivateRetireReregisterTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "register-activate-retire-reregister",
    referenceScriptsLucid,
  ).pipe(
    Effect.map(
      ({
        registerTxHash,
        activateTxHash,
        retireTxHash,
        reregisterTxHash,
      }) => ({
        registerTxHash,
        activateTxHash,
        retireTxHash,
        reregisterTxHash,
      }),
    ),
  );

export const registerOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
): Effect.Effect<
  RegistrationTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "register-only",
    referenceScriptsLucid,
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
): Effect.Effect<
  ActivationTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "activate-only",
    referenceScriptsLucid,
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
): Effect.Effect<
  DeregistrationTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "deregister-only",
    referenceScriptsLucid,
  ).pipe(
    Effect.map(({ deregisterTxHash }) => ({
      deregisterTxHash,
    })),
  );

export const program = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;
  yield* lucidService.switchToOperatorsMainWallet;
  return yield* registerAndActivateOperatorProgram(
    lucidService.api,
    contracts,
    nodeConfig.OPERATOR_REQUIRED_BOND_LOVELACE,
    lucidService.referenceScriptsApi,
  );
});
