/**
 * Operator lifecycle transaction orchestration for register/activate flows.
 * This module is the main off-chain entrypoint for operator-set updates and
 * composes the shared layout and clock helpers extracted from the monolith.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data as LucidData,
  LucidEvolution,
  type RedeemerBuilder,
  Script,
  UTxO,
  coreToTxOutput,
  credentialToAddress,
  paymentCredentialOf,
  scriptHashToCredential,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { formatUnknownError } from "@/error-format.js";
import { Lucid, MidgardContracts, NodeConfig } from "@/services/index.js";
import { ActiveOperatorSpendRedeemerSchema } from "@/workers/utils/commit-redeemers.js";
import {
  TxSignError,
  TxSubmitError,
  handleSignSubmit,
} from "@/transactions/utils.js";
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
  activateLayoutToLogString,
  activateLayoutsEqual,
  describePolicyOutputDatumAtIndex,
  deriveActivateRedeemerLayout,
  deriveRegisterRedeemerLayout,
  findNodeOutputIndexByUnit,
  findReferenceInputIndex,
  getAssetNameByPolicy,
  getNodeDatumAtPolicyOutputIndex,
  linkPointsTo,
  nodeKeyEquals,
  orderedNotMemberWitness,
  registerLayoutToLogString,
  registerLayoutsEqual,
  resolveInitialActivateRedeemerLayout,
  resolveInitialRegisterRedeemerLayout,
} from "@/transactions/register-active-operator/lifecycle-layout.js";
import {
  withStubbedProviderEvaluation as withSharedStubbedProviderEvaluation,
} from "@/cml-redeemers.js";
import { compareOutRefs } from "@/tx-context.js";

const REGISTERED_ACTIVATION_DELAY_MS = 30n;
const ACTIVATION_VALIDITY_WINDOW_MS = 120_000n;
const DRAFT_REDEEMER_EX_UNITS = {
  mem: 1_000_000,
  steps: 1_000_000,
} as const;
const SCRIPT_REF_OUTPUT_LOVELACE = 4_000_000n;
const SCRIPT_REF_PUBLICATION_MAX_RETRIES = 12;
const SCRIPT_REF_PUBLICATION_RETRY_DELAY = "2 seconds";
const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE = 10_000_000n;
const REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE = 50_000_000n;
const REFERENCE_SCRIPT_PUBLICATION_BALANCE_INSUFFICIENT_PATTERN =
  /UTxO Balance Insufficient/i;
const REFERENCE_SCRIPT_PUBLICATION_TX_SIZE_EXCEEDED_PATTERN =
  /Max transaction size of \d+ exceeded\. Found: \d+/i;
const REFERENCE_SCRIPT_PUBLICATION_BALANCE_GAP_PATTERN =
  /Inputs:\s*Value\s*\{\s*coin:\s*(\d+)[\s\S]*?Outputs:\s*Value\s*\{\s*coin:\s*(\d+)/i;
const WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES = 6;
const WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY = "1 second";
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
  ActiveOperatorSpendRedeemerSchema,
);
const ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA = LucidData.Enum([
  LucidData.Object({
    Some: LucidData.Tuple([LucidData.Integer()]),
  }),
  LucidData.Literal("None"),
]);
const ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA = LucidData.Object({
  bond_unlock_time: ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA,
});
const ACTIVE_OPERATOR_DATUM_OPTION_SCHEMA = LucidData.Nullable(
  LucidData.Integer(),
);
const REGISTERED_OPERATOR_DATUM_AIKEN_SCHEMA = LucidData.Object({
  activation_time: LucidData.Integer(),
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
};

type OperatorLifecycleMode =
  | "register-and-activate"
  | "register-only"
  | "activate-only"
  | "deregister-only";

export const REFERENCE_SCRIPT_COMMAND_NAMES = [
  "hub-oracle",
  "state-queue",
  "scheduler",
  "registered-operators",
  "active-operators",
  "retired-operators",
] as const;

export type ReferenceScriptCommandName =
  (typeof REFERENCE_SCRIPT_COMMAND_NAMES)[number];

type StubEvaluationMode = "draft" | "submission";
type ActiveOperatorDatumEncoding = "aiken" | "record" | "option";

const isReferenceScriptPublicationTxTooLarge = (cause: unknown): boolean =>
  REFERENCE_SCRIPT_PUBLICATION_TX_SIZE_EXCEEDED_PATTERN.test(
    formatUnknownError(cause),
  );

const isReferenceScriptPublicationBalanceInsufficient = (
  cause: unknown,
): boolean =>
  REFERENCE_SCRIPT_PUBLICATION_BALANCE_INSUFFICIENT_PATTERN.test(
    formatUnknownError(cause),
  );

const resolveReferenceScriptPublicationAdditionalFunding = (
  cause: unknown,
): bigint | undefined => {
  const match =
    REFERENCE_SCRIPT_PUBLICATION_BALANCE_GAP_PATTERN.exec(
      formatUnknownError(cause),
    );
  if (match === null) {
    return undefined;
  }
  const inputs = BigInt(match[1]);
  const outputs = BigInt(match[2]);
  if (outputs <= inputs) {
    return undefined;
  }
  return outputs - inputs + SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;
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
  encoding: ActiveOperatorDatumEncoding,
  commitmentTime: bigint | null,
) => {
  switch (encoding) {
    case "aiken":
      return LucidData.castTo(
        { bond_unlock_time: commitmentTime } as never,
        ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA as never,
      );
    case "record":
      return LucidData.castTo({ commitmentTime }, SDK.ActiveOperatorDatum);
    case "option":
      return LucidData.castTo(
        commitmentTime as never,
        ACTIVE_OPERATOR_DATUM_OPTION_SCHEMA as never,
      );
  }
};

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

const decodeActiveOperatorDatumValue = (
  value: unknown,
):
  | {
      readonly encoding: ActiveOperatorDatumEncoding;
      readonly commitmentTime: bigint | null;
    }
  | undefined => {
  if (typeof value === "string") {
    try {
      const decoded = LucidData.from(value);
      if (decoded !== value) {
        return decodeActiveOperatorDatumValue(decoded);
      }
    } catch {
      // Fall through to direct schema decoding.
    }
  }
  try {
    const parsed = LucidData.castFrom(
      value,
      ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA as never,
    ) as {
      readonly bond_unlock_time: bigint | null;
    };
    return {
      encoding: "aiken",
      commitmentTime:
        parsed.bond_unlock_time === null
          ? null
          : BigInt(parsed.bond_unlock_time),
    };
  } catch {
    // Fall through to legacy record encoding.
  }
  if (typeof value === "object" && value !== null) {
    if ("bond_unlock_time" in value) {
      const commitmentTime = decodeOptionalBigIntValue(value.bond_unlock_time);
      if (commitmentTime !== undefined) {
        return {
          encoding: "aiken",
          commitmentTime,
        };
      }
    }
    if ("commitmentTime" in value) {
      const commitmentTime = decodeOptionalBigIntValue(value.commitmentTime);
      if (commitmentTime !== undefined) {
        return {
          encoding: "record",
          commitmentTime,
        };
      }
    }
  }
  const optionCommitmentTime = decodeOptionalBigIntValue(value);
  if (optionCommitmentTime !== undefined) {
    return {
      encoding: "option",
      commitmentTime: optionCommitmentTime,
    };
  }
  try {
    const parsed = LucidData.castFrom(
      value,
      SDK.ActiveOperatorDatum,
    ) as SDK.ActiveOperatorDatum;
    return {
      encoding: "record",
      commitmentTime:
        parsed.commitmentTime === null
          ? null
          : BigInt(parsed.commitmentTime),
    };
  } catch {
    try {
      const parsed = LucidData.castFrom(
        value as never,
        ACTIVE_OPERATOR_DATUM_OPTION_SCHEMA as never,
      ) as bigint | null;
      return {
        encoding: "option",
        commitmentTime: parsed,
      };
    } catch {
      return undefined;
    }
  }
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

const resolveActiveOperatorDatumEncoding = (
  value: unknown,
  context: string,
): Effect.Effect<ActiveOperatorDatumEncoding, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const decoded = decodeActiveOperatorDatumValue(value);
    if (decoded !== undefined) {
      return decoded.encoding;
    }
    yield* Effect.logWarning(
      `Unsupported active-operator datum encoding payload for ${context}: ${describeUnknownValue(value)}`,
    );
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: `Unsupported active-operator datum encoding for ${context}`,
        cause: describeUnknownValue(value),
      }),
    );
  });

const decodeActiveOperatorDatumCommitmentTime = (
  value: unknown,
): bigint | null => {
  const decoded = decodeActiveOperatorDatumValue(value);
  if (decoded !== undefined) {
    return decoded.commitmentTime;
  }
  try {
    const parsed = LucidData.castFrom(
      value,
      ACTIVE_OPERATOR_DATUM_OPTION_SCHEMA,
    ) as bigint | null;
    return parsed === null ? null : BigInt(parsed);
  } catch {
    // Fall back to the canonical "no lock" value.
  }
  return null;
};

/**
 * Encodes a registered-operator datum into the value shape expected by the transaction builder.
 */
const encodeRegisteredOperatorDatumValue = (activationTime: bigint) =>
  LucidData.castTo(
    { activation_time: activationTime } as never,
    REGISTERED_OPERATOR_DATUM_AIKEN_SCHEMA as never,
  );

const decodeRegisteredOperatorActivationTime = (
  value: unknown,
): bigint | undefined => {
  if (typeof value === "object" && value !== null) {
    if (
      "registrationTime" in value &&
      typeof value.registrationTime === "bigint"
    ) {
      return value.registrationTime;
    }
    if (
      "registrationTime" in value &&
      typeof value.registrationTime === "number" &&
      Number.isInteger(value.registrationTime)
    ) {
      return BigInt(value.registrationTime);
    }
    if (
      "activation_time" in value &&
      typeof value.activation_time === "bigint"
    ) {
      return value.activation_time;
    }
    if (
      "activation_time" in value &&
      typeof value.activation_time === "number" &&
      Number.isInteger(value.activation_time)
    ) {
      return BigInt(value.activation_time);
    }
  }
  try {
    const parsed = LucidData.castFrom(
      value as never,
      SDK.RegisteredOperatorDatum as never,
    ) as SDK.RegisteredOperatorDatum;
    return BigInt(parsed.registrationTime);
  } catch {
    try {
      const parsed = LucidData.castFrom(
        value as never,
        REGISTERED_OPERATOR_DATUM_AIKEN_SCHEMA as never,
      ) as {
        readonly activation_time: bigint | number;
      };
      return typeof parsed.activation_time === "bigint"
        ? parsed.activation_time
        : BigInt(parsed.activation_time);
    } catch {
      return undefined;
    }
  }
};

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
      Activate: {
        nodeToActivateKey: operatorKeyHash,
        hubOracleRefInputIndex: layout.hubOracleRefInputIndex,
        retiredOperatorRefInputIndex: layout.retiredOperatorRefInputIndex,
        retiredOperatorAsset_name: retiredOperatorAssetName,
        removedNodeInputIndex: layout.removedNodeInputIndex,
        anchorNodeInputIndex: layout.anchorNodeInputIndex,
        activeOperatorsInsertedNodeOutputIndex:
          layout.activeOperatorsInsertedNodeOutputIndex,
        activeOperatorsAnchorNodeOutputIndex:
          layout.activeOperatorsAnchorNodeOutputIndex,
      },
    },
    SDK.RegisteredOperatorMintRedeemer,
  );
};

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
    return mkRegisteredActivateRedeemer({
      operatorKeyHash,
      layout,
      retiredOperatorAssetName,
    });
  },
});

const completeWithLocalEvaluation = async <A>(
  build: (options: { localUPLCEval: boolean }) => Promise<A>,
): Promise<A> => {
  return await build({ localUPLCEval: true });
};

const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;
const utxoOutRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
const WALLET_OUTREF_RECONCILE_MAX_RETRIES = 4;
const WALLET_OUTREF_RECONCILE_RETRY_DELAY = "750 millis";
const filterPlainWalletUtxos = (utxos: readonly UTxO[]): readonly UTxO[] =>
  utxos.filter((utxo) => utxo.scriptRef === undefined);
const sumWalletLovelace = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + lovelaceOf(utxo), 0n);
const resolveReferenceScriptPublicationFundingTarget = (
  missingTargetCount: number,
): bigint =>
  SCRIPT_REF_OUTPUT_LOVELACE * (BigInt(missingTargetCount) + 1n) +
  SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;

const orderWalletFundingUtxos = (utxos: readonly UTxO[]): readonly UTxO[] =>
  [...utxos].sort((left, right) => {
    const leftIsPlain = left.scriptRef === undefined;
    const rightIsPlain = right.scriptRef === undefined;
    if (leftIsPlain !== rightIsPlain) {
      return leftIsPlain ? -1 : 1;
    }
    const leftLovelace = lovelaceOf(left);
    const rightLovelace = lovelaceOf(right);
    if (leftLovelace === rightLovelace) {
      return compareOutRefs(left, right);
    }
    return leftLovelace > rightLovelace ? -1 : 1;
  });

const selectWalletFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] => {
  const sorted = orderWalletFundingUtxos(utxos);
  const selected: UTxO[] = [];
  let covered = 0n;
  for (const utxo of sorted) {
    selected.push(utxo);
    covered += lovelaceOf(utxo);
    if (covered >= targetLovelace) {
      break;
    }
  }
  return selected;
};

const mergeWalletUtxosPreservingScriptRefs = (
  liveUtxos: readonly UTxO[],
  cachedUtxos: readonly UTxO[],
): readonly UTxO[] => {
  const cachedByOutRef = new Map(
    cachedUtxos.map((utxo) => [utxoOutRefKey(utxo), utxo]),
  );
  return liveUtxos.map((utxo) => {
    if (utxo.scriptRef !== undefined) {
      return utxo;
    }
    const cached = cachedByOutRef.get(utxoOutRefKey(utxo));
    if (cached?.scriptRef === undefined) {
      return utxo;
    }
    return {
      ...utxo,
      scriptRef: cached.scriptRef,
    };
  });
};

const reconcileLiveWalletUtxos = (
  lucid: LucidEvolution,
  utxos: readonly UTxO[],
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (utxos.length === 0) {
      return [];
    }
    const uniqueOutRefs = Array.from(
      new Map(
        utxos.map((utxo) => [
          `${utxo.txHash}#${utxo.outputIndex.toString()}`,
          {
            txHash: utxo.txHash,
            outputIndex: utxo.outputIndex,
          },
        ]),
      ).values(),
    );
    let live: readonly UTxO[] | null = null;
    let lastCause: unknown = null;
    for (
      let attempt = 0;
      attempt < WALLET_OUTREF_RECONCILE_MAX_RETRIES;
      attempt += 1
    ) {
      const liveAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: () => lucid.utxosByOutRef(uniqueOutRefs),
          catch: (cause) => cause,
        }),
      );
      if (liveAttempt._tag === "Right") {
        live = liveAttempt.right;
        break;
      }
      lastCause = liveAttempt.left;
      if (attempt + 1 < WALLET_OUTREF_RECONCILE_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Wallet UTxO out-ref reconciliation failed (attempt ${(attempt + 1).toString()}/${WALLET_OUTREF_RECONCILE_MAX_RETRIES.toString()}); retrying in ${WALLET_OUTREF_RECONCILE_RETRY_DELAY}. cause=${String(lastCause)}`,
        );
        yield* Effect.sleep(WALLET_OUTREF_RECONCILE_RETRY_DELAY);
      }
    }
    if (live === null) {
      yield* Effect.logWarning(
        `Wallet UTxO out-ref reconciliation exhausted retries; using wallet snapshot as fallback. attempts=${WALLET_OUTREF_RECONCILE_MAX_RETRIES.toString()},last_cause=${String(lastCause)}`,
      );
      return utxos;
    }
    if (utxos.length > 0 && live.length === 0) {
      yield* Effect.logWarning(
        "Wallet UTxO out-ref reconciliation returned zero live entries from a non-empty snapshot; keeping wallet snapshot to avoid false empty-input failures.",
      );
      return utxos;
    }
    return mergeWalletUtxosPreservingScriptRefs(live, utxos);
  });

const fetchReconciledWalletUtxos = (
  lucid: LucidEvolution,
  failureMessage: string,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletUtxosRaw = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: failureMessage,
          cause,
        }),
    });
    return yield* reconcileLiveWalletUtxos(lucid, walletUtxosRaw);
  });

const refreshWalletUtxosFromOwnAddress = (
  lucid: LucidEvolution,
  {
    scopeName,
    failureMessage,
    minimumPlainBalance,
  }: {
    readonly scopeName: string;
    readonly failureMessage: string;
    readonly minimumPlainBalance?: bigint;
  },
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve wallet address while refreshing ${scopeName}`,
          cause,
        }),
    });
    const cachedWalletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: () => [] as readonly UTxO[],
    }).pipe(Effect.catchAll(() => Effect.succeed([] as readonly UTxO[])));

    let refreshedWalletUtxos: readonly UTxO[] | null = null;
    let lastCause: unknown = null;
    for (
      let attempt = 0;
      attempt < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES;
      attempt += 1
    ) {
      const atAddressAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: () => lucid.utxosAt(walletAddress),
          catch: (cause) => cause,
        }),
      );
      if (atAddressAttempt._tag === "Right") {
        const mergedWalletUtxos = mergeWalletUtxosPreservingScriptRefs(
          atAddressAttempt.right,
          cachedWalletUtxos,
        );
        yield* Effect.sync(() => lucid.overrideUTxOs([...mergedWalletUtxos]));
        const plainBalance = sumWalletLovelace(
          filterPlainWalletUtxos(mergedWalletUtxos),
        );
        if (
          minimumPlainBalance === undefined ||
          plainBalance >= minimumPlainBalance
        ) {
          refreshedWalletUtxos = mergedWalletUtxos;
          break;
        }
        lastCause = `wallet_address=${walletAddress},plain_balance=${plainBalance.toString()},required_plain_balance=${minimumPlainBalance.toString()}`;
      } else {
        lastCause = atAddressAttempt.left;
      }

      if (attempt + 1 < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Wallet own-address refresh for ${scopeName} did not reach the required state (attempt ${(attempt + 1).toString()}/${WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES.toString()}); retrying in ${WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY}. cause=${String(lastCause)}`,
        );
        yield* Effect.sleep(WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY);
      }
    }

    if (refreshedWalletUtxos !== null) {
      return refreshedWalletUtxos;
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: failureMessage,
        cause: lastCause ?? `wallet_address=${walletAddress}`,
      }),
    );
  });

const resolveSpendableWalletUtxos = (
  lucid: LucidEvolution,
  excludedOutRefKeys: ReadonlySet<string>,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletUtxos = yield* fetchReconciledWalletUtxos(
      lucid,
      "Failed to fetch wallet UTxOs for transaction input preset",
    );
    // Reference-script publications must remain available for `.readFrom(...)`.
    // Treat them as reserved infrastructure UTxOs, not generic funding inputs.
    return filterPlainWalletUtxos(walletUtxos).filter(
      (utxo) =>
        !excludedOutRefKeys.has(utxoOutRefKey(utxo)),
    );
  });

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
        const datum = yield* SDK.getNodeDatumFromUTxO(utxo).pipe(
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
      if (hubOracleRefInput.datum === undefined) {
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

const isSameScriptRef = (
  left: Script | null | undefined,
  right: Script,
): boolean => {
  if (left === undefined || left === null || left.type !== right.type) {
    return false;
  }
  try {
    return validatorToScriptHash(left) === validatorToScriptHash(right);
  } catch {
    return false;
  }
};

const resolveLiveWalletUtxo = (
  lucid: LucidEvolution,
  utxo: UTxO,
): Effect.Effect<UTxO | undefined, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const resolved = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosByOutRef([
          {
            txHash: utxo.txHash,
            outputIndex: utxo.outputIndex,
          },
        ]),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to resolve wallet UTxO by out-ref while validating script reference",
          cause,
        }),
    });
    const live = resolved.find(
      (candidate) =>
        candidate.txHash === utxo.txHash &&
        candidate.outputIndex === utxo.outputIndex,
    );
    if (live === undefined) {
      return undefined;
    }
    if (live.scriptRef === undefined && utxo.scriptRef !== undefined) {
      return {
        ...live,
        scriptRef: utxo.scriptRef,
      };
    }
    return live;
  });

const resolveExistingReferenceScriptPublication = (
  lucid: LucidEvolution,
  walletUtxos: readonly UTxO[],
  target: { readonly name: string; readonly script: Script },
): Effect.Effect<ReferenceScriptPublication | undefined, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const existingCandidates = walletUtxos
      .filter((utxo) => isSameScriptRef(utxo.scriptRef, target.script))
      .sort(compareOutRefs)
      .reverse();
    for (const existingCandidate of existingCandidates) {
      const existing = yield* resolveLiveWalletUtxo(lucid, existingCandidate);
      if (existing !== undefined && isSameScriptRef(existing.scriptRef, target.script)) {
        return {
          name: target.name,
          utxo: existing,
        };
      }
    }
    return undefined;
  });

const ensureReferenceScriptWalletWorkingCapital = (
  fundingLucid: LucidEvolution,
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  requiredPlainBalance: bigint,
): Effect.Effect<
  void,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const referenceScriptWalletUtxos = yield* refreshWalletUtxosFromOwnAddress(
      referenceScriptsLucid,
      {
        scopeName: `${scopeName} reference scripts`,
        failureMessage:
          `Failed to fetch wallet UTxOs while preparing ${scopeName} reference scripts`,
      },
    );
    const currentPlainBalance = sumWalletLovelace(
      filterPlainWalletUtxos(referenceScriptWalletUtxos),
    );
    const targetPlainBalance =
      requiredPlainBalance > REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE
        ? requiredPlainBalance
        : REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE;
    if (currentPlainBalance >= targetPlainBalance) {
      return;
    }

    const referenceScriptAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve reference-script wallet address while preparing ${scopeName} reference scripts`,
          cause,
        }),
    });
    const fundingAddress = yield* Effect.tryPromise({
      try: () => fundingLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve funding wallet address while preparing ${scopeName} reference scripts`,
          cause,
        }),
    });
    if (fundingAddress === referenceScriptAddress) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            `Reference-script wallet plain balance is below the required working-capital floor while preparing ${scopeName} reference scripts`,
          cause: `plain_balance=${currentPlainBalance.toString()},required=${targetPlainBalance.toString()},wallet_address=${referenceScriptAddress},reason=same-wallet-funding-would-risk-scriptref-spend`,
        }),
      );
    }

    const topUpAmount = targetPlainBalance - currentPlainBalance;
    const fundingInputs = yield* resolveSpendableWalletUtxos(
      fundingLucid,
      new Set<string>(),
    );
    if (fundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            `No operator wallet funding UTxOs available to replenish ${scopeName} reference scripts`,
          cause: `reference_script_wallet=${referenceScriptAddress},required_top_up=${topUpAmount.toString()}`,
        }),
      );
    }
    const selectedFundingInputs = selectWalletFundingUtxos(
      fundingInputs,
      topUpAmount + SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE,
    );
    if (selectedFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            `Failed to select operator wallet funding UTxOs to replenish ${scopeName} reference scripts`,
          cause: `reference_script_wallet=${referenceScriptAddress},required_top_up=${topUpAmount.toString()}`,
        }),
      );
    }

    yield* Effect.logInfo(
      `Replenishing reference-script wallet for ${scopeName}: current_plain_balance=${currentPlainBalance.toString()},target_plain_balance=${targetPlainBalance.toString()},top_up_amount=${topUpAmount.toString()},reference_script_wallet=${referenceScriptAddress}`,
    );
    const unsigned = yield* Effect.tryPromise({
      try: () =>
        fundingLucid
          .newTx()
          .collectFrom([...selectedFundingInputs])
          .pay.ToAddress(referenceScriptAddress, {
            lovelace: topUpAmount,
          })
          .complete({
            coinSelection: false,
            localUPLCEval: true,
            presetWalletInputs: [...selectedFundingInputs],
          }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build reference-script wallet replenishment transaction for ${scopeName}: ${String(cause)}`,
          cause,
        }),
    });
    const txHash = yield* handleSignSubmit(fundingLucid, unsigned);
    yield* refreshWalletUtxosFromOwnAddress(referenceScriptsLucid, {
      scopeName: `${scopeName} reference scripts after replenishment`,
      failureMessage:
        `Failed to refresh reference-script wallet after replenishing ${scopeName} reference scripts`,
      minimumPlainBalance: targetPlainBalance,
    });
    yield* Effect.logInfo(
      `Reference-script wallet replenishment confirmed for ${scopeName}: txHash=${txHash},top_up_amount=${topUpAmount.toString()}`,
    );
  });

const publishMissingReferenceScriptTargets = (
  lucid: LucidEvolution,
  operatorAddress: string,
  walletUtxos: readonly UTxO[],
  fundingCandidateUtxos: readonly UTxO[],
  missingTargets: readonly { readonly name: string; readonly script: Script }[],
): Effect.Effect<
  readonly ReferenceScriptPublication[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const orderedFundingCandidates = orderWalletFundingUtxos(
      fundingCandidateUtxos,
    );
    let selectedFundingInputs = selectWalletFundingUtxos(
      fundingCandidateUtxos,
      resolveReferenceScriptPublicationFundingTarget(missingTargets.length),
    );
    if (selectedFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to select non-reference wallet UTxOs for reference-script publication",
          cause: missingTargets.map(({ name }) => name).join(","),
        }),
      );
    }
    let nextFundingCandidateIndex = selectedFundingInputs.length;
    const [txHash, localReferenceOutputs, walletOutputs] = yield* Effect.gen(
      function* () {
        while (true) {
          yield* Effect.sync(() => lucid.overrideUTxOs([...selectedFundingInputs]));
          const buildAttempt = yield* Effect.either(
            Effect.gen(function* () {
              let tx = lucid.newTx().collectFrom([...selectedFundingInputs]);
              tx = tx.pay.ToAddressWithData(operatorAddress, undefined, {
                lovelace: SCRIPT_REF_OUTPUT_LOVELACE,
              });
              for (const target of missingTargets) {
                tx = tx.pay.ToAddressWithData(
                  operatorAddress,
                  undefined,
                  { lovelace: SCRIPT_REF_OUTPUT_LOVELACE },
                  target.script,
                );
              }
              const unsigned = yield* Effect.tryPromise({
                try: () =>
                  tx.complete({
                    coinSelection: false,
                    localUPLCEval: true,
                    presetWalletInputs: [...selectedFundingInputs],
                  }),
                catch: (cause) =>
                  new SDK.LucidError({
                    message: `Failed to build reference-script publication transaction for ${missingTargets
                      .map(({ name }) => name)
                      .join(", ")}: ${String(cause)}`,
                    cause,
                  }),
              });
              const publicationTx = unsigned.toTransaction();
              const publicationOutputs = publicationTx.body().outputs();
              const localReferenceOutputs = new Map<
                string,
                Omit<UTxO, "txHash">
              >();
              const walletOutputs: Omit<UTxO, "txHash">[] = [];
              for (
                let outputIndex = 0;
                outputIndex < publicationOutputs.len();
                outputIndex += 1
              ) {
                const output = coreToTxOutput(publicationOutputs.get(outputIndex));
                if (output.address !== operatorAddress) {
                  continue;
                }
                walletOutputs.push({
                  outputIndex,
                  address: output.address,
                  assets: output.assets,
                  datum: output.datum ?? undefined,
                  datumHash: output.datumHash ?? undefined,
                  scriptRef: output.scriptRef ?? undefined,
                });
                if (output.scriptRef === undefined) {
                  continue;
                }
                const matchingTarget = missingTargets.find(
                  (target) =>
                    !localReferenceOutputs.has(target.name) &&
                    isSameScriptRef(output.scriptRef, target.script),
                );
                if (matchingTarget === undefined) {
                  continue;
                }
                localReferenceOutputs.set(matchingTarget.name, {
                  outputIndex,
                  address: output.address,
                  assets: output.assets,
                  datum: output.datum ?? undefined,
                  datumHash: output.datumHash ?? undefined,
                  scriptRef: output.scriptRef,
                });
              }
              const txHash = yield* handleSignSubmit(lucid, unsigned);
              return [txHash, localReferenceOutputs, walletOutputs] as const;
            }),
          ).pipe(
            Effect.ensuring(
              Effect.sync(() => lucid.overrideUTxOs([...walletUtxos])),
            ),
          );

          if (buildAttempt._tag === "Right") {
            return buildAttempt.right;
          }
          if (
            !isReferenceScriptPublicationBalanceInsufficient(buildAttempt.left) ||
            nextFundingCandidateIndex >= orderedFundingCandidates.length
          ) {
            return yield* Effect.fail(buildAttempt.left);
          }

          const nextFundingInput =
            orderedFundingCandidates[nextFundingCandidateIndex];
          nextFundingCandidateIndex += 1;
          selectedFundingInputs = [
            ...selectedFundingInputs,
            nextFundingInput,
          ];
          yield* Effect.logWarning(
            `Reference-script publication for ${missingTargets
              .map(({ name }) => name)
              .join(", ")} needed more wallet funding than the seed estimate; retrying with ${selectedFundingInputs.length.toString()} funding input(s).`,
          );
        }
      },
    );
    const restoredWalletUtxos = [
      ...walletUtxos.filter(
        (utxo) =>
          !selectedFundingInputs.some(
            (selected) =>
              selected.txHash === utxo.txHash &&
              selected.outputIndex === utxo.outputIndex,
          ),
      ),
      ...walletOutputs.map((output) => ({
        txHash,
        ...output,
      })),
    ];
    yield* Effect.sync(() => lucid.overrideUTxOs(restoredWalletUtxos));
    return yield* Effect.forEach(missingTargets, (target) =>
      Effect.gen(function* () {
        const localReferenceOutput = localReferenceOutputs.get(target.name);
        if (localReferenceOutput === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Reference-script publication transaction did not contain the expected script-ref output",
              cause: `${target.name},txHash=${txHash}`,
            }),
          );
        }
        const localReferenceUtxo: UTxO = {
          txHash,
          ...localReferenceOutput,
        };
        const liveReference = yield* Effect.either(
          resolveLiveWalletUtxo(lucid, localReferenceUtxo),
        );
        if (
          liveReference._tag === "Right" &&
          liveReference.right !== undefined &&
          isSameScriptRef(liveReference.right.scriptRef, target.script)
        ) {
          return {
            name: target.name,
            utxo: liveReference.right,
          };
        }
        if (
          liveReference._tag === "Right" &&
          liveReference.right !== undefined
        ) {
          return {
            name: target.name,
            utxo: {
              ...liveReference.right,
              scriptRef: target.script,
            },
          };
        }
        return {
          name: target.name,
          utxo: {
            ...localReferenceUtxo,
            scriptRef: target.script,
          },
        };
      }),
    );
  });

const referenceScriptTargetsByCommand = (
  contracts: SDK.MidgardValidators,
): Readonly<Record<ReferenceScriptCommandName, readonly { readonly name: string; readonly script: Script }[]>> => ({
  "hub-oracle": [
    {
      name: "hub-oracle minting",
      script: contracts.hubOracle.mintingScript,
    },
  ],
  "state-queue": [
    {
      name: "state-queue spending",
      script: contracts.stateQueue.spendingScript,
    },
    {
      name: "state-queue minting",
      script: contracts.stateQueue.mintingScript,
    },
  ],
  scheduler: [
    {
      name: "scheduler spending",
      script: contracts.scheduler.spendingScript,
    },
    {
      name: "scheduler minting",
      script: contracts.scheduler.mintingScript,
    },
  ],
  "registered-operators": [
    {
      name: "registered-operators spending",
      script: contracts.registeredOperators.spendingScript,
    },
    {
      name: "registered-operators minting",
      script: contracts.registeredOperators.mintingScript,
    },
  ],
  "active-operators": [
    {
      name: "active-operators spending",
      script: contracts.activeOperators.spendingScript,
    },
    {
      name: "active-operators minting",
      script: contracts.activeOperators.mintingScript,
    },
  ],
  "retired-operators": [
    {
      name: "retired-operators spending",
      script: contracts.retiredOperators.spendingScript,
    },
    {
      name: "retired-operators minting",
      script: contracts.retiredOperators.mintingScript,
    },
  ],
});

const ensureReferenceScriptTargetsProgram = (
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  targets: readonly { readonly name: string; readonly script: Script }[],
  fundingLucid: LucidEvolution = referenceScriptsLucid,
): Effect.Effect<
  readonly ReferenceScriptPublication[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const fetchReferenceScriptWalletUtxos = (): Effect.Effect<
      readonly UTxO[],
      SDK.StateQueueError
    > =>
      refreshWalletUtxosFromOwnAddress(
        referenceScriptsLucid,
        {
          scopeName: `${scopeName} reference scripts`,
          failureMessage:
            `Failed to fetch wallet UTxOs while resolving ${scopeName} reference scripts`,
        },
      );

    const publishMissingTargetsInBatches = (
      missingTargets: readonly { readonly name: string; readonly script: Script }[],
    ): Effect.Effect<
      readonly ReferenceScriptPublication[],
      SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
    > =>
      Effect.gen(function* () {
        if (missingTargets.length === 0) {
          return [];
        }
        let requiredPlainBalance =
          resolveReferenceScriptPublicationFundingTarget(missingTargets.length);
        while (true) {
          yield* ensureReferenceScriptWalletWorkingCapital(
            fundingLucid,
            referenceScriptsLucid,
            scopeName,
            requiredPlainBalance,
          );
          const walletUtxos = yield* fetchReferenceScriptWalletUtxos();
          const operatorAddress = yield* Effect.tryPromise({
            try: () => referenceScriptsLucid.wallet().address(),
            catch: (cause) =>
              new SDK.StateQueueError({
                message: `Failed to resolve wallet address while creating ${scopeName} reference scripts`,
                cause,
              }),
          });
          if (walletUtxos.length === 0) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  `No wallet UTxOs available while publishing ${scopeName} reference scripts`,
                cause: "wallet-has-no-live-utxos",
              }),
            );
          }
          const plainFundingCandidateUtxos = filterPlainWalletUtxos(walletUtxos);
          if (plainFundingCandidateUtxos.length === 0) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  `No plain wallet UTxOs available while publishing ${scopeName} reference scripts`,
                cause: "wallet-has-no-plain-utxos",
              }),
            );
          }
          const publishAttempt = yield* Effect.either(
            publishMissingReferenceScriptTargets(
              referenceScriptsLucid,
              operatorAddress,
              walletUtxos,
              plainFundingCandidateUtxos,
              missingTargets,
            ),
          );
          if (publishAttempt._tag === "Right") {
            return publishAttempt.right;
          }
          if (
            isReferenceScriptPublicationBalanceInsufficient(publishAttempt.left)
          ) {
            const additionalFunding =
              resolveReferenceScriptPublicationAdditionalFunding(
                publishAttempt.left,
              );
            if (additionalFunding !== undefined) {
              requiredPlainBalance =
                sumWalletLovelace(plainFundingCandidateUtxos) +
                additionalFunding;
              yield* Effect.logWarning(
                `Reference-script publication for ${scopeName} needed more dedicated-wallet funding than the current working-capital floor; retrying after replenishing an additional ${additionalFunding.toString()} lovelace.`,
              );
              continue;
            }
          }
          if (
            missingTargets.length === 1 ||
            !isReferenceScriptPublicationTxTooLarge(publishAttempt.left)
          ) {
            return yield* Effect.fail(publishAttempt.left);
          }
          const splitIndex = Math.ceil(missingTargets.length / 2);
          const leftTargets = missingTargets.slice(0, splitIndex);
          const rightTargets = missingTargets.slice(splitIndex);
          yield* Effect.logWarning(
            `Reference-script publication for ${scopeName} exceeded max tx size; retrying in smaller batches: left=[${leftTargets
              .map(({ name }) => name)
              .join(", ")}], right=[${rightTargets
              .map(({ name }) => name)
              .join(", ")}]`,
          );
          const leftPublications = yield* publishMissingTargetsInBatches(
            leftTargets,
          );
          const rightPublications = yield* publishMissingTargetsInBatches(
            rightTargets,
          );
          return [...leftPublications, ...rightPublications];
        }
      });

    const walletUtxos = yield* fetchReferenceScriptWalletUtxos();
    const existingPublications = yield* Effect.forEach(targets, (target) =>
      resolveExistingReferenceScriptPublication(
        referenceScriptsLucid,
        walletUtxos,
        target,
      ),
    );
    const existingByName = new Map(
      existingPublications
        .filter(
          (
            publication,
          ): publication is ReferenceScriptPublication => publication !== undefined,
        )
        .map((publication) => [publication.name, publication]),
    );
    const missingTargets = targets.filter(
      (target) => !existingByName.has(target.name),
    );
    let createdByName = new Map<string, ReferenceScriptPublication>();
    if (missingTargets.length > 0) {
      createdByName = new Map(
        (
          yield* publishMissingTargetsInBatches(missingTargets)
        ).map((publication) => [publication.name, publication]),
      );
    }
    const resolvedPublications: ReferenceScriptPublication[] = [];
    for (const target of targets) {
      const publication =
        existingByName.get(target.name) ?? createdByName.get(target.name);
      if (publication === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Missing resolved reference script publication",
            cause: `${scopeName}:${target.name}`,
          }),
        );
      }
      resolvedPublications.push(publication);
    }
    return resolvedPublications;
  });

export const deployReferenceScriptCommandProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  commandName: ReferenceScriptCommandName,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
): Effect.Effect<
  readonly ReferenceScriptPublication[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  ensureReferenceScriptTargetsProgram(
    referenceScriptsLucid,
    commandName,
    referenceScriptTargetsByCommand(contracts)[commandName],
    fundingLucid,
  );

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
): Effect.Effect<
  OperatorLifecycleTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const usesWallClockTime = lucid.config().network === "Custom";
    const hubOracleRefInput = yield* fetchHubOracleRefInput(lucid, contracts);
    const hubOracleDatum = yield* decodeHubOracleDatum(hubOracleRefInput);
    yield* Effect.logInfo(
      `Hub oracle policies: registered=${hubOracleDatum.registeredOperators},active=${hubOracleDatum.activeOperators},retired=${hubOracleDatum.retiredOperators}`,
    );
    const policyMismatches: string[] = [];
    if (hubOracleDatum.registeredOperators !== contracts.registeredOperators.policyId) {
      policyMismatches.push(
        `registered(hub=${hubOracleDatum.registeredOperators},contracts=${contracts.registeredOperators.policyId})`,
      );
    }
    if (hubOracleDatum.activeOperators !== contracts.activeOperators.policyId) {
      policyMismatches.push(
        `active(hub=${hubOracleDatum.activeOperators},contracts=${contracts.activeOperators.policyId})`,
      );
    }
    if (hubOracleDatum.retiredOperators !== contracts.retiredOperators.policyId) {
      policyMismatches.push(
        `retired(hub=${hubOracleDatum.retiredOperators},contracts=${contracts.retiredOperators.policyId})`,
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
        ? yield* ensureReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "registered-operators",
            referenceScriptTargetsByCommand(contracts)["registered-operators"],
            lucid,
          )
        : yield* ensureReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "operator lifecycle",
            operatorReferenceTargets,
            lucid,
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
      nodeKeyEquals(datum, operatorKeyHash),
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
        });
      }

      if (mode !== "activate-only") {
        const existingRegisteredNode = existingRegisteredNodes[0];
        const existingRegisteredAnchor = currentRegisteredNodes.find(({ datum }) =>
          linkPointsTo(datum, operatorKeyHash),
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
          SDK.NODE_ASSET_NAME + operatorKeyHash,
        );
        const updatedRegisteredAnchorDatumAfterDeregister: SDK.NodeDatum = {
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
          | null = null;
        for (const deregisterLayout of deregisterLayouts) {
          const deregisterRedeemer = LucidData.to(
            {
              Deregister: {
                nodeToDeregisterKey: operatorKeyHash,
                removedNodeInputIndex: deregisterLayout.removedNodeInputIndex,
                anchorNodeInputIndex: deregisterLayout.anchorNodeInputIndex,
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
                        value: LucidData.to(
                          updatedRegisteredAnchorDatumAfterDeregister,
                          SDK.NodeDatum,
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
            ({ datum }) => nodeKeyEquals(datum, operatorKeyHash),
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
      nodeKeyEquals(datum, operatorKeyHash),
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
        registerValidTo + REGISTERED_ACTIVATION_DELAY_MS;
      const prependedNodeDatum: SDK.NodeDatum = {
        key: { Key: { key: operatorKeyHash } },
        next: registeredRootNode.datum.next,
        data: encodeRegisteredOperatorDatumValue(registrationTime),
      };
      const updatedRegisteredRootDatum: SDK.NodeDatum = {
        ...registeredRootNode.datum,
        next: { Key: { key: operatorKeyHash } },
      };

      const registeredNodeUnit = toUnit(
        contracts.registeredOperators.policyId,
        SDK.NODE_ASSET_NAME + operatorKeyHash,
      );
      const registerMintAssets = {
        [registeredNodeUnit]: 1n,
      };
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
      const mkRegisterTx = (layout: RegisterRedeemerLayout) => {
        const registerRedeemer = LucidData.to(
          {
            Register: {
              keyToPrepend: operatorKeyHash,
              hubOracleRefInputIndex: layout.hubOracleRefInputIndex,
              activeOperatorRefInputIndex: layout.activeOperatorRefInputIndex,
              activeOperatorAssetName: activeNotMemberWitness.assetName,
              retiredOperatorRefInputIndex: layout.retiredOperatorRefInputIndex,
              retiredOperatorAssetName: retiredNotMemberWitness.assetName,
              prependedNodeOutputIndex: layout.prependedNodeOutputIndex,
              anchorNodeOutputIndex: layout.anchorNodeOutputIndex,
            },
          },
          SDK.RegisteredOperatorMintRedeemer,
        );
        let tx = lucid
          .newTx()
          .collectFrom([registeredRootNode.utxo], LucidData.void())
          .readFrom([
            ...registeredOperatorScriptRefs.map(({ utxo }) => utxo),
            hubOracleRefInput,
            activeNotMemberWitness.utxo,
            retiredNotMemberWitness.utxo,
          ])
          .mintAssets(registerMintAssets, registerRedeemer)
          .pay.ToContract(
            contracts.registeredOperators.spendingScriptAddress,
            {
              kind: "inline",
              value: LucidData.to(prependedNodeDatum, SDK.NodeDatum),
            },
            prependedNodeAssets,
          )
          .pay.ToContract(
            contracts.registeredOperators.spendingScriptAddress,
            {
              kind: "inline",
              value: LucidData.to(updatedRegisteredRootDatum, SDK.NodeDatum),
            },
            registeredRootNode.utxo.assets,
          )
          .addSignerKey(operatorKeyHash);
        tx = tx.validTo(Number(registerValidTo));
        return tx;
      };

      const layoutDerivationParams = {
        hubOracleRefInput,
        activeNotMemberWitness,
        retiredNotMemberWitness,
        registeredOperatorsPolicyId: contracts.registeredOperators.policyId,
        registeredOperatorsAddress:
          contracts.registeredOperators.spendingScriptAddress,
        registeredNodeUnit,
        registeredRootNodeUnit,
      } as const;
      const draftRegisterLayout = resolveInitialRegisterRedeemerLayout({
        registeredOperatorScriptRefs,
        hubOracleRefInput,
        activeNotMemberWitness,
        retiredNotMemberWitness,
      });
      const draftRegisterUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          withRegisteredOperatorStubbedProviderEvaluation(lucid, () =>
            mkRegisterTx(draftRegisterLayout).complete({
              localUPLCEval: false,
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
          `hub=${hubOracleRefInput.txHash}#${hubOracleRefInput.outputIndex.toString()}`,
          `active=${activeNotMemberWitness.utxo.txHash}#${activeNotMemberWitness.utxo.outputIndex.toString()}:${activeNotMemberWitness.assetName}`,
          `retired=${retiredNotMemberWitness.utxo.txHash}#${retiredNotMemberWitness.utxo.outputIndex.toString()}:${retiredNotMemberWitness.assetName}`,
          `valid_to=${registerValidTo.toString()}`,
          `registration_time=${registrationTime.toString()}`,
          `prepended_node_datum=${LucidData.to(prependedNodeDatum, SDK.NodeDatum)}`,
        ].join(" "),
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
          nodeKeyEquals(datum, operatorKeyHash),
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
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Registered set refresh did not expose the operator node after successful registration",
            cause: operatorKeyHash,
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
      nodeKeyEquals(datum, operatorKeyHash),
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
      linkPointsTo(datum, operatorKeyHash),
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

    const activationTime = decodeRegisteredOperatorActivationTime(
      registeredNode.datum.data,
    );
    if (activationTime === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to decode registered-operator activation time from registration node datum",
          cause: describeUnknownValue(registeredNode.datum.data),
        }),
      );
    }
    const activeListIsEmpty =
      activeNodes.length === 1 &&
      activeNodes[0].datum.key === "Empty" &&
      activeNodes[0].datum.next === "Empty";
    const initialNow = yield* resolveCurrentTimeMs(lucid);
    if (!usesWallClockTime && !activeListIsEmpty && initialNow < activationTime) {
      const waitMs = activationTime - initialNow + 1_000n;
      yield* Effect.logInfo(
        `Waiting ${waitMs.toString()}ms until operator activation time (ledger_now=${initialNow.toString()},activation_time=${activationTime.toString()})`,
      );
      yield* Effect.sleep(Number(waitMs));
    } else if (!usesWallClockTime && activeListIsEmpty && initialNow < activationTime) {
      yield* Effect.logInfo(
        [
          "Bypassing activation-time wait because active-operators list is empty",
          `(ledger_now=${initialNow.toString()},activation_time=${activationTime.toString()}).`,
        ].join(" "),
      );
    }
    const resolveActivationValidityWindow = (): {
      readonly validFrom: bigint;
      readonly validTo?: bigint;
    } => {
      const currentTime = currentTimeMsForLucidOrEmulatorFallback(lucid);
      const lowerBoundTarget = activeListIsEmpty ? currentTime : activationTime;
      // Keep a finite lower bound at/after the required activation lower-bound:
      // - empty active list: current time (registration delay waived),
      // - otherwise: operator activation time.
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
      SDK.NODE_ASSET_NAME + operatorKeyHash,
    );
    const activeNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.NODE_ASSET_NAME + operatorKeyHash,
    );
    const activeAnchorNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      activeAppendAnchor.assetName,
    );
    const activeOperatorDatumEncoding = yield* resolveActiveOperatorDatumEncoding(
      activeAppendAnchor.datum.data,
      "active-operators anchor datum",
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
    const activationCompleteOptions = (
      options: { readonly localUPLCEval: boolean },
    ) => ({
      ...options,
      presetWalletInputs: [...spendableWalletUtxosForActivation],
    });
    const activationDraftCompleteOptions = {
      presetWalletInputs: [...spendableWalletUtxosForActivation],
    } as const;

    const updatedRegisteredAnchorDatum: SDK.NodeDatum = {
      ...registeredAnchor.datum,
      next: registeredNode.datum.next,
    };
    /**
     * Builds the activation transaction for a registered operator.
     */
    const mkActivateTx = (layout: ActivateRedeemerLayout) => {
      const { validFrom, validTo } = resolveActivationValidityWindow();
      const activatedNodeDatum: SDK.NodeDatum = {
        key: { Key: { key: operatorKeyHash } },
        next: activeAppendAnchor.datum.next,
        data: encodeActiveOperatorDatumValue(activeOperatorDatumEncoding, null),
      };
      const updatedActiveAnchorDatum: SDK.NodeDatum = {
        ...activeAppendAnchor.datum,
        next: { Key: { key: operatorKeyHash } },
        data: encodeActiveOperatorDatumValue(
          activeOperatorDatumEncoding,
          decodeActiveOperatorDatumCommitmentTime(activeAppendAnchor.datum.data),
        ),
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
            newActiveOperatorKey: operatorKeyHash,
            hubOracleRefInputIndex: layout.hubOracleRefInputIndex,
            activeOperatorAppendedNodeOutputIndex:
              layout.activeOperatorsInsertedNodeOutputIndex,
            activeOperatorAnchorNodeOutputIndex:
              layout.activeOperatorsAnchorNodeOutputIndex,
            registeredOperatorsRedeemerIndex:
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
            value: LucidData.to(activatedNodeDatum, SDK.NodeDatum),
          },
          transferredOperatorAssets,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: LucidData.to(updatedActiveAnchorDatum, SDK.NodeDatum),
          },
          activeAppendAnchor.utxo.assets,
        )
        .pay.ToContract(
          contracts.registeredOperators.spendingScriptAddress,
          {
            kind: "inline",
            value: LucidData.to(updatedRegisteredAnchorDatum, SDK.NodeDatum),
          },
          registeredAnchor.utxo.assets,
        )
        .addSignerKey(operatorKeyHash);
    };

    const activateLayoutParams = {
      hubOracleRefInput,
      retiredNotMemberWitnessForActivate,
      registeredNode,
      registeredAnchor,
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
    });
    const activationDraft = yield* Effect.tryPromise({
      try: () =>
        withRegisteredOperatorStubbedProviderEvaluation(lucid, () =>
          mkActivateTx(activateLayout).complete({
            localUPLCEval: false,
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
    yield* Effect.logInfo(
      `Using active-operator datum encoding for activation: ${activeOperatorDatumEncoding}`,
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
): Effect.Effect<
  ActivationTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
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

export const registerOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
): Effect.Effect<
  RegistrationTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
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
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
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
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
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
