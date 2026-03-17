import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data as LucidData,
  LucidEvolution,
  Network,
  type RedeemerBuilder,
  Script,
  UTxO,
  coreToTxOutput,
  credentialToAddress,
  paymentCredentialOf,
  scriptHashToCredential,
  slotToUnixTime,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
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

const REGISTERED_ACTIVATION_DELAY_MS = 30n;
const ACTIVATION_VALIDITY_WINDOW_MS = 120_000n;
const DRAFT_REDEEMER_EX_UNITS = {
  mem: 1_000_000,
  steps: 1_000_000,
} as const;
const SCRIPT_REF_OUTPUT_LOVELACE = 4_000_000n;
const SCRIPT_REF_PUBLICATION_MAX_RETRIES = 12;
const SCRIPT_REF_PUBLICATION_RETRY_DELAY = "2 seconds";
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
const ACTIVATION_DEBUG_REDEEMERS_ENV =
  "MIDGARD_DEBUG_ACTIVATION_REDEEMERS";
const ACTIVATION_DEBUG_INPUTS_ENV = "MIDGARD_DEBUG_ACTIVATION_INPUTS";
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
const ACTIVATION_SELECTED_REGISTERED_INPUTS_COUNT = 2;

type ProviderRedeemerTag =
  | "spend"
  | "mint"
  | "publish"
  | "withdraw"
  | "vote"
  | "propose";

type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

type NodeWithDatum = {
  readonly utxo: UTxO;
  readonly datum: SDK.NodeDatum;
  readonly assetName: string;
};

type RegisterRedeemerLayout = {
  readonly hubOracleRefInputIndex: bigint;
  readonly activeOperatorRefInputIndex: bigint;
  readonly retiredOperatorRefInputIndex: bigint;
  readonly prependedNodeOutputIndex: bigint;
  readonly anchorNodeOutputIndex: bigint;
};

type ActivateRedeemerLayout = {
  readonly hubOracleRefInputIndex: bigint;
  readonly retiredOperatorRefInputIndex: bigint;
  readonly registeredOperatorsRedeemerIndex: bigint;
  readonly removedNodeInputIndex: bigint;
  readonly anchorNodeInputIndex: bigint;
  readonly activeOperatorsInsertedNodeOutputIndex: bigint;
  readonly activeOperatorsAnchorNodeOutputIndex: bigint;
};

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

type ActivationScriptRef = {
  readonly name: string;
  readonly utxo: UTxO;
};

type StubEvaluationMode = "draft" | "submission";
type ActiveOperatorDatumEncoding = "aiken" | "record" | "option";

const slotToUnixTimeForLucid = (
  lucid: LucidEvolution,
  slot: number,
): number => {
  const network = lucid.config().network;
  if (network === "Custom") {
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (typeof provider.time === "number" && typeof provider.slot === "number") {
      const slotLength = 1000;
      const zeroTime = provider.time - provider.slot * slotLength;
      return zeroTime + slot * slotLength;
    }
    // Deterministic emulator fallback: derive unix-ish time purely from slot.
    return slot * 1000;
  }
  return slotToUnixTime(network as Exclude<Network, "Custom">, slot);
};

const resolveCurrentTimeMs = (
  lucid: LucidEvolution,
): Effect.Effect<bigint, never> =>
  Effect.sync(() => {
    const currentSlot = lucid.currentSlot();
    return BigInt(slotToUnixTimeForLucid(lucid, currentSlot));
  });

const alignUnixTimeMsToSlotBoundary = (
  lucid: LucidEvolution,
  unixTimeMs: bigint,
): bigint => BigInt(alignUnixTimeToSlotBoundary(lucid, Number(unixTimeMs)));

const isRemoteEvaluationFailure = (cause: unknown): boolean => {
  const message = String(cause);
  return (
    message.includes("EvaluateTransaction fails") ||
    message.includes("EvaluationFailure") ||
    message.includes("TxBuilderError")
  );
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

const completeWithLocalEvaluationFallback = async <A>(
  lucid: LucidEvolution,
  build: (options: { localUPLCEval: boolean }) => Promise<A>,
): Promise<A> => {
  try {
    return await build({ localUPLCEval: false });
  } catch (remoteCause) {
    if (!isRemoteEvaluationFailure(remoteCause)) {
      throw remoteCause;
    }
    try {
      return await build({ localUPLCEval: true });
    } catch (localCause) {
      try {
        return await withStubbedProviderEvaluation(
          lucid,
          () => build({ localUPLCEval: false }),
          "submission",
        );
      } catch (stubCause) {
        throw new Error(
          `remote_evaluation_error=${String(remoteCause)}; local_evaluation_error=${String(localCause)}; stubbed_submission_evaluation_error=${String(stubCause)}`,
        );
      }
    }
  }
};

const compareOutRefs = (a: UTxO, b: UTxO): number => {
  const txHashCompare = a.txHash.localeCompare(b.txHash);
  if (txHashCompare !== 0) {
    return txHashCompare;
  }
  return a.outputIndex - b.outputIndex;
};

const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;
const utxoOutRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
const WALLET_OUTREF_RECONCILE_MAX_RETRIES = 4;
const WALLET_OUTREF_RECONCILE_RETRY_DELAY = "750 millis";

const selectWalletFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] => {
  const sorted = [...utxos].sort((left, right) => {
    const leftLovelace = lovelaceOf(left);
    const rightLovelace = lovelaceOf(right);
    if (leftLovelace === rightLovelace) {
      return compareOutRefs(left, right);
    }
    return leftLovelace > rightLovelace ? -1 : 1;
  });
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
    const cachedByOutRef = new Map(
      utxos.map((utxo) => [
        `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        utxo,
      ]),
    );
    return live.map((utxo) => {
      if (utxo.scriptRef !== undefined) {
        return utxo;
      }
      const cached = cachedByOutRef.get(
        `${utxo.txHash}#${utxo.outputIndex.toString()}`,
      );
      if (cached?.scriptRef === undefined) {
        return utxo;
      }
      return {
        ...utxo,
        scriptRef: cached.scriptRef,
      };
    });
  });

const resolveSpendableWalletUtxos = (
  lucid: LucidEvolution,
  excludedOutRefKeys: ReadonlySet<string>,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletUtxosRaw = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to fetch wallet UTxOs for transaction input preset",
          cause,
        }),
    });
    const walletUtxos = yield* reconcileLiveWalletUtxos(lucid, walletUtxosRaw);
    return walletUtxos.filter(
      (utxo) => !excludedOutRefKeys.has(utxoOutRefKey(utxo)),
    );
  });

const referenceInputsFromTx = (tx: CML.Transaction): readonly UTxO[] => {
  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    return [];
  }
  return Array.from({ length: referenceInputs.len() }, (_, index) => {
    const referenceInput = referenceInputs.get(index);
    return {
      txHash: referenceInput.transaction_id().to_hex(),
      outputIndex: Number(referenceInput.index()),
      address: "",
      assets: { lovelace: 0n },
    };
  });
};

const spendingInputsFromTx = (tx: CML.Transaction): readonly UTxO[] => {
  const spendingInputs = tx.body().inputs();
  return Array.from({ length: spendingInputs.len() }, (_, index) => {
    const spendingInput = spendingInputs.get(index);
    return {
      txHash: spendingInput.transaction_id().to_hex(),
      outputIndex: Number(spendingInput.index()),
      address: "",
      assets: { lovelace: 0n },
    };
  });
};

const collateralInputsFromTx = (tx: CML.Transaction): readonly UTxO[] => {
  const collateralInputs = tx.body().collateral_inputs();
  if (collateralInputs === undefined) {
    return [];
  }
  return Array.from({ length: collateralInputs.len() }, (_, index) => {
    const collateralInput = collateralInputs.get(index);
    return {
      txHash: collateralInput.transaction_id().to_hex(),
      outputIndex: Number(collateralInput.index()),
      address: "",
      assets: { lovelace: 0n },
    };
  });
};

const resolveMissingOutRefs = (
  lucid: LucidEvolution,
  refs: readonly UTxO[],
  message: string,
): Effect.Effect<readonly string[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (refs.length === 0) {
      return [];
    }
    const uniqueOutRefs = Array.from(
      new Map(
        refs.map((ref) => [
          `${ref.txHash}#${ref.outputIndex.toString()}`,
          {
            txHash: ref.txHash,
            outputIndex: ref.outputIndex,
          },
        ]),
      ).values(),
    );
    const resolved = yield* Effect.tryPromise({
      try: () => lucid.utxosByOutRef(uniqueOutRefs),
      catch: (cause) =>
        new SDK.StateQueueError({
          message,
          cause,
        }),
    });
    const resolvedSet = new Set(
      resolved.map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`),
    );
    return uniqueOutRefs
      .map((ref) => `${ref.txHash}#${ref.outputIndex.toString()}`)
      .filter((refKey) => !resolvedSet.has(refKey));
  });

const resolveMissingReferenceInputs = (
  lucid: LucidEvolution,
  tx: CML.Transaction,
): Effect.Effect<readonly string[], SDK.StateQueueError> =>
  resolveMissingOutRefs(
    lucid,
    referenceInputsFromTx(tx),
    "Failed to resolve activation transaction reference inputs by out-ref",
  );

const resolveMissingSpendingInputs = (
  lucid: LucidEvolution,
  tx: CML.Transaction,
): Effect.Effect<readonly string[], SDK.StateQueueError> =>
  resolveMissingOutRefs(
    lucid,
    spendingInputsFromTx(tx),
    "Failed to resolve activation transaction spending inputs by out-ref",
  );

const resolveMissingCollateralInputs = (
  lucid: LucidEvolution,
  tx: CML.Transaction,
): Effect.Effect<readonly string[], SDK.StateQueueError> =>
  resolveMissingOutRefs(
    lucid,
    collateralInputsFromTx(tx),
    "Failed to resolve activation transaction collateral inputs by out-ref",
  );

const findReferenceInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
): bigint | undefined => {
  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    return undefined;
  }
  const sortedReferenceInputs = Array.from(
    { length: referenceInputs.len() },
    (_, index) => {
      const input = referenceInputs.get(index);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    },
  ).sort((left, right) => {
    const txHashCompare = left.txHash.localeCompare(right.txHash);
    if (txHashCompare !== 0) {
      return txHashCompare;
    }
    return left.outputIndex - right.outputIndex;
  });
  const position = sortedReferenceInputs.findIndex(
    (input) =>
      input.txHash === target.txHash && input.outputIndex === target.outputIndex,
  );
  return position >= 0 ? BigInt(position) : undefined;
};

const findInputIndex = (tx: CML.Transaction, target: UTxO): bigint | undefined => {
  const inputs = tx.body().inputs();
  const sortedInputs = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort((left, right) => {
    const txHashCompare = left.txHash.localeCompare(right.txHash);
    if (txHashCompare !== 0) {
      return txHashCompare;
    }
    return left.outputIndex - right.outputIndex;
  });
  const position = sortedInputs.findIndex(
    (input) =>
      input.txHash === target.txHash && input.outputIndex === target.outputIndex,
  );
  return position >= 0 ? BigInt(position) : undefined;
};

const findNodeOutputIndexByUnit = (
  tx: CML.Transaction,
  policyId: string,
  address: string,
  unit: string,
): bigint | undefined => {
  const outputs = tx.body().outputs();
  let nodeOutputIndex = 0n;
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    const containsPolicyAsset = Object.entries(output.assets).some(
      ([assetUnit, quantity]) =>
        assetUnit !== "lovelace" &&
        quantity > 0n &&
        assetUnit.startsWith(policyId),
    );
    if (!containsPolicyAsset) {
      continue;
    }
    if (output.address !== address) {
      nodeOutputIndex += 1n;
      continue;
    }
    if ((output.assets[unit] ?? 0n) === 1n) {
      return nodeOutputIndex;
    }
    nodeOutputIndex += 1n;
  }
  return undefined;
};

const getPolicyOutputsFromTx = (
  tx: CML.Transaction,
  policyId: string,
): readonly ReturnType<typeof coreToTxOutput>[] => {
  const outputs = tx.body().outputs();
  const policyOutputs: ReturnType<typeof coreToTxOutput>[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    const hasPolicyAsset = Object.entries(output.assets).some(
      ([assetUnit, quantity]) =>
        assetUnit !== "lovelace" &&
        quantity > 0n &&
        assetUnit.startsWith(policyId),
    );
    if (!hasPolicyAsset) {
      continue;
    }
    policyOutputs.push(output);
  }
  return policyOutputs;
};

const registerLayoutsEqual = (
  left: RegisterRedeemerLayout,
  right: RegisterRedeemerLayout,
): boolean =>
  left.hubOracleRefInputIndex === right.hubOracleRefInputIndex &&
  left.activeOperatorRefInputIndex === right.activeOperatorRefInputIndex &&
  left.retiredOperatorRefInputIndex === right.retiredOperatorRefInputIndex &&
  left.prependedNodeOutputIndex === right.prependedNodeOutputIndex &&
  left.anchorNodeOutputIndex === right.anchorNodeOutputIndex;

const registerLayoutToLogString = (layout: RegisterRedeemerLayout): string =>
  `hub_ref=${layout.hubOracleRefInputIndex.toString()},active_ref=${layout.activeOperatorRefInputIndex.toString()},retired_ref=${layout.retiredOperatorRefInputIndex.toString()},prepended_out=${layout.prependedNodeOutputIndex.toString()},anchor_out=${layout.anchorNodeOutputIndex.toString()}`;

const activateLayoutsEqual = (
  left: ActivateRedeemerLayout,
  right: ActivateRedeemerLayout,
): boolean =>
  left.hubOracleRefInputIndex === right.hubOracleRefInputIndex &&
  left.retiredOperatorRefInputIndex === right.retiredOperatorRefInputIndex &&
  left.registeredOperatorsRedeemerIndex ===
    right.registeredOperatorsRedeemerIndex &&
  left.removedNodeInputIndex === right.removedNodeInputIndex &&
  left.anchorNodeInputIndex === right.anchorNodeInputIndex &&
  left.activeOperatorsInsertedNodeOutputIndex ===
    right.activeOperatorsInsertedNodeOutputIndex &&
  left.activeOperatorsAnchorNodeOutputIndex ===
    right.activeOperatorsAnchorNodeOutputIndex;

const activateLayoutToLogString = (layout: ActivateRedeemerLayout): string =>
  [
    `hub_ref=${layout.hubOracleRefInputIndex.toString()}`,
    `retired_ref=${layout.retiredOperatorRefInputIndex.toString()}`,
    `registered_redeemer=${layout.registeredOperatorsRedeemerIndex.toString()}`,
    `removed_in=${layout.removedNodeInputIndex.toString()}`,
    `anchor_in=${layout.anchorNodeInputIndex.toString()}`,
    `active_inserted_out=${layout.activeOperatorsInsertedNodeOutputIndex.toString()}`,
    `active_anchor_out=${layout.activeOperatorsAnchorNodeOutputIndex.toString()}`,
  ].join(",");

const toProviderRedeemerTag = (tag: number): ProviderRedeemerTag => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "publish";
    case CML.RedeemerTag.Reward:
      return "withdraw";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      throw new Error(`Unsupported redeemer tag: ${tag}`);
  }
};

// Aiken exposes `self.redeemers` in ScriptPurpose order:
// Spend < Mint < Withdraw < Publish < Vote < Propose.
// CML redeemer pointers are emitted in context order, so cross-redeemer
// references must map context positions into this tx-info order.
const txInfoRedeemerPurposeRank = (tag: number): number => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return 0;
    case CML.RedeemerTag.Mint:
      return 1;
    case CML.RedeemerTag.Reward:
      return 2;
    case CML.RedeemerTag.Cert:
      return 3;
    case CML.RedeemerTag.Voting:
      return 4;
    case CML.RedeemerTag.Proposing:
      return 5;
    default:
      return Number.MAX_SAFE_INTEGER;
  }
};

const getTxInfoRedeemerIndexes = (
  pointers: readonly RedeemerPointer[],
): readonly number[] => {
  const inContextOrder = pointers.map((pointer, contextIndex) => ({
    pointer,
    contextIndex,
  }));
  const inTxInfoOrder = [...inContextOrder].sort((a, b) => {
    const rankA = txInfoRedeemerPurposeRank(a.pointer.tag);
    const rankB = txInfoRedeemerPurposeRank(b.pointer.tag);
    if (rankA !== rankB) {
      return rankA - rankB;
    }
    if (a.pointer.index !== b.pointer.index) {
      return a.pointer.index < b.pointer.index ? -1 : 1;
    }
    return a.contextIndex - b.contextIndex;
  });

  const txInfoIndexes = Array<number>(pointers.length).fill(-1);
  for (
    let txInfoIndex = 0;
    txInfoIndex < inTxInfoOrder.length;
    txInfoIndex += 1
  ) {
    const { contextIndex } = inTxInfoOrder[txInfoIndex];
    txInfoIndexes[contextIndex] = txInfoIndex;
  }
  return txInfoIndexes;
};

const getRedeemerPointersInContextOrder = (
  tx: CML.Transaction,
): readonly RedeemerPointer[] => {
  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    return [];
  }

  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    const pointers: RedeemerPointer[] = [];
    for (let i = 0; i < legacy.len(); i += 1) {
      const redeemer = legacy.get(i);
      pointers.push({
        tag: redeemer.tag(),
        index: redeemer.index(),
      });
    }
    return pointers;
  }

  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map === undefined) {
    return [];
  }
  const pointers: RedeemerPointer[] = [];
  const keys = map.keys();
  for (let i = 0; i < keys.len(); i += 1) {
    const key = keys.get(i);
    pointers.push({
      tag: key.tag(),
      index: key.index(),
    });
  }
  return pointers;
};

const redeemerTagLabel = (tag: number): string => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "cert";
    case CML.RedeemerTag.Reward:
      return "reward";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      return `unknown(${tag})`;
  }
};

const describeRedeemerData = (value: CML.RedeemerVal | undefined): string => {
  if (value === undefined) {
    return "missing";
  }
  try {
    return value.data().to_json();
  } catch {
    return "<unprintable>";
  }
};

const describePlutusData = (data: CML.PlutusData): string => {
  try {
    return data.to_json();
  } catch {
    return "<unprintable>";
  }
};

const describeDraftRedeemers = (tx: CML.Transaction): string => {
  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    return "none";
  }

  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    const entries: string[] = [];
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      entries.push(
        `#${index.toString()}:${redeemerTagLabel(redeemer.tag())}:${redeemer.index().toString()}:${describePlutusData(redeemer.data())}`,
      );
    }
    return entries.join(" | ");
  }

  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map === undefined) {
    return "unknown";
  }
  const keys = map.keys();
  const entries: string[] = [];
  for (let index = 0; index < keys.len(); index += 1) {
    const key = keys.get(index);
    entries.push(
      `#${index.toString()}:${redeemerTagLabel(key.tag())}:${key.index().toString()}:${describeRedeemerData(map.get(key))}`,
    );
  }
  return entries.join(" | ");
};

const describePolicyOutputDatumAtIndex = (
  tx: CML.Transaction,
  policyId: string,
  outputIndex: bigint,
): string => {
  const outputs = getPolicyOutputsFromTx(tx, policyId);
  const index = Number(outputIndex);
  if (
    !Number.isSafeInteger(index) ||
    index < 0 ||
    index >= outputs.length
  ) {
    return `<missing:${outputIndex.toString()}>`;
  }
  const output = outputs[index];
  if (output.datum === undefined) {
    return "<no-datum>";
  }
  try {
    const nodeDatum = LucidData.from(output.datum, SDK.NodeDatum);
    return `cbor=${output.datum},decoded=${JSON.stringify(nodeDatum)}`;
  } catch (cause) {
    return `<datum-decode-error:${String(cause)},cbor=${output.datum}>`;
  }
};

const getNodeDatumAtPolicyOutputIndex = (
  tx: CML.Transaction,
  policyId: string,
  outputIndex: bigint,
): SDK.NodeDatum | undefined => {
  const outputs = getPolicyOutputsFromTx(tx, policyId);
  const index = Number(outputIndex);
  if (
    !Number.isSafeInteger(index) ||
    index < 0 ||
    index >= outputs.length
  ) {
    return undefined;
  }
  const output = outputs[index];
  if (output.datum === undefined) {
    return undefined;
  }
  try {
    return LucidData.from(output.datum, SDK.NodeDatum);
  } catch {
    return undefined;
  }
};

const withStubbedProviderEvaluation = async <A>(
  lucid: LucidEvolution,
  run: () => Promise<A>,
  mode: StubEvaluationMode = "draft",
): Promise<A> => {
  const provider = lucid.config().provider as {
    evaluateTx?: (
      tx: string,
      additionalUTxOs?: readonly UTxO[],
    ) => Promise<
      readonly {
        redeemer_tag: ProviderRedeemerTag;
        redeemer_index: number;
        ex_units: { mem: number; steps: number };
      }[]
    >;
    getProtocolParameters?: () => Promise<{
      maxTxExMem: bigint;
      maxTxExSteps: bigint;
    }>;
  };
  if (typeof provider.evaluateTx !== "function") {
    return run();
  }

  const resolveStubExUnits = async (
    redeemerCount: number,
  ): Promise<{ mem: number; steps: number }> => {
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

  const originalEvaluateTx = provider.evaluateTx.bind(provider);
  provider.evaluateTx = async (txCbor) => {
    const tx = CML.Transaction.from_cbor_hex(txCbor);
    const pointers = getRedeemerPointersInContextOrder(tx);
    const exUnits = await resolveStubExUnits(pointers.length);
    return pointers.map((pointer) => ({
      redeemer_tag: toProviderRedeemerTag(pointer.tag),
      redeemer_index: Number(pointer.index),
      ex_units: exUnits,
    }));
  };
  try {
    return await run();
  } finally {
    provider.evaluateTx = originalEvaluateTx;
  }
};

const compareHex = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

const getAssetNameByPolicy = (
  assets: Readonly<Record<string, bigint>>,
  policyId: string,
): string | null => {
  const entries = Object.entries(assets).filter(
    ([unit, quantity]) =>
      unit !== "lovelace" && quantity > 0n && unit.startsWith(policyId),
  );
  if (entries.length !== 1) {
    return null;
  }
  return entries[0][0].slice(56);
};

const nodeKeyEquals = (node: SDK.NodeDatum, keyHash: string): boolean =>
  node.key !== "Empty" && node.key.Key.key === keyHash;

const linkPointsTo = (node: SDK.NodeDatum, keyHash: string): boolean =>
  node.next !== "Empty" && node.next.Key.key === keyHash;

const orderedNotMemberWitness = (
  node: SDK.NodeDatum,
  keyHash: string,
): boolean => {
  const lowerBoundSatisfied =
    node.key === "Empty" || compareHex(node.key.Key.key, keyHash) < 0;
  const upperBoundSatisfied =
    node.next === "Empty" || compareHex(keyHash, node.next.Key.key) < 0;
  return lowerBoundSatisfied && upperBoundSatisfied;
};

const activeAppendAnchorWitness = (
  node: SDK.NodeDatum,
  keyHash: string,
): boolean =>
  node.next === "Empty" &&
  (node.key === "Empty" || compareHex(node.key.Key.key, keyHash) < 0);

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

const ensureScriptReferenceUTxO = (
  lucid: LucidEvolution,
  name: string,
  script: Script,
): Effect.Effect<UTxO, SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const walletUtxosRaw = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to fetch wallet UTxOs while resolving ${name} reference script`,
          cause,
        }),
    });
    const walletUtxos = yield* reconcileLiveWalletUtxos(lucid, walletUtxosRaw);
    const existingCandidates = walletUtxos
      .filter((utxo) => isSameScriptRef(utxo.scriptRef, script))
      .sort(compareOutRefs)
      .reverse();
    for (const existingCandidate of existingCandidates) {
      const existing = yield* resolveLiveWalletUtxo(lucid, existingCandidate);
      if (existing !== undefined && isSameScriptRef(existing.scriptRef, script)) {
        return existing;
      }
    }

    const operatorAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve wallet address while creating ${name} reference script`,
          cause,
        }),
    });
    const fundingWalletUtxos = walletUtxos;
    if (fundingWalletUtxos.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `No wallet UTxOs available while publishing ${name} reference script`,
          cause: "wallet-has-no-live-utxos",
        }),
      );
    }
    const unsigned = yield* Effect.tryPromise({
      try: () =>
        lucid
          .newTx()
          .pay.ToAddressWithData(
            operatorAddress,
            undefined,
            { lovelace: SCRIPT_REF_OUTPUT_LOVELACE },
            script,
          )
          .complete({
            localUPLCEval: false,
            presetWalletInputs: [...fundingWalletUtxos],
          }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build ${name} reference-script publication transaction`,
          cause,
        }),
    });
    const publicationTx = unsigned.toTransaction();
    const publicationOutputs = publicationTx.body().outputs();
    let locallyBuiltReferenceOutput:
      | Omit<UTxO, "txHash">
      | undefined = undefined;
    for (
      let outputIndex = 0;
      outputIndex < publicationOutputs.len();
      outputIndex += 1
    ) {
      const output = coreToTxOutput(publicationOutputs.get(outputIndex));
      if (
        output.address === operatorAddress &&
        output.scriptRef !== undefined &&
        isSameScriptRef(output.scriptRef, script)
      ) {
        locallyBuiltReferenceOutput = {
          outputIndex,
          address: output.address,
          assets: output.assets,
          datum: output.datum ?? undefined,
          datumHash: output.datumHash ?? undefined,
          scriptRef: output.scriptRef,
        };
        break;
      }
    }
    const txHash = yield* handleSignSubmit(lucid, unsigned);
    if (locallyBuiltReferenceOutput !== undefined) {
      const localReferenceUtxo: UTxO = {
        txHash,
        ...locallyBuiltReferenceOutput,
      };
      const liveReference = yield* Effect.either(
        resolveLiveWalletUtxo(lucid, localReferenceUtxo),
      );
      if (
        liveReference._tag === "Right" &&
        liveReference.right !== undefined &&
        isSameScriptRef(liveReference.right.scriptRef, script)
      ) {
        return liveReference.right;
      }
      if (
        liveReference._tag === "Right" &&
        liveReference.right !== undefined
      ) {
        return {
          ...liveReference.right,
          scriptRef: script,
        };
      }
      yield* Effect.logWarning(
        `Published ${name} reference script tx output could not be resolved immediately by out-ref; using locally-built output reference.`,
      );
      return {
        ...localReferenceUtxo,
        scriptRef: script,
      };
    }

    for (
      let attempt = 0;
      attempt < SCRIPT_REF_PUBLICATION_MAX_RETRIES;
      attempt += 1
    ) {
      const refreshed = yield* Effect.tryPromise({
        try: () => lucid.wallet().getUtxos(),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: `Failed to refresh wallet UTxOs after publishing ${name} reference script`,
            cause,
          }),
      });
      const createdMatchingScript = refreshed.find(
        (utxo) =>
          utxo.txHash === txHash && isSameScriptRef(utxo.scriptRef, script),
      );
      if (createdMatchingScript !== undefined) {
        const liveCreated = yield* resolveLiveWalletUtxo(lucid, createdMatchingScript);
        if (
          liveCreated !== undefined &&
          isSameScriptRef(liveCreated.scriptRef, script)
        ) {
          return liveCreated;
        }
      }
      const createdWithAnyScriptRef = refreshed
        .filter((utxo) => utxo.txHash === txHash && utxo.scriptRef !== undefined)
        .sort(compareOutRefs)
        .reverse();
      if (createdWithAnyScriptRef.length > 0) {
        const candidate = createdWithAnyScriptRef[0];
        const liveCandidate = yield* resolveLiveWalletUtxo(lucid, candidate);
        if (liveCandidate !== undefined) {
          if (!isSameScriptRef(liveCandidate.scriptRef, script)) {
            yield* Effect.logWarning(
              `Published ${name} reference script tx output scriptRef did not hash-match expected script; accepting tx-matched scriptRef output as fallback.`,
            );
          }
          return {
            ...liveCandidate,
            scriptRef: script,
          };
        }
      }
      if (attempt + 1 < SCRIPT_REF_PUBLICATION_MAX_RETRIES) {
        yield* Effect.sleep(SCRIPT_REF_PUBLICATION_RETRY_DELAY);
      }
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: `Published ${name} reference script transaction but did not find resulting reference UTxO`,
        cause: txHash,
      }),
    );
  });

const ensureActivationScriptRefs = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  readonly ActivationScriptRef[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const targets: readonly { readonly name: string; readonly script: Script }[] = [
      {
        name: "registered-operators spending",
        script: contracts.registeredOperators.spendingScript,
      },
      {
        name: "active-operators spending",
        script: contracts.activeOperators.spendingScript,
      },
      {
        name: "registered-operators minting",
        script: contracts.registeredOperators.mintingScript,
      },
      {
        name: "active-operators minting",
        script: contracts.activeOperators.mintingScript,
      },
    ];

    return yield* Effect.forEach(targets, (target) =>
      ensureScriptReferenceUTxO(
        referenceScriptsLucid,
        target.name,
        target.script,
      ).pipe(
        Effect.map((utxo) => ({ name: target.name, utxo })),
      ),
    );
  });

const resolveRegisteredMintRedeemerIndex = (
  draftTx: CML.Transaction,
  contracts: SDK.MidgardValidators,
): Effect.Effect<number, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const pointers = getRedeemerPointersInContextOrder(draftTx);
    const txInfoRedeemerIndexes = getTxInfoRedeemerIndexes(pointers);
    const mintPolicyIds = [
      contracts.registeredOperators.policyId,
      contracts.activeOperators.policyId,
    ].sort();
    const registeredMintContextIndex = BigInt(
      mintPolicyIds.indexOf(contracts.registeredOperators.policyId),
    );
    const registeredMintRedeemerContextIndex = pointers.findIndex(
      (pointer) =>
        pointer.tag === CML.RedeemerTag.Mint &&
        pointer.index === registeredMintContextIndex,
    );
    if (registeredMintRedeemerContextIndex < 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to locate registered-operators mint redeemer index in balanced draft tx",
          cause: JSON.stringify(
            pointers.map((pointer) => ({
              tag: pointer.tag,
              index: pointer.index.toString(),
            })),
          ),
        }),
      );
    }
    const registeredMintRedeemerTxInfoIndex =
      txInfoRedeemerIndexes[registeredMintRedeemerContextIndex] ?? -1;
    if (registeredMintRedeemerTxInfoIndex < 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to map registered-operators mint redeemer from context order to tx-info order",
          cause: JSON.stringify({
            registeredMintRedeemerContextIndex,
            txInfoRedeemerIndexes,
            pointers: pointers.map((pointer, contextIndex) => ({
              contextIndex,
              tag: pointer.tag,
              index: pointer.index.toString(),
            })),
          }),
        }),
      );
    }
    return registeredMintRedeemerTxInfoIndex;
  });

const deriveActivateRedeemerLayout = (
  tx: CML.Transaction,
  params: {
    readonly hubOracleRefInput: UTxO;
    readonly retiredNotMemberWitnessForActivate: NodeWithDatum;
    readonly registeredNode: NodeWithDatum;
    readonly registeredAnchor: NodeWithDatum;
    readonly activeOperatorsPolicyId: string;
    readonly activeOperatorsAddress: string;
    readonly activeNodeUnit: string;
    readonly activeAnchorNodeUnit: string;
    readonly contracts: SDK.MidgardValidators;
  },
): Effect.Effect<ActivateRedeemerLayout, SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (process.env[ACTIVATION_DEBUG_REDEEMERS_ENV] === "1") {
      yield* Effect.logInfo(
        `Activation draft redeemers: ${describeDraftRedeemers(tx)}`,
      );
    }
    const hubOracleRefInputIndex = findReferenceInputIndex(
      tx,
      params.hubOracleRefInput,
    );
    const retiredOperatorRefInputIndex = findReferenceInputIndex(
      tx,
      params.retiredNotMemberWitnessForActivate.utxo,
    );
    const registeredOperatorsRedeemerIndex = yield* resolveRegisteredMintRedeemerIndex(
      tx,
      params.contracts,
    );
    const registeredNodeInputPosition = findInputIndex(
      tx,
      params.registeredNode.utxo,
    );
    const registeredAnchorInputPosition = findInputIndex(
      tx,
      params.registeredAnchor.utxo,
    );
    const activeOperatorsInsertedNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.activeOperatorsPolicyId,
      params.activeOperatorsAddress,
      params.activeNodeUnit,
    );
    const activeOperatorsAnchorNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.activeOperatorsPolicyId,
      params.activeOperatorsAddress,
      params.activeAnchorNodeUnit,
    );
    if (
      hubOracleRefInputIndex === undefined ||
      retiredOperatorRefInputIndex === undefined ||
      registeredNodeInputPosition === undefined ||
      registeredAnchorInputPosition === undefined ||
      registeredNodeInputPosition === registeredAnchorInputPosition ||
      activeOperatorsInsertedNodeOutputIndex === undefined ||
      activeOperatorsAnchorNodeOutputIndex === undefined
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to derive activate redeemer layout from balanced draft transaction",
          cause: JSON.stringify({
            hubOracleRefInputIndex:
              hubOracleRefInputIndex?.toString() ?? "missing",
            retiredOperatorRefInputIndex:
              retiredOperatorRefInputIndex?.toString() ?? "missing",
            registeredOperatorsRedeemerIndex:
              registeredOperatorsRedeemerIndex.toString(),
            registeredNodeInputPosition:
              registeredNodeInputPosition?.toString() ?? "missing",
            registeredAnchorInputPosition:
              registeredAnchorInputPosition?.toString() ?? "missing",
            activeOperatorsInsertedNodeOutputIndex:
              activeOperatorsInsertedNodeOutputIndex?.toString() ?? "missing",
            activeOperatorsAnchorNodeOutputIndex:
              activeOperatorsAnchorNodeOutputIndex?.toString() ?? "missing",
          }),
        }),
      );
    }
    if (params.registeredNode.datum.key === "Empty") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Registered node key is unexpectedly Empty during activation",
          cause: JSON.stringify({
            registeredNodeOutRef: `${params.registeredNode.utxo.txHash}#${params.registeredNode.utxo.outputIndex.toString()}`,
          }),
        }),
      );
    }
    const operatorKeyHash = params.registeredNode.datum.key.Key.key;
    const insertedOutputDatum = getNodeDatumAtPolicyOutputIndex(
      tx,
      params.activeOperatorsPolicyId,
      activeOperatorsInsertedNodeOutputIndex,
    );
    const anchorOutputDatum = getNodeDatumAtPolicyOutputIndex(
      tx,
      params.activeOperatorsPolicyId,
      activeOperatorsAnchorNodeOutputIndex,
    );
    if (insertedOutputDatum === undefined || anchorOutputDatum === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to decode active policy output datum(s) while deriving activation layout",
          cause: JSON.stringify({
            insertedIndex: activeOperatorsInsertedNodeOutputIndex.toString(),
            anchorIndex: activeOperatorsAnchorNodeOutputIndex.toString(),
            insertedDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsInsertedNodeOutputIndex,
            ),
            anchorDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsAnchorNodeOutputIndex,
            ),
          }),
        }),
      );
    }
    const insertedMatchesOperator = nodeKeyEquals(
      insertedOutputDatum,
      operatorKeyHash,
    );
    const anchorMatchesOperator = nodeKeyEquals(anchorOutputDatum, operatorKeyHash);
    let resolvedInsertedNodeOutputIndex = activeOperatorsInsertedNodeOutputIndex;
    let resolvedAnchorNodeOutputIndex = activeOperatorsAnchorNodeOutputIndex;
    if (!insertedMatchesOperator && anchorMatchesOperator) {
      resolvedInsertedNodeOutputIndex = activeOperatorsAnchorNodeOutputIndex;
      resolvedAnchorNodeOutputIndex = activeOperatorsInsertedNodeOutputIndex;
      yield* Effect.logWarning(
        [
          "Detected swapped active output indexes while deriving activation layout;",
          " correcting inserted/anchor indexes from policy output datums.",
          `operator=${operatorKeyHash}`,
          `inserted_out=${activeOperatorsInsertedNodeOutputIndex.toString()}`,
          `anchor_out=${activeOperatorsAnchorNodeOutputIndex.toString()}`,
          `corrected_inserted_out=${resolvedInsertedNodeOutputIndex.toString()}`,
          `corrected_anchor_out=${resolvedAnchorNodeOutputIndex.toString()}`,
        ].join(" "),
      );
    } else if (!insertedMatchesOperator) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Derived inserted active output index does not point to the operator node",
          cause: JSON.stringify({
            operatorKeyHash,
            insertedIndex: activeOperatorsInsertedNodeOutputIndex.toString(),
            anchorIndex: activeOperatorsAnchorNodeOutputIndex.toString(),
            insertedDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsInsertedNodeOutputIndex,
            ),
            anchorDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsAnchorNodeOutputIndex,
            ),
          }),
        }),
      );
    }
    if (process.env[ACTIVATION_DEBUG_REDEEMERS_ENV] === "1") {
      yield* Effect.logInfo(
        `Activation draft active output datums: inserted=${describePolicyOutputDatumAtIndex(tx, params.activeOperatorsPolicyId, resolvedInsertedNodeOutputIndex)}, anchor=${describePolicyOutputDatumAtIndex(tx, params.activeOperatorsPolicyId, resolvedAnchorNodeOutputIndex)}`,
      );
    }
    const removedNodeInputIndex =
      registeredNodeInputPosition < registeredAnchorInputPosition ? 0n : 1n;
    const anchorNodeInputIndex =
      removedNodeInputIndex === 0n ? 1n : 0n;
    return {
      hubOracleRefInputIndex,
      retiredOperatorRefInputIndex,
      registeredOperatorsRedeemerIndex: BigInt(registeredOperatorsRedeemerIndex),
      removedNodeInputIndex,
      anchorNodeInputIndex,
      activeOperatorsInsertedNodeOutputIndex: resolvedInsertedNodeOutputIndex,
      activeOperatorsAnchorNodeOutputIndex: resolvedAnchorNodeOutputIndex,
    };
  });

const deriveRegisterRedeemerLayout = (
  tx: CML.Transaction,
  params: {
    readonly hubOracleRefInput: UTxO;
    readonly activeNotMemberWitness: NodeWithDatum;
    readonly retiredNotMemberWitness: NodeWithDatum;
    readonly registeredOperatorsPolicyId: string;
    readonly registeredOperatorsAddress: string;
    readonly registeredNodeUnit: string;
    readonly registeredRootNodeUnit: string;
  },
): Effect.Effect<RegisterRedeemerLayout, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const hubOracleRefInputIndex = findReferenceInputIndex(
      tx,
      params.hubOracleRefInput,
    );
    const activeOperatorRefInputIndex = findReferenceInputIndex(
      tx,
      params.activeNotMemberWitness.utxo,
    );
    const retiredOperatorRefInputIndex = findReferenceInputIndex(
      tx,
      params.retiredNotMemberWitness.utxo,
    );
    const prependedNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.registeredOperatorsPolicyId,
      params.registeredOperatorsAddress,
      params.registeredNodeUnit,
    );
    const anchorNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.registeredOperatorsPolicyId,
      params.registeredOperatorsAddress,
      params.registeredRootNodeUnit,
    );

    if (
      hubOracleRefInputIndex === undefined ||
      activeOperatorRefInputIndex === undefined ||
      retiredOperatorRefInputIndex === undefined ||
      prependedNodeOutputIndex === undefined ||
      anchorNodeOutputIndex === undefined
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to derive register redeemer layout from balanced draft transaction",
          cause: JSON.stringify({
            hubOracleRefInputIndex:
              hubOracleRefInputIndex?.toString() ?? "missing",
            activeOperatorRefInputIndex:
              activeOperatorRefInputIndex?.toString() ?? "missing",
            retiredOperatorRefInputIndex:
              retiredOperatorRefInputIndex?.toString() ?? "missing",
            prependedNodeOutputIndex:
              prependedNodeOutputIndex?.toString() ?? "missing",
            anchorNodeOutputIndex: anchorNodeOutputIndex?.toString() ?? "missing",
          }),
        }),
      );
    }

    return {
      hubOracleRefInputIndex,
      activeOperatorRefInputIndex,
      retiredOperatorRefInputIndex,
      prependedNodeOutputIndex,
      anchorNodeOutputIndex,
    };
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
): Effect.Effect<
  OperatorLifecycleTxHashes,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const usesWallClockTime = lucid.config().network === "Custom";
    const useActivationReferenceScripts = lucid.config().network !== "Custom";
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
    const activationScriptRefs = useActivationReferenceScripts
      ? yield* ensureActivationScriptRefs(referenceScriptsLucid, contracts)
      : [];
    const lifecycleScriptRefOutRefs = new Set(
      activationScriptRefs.map(({ utxo }) => utxoOutRefKey(utxo)),
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
                completeWithLocalEvaluationFallback(lucid, (options) =>
                  lucid
                    .newTx()
                    .collectFrom(
                      [existingRegisteredNode.utxo, existingRegisteredAnchor.utxo],
                      LucidData.void(),
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
                    .attach.Script(contracts.registeredOperators.spendingScript)
                    .attach.Script(contracts.registeredOperators.mintingScript)
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
      const registrationTime = usesWallClockTime
        ? alignUnixTimeMsToSlotBoundary(
            lucid,
            registerBuildTime + REGISTERED_ACTIVATION_DELAY_MS,
          )
        : registerValidTo + REGISTERED_ACTIVATION_DELAY_MS;
      const registerDatum: SDK.RegisteredOperatorDatum = {
        registrationTime,
      };
      const prependedNodeDatum: SDK.NodeDatum = {
        key: { Key: { key: operatorKeyHash } },
        next: registeredRootNode.datum.next,
        data: LucidData.castTo(registerDatum, SDK.RegisteredOperatorDatum),
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
      const prependedNodeAssets = {
        lovelace: requiredBondLovelace,
        [registeredNodeUnit]: 1n,
      };
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
          .attach.Script(contracts.registeredOperators.spendingScript)
          .attach.Script(contracts.registeredOperators.mintingScript)
          .addSignerKey(operatorKeyHash);
        if (!usesWallClockTime) {
          tx = tx.validTo(Number(registerValidTo));
        }
        return tx;
      };

      const draftRegisterUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          withStubbedProviderEvaluation(lucid, () =>
            mkRegisterTx({
              hubOracleRefInputIndex: 0n,
              activeOperatorRefInputIndex: 1n,
              retiredOperatorRefInputIndex: 2n,
              prependedNodeOutputIndex: 0n,
              anchorNodeOutputIndex: 1n,
            }).complete({
              localUPLCEval: false,
              presetWalletInputs: [...spendableWalletUtxosForRegister],
            }),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: `Failed to build draft operator registration transaction: ${cause}`,
            cause,
          }),
      });

      const draftRegisterTx = draftRegisterUnsignedTx.toTransaction();
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
      let registerLayout = yield* deriveRegisterRedeemerLayout(
        draftRegisterTx,
        layoutDerivationParams,
      );
      for (let iteration = 0; iteration < 2; iteration += 1) {
        const rebalanceDraftUnsignedTx = yield* Effect.tryPromise({
          try: () =>
            withStubbedProviderEvaluation(lucid, () =>
              mkRegisterTx(registerLayout).complete({
                localUPLCEval: false,
                presetWalletInputs: [...spendableWalletUtxosForRegister],
              }),
            ),
          catch: (cause) =>
            new SDK.LucidError({
              message:
                "Failed to build register-layout stabilization draft transaction",
              cause,
            }),
        });
        const nextLayout = yield* deriveRegisterRedeemerLayout(
          rebalanceDraftUnsignedTx.toTransaction(),
          layoutDerivationParams,
        );
        if (registerLayoutsEqual(registerLayout, nextLayout)) {
          break;
        }
        registerLayout = nextLayout;
      }
      yield* Effect.logInfo(
        `Resolved register redeemer layout: ${registerLayoutToLogString(registerLayout)}`,
      );
      let registerUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          completeWithLocalEvaluationFallback(lucid, (options) =>
            mkRegisterTx(registerLayout).complete({
              ...options,
              presetWalletInputs: [...spendableWalletUtxosForRegister],
            }),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message:
              "Failed to build operator registration transaction with resolved redeemer layout",
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
            completeWithLocalEvaluationFallback(lucid, (options) =>
              mkRegisterTx(registerLayout).complete({
                ...options,
                presetWalletInputs: [...spendableWalletUtxosForRegister],
              }),
            ),
          catch: (cause) =>
            new SDK.LucidError({
              message:
                "Failed to rebuild operator registration transaction with tx-derived redeemer layout",
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

    const registeredOperatorData = LucidData.castFrom(
      registeredNode.datum.data,
      SDK.RegisteredOperatorDatum,
    );
    const activationTime = BigInt(registeredOperatorData.registrationTime);
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
      const currentTime = BigInt(slotToUnixTimeForLucid(lucid, lucid.currentSlot()));
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
    const activationCompleteWithoutLocalEval = {
      localUPLCEval: false,
      presetWalletInputs: [...spendableWalletUtxosForActivation],
    } as const;

    const updatedRegisteredAnchorDatum: SDK.NodeDatum = {
      ...registeredAnchor.datum,
      next: registeredNode.datum.next,
    };
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
          useActivationReferenceScripts
            ? [
                ...activationScriptRefs.map(({ utxo }) => utxo),
                hubOracleRefInput,
                retiredNotMemberWitnessForActivate.utxo,
              ]
            : [hubOracleRefInput, retiredNotMemberWitnessForActivate.utxo],
        )
        .mintAssets(
          { [registeredNodeUnit]: -1n },
          registeredActivateRedeemerBuilder,
        )
        .mintAssets({ [activeNodeUnit]: 1n }, activeActivateRedeemer);
      if (!useActivationReferenceScripts) {
        tx = tx
          .attach.Script(contracts.registeredOperators.spendingScript)
          .attach.Script(contracts.activeOperators.spendingScript)
          .attach.Script(contracts.registeredOperators.mintingScript)
          .attach.Script(contracts.activeOperators.mintingScript);
      }
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

    const draftUnsignedTx = yield* Effect.tryPromise({
      try: () =>
        withStubbedProviderEvaluation(lucid, () =>
          mkActivateTx({
            hubOracleRefInputIndex: 0n,
            retiredOperatorRefInputIndex: 1n,
            registeredOperatorsRedeemerIndex: 0n,
            removedNodeInputIndex: 0n,
            anchorNodeInputIndex: 1n,
            activeOperatorsInsertedNodeOutputIndex: 0n,
            activeOperatorsAnchorNodeOutputIndex: 1n,
          }).complete(activationCompleteWithoutLocalEval),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build draft activation transaction: ${cause}`,
          cause,
        }),
    });
    let activateLayoutDraftTx = draftUnsignedTx.toTransaction();

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
    let activateLayout = yield* deriveActivateRedeemerLayout(
      activateLayoutDraftTx,
      activateLayoutParams,
    );
    for (let iteration = 0; iteration < 2; iteration += 1) {
      const rebalanceDraftUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          withStubbedProviderEvaluation(lucid, () =>
            mkActivateTx(activateLayout).complete(activationCompleteWithoutLocalEval),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message:
              "Failed to build activate-layout stabilization draft transaction",
            cause,
          }),
      });
      activateLayoutDraftTx = rebalanceDraftUnsignedTx.toTransaction();
      const nextLayout = yield* deriveActivateRedeemerLayout(
        activateLayoutDraftTx,
        activateLayoutParams,
      );
      if (activateLayoutsEqual(activateLayout, nextLayout)) {
        break;
      }
      activateLayout = nextLayout;
    }
    yield* Effect.logInfo(
      `Resolved activate redeemer layout: ${activateLayoutToLogString(activateLayout)}`,
    );
    yield* Effect.logInfo(
      `Using active-operator datum encoding for activation: ${activeOperatorDatumEncoding}`,
    );

    let activationUnsignedTx = yield* Effect.tryPromise({
      try: () =>
        completeWithLocalEvaluationFallback(lucid, (options) =>
          mkActivateTx(activateLayout).complete(activationCompleteOptions(options)),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build activation transaction: ${cause}`,
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
          completeWithLocalEvaluationFallback(lucid, (options) =>
            mkActivateTx(activateLayout).complete(activationCompleteOptions(options)),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message:
              "Failed to rebuild activation transaction with tx-derived layout",
            cause,
          }),
      });
    }
    const activateUnsignedDraftTx = activationUnsignedTx.toTransaction();
    if (process.env[ACTIVATION_DEBUG_REDEEMERS_ENV] === "1") {
      yield* Effect.logInfo(
        `Activation submit tx redeemers: ${describeDraftRedeemers(activateUnsignedDraftTx)}`,
      );
      yield* Effect.logInfo(
        `Activation submit tx active output datums: inserted=${describePolicyOutputDatumAtIndex(
          activateUnsignedDraftTx,
          contracts.activeOperators.policyId,
          activateLayout.activeOperatorsInsertedNodeOutputIndex,
        )}, anchor=${describePolicyOutputDatumAtIndex(
          activateUnsignedDraftTx,
          contracts.activeOperators.policyId,
          activateLayout.activeOperatorsAnchorNodeOutputIndex,
        )}`,
      );
    }
    if (process.env[ACTIVATION_DEBUG_INPUTS_ENV] === "1") {
      const [
        missingSpendingInputs,
        missingReferenceInputs,
        missingCollateralInputs,
      ] = yield* Effect.all([
        resolveMissingSpendingInputs(lucid, activateUnsignedDraftTx),
        resolveMissingReferenceInputs(lucid, activateUnsignedDraftTx),
        resolveMissingCollateralInputs(lucid, activateUnsignedDraftTx),
      ]);
      if (missingSpendingInputs.length > 0) {
        yield* Effect.logWarning(
          `Activation submit tx has missing spending inputs before submission: ${missingSpendingInputs.join(",")}`,
        );
      }
      if (missingReferenceInputs.length > 0) {
        yield* Effect.logWarning(
          `Activation submit tx has missing reference inputs before submission: ${missingReferenceInputs.join(",")}`,
        );
      }
      if (missingCollateralInputs.length > 0) {
        yield* Effect.logWarning(
          `Activation submit tx has missing collateral inputs before submission: ${missingCollateralInputs.join(",")}`,
        );
      }
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
