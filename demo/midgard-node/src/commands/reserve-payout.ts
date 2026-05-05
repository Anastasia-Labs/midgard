import * as SDK from "@al-ft/midgard-sdk";
import {
  Data as LucidData,
  type Assets,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  type ReferenceScriptTarget,
} from "@/transactions/reference-scripts.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  submitAbsorbConfirmedDepositToReserveProgram,
  submitAddReserveFundsToPayoutProgram,
  submitConcludePayoutProgram,
  submitInitializePayoutProgram,
  type ReservePayoutReferenceScripts,
  valueToAssets,
} from "@/transactions/reserve-payout.js";
import {
  type EventSettlementProofResolution,
  resolveEventSettlementProofProgram,
} from "@/commands/event-settlement-proof.js";
import {
  addressDataToBech32,
  formatJson,
  parseEventId,
} from "@/commands/withdrawal-utils.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { Option } from "effect";

export type EventIdConfig = {
  readonly eventId: string;
};

export type PayoutCommandResult = {
  readonly txHash: string;
  readonly eventId: string;
  readonly details: Record<string, unknown>;
};

const LOVELACE_UNIT = "lovelace";

const normalizeAssets = (assets: Readonly<Assets>): Assets => {
  const normalized: Record<string, bigint> = {};
  for (const [unit, quantity] of Object.entries(assets)) {
    if (quantity !== 0n) {
      normalized[unit] = quantity;
    }
  }
  return normalized as Assets;
};

const subtractAssets = (
  left: Readonly<Assets>,
  right: Readonly<Assets>,
): Assets => {
  const result: Record<string, bigint> = { ...left };
  for (const [unit, quantity] of Object.entries(right)) {
    result[unit] = (result[unit] ?? 0n) - quantity;
  }
  return normalizeAssets(result as Assets);
};

const assetsEqual = (
  left: Readonly<Assets>,
  right: Readonly<Assets>,
): boolean => {
  const leftNormalized = normalizeAssets(left);
  const rightNormalized = normalizeAssets(right);
  const units = new Set([
    ...Object.keys(leftNormalized),
    ...Object.keys(rightNormalized),
  ]);
  for (const unit of units) {
    if ((leftNormalized[unit] ?? 0n) !== (rightNormalized[unit] ?? 0n)) {
      return false;
    }
  }
  return true;
};

const contributesToNeed = (
  reserveAssets: Readonly<Assets>,
  neededAssets: Readonly<Assets>,
): boolean =>
  Object.entries(neededAssets).some(
    ([unit, needed]) => needed > 0n && (reserveAssets[unit] ?? 0n) > 0n,
  );

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const fetchReferenceScripts = (
  targets: readonly ReferenceScriptTarget[],
): Effect.Effect<ReservePayoutReferenceScripts, SDK.StateQueueError, Lucid> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const resolved = yield* fetchReferenceScriptUtxosProgram(
      lucidService.api,
      lucidService.referenceScriptsAddress,
      targets,
    );
    const byName = (name: string): UTxO =>
      referenceScriptByName(resolved, name);
    return {
      depositMinting: resolved.some((entry) => entry.name === "deposit minting")
        ? byName("deposit minting")
        : undefined,
      depositSpending: resolved.some(
        (entry) => entry.name === "deposit spending",
      )
        ? byName("deposit spending")
        : undefined,
      withdrawalMinting: resolved.some(
        (entry) => entry.name === "withdrawal minting",
      )
        ? byName("withdrawal minting")
        : undefined,
      withdrawalSpending: resolved.some(
        (entry) => entry.name === "withdrawal spending",
      )
        ? byName("withdrawal spending")
        : undefined,
      reserveSpending: resolved.some(
        (entry) => entry.name === "reserve spending",
      )
        ? byName("reserve spending")
        : undefined,
      payoutSpending: resolved.some((entry) => entry.name === "payout spending")
        ? byName("payout spending")
        : undefined,
      payoutMinting: resolved.some((entry) => entry.name === "payout minting")
        ? byName("payout minting")
        : undefined,
    };
  });

const fetchDepositUtxoByEventId = (
  eventId: Buffer,
): Effect.Effect<
  SDK.DepositUTxO,
  SDK.LucidError | Error,
  Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const deposits = yield* SDK.fetchDepositUTxOsProgram(lucid, {
      eventAddress: contracts.deposit.spendingScriptAddress,
      eventPolicyId: contracts.deposit.policyId,
    });
    const match = deposits.find((deposit) =>
      Buffer.from(deposit.idCbor).equals(eventId),
    );
    if (match === undefined) {
      return yield* Effect.fail(
        new Error(
          `Deposit UTxO for event ${eventId.toString("hex")} is not present on L1.`,
        ),
      );
    }
    return match;
  });

const fetchWithdrawalUtxoByEventId = (
  eventId: Buffer,
): Effect.Effect<
  SDK.WithdrawalUTxO,
  SDK.LucidError | Error,
  Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const withdrawals = yield* SDK.fetchWithdrawalUTxOsProgram(lucid, {
      eventAddress: contracts.withdrawal.spendingScriptAddress,
      eventPolicyId: contracts.withdrawal.policyId,
    });
    const match = withdrawals.find((withdrawal) =>
      Buffer.from(withdrawal.idCbor).equals(eventId),
    );
    if (match === undefined) {
      return yield* Effect.fail(
        new Error(
          `Withdrawal UTxO for event ${eventId.toString("hex")} is not present on L1.`,
        ),
      );
    }
    return match;
  });

const requireDepositResolution = (
  resolution: EventSettlementProofResolution,
): Extract<EventSettlementProofResolution, { readonly kind: "deposit" }> => {
  if (resolution.kind !== "deposit") {
    throw new Error("Expected deposit event settlement proof resolution.");
  }
  return resolution;
};

const requireWithdrawalResolution = (
  resolution: EventSettlementProofResolution,
): Extract<EventSettlementProofResolution, { readonly kind: "withdrawal" }> => {
  if (resolution.kind !== "withdrawal") {
    throw new Error("Expected withdrawal event settlement proof resolution.");
  }
  return resolution;
};

export const absorbConfirmedDepositToReserveProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--deposit-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    yield* lucidService.switchToOperatorsMainWallet;
    const resolution = requireDepositResolution(
      yield* resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId,
      }),
    );
    const deposit = yield* fetchDepositUtxoByEventId(eventId);
    const membershipProofWithdrawal = {
      script: loadPhasMembershipWithdrawalScript(),
    };
    const refs = yield* fetchReferenceScripts([
      { name: "deposit minting", script: contracts.deposit.mintingScript },
      { name: "deposit spending", script: contracts.deposit.spendingScript },
    ]);
    const txHash = yield* submitAbsorbConfirmedDepositToReserveProgram(
      lucidService.api,
      contracts,
      {
        deposit,
        settlementRefInput: resolution.settlementRefInput,
        membershipProof: resolution.proof,
        membershipProofWithdrawal,
        referenceScripts: refs,
      },
    );
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        settlementOutRef: outRef(resolution.settlementRefInput),
        depositOutRef: outRef(deposit.utxo),
        depositAssets: deposit.utxo.assets,
      },
    };
  });

export const initializePayoutProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    yield* lucidService.switchToOperatorsMainWallet;
    const resolution = requireWithdrawalResolution(
      yield* resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId,
      }),
    );
    if (resolution.validity !== "WithdrawalIsValid") {
      return yield* Effect.fail(
        new Error(
          `Withdrawal ${eventId.toString("hex")} is not valid; validity=${resolution.validity ?? "null"}.`,
        ),
      );
    }
    const withdrawal = yield* fetchWithdrawalUtxoByEventId(eventId);
    const membershipProofWithdrawal = {
      script: loadPhasMembershipWithdrawalScript(),
    };
    const refs = yield* fetchReferenceScripts([
      {
        name: "withdrawal minting",
        script: contracts.withdrawal.mintingScript,
      },
      {
        name: "withdrawal spending",
        script: contracts.withdrawal.spendingScript,
      },
      { name: "payout minting", script: contracts.payout.mintingScript },
    ]);
    const txHash = yield* submitInitializePayoutProgram(
      lucidService.api,
      contracts,
      {
        withdrawal,
        settlementRefInput: resolution.settlementRefInput,
        membershipProof: resolution.proof,
        membershipProofWithdrawal,
        referenceScripts: refs,
      },
    );
    const payoutUnit = toUnit(contracts.payout.policyId, withdrawal.assetName);
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        settlementOutRef: outRef(resolution.settlementRefInput),
        withdrawalOutRef: outRef(withdrawal.utxo),
        payoutUnit,
      },
    };
  });

const payoutUnitFromWithdrawalEventId = (
  eventId: Buffer,
): Effect.Effect<string, Error, Database | MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const maybeEntry = yield* WithdrawalsDB.retrieveByEventId(eventId);
    if (Option.isNone(maybeEntry)) {
      return yield* Effect.fail(
        new Error(`Withdrawal event ${eventId.toString("hex")} not found.`),
      );
    }
    return toUnit(
      contracts.payout.policyId,
      maybeEntry.value[WithdrawalsDB.Columns.ASSET_NAME].toString("hex"),
    );
  });

const fetchPayoutByWithdrawalEvent = (
  eventId: Buffer,
): Effect.Effect<
  UTxO,
  SDK.LucidError | Error,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const payoutUnit = yield* payoutUnitFromWithdrawalEventId(eventId);
    const payouts = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.payout.spendingScriptAddress,
          payoutUnit,
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch payout UTxOs",
          cause,
        }),
    });
    const matches = payouts.filter(
      (utxo) => (utxo.assets[payoutUnit] ?? 0n) === 1n,
    );
    if (matches.length !== 1) {
      return yield* Effect.fail(
        new Error(
          `Expected exactly one payout UTxO for ${payoutUnit}, found ${matches.length.toString()}.`,
        ),
      );
    }
    return matches[0]!;
  });

const decodePayoutDatum = (payout: UTxO): SDK.PayoutDatum => {
  if (payout.datum == null) {
    throw new Error(`Payout UTxO ${outRef(payout)} has no inline datum.`);
  }
  return LucidData.from(payout.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

export const addReserveFundsToPayoutProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    yield* lucidService.switchToOperatorsMainWallet;
    const payout = yield* fetchPayoutByWithdrawalEvent(eventId);
    const payoutDatum = decodePayoutDatum(payout);
    const payoutUnit = Object.keys(payout.assets).find(
      (unit) =>
        unit.startsWith(contracts.payout.policyId) &&
        payout.assets[unit] === 1n,
    );
    if (payoutUnit === undefined) {
      return yield* Effect.fail(
        new Error("Payout UTxO is missing payout NFT."),
      );
    }
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentAssets = normalizeAssets({
      ...payout.assets,
      [payoutUnit]: (payout.assets[payoutUnit] ?? 0n) - 1n,
    });
    const remaining = subtractAssets(targetAssets, currentAssets);
    const reserveUtxos = yield* Effect.tryPromise({
      try: () =>
        lucidService.api.utxosAt(contracts.reserve.spendingScriptAddress),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch reserve UTxOs",
          cause,
        }),
    });
    const reserve = reserveUtxos.find((utxo) =>
      contributesToNeed(utxo.assets, remaining),
    );
    if (reserve === undefined) {
      return yield* Effect.fail(
        new Error(
          "No reserve UTxO contributes to the payout's remaining target.",
        ),
      );
    }
    const refs = yield* fetchReferenceScripts([
      { name: "reserve spending", script: contracts.reserve.spendingScript },
      { name: "payout spending", script: contracts.payout.spendingScript },
    ]);
    const txHash = yield* submitAddReserveFundsToPayoutProgram(
      lucidService.api,
      contracts,
      {
        payoutInput: payout,
        reserveInput: reserve,
        referenceScripts: refs,
      },
    );
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        payoutOutRef: outRef(payout),
        reserveOutRef: outRef(reserve),
        targetAssets,
        currentAssets,
        remainingAssetsBeforeFunding: remaining,
      },
    };
  });

export const concludePayoutProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    yield* lucidService.switchToOperatorsMainWallet;
    const payout = yield* fetchPayoutByWithdrawalEvent(eventId);
    const payoutDatum = decodePayoutDatum(payout);
    const payoutUnit = Object.keys(payout.assets).find(
      (unit) =>
        unit.startsWith(contracts.payout.policyId) &&
        payout.assets[unit] === 1n,
    );
    if (payoutUnit === undefined) {
      return yield* Effect.fail(
        new Error("Payout UTxO is missing payout NFT."),
      );
    }
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentAssets = normalizeAssets({
      ...payout.assets,
      [payoutUnit]: (payout.assets[payoutUnit] ?? 0n) - 1n,
    });
    if (!assetsEqual(currentAssets, targetAssets)) {
      return yield* Effect.fail(
        new Error(
          `Payout is not exactly funded. target=${formatJson(targetAssets)}, current=${formatJson(currentAssets)}`,
        ),
      );
    }
    const refs = yield* fetchReferenceScripts([
      { name: "payout spending", script: contracts.payout.spendingScript },
      { name: "payout minting", script: contracts.payout.mintingScript },
    ]);
    const txHash = yield* submitConcludePayoutProgram(
      lucidService.api,
      contracts,
      {
        payoutInput: payout,
        referenceScripts: refs,
      },
    );
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        payoutOutRef: outRef(payout),
        payoutUnit,
        l1Address: addressDataToBech32(
          nodeConfig.NETWORK,
          payoutDatum.l1_address,
        ),
        paidAssets: targetAssets,
      },
    };
  });

export const formatPayoutCommandResult = (
  result: PayoutCommandResult,
): string => formatJson(result);

export const __reservePayoutCommandsTest = {
  loadPhasMembershipWithdrawalScript,
  normalizeAssets,
  subtractAssets,
};
