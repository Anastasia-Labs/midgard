import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsEqual,
  mergeReferenceScripts,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "@al-ft/midgard-sdk";
import {
  type Assets,
  Data as LucidData,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { formatJson, parseEventId } from "@/commands/command-utils.js";
import {
  type EventSettlementProofResolution,
  resolveEventSettlementProofProgram,
} from "@/commands/event-settlement-proof.js";
import { addressDataToBech32 } from "@/commands/withdrawal-utils.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  type ReferenceScriptTarget,
} from "@/transactions/reference-scripts.js";
import {
  type ReservePayoutReferenceScripts,
  submitAbsorbConfirmedDepositToReserveProgram,
  submitAddReserveFundsToPayoutProgram,
  submitConcludePayoutProgram,
  submitInitializePayoutProgram,
} from "@/transactions/reserve-payout.js";
import { outRefLabel } from "@/tx-context.js";

export type EventIdConfig = {
  readonly eventId: string;
};

export type PayoutCommandResult = {
  readonly txHash: string;
  readonly eventId: string;
  readonly details: Record<string, unknown>;
};

type PayoutByWithdrawalEvent = {
  readonly payout: UTxO;
  readonly payoutUnit: string;
};

const contributesToNeed = (
  reserveAssets: Readonly<Assets>,
  neededAssets: Readonly<Assets>,
): boolean =>
  Object.entries(neededAssets).some(
    ([unit, needed]) => needed > 0n && (reserveAssets[unit] ?? 0n) > 0n,
  );

const fetchReferenceScripts = (
  targets: readonly ReferenceScriptTarget[],
): Effect.Effect<
  ReservePayoutReferenceScripts,
  SDK.StateQueueError,
  Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const resolved = yield* fetchReferenceScriptUtxosProgram(
      lucidService.api,
      lucidService.referenceScriptsAddress,
      targets,
      contracts.referenceScriptAuth,
    );
    return mergeReferenceScripts(undefined, resolved);
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

const requireResolution = <Kind extends EventSettlementProofResolution["kind"]>(
  resolution: EventSettlementProofResolution,
  kind: Kind,
): Extract<EventSettlementProofResolution, { readonly kind: Kind }> => {
  if (resolution.kind !== kind) {
    throw new Error(`Expected ${kind} event settlement proof resolution.`);
  }
  return resolution as Extract<
    EventSettlementProofResolution,
    { readonly kind: Kind }
  >;
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
    const resolution = requireResolution(
      yield* resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId,
      }),
      "deposit",
    );
    const deposit = yield* fetchDepositUtxoByEventId(eventId);
    const membershipProofWithdrawal = {
      script: loadPhasMembershipWithdrawalScript(),
    };
    const refs = yield* fetchReferenceScripts([
      { name: "deposit minting", script: contracts.deposit.mintingScript },
      { name: "deposit spending", script: contracts.deposit.spendingScript },
      {
        name: "membership proof withdrawal",
        script: membershipProofWithdrawal.script,
      },
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
        settlementOutRef: outRefLabel(resolution.settlementRefInput),
        depositOutRef: outRefLabel(deposit.utxo),
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
    const resolution = requireResolution(
      yield* resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId,
      }),
      "withdrawal",
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
      {
        name: "membership proof withdrawal",
        script: membershipProofWithdrawal.script,
      },
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
        settlementOutRef: outRefLabel(resolution.settlementRefInput),
        withdrawalOutRef: outRefLabel(withdrawal.utxo),
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
  PayoutByWithdrawalEvent,
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
    return { payout: matches[0]!, payoutUnit };
  });

const decodePayoutDatum = (payout: UTxO): SDK.PayoutDatum => {
  if (payout.datum == null) {
    throw new Error(`Payout UTxO ${outRefLabel(payout)} has no inline datum.`);
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
    const { payout, payoutUnit } = yield* fetchPayoutByWithdrawalEvent(eventId);
    const payoutDatum = decodePayoutDatum(payout);
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentAssets = removeAssetUnit(payout.assets, payoutUnit, 1n);
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
        payoutOutRef: outRefLabel(payout),
        reserveOutRef: outRefLabel(reserve),
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
    const { payout, payoutUnit } = yield* fetchPayoutByWithdrawalEvent(eventId);
    const payoutDatum = decodePayoutDatum(payout);
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentAssets = removeAssetUnit(payout.assets, payoutUnit, 1n);
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
        payoutOutRef: outRefLabel(payout),
        payoutUnit,
        l1Address: addressDataToBech32(
          nodeConfig.NETWORK,
          payoutDatum.l1_address,
        ),
        paidAssets: targetAssets,
      },
    };
  });
