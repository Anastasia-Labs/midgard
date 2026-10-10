import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type IntentJournal, openPlan } from "../../services/intent-journal.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../../transactions/reference-scripts.js";
import {
  type TxSignError,
  type TxSubmitError,
} from "../../transactions/utils.js";
import { readSelectedWalletView } from "../../transactions/utils.wallet-view.js";
import { ensureSchedulerAlignedForCommit } from "./scheduler-refresh.ensure-scheduler-aligned-for-commit.js";
import {
  fetchActiveOperatorUtxos,
  fetchFreshActiveOperatorInputForCommit,
  getOperatorKeyHash,
  requireExistingSchedulerWitnessUtxo,
} from "./scheduler-refresh.fetch-fresh-active-operator-input-for-commit.js";
import {
  type CommitTimingDueWork,
  type RealStateQueueWitnessContext,
} from "./scheduler-refresh.scheduler-refresh-due-work-from-no-inline-submit-defer.js";

export const fetchRealStateQueueWitnessContext = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  alignedEndTime: number,
  referenceScriptsAddress?: string,
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
  allowSchedulerRefresh: boolean = true,
): Effect.Effect<
  RealStateQueueWitnessContext | CommitTimingDueWork,
  SDK.StateQueueError | TxSignError | TxSubmitError,
  IntentJournal
> =>
  Effect.gen(function* () {
    // S5: the plan opens before the reads a scheduler refresh rests on.
    const plan = yield* openPlan;
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const resolvedReferenceScripts =
      referenceScriptsAddress === undefined
        ? []
        : yield* fetchReferenceScriptUtxosProgram(
            lucid,
            referenceScriptsAddress,
            [
              {
                name: "scheduler spending",
                script: contracts.scheduler.spendingScript,
              },
              {
                name: "active-operators spending",
                script: contracts.activeOperators.spendingScript,
              },
              {
                name: "state-queue spending",
                script: contracts.stateQueue.spendingScript,
              },
              {
                name: "state-queue minting",
                script: contracts.stateQueue.mintingScript,
              },
              {
                name: "state-queue commit withdrawal",
                script: contracts.stateQueue.yields.commit.withdrawalScript,
              },
            ],
            contracts.referenceScriptAuth,
          );
    const optionalReferenceScript = (name: string): UTxO | undefined =>
      referenceScriptsAddress === undefined
        ? undefined
        : referenceScriptByName(resolvedReferenceScripts, name);
    const schedulerSpendingScriptRef =
      optionalReferenceScript("scheduler spending");
    const activeOperatorsSpendingScriptRef = optionalReferenceScript(
      "active-operators spending",
    );
    const stateQueueSpendingScriptRef = optionalReferenceScript(
      "state-queue spending",
    );
    const stateQueueMintingScriptRef = optionalReferenceScript(
      "state-queue minting",
    );
    const stateQueueCommitYieldScriptRef = referenceScriptByName(
      resolvedReferenceScripts,
      "state-queue commit withdrawal",
    );
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const activeOperatorUtxosForRefresh = yield* fetchActiveOperatorUtxos(
      lucid,
      contracts,
      "Failed to fetch active-operators UTxOs for state_queue commit",
    );
    const registeredOperatorUtxos = (yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      contracts.registeredOperators.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch registered-operators UTxOs for scheduler refresh",
            cause,
          }),
      ),
    )).map((beacon) => beacon.utxo);

    const schedulerUtxos = (yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.scheduler.spendingScriptAddress,
      contracts.scheduler.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Failed to fetch scheduler UTxOs for state_queue commit",
            cause,
          }),
      ),
    )).map((beacon) => beacon.utxo);
    const initialSchedulerRefInput = yield* requireExistingSchedulerWitnessUtxo(
      schedulerUtxos,
      schedulerWitnessUnit,
    );
    const schedulerRefInput = yield* ensureSchedulerAlignedForCommit(
      lucid,
      contracts,
      operatorKeyHash,
      initialSchedulerRefInput,
      activeOperatorUtxosForRefresh,
      registeredOperatorUtxos,
      alignedEndTime,
      schedulerWitnessUnit,
      plan,
      schedulerSpendingScriptRef,
      submitSlotSnapshot,
      allowSchedulerRefresh,
    );
    if ("dueWork" in schedulerRefInput) {
      return schedulerRefInput;
    }
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
    const hubOracleWitnessUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to fetch hub-oracle UTxOs for state_queue commit witness",
          cause,
        }),
    });
    if (hubOracleWitnessUtxos.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to resolve unique hub-oracle UTxO for state_queue commit witness",
          cause: `expected=1,found=${hubOracleWitnessUtxos.length},address=${hubOracleAddress},unit=${hubOracleUnit}`,
        }),
      );
    }
    const hubOracleRefInput = hubOracleWitnessUtxos[0];
    const correctionLockRefInput = yield* SDK.fetchCorrectionLockUTxOProgram(
      lucid,
      {
        correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
        hubOraclePolicyId: contracts.hubOracle.policyId,
      },
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch authenticated correction-lock witness for state_queue commit",
            cause,
          }),
      ),
    );

    // Read after scheduler alignment: a refresh submitted above is a live
    // own intent (or landed) by now, and the view follows it.
    const operatorWalletView = yield* readSelectedWalletView(lucid).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: `Failed to read the operator wallet view for the state_queue commit: ${cause.message}`,
            cause,
          }),
      ),
    );
    const activeOperatorInput = yield* fetchFreshActiveOperatorInputForCommit(
      lucid,
      contracts,
      operatorKeyHash,
      operatorWalletView.held,
    );

    return {
      operatorKeyHash,
      schedulerRefInput: schedulerRefInput.schedulerRefInput,
      hubOracleRefInput,
      correctionLockRefInput,
      activeOperatorInput,
      activeOperatorsSpendingScript: contracts.activeOperators.spendingScript,
      activeOperatorsSpendingScriptRef,
      stateQueueSpendingScriptRef,
      stateQueueMintingScriptRef,
      stateQueueCommitYieldScriptRef,
      operatorWalletInputs: operatorWalletView.utxos,
    };
  });
