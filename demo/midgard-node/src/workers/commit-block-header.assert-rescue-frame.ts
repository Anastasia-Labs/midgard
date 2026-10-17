import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { TxAdmissionsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { Columns as TxColumns } from "../database/utils/tx.js";
import { unixTimeToSlotForConfig } from "../lucid-time.js";
import { emptyRootHexProgram, type processMpfs } from "../mpf/index.js";
import type { Lucid } from "../services/index.js";
import { CommitWorkerInvariantError } from "./commit-block-header.pending-user-event-counts-up-to.js";
import {
  resolveDepositsRoot,
  resolveForcedTransactionsRoot,
  resolveWithdrawalsRoot,
} from "./commit-block-header/event-roots.js";
import {
  hashBlockHeaderLocal,
  localizeSdkEffect,
  updateLatestBlocksDatumAndGetTheNewHeaderLocal,
} from "./commit-block-header/state-queue.js";
import {
  assertPreSubmitDaPayloadSize,
  daProgramMaterialFromSidecars,
  forcedProgramMaterialSidecars,
} from "./commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import { makeEventCommitments } from "./commit-block-header/transition-commitments.js";
import { selectCommitRoots } from "./utils/commit-block-planner.js";

/** An exact rescue never proceeds on the synthetic widest-header estimate.
 * Derive the actual header with the same SDK/count/root helpers as submission,
 * then run the unconditional hard inner/envelope/request frame check here.
 * Submission independently repeats the guard against its live admitted tail.
 * This constructs no transaction and signs/publishes nothing. */
export const assertExactCommitRescueDaFrame = (input: {
  readonly processed: Effect.Effect.Success<ReturnType<typeof processMpfs>>;
  readonly parent: SDK.StateQueueUTxO | undefined;
  readonly lucid: Lucid;
  readonly profile: MidgardConsensusProfile;
  readonly endTimeMs: number;
  readonly blockTimeMs: number;
  readonly slotConfig: L1SlotConfig;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
}) =>
  Effect.gen(function* () {
    if (input.parent === undefined)
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Exact DA rescue requires an admitted confirmed commit parent",
        }),
      );
    const built = input.processed;
    const emptyRoot = yield* emptyRootHexProgram;
    const roots = selectCommitRoots({
      hasTxRequests: built.processedMempoolTxs.length > 0,
      computedUtxoRoot: built.utxoRoot,
      computedTxRoot: built.txRoot,
      emptyRoot,
    });
    const deposit = yield* resolveDepositsRoot(built.includedDepositEntries);
    const forced = yield* resolveForcedTransactionsRoot(
      built.includedForcedTransactionEntries,
      input.profile,
    );
    const withdrawal = yield* resolveWithdrawalsRoot(
      built.includedWithdrawalEntries,
    );
    const depositsRoot = Option.getOrElse(deposit, () => emptyRoot);
    const forcedTransactionsRoot = Option.getOrElse(forced, () => emptyRoot);
    const withdrawalsRoot = Option.getOrElse(withdrawal, () => emptyRoot);
    const commitments = yield* localizeSdkEffect<
      SDK.HeaderTransitionCommitments,
      SDK.HeaderTransitionCommitmentsError
    >(
      makeEventCommitments(
        {
          withdrawalsRoot,
          forcedTransactionsRoot,
          transactionsRoot: roots.txRoot,
          depositsRoot,
          transitionTraceRoot: built.transitionTraceRoot,
          eventToStepRoot: built.eventToStepRoot,
        },
        {
          withdrawalCount: BigInt(built.includedWithdrawalEntries.length),
          forcedTransactionCount: BigInt(
            built.includedForcedTransactionEntries.length,
          ),
          l2TransactionCount: BigInt(built.processedMempoolTxs.length),
          depositCount: BigInt(built.includedDepositEntries.length),
          transitionStepCount: BigInt(built.transitionStepCount),
        },
        {
          validationTracesRoot: built.validationTracesRoot,
          validationTraceCount: BigInt(built.validationTraceCount),
        },
      ),
    );
    yield* input.lucid.switchToOperatorsMainWallet;
    const { header } = yield* updateLatestBlocksDatumAndGetTheNewHeaderLocal(
      input.lucid.api,
      input.parent.datum,
      roots.utxoRoot,
      roots.txRoot,
      depositsRoot,
      withdrawalsRoot,
      commitments,
      BigInt(input.endTimeMs),
      {
        blockSlot: BigInt(
          unixTimeToSlotForConfig(input.blockTimeMs, input.slotConfig),
        ),
        expectedNetworkId: input.expectedNetworkId,
        minFeeA: input.minFeeA,
        minFeeB: input.minFeeB,
      },
    );
    const headerHash = yield* hashBlockHeaderLocal(header);
    const sidecars = yield* TxAdmissionsDB.retrieveProgramMaterialSidecars(
      built.processedMempoolTxs.map((row) => row[TxColumns.TX_ID]),
    );
    if (sidecars.length !== built.processedMempoolTxs.length)
      return yield* Effect.fail(
        new DatabaseError({
          table: TxAdmissionsDB.payloadTableName,
          message: "Exact rescue lacks canonical ordinary program material",
          cause: "missing-sidecar",
        }),
      );
    const cekProgramMaterial = yield* Effect.try({
      try: () =>
        daProgramMaterialFromSidecars([
          ...sidecars.map((row) => row.sidecarCbor),
          ...forcedProgramMaterialSidecars(
            built.includedForcedTransactionEntries,
          ),
        ]),
      catch: (cause) =>
        new DatabaseError({
          table: TxAdmissionsDB.payloadTableName,
          message: "Exact rescue has conflicting program material",
          cause,
        }),
    });
    return yield* assertPreSubmitDaPayloadSize({
      headerHash,
      header,
      includedDepositEntries: built.includedDepositEntries,
      includedForcedTransactionEntries: built.includedForcedTransactionEntries,
      includedWithdrawalEntries: built.includedWithdrawalEntries,
      processedMempoolTxs: built.processedMempoolTxs,
      transitionTraceMembers: built.transitionTraceMembers,
      eventToStepMembers: built.eventToStepMembers,
      validationTraceMembers: built.validationTraceMembers,
      cekProgramMaterial,
    });
  });
