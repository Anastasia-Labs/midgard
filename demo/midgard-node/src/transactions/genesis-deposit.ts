import { BlocksDB, ConfirmedLedgerDB } from "@/database/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Address,
  LucidEvolution,
  Script,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { DateTime, Effect, Metric, Ref } from "effect";
import {
  TxConfirmError,
  fetchFirstBlockTxs,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "./utils.js";
import { Entry as LedgerEntry } from "@/database/utils/ledger.js";
import { DatabaseError } from "@/database/utils/common.js";
import { breakDownTx } from "@/utils.js";
import { AuthenticatedValidator, Database, Globals } from "@/services/index.js";


/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param spendScript - State queue's spending script.
 * @param mintScript - State queue's minting script.
 * @returns An Effect that resolves when the merge transaction is built and
 *          submitted.
 */
export const buildAndSubmitGenesisDeposit = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  depositAuthValidator: AuthenticatedValidator,
): Effect.Effect<
  void,
  Error,
  Globals
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const depositParams : SDK.TxBuilder.Deposit.DepositParams =
    ({
        depositScriptAddress: depositAuthValidator.spendScriptAddress,
        mintingPolicy: depositAuthValidator.mintScript,
        policyId: depositAuthValidator.policyId,
        depositInfo: ({
            l2Address: "TODO: l2Address",
            l2Datum: "TODO: l2Datum",
        }),
        inclusionTime: BigInt(Date.now()) // TODO: add eventWaitDuration
    })
    yield* SDK.Endpoints.commitDepositsProgram(lucid, depositParams)
  });
