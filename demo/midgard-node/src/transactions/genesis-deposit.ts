import * as SDK from "@al-ft/midgard-sdk";
import {
  Address,
  LucidEvolution,
  Script,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect, } from "effect";
import { AuthenticatedValidator, Globals } from "@/services/index.js";

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param depositAuthValidator - The configuration of the deposit validator.
 * @returns An Effect that commits a genesis deposit on L1.
 */
export const buildAndSubmitGenesisDeposit = (
  lucid: LucidEvolution,
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
