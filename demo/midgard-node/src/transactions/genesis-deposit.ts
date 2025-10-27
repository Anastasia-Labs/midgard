import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { AlwaysSucceedsContract, Lucid } from "@/services/index.js";
import { GenesisDepositError } from "./utils.js";

/**
 * Build and submit the merge transaction using depositAuthValidator.
 * @returns An Effect that commits a genesis deposit on L1.
 */
export const buildAndSubmitGenesisDeposit = (
): Effect.Effect<
  void,
  SDK.Utils.LucidError | GenesisDepositError,
  SDK.Services.Parameters |
  AlwaysSucceedsContract |
  Lucid
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const { depositAuthValidator } = yield* AlwaysSucceedsContract

    const onchainParameters = yield* SDK.Services.Parameters;
    const depositParams : SDK.TxBuilder.Deposit.DepositParams =
    ({
        depositScriptAddress: depositAuthValidator.spendScriptAddress,
        mintingPolicy: depositAuthValidator.mintScript,
        policyId: depositAuthValidator.policyId,
        depositInfo: ({
            l2Address: "TODO: l2Address",
            l2Datum: "TODO: l2Datum",
        }),
        inclusionTime: BigInt(Date.now() + onchainParameters.event_wait_duration)
    })

    const onHashingFailure = (err: SDK.Utils.HashingError) =>
      Effect.gen(function* () {
        yield* Effect.logError(`Hashing error: ${err}`);
        yield* Effect.fail(
          new GenesisDepositError({
            message: "failed to submit genesis deposits",
            cause: err,
          }),
        );
      });

    const onDepositFailure = (err: SDK.Utils.DepositError) =>
      Effect.gen(function* () {
        yield* Effect.logError(`Deposit error: ${err}`);
        yield* Effect.fail(
          new GenesisDepositError({
            message: "failed to submit genesis deposits",
            cause: err,
          }),
        );
      });


    yield* SDK.Endpoints.commitDepositsProgram(lucid, depositParams).pipe(
      Effect.catchTag("HashingError", onHashingFailure),
      Effect.catchTag("DepositError", onDepositFailure),
    )
  });
