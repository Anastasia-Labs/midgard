import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { AlwaysSucceedsContract, Lucid, NodeConfig } from "@/services/index.js";
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
  NodeConfig |
  Lucid
> =>
  Effect.gen(function* () {
    yield* Effect.log("DEBUG: In buildAndSubmitGenesisDeposit")
    const { api: lucid } = yield* Lucid;
    yield* Effect.log("DEBUG: lucid")

    const { depositAuthValidator } = yield* AlwaysSucceedsContract
    yield* Effect.log("DEBUG: depositAuthValidator")

    const config = yield* NodeConfig
    yield* Effect.log("DEBUG: config")

    let l2Address: string
    if (config.GENESIS_UTXOS.length <= 0) {
      yield* Effect.fail(new GenesisDepositError({
        message: "No GENESIS_UTXOS provided, abort buildAndSubmitGenesisDeposit",
        cause: "",
      }))
      return;
    } else {
      l2Address = config.GENESIS_UTXOS[0].address
    }
    yield* Effect.log(`DEBUG: l2Address ${l2Address}`)

    const onchainParameters = yield* SDK.Services.Parameters;
    yield* Effect.log(`DEBUG: onchainParameters ${JSON.stringify(onchainParameters)}`)

    const depositParams : SDK.TxBuilder.Deposit.DepositParams =
    ({
        depositScriptAddress: depositAuthValidator.spendScriptAddress,
        mintingPolicy: depositAuthValidator.mintScript,
        policyId: depositAuthValidator.policyId,
        depositInfo: ({
            l2Address: l2Address,
            l2Datum: "",
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

    yield* SDK.Endpoints.depositsBuilderProgram(lucid, depositParams).pipe(
      Effect.catchTag("HashingError", onHashingFailure),
      Effect.catchTag("DepositError", onDepositFailure),
    )
  });
