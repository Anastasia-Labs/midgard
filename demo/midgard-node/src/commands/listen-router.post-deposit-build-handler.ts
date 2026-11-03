import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect } from "effect";

import { runProviderStepWithRetry } from "../provider-retry.js";
import { Lucid, MidgardContracts } from "../services/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "../transactions/reference-scripts.js";
import * as SubmitDeposit from "../transactions/submit-deposit.js";
import { failWith500 } from "./listen-response.js";
import { DEPOSIT_BUILD_ENDPOINT } from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";

/** The build reads protocol parameters and the hub oracle from the provider
 * and writes nothing, so a transient provider failure is retried in place. */
export const DEPOSIT_BUILD_PROVIDER_RETRY = {
  maxAttempts: 4,
  baseDelayMs: 500,
  maxDelayMs: 4_000,
} as const;

/**
 * `POST /deposit/build`: builds an unsigned L1 deposit transaction from a
 * caller-supplied wallet view and returns the CBOR for external signing. A
 * transient provider failure is retried (the reference-script reads retry on
 * their own); an invalid request, or a failure no retry can clear, is
 * answered at once.
 */
export const postDepositBuildHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    yield* Effect.logInfo(
      `POST /${DEPOSIT_BUILD_ENDPOINT} - invalid JSON request body`,
    );
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }

  const lucid = yield* Lucid;
  let buildRequest: SubmitDeposit.BuildDepositRequest;
  try {
    buildRequest = SubmitDeposit.parseBuildDepositRequest(parsedBody.right, {
      expectedNetwork: lucid.api.config().network,
    });
  } catch {
    const message = "Invalid request";
    yield* Effect.logInfo(
      `POST /${DEPOSIT_BUILD_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const contracts = yield* MidgardContracts;
  const depositReferenceScripts = yield* fetchReferenceScriptUtxosProgram(
    lucid.api,
    lucid.referenceScriptsAddress,
    referenceScriptTargetsByCommand(contracts).deposit,
    contracts.referenceScriptAuth,
  ).pipe(
    Effect.map((resolved) => ({
      depositMinting: referenceScriptByName(resolved, "deposit minting"),
    })),
  );
  const built = yield* runProviderStepWithRetry(
    `POST /${DEPOSIT_BUILD_ENDPOINT} build`,
    SubmitDeposit.buildUnsignedDepositTxFromFundingContextProgram(
      lucid.api,
      contracts,
      { ...buildRequest, referenceScripts: depositReferenceScripts },
    ),
    DEPOSIT_BUILD_PROVIDER_RETRY,
  );
  return yield* HttpServerResponse.json(built);
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e),
  ),
  Effect.catchTag("SubmitDepositError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HubOracleError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("Bech32DeserializationError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
);
