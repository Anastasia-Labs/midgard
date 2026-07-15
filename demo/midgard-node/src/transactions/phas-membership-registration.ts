import { createHash } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  type LucidEvolution,
  type Script,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  classifyProviderHttpResponse,
  L1_REWARD_ACCOUNT_REGISTRATION_SOURCES,
  type L1RewardAccountRegistrationSource,
  redactEndpoint,
  summarizeProviderBody,
} from "@/provider-diagnostics.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

type FetchLike = (input: string, init?: RequestInit) => Promise<Response>;

export type PhasMembershipRewardRegistrationResult = {
  readonly rewardAddress: string;
  readonly scriptHash: string;
} & (
  | {
      readonly status: "registration_submitted";
      readonly txHash: string;
      readonly transactionBody: PhasMembershipRegistrationTransactionBodyEvidence;
    }
  | {
      readonly status: "already_registered";
      readonly txHash: null;
      readonly transactionBody: null;
    }
);

export type PhasMembershipRegistrationTransactionBodyEvidence = {
  readonly schemaVersion: "midgard-phas-registration-transaction-body-v1";
  readonly txHash: string;
  readonly cborSha256: string;
  readonly cborSizeBytes: number;
  readonly certificate: {
    readonly kind: "stake_registration";
    readonly index: 0;
    readonly count: 1;
    readonly credentialType: "script";
    readonly scriptHash: string;
  };
};

export type CapturedPhasMembershipRegistrationTransaction = {
  readonly evidence: PhasMembershipRegistrationTransactionBodyEvidence;
  /** Complete unsigned transaction CBOR; it contains no witnesses or secrets. */
  readonly unsignedTransactionCborHex: string;
};

export type PhasMembershipRegistrationQuery = (input: {
  readonly lucid: LucidEvolution;
  readonly rewardAddress: string;
  readonly scriptHash: string;
}) => Effect.Effect<PhasMembershipRegistrationStatus, SDK.LucidError>;

export type PhasMembershipRegistrationStatus =
  | "registered"
  | "unregistered"
  | "unknown";

export type PhasMembershipRegistrationOptions = {
  readonly queryRegistration?: PhasMembershipRegistrationQuery;
  readonly buildRegistrationTx?: (
    lucid: LucidEvolution,
    config: { readonly script: Script },
  ) => Effect.Effect<
    SDK.BuiltPhasMembershipRewardRegistrationTx,
    SDK.LucidError | SDK.UnspecifiedNetworkError
  >;
  readonly submitRegistrationTx?: (
    lucid: LucidEvolution,
    built: SDK.BuiltPhasMembershipRewardRegistrationTx,
  ) => Effect.Effect<string, TxConfirmError | TxSignError | TxSubmitError>;
  readonly fetchImpl?: FetchLike;
  readonly inspectRegistrationTx?: (
    built: SDK.BuiltPhasMembershipRewardRegistrationTx,
    expected: { readonly rewardAddress: string; readonly scriptHash: string },
  ) => CapturedPhasMembershipRegistrationTransaction;
  readonly onSubmittedRegistrationTx?: (
    capture: CapturedPhasMembershipRegistrationTransaction,
  ) => void;
};

export const inspectPhasMembershipRegistrationTransaction = (
  built: SDK.BuiltPhasMembershipRewardRegistrationTx,
  expected: { readonly rewardAddress: string; readonly scriptHash: string },
): CapturedPhasMembershipRegistrationTransaction => {
  if (
    built.rewardAddress !== expected.rewardAddress ||
    built.scriptHash !== expected.scriptHash
  ) {
    throw new Error(
      `Built PHAS registration identity mismatch: expected=${expected.rewardAddress}:${expected.scriptHash},actual=${built.rewardAddress}:${built.scriptHash}`,
    );
  }
  const unsignedTransactionCborHex = built.tx.toCBOR().toLowerCase();
  if (
    unsignedTransactionCborHex.length === 0 ||
    unsignedTransactionCborHex.length % 2 !== 0 ||
    !/^[a-f0-9]+$/u.test(unsignedTransactionCborHex)
  ) {
    throw new Error("PHAS registration unsigned transaction CBOR is invalid");
  }
  const transaction = CML.Transaction.from_cbor_hex(unsignedTransactionCborHex);
  if (
    transaction.to_canonical_cbor_hex() !== unsignedTransactionCborHex ||
    transaction.witness_set().to_cbor_hex() !== "a0"
  ) {
    throw new Error(
      "PHAS registration evidence must be canonical unsigned transaction CBOR with an empty witness set",
    );
  }
  const body = transaction.body();
  const certificates = body.certs();
  if (certificates === undefined || certificates.len() !== 1) {
    throw new Error(
      `PHAS registration transaction must contain exactly one certificate; found ${certificates?.len().toString() ?? "0"}`,
    );
  }
  const certificate = certificates.get(0);
  if (certificate.kind() !== CML.CertificateKind.StakeRegistration) {
    throw new Error(
      `PHAS registration transaction certificate kind is not StakeRegistration: ${certificate.kind().toString()}`,
    );
  }
  const credential = certificate.as_stake_registration()?.stake_credential();
  const certificateScriptHash = credential?.as_script()?.to_hex();
  if (
    credential?.kind() !== CML.CredentialKind.Script ||
    certificateScriptHash !== expected.scriptHash
  ) {
    throw new Error(
      `PHAS registration transaction certificate credential mismatch: expected script ${expected.scriptHash}, found ${String(certificateScriptHash)}`,
    );
  }
  const bytes = Buffer.from(unsignedTransactionCborHex, "hex");
  return {
    evidence: {
      schemaVersion: "midgard-phas-registration-transaction-body-v1",
      txHash: CML.hash_transaction(body).to_hex(),
      cborSha256: createHash("sha256").update(bytes).digest("hex"),
      cborSizeBytes: bytes.length,
      certificate: {
        kind: "stake_registration",
        index: 0,
        count: 1,
        credentialType: "script",
        scriptHash: certificateScriptHash,
      },
    },
    unsignedTransactionCborHex,
  };
};

export const isPhasMembershipAlreadyRegisteredError = (
  error: TxSubmitError,
  scriptHash: string,
): boolean => {
  const message = formatUnknownError(error, {
    includeCause: true,
  }).toLowerCase();
  const normalizedScriptHash = scriptHash.toLowerCase();
  return (
    (message.includes("stakekeyregistereddeleg") ||
      message.includes("already known credential") ||
      message.includes("knowncredential")) &&
    message.includes(normalizedScriptHash)
  );
};

const hasObjectShape = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

const providerQueryError = ({
  message,
  source,
  retryable,
  cause,
}: {
  readonly message: string;
  readonly source: string;
  readonly retryable: boolean;
  readonly cause: unknown;
}): SDK.LucidError =>
  new SDK.LucidError({
    message,
    cause: {
      source,
      retryable,
      cause,
    },
  });

const isRetryableProviderQueryError = (error: unknown): boolean => {
  if (!hasObjectShape(error)) {
    return false;
  }
  const cause = error.cause;
  return hasObjectShape(cause) && cause.retryable === true;
};

const readProviderJson = async <A>({
  response,
  source,
  query,
}: {
  readonly response: Response;
  readonly source: string;
  readonly query: string;
}): Promise<A> => {
  const body = await response.text();
  if (!response.ok) {
    const classification = classifyProviderHttpResponse({
      status: response.status,
      body,
      retryAfter: response.headers.get("retry-after"),
    });
    throw providerQueryError({
      message: `Failed PHAS reward-account registration query: source=${source},query=${query},status=${response.status.toString()},result=${classification.kind},endpoint=${redactEndpoint(response.url)}`,
      source,
      retryable: classification.retryable,
      cause: classification.summary,
    });
  }
  try {
    return JSON.parse(body) as A;
  } catch (cause) {
    throw providerQueryError({
      message: `Failed PHAS reward-account registration query: source=${source},query=${query},status=${response.status.toString()},result=malformed_json,endpoint=${redactEndpoint(response.url)}`,
      source,
      retryable: true,
      cause: summarizeProviderBody(body),
    });
  }
};

const parseOgmiosRewardAccountSummary = (
  result: unknown,
  scriptHash: string,
  source: string,
): PhasMembershipRegistrationStatus => {
  if (!Array.isArray(result)) {
    throw providerQueryError({
      message: `Failed PHAS reward-account registration query: source=${source},query=reward-account-registration,result=unexpected_result_shape`,
      source,
      retryable: false,
      cause: "Expected Ogmios v7 rewardAccountSummaries to return a list.",
    });
  }
  if (result.length === 0) {
    return "unknown";
  }
  if (result.length !== 1) {
    throw providerQueryError({
      message: `Failed PHAS reward-account registration query: source=${source},query=reward-account-registration,result=unexpected_summary_count`,
      source,
      retryable: false,
      cause: `Expected exactly one summary for PHAS script ${scriptHash}, received ${result.length.toString()}.`,
    });
  }
  const [summary] = result;
  if (
    !hasObjectShape(summary) ||
    summary.from !== "script" ||
    summary.credential !== scriptHash
  ) {
    throw providerQueryError({
      message: `Failed PHAS reward-account registration query: source=${source},query=reward-account-registration,result=credential_mismatch`,
      source,
      retryable: false,
      cause:
        "Ogmios returned a reward-account summary that did not exactly match the requested PHAS script credential.",
    });
  }
  return "registered";
};

const queryOgmiosRewardAccountRegistered = async ({
  source,
  scriptHash,
  fetchImpl,
}: {
  readonly source: Extract<
    L1RewardAccountRegistrationSource,
    { readonly kind: "ogmios" }
  >;
  readonly scriptHash: string;
  readonly fetchImpl: FetchLike;
}): Promise<PhasMembershipRegistrationStatus> => {
  const response = await fetchImpl(source.url, {
    method: "POST",
    headers: {
      "content-type": "application/json",
      ...(source.headers ?? {}),
    },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryLedgerState/rewardAccountSummaries",
      params: { scripts: [scriptHash] },
      id: null,
    }),
  });
  const body = await readProviderJson<unknown>({
    response,
    source: source.source,
    query: "reward-account-registration",
  });
  if (hasObjectShape(body) && "error" in body) {
    throw providerQueryError({
      message: `Failed PHAS reward-account registration query: source=${source.source},query=reward-account-registration,result=ogmios_json_rpc_error,endpoint=${redactEndpoint(source.url)}`,
      source: source.source,
      retryable: false,
      cause: body.error,
    });
  }
  return parseOgmiosRewardAccountSummary(
    hasObjectShape(body) && "result" in body ? body.result : body,
    scriptHash,
    source.source,
  );
};

const queryRewardAccountRegistrationSource = (
  source: L1RewardAccountRegistrationSource,
  scriptHash: string,
  fetchImpl: FetchLike,
): Promise<PhasMembershipRegistrationStatus> => {
  return queryOgmiosRewardAccountRegistered({
    source,
    scriptHash,
    fetchImpl,
  });
};

const providerRewardAccountSources = (
  provider: unknown,
): readonly L1RewardAccountRegistrationSource[] => {
  if (!hasObjectShape(provider)) {
    return [];
  }
  const configured = provider[L1_REWARD_ACCOUNT_REGISTRATION_SOURCES];
  if (Array.isArray(configured)) {
    return configured.filter(
      hasObjectShape,
    ) as L1RewardAccountRegistrationSource[];
  }
  if (typeof provider.ogmiosUrl === "string") {
    const headers = hasObjectShape(provider.headers)
      ? provider.headers.ogmiosHeader
      : undefined;
    return [
      {
        kind: "ogmios",
        source: "kupmios",
        url: provider.ogmiosUrl,
        ...(hasObjectShape(headers)
          ? { headers: headers as Record<string, string> }
          : {}),
      },
    ];
  }
  return [];
};

const queryEmulatorRewardAccountRegistered = (
  provider: unknown,
  rewardAddress: string,
): PhasMembershipRegistrationStatus | null => {
  if (!hasObjectShape(provider) || !hasObjectShape(provider.chain)) {
    return null;
  }
  const entry = provider.chain[rewardAddress];
  if (!hasObjectShape(entry)) {
    return "unregistered";
  }
  return entry.registeredStake === true ? "registered" : "unregistered";
};

export const queryPhasMembershipRewardAccountRegisteredProgram = (
  lucid: LucidEvolution,
  rewardAddress: string,
  scriptHash: string,
  fetchImpl: FetchLike = fetch,
): Effect.Effect<PhasMembershipRegistrationStatus, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const provider = lucid.config().provider;
      const emulatorResult = queryEmulatorRewardAccountRegistered(
        provider,
        rewardAddress,
      );
      if (emulatorResult !== null) {
        return emulatorResult;
      }

      const sources = providerRewardAccountSources(provider);
      if (sources.length === 0) {
        throw providerQueryError({
          message:
            "Cannot determine PHAS reward-account registration status before submission: unsupported L1 provider shape",
          source: "unsupported",
          retryable: false,
          cause:
            "Expected emulator chain state, Kupmios Ogmios URL, or Midgard Ogmios reward-account source metadata.",
        });
      }

      let lastError: unknown;
      let sawUnknown = false;
      for (const [index, source] of sources.entries()) {
        try {
          const status = await queryRewardAccountRegistrationSource(
            source,
            scriptHash,
            fetchImpl,
          );
          if (status === "registered") {
            return status;
          }
          sawUnknown ||= status === "unknown";
        } catch (cause) {
          lastError = cause;
          const canTryNext =
            index < sources.length - 1 && isRetryableProviderQueryError(cause);
          if (!canTryNext) {
            throw cause;
          }
        }
      }
      if (sawUnknown) {
        return "unknown";
      }
      throw lastError;
    },
    catch: (cause) =>
      cause instanceof SDK.LucidError
        ? cause
        : new SDK.LucidError({
            message:
              "Failed to query PHAS reward-account registration status before submission",
            cause,
          }),
  });

const defaultBuildRegistrationTx = (
  lucid: LucidEvolution,
  config: { readonly script: Script },
): Effect.Effect<
  SDK.BuiltPhasMembershipRewardRegistrationTx,
  SDK.LucidError | SDK.UnspecifiedNetworkError
> => SDK.buildPhasMembershipRewardRegistrationTxProgram(lucid, config);

const defaultSubmitRegistrationTx = (
  lucid: LucidEvolution,
  built: {
    readonly tx: TxSignBuilder;
  },
): Effect.Effect<string, TxConfirmError | TxSignError | TxSubmitError> =>
  handleSignSubmit(lucid, built.tx);

export const ensurePhasMembershipRewardAccountRegisteredProgram = (
  lucid: LucidEvolution,
  options: PhasMembershipRegistrationOptions = {},
): Effect.Effect<
  PhasMembershipRewardRegistrationResult,
  | SDK.LucidError
  | SDK.UnspecifiedNetworkError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    const script = loadPhasMembershipWithdrawalScript();
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.UnspecifiedNetworkError({
          message:
            "Cannot derive PHAS membership reward-account identity without a configured Lucid network",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const identity = SDK.phasMembershipIdentity(network, script);
    const registered = yield* (
      options.queryRegistration ??
      ((input) =>
        queryPhasMembershipRewardAccountRegisteredProgram(
          input.lucid,
          input.rewardAddress,
          input.scriptHash,
          options.fetchImpl ?? fetch,
        ))
    )({
      lucid,
      rewardAddress: identity.rewardAddress,
      scriptHash: identity.scriptHash,
    });

    if (registered === "registered") {
      yield* Effect.logInfo(
        `PHAS membership reward account is already registered before submission: scriptHash=${identity.scriptHash},rewardAddress=${identity.rewardAddress}`,
      );
      return {
        status: "already_registered",
        rewardAddress: identity.rewardAddress,
        scriptHash: identity.scriptHash,
        txHash: null,
        transactionBody: null,
      } satisfies PhasMembershipRewardRegistrationResult;
    }

    if (registered === "unknown") {
      yield* Effect.logInfo(
        `PHAS membership reward-account registration status is unknown; proceeding with explicit idempotent registration: scriptHash=${identity.scriptHash},rewardAddress=${identity.rewardAddress}`,
      );
    }

    const built = yield* (
      options.buildRegistrationTx ?? defaultBuildRegistrationTx
    )(lucid, { script });
    const capturedTransaction = yield* Effect.try({
      try: () =>
        (
          options.inspectRegistrationTx ??
          inspectPhasMembershipRegistrationTransaction
        )(built, identity),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to verify the exact PHAS registration transaction body",
          cause,
        }),
    });
    const submitted = yield* Effect.either(
      (options.submitRegistrationTx ?? defaultSubmitRegistrationTx)(
        lucid,
        built,
      ),
    );
    if (submitted._tag === "Left") {
      if (
        submitted.left instanceof TxSubmitError &&
        isPhasMembershipAlreadyRegisteredError(submitted.left, built.scriptHash)
      ) {
        yield* Effect.logInfo(
          `PHAS membership reward account is already registered: scriptHash=${built.scriptHash},rewardAddress=${built.rewardAddress}`,
        );
        return {
          status: "already_registered",
          rewardAddress: built.rewardAddress,
          scriptHash: built.scriptHash,
          txHash: null,
          transactionBody: null,
        } satisfies PhasMembershipRewardRegistrationResult;
      }
      return yield* Effect.fail(submitted.left);
    }
    if (submitted.right !== capturedTransaction.evidence.txHash) {
      return yield* Effect.fail(
        new TxSubmitError({
          message:
            "Submitted PHAS registration transaction hash does not match the verified unsigned transaction body",
          txHash: submitted.right,
          cause: `expected=${capturedTransaction.evidence.txHash},actual=${submitted.right}`,
        }),
      );
    }
    options.onSubmittedRegistrationTx?.(capturedTransaction);
    return {
      status: "registration_submitted",
      rewardAddress: built.rewardAddress,
      scriptHash: built.scriptHash,
      txHash: submitted.right,
      transactionBody: capturedTransaction.evidence,
    } satisfies PhasMembershipRewardRegistrationResult;
  });
