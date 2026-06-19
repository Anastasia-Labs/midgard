import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Script,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  classifyProviderHttpResponse,
  L1_REWARD_ACCOUNT_REGISTRATION_SOURCES,
  redactEndpoint,
  summarizeProviderBody,
  type L1RewardAccountRegistrationSource,
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
    }
  | {
      readonly status: "already_registered";
      readonly txHash: null;
    }
);

export type PhasMembershipRegistrationQuery = (input: {
  readonly lucid: LucidEvolution;
  readonly rewardAddress: string;
  readonly scriptHash: string;
}) => Effect.Effect<boolean, SDK.LucidError>;

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

const ogmiosSummaryProvesRegistered = (result: unknown): boolean => {
  if (result === null || result === undefined) {
    return false;
  }
  if (Array.isArray(result)) {
    return result.length > 0;
  }
  if (hasObjectShape(result)) {
    return Object.keys(result).length > 0;
  }
  return false;
};

const queryOgmiosRewardAccountRegistered = async ({
  source,
  rewardAddress,
  fetchImpl,
}: {
  readonly source: Extract<
    L1RewardAccountRegistrationSource,
    { readonly kind: "ogmios" }
  >;
  readonly rewardAddress: string;
  readonly fetchImpl: FetchLike;
}): Promise<boolean> => {
  const response = await fetchImpl(source.url, {
    method: "POST",
    headers: {
      "content-type": "application/json",
      ...(source.headers ?? {}),
    },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryLedgerState/rewardAccountSummaries",
      params: { keys: [rewardAddress] },
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
  return ogmiosSummaryProvesRegistered(
    hasObjectShape(body) && "result" in body ? body.result : body,
  );
};

const queryRewardAccountRegistrationSource = (
  source: L1RewardAccountRegistrationSource,
  rewardAddress: string,
  fetchImpl: FetchLike,
): Promise<boolean> => {
  return queryOgmiosRewardAccountRegistered({
    source,
    rewardAddress,
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
): boolean | null => {
  if (!hasObjectShape(provider) || !hasObjectShape(provider.chain)) {
    return null;
  }
  const entry = provider.chain[rewardAddress];
  if (!hasObjectShape(entry)) {
    return false;
  }
  return entry.registeredStake === true;
};

export const queryPhasMembershipRewardAccountRegisteredProgram = (
  lucid: LucidEvolution,
  rewardAddress: string,
  fetchImpl: FetchLike = fetch,
): Effect.Effect<boolean, SDK.LucidError> =>
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
      for (const [index, source] of sources.entries()) {
        try {
          return await queryRewardAccountRegistrationSource(
            source,
            rewardAddress,
            fetchImpl,
          );
        } catch (cause) {
          lastError = cause;
          const canTryNext =
            index < sources.length - 1 && isRetryableProviderQueryError(cause);
          if (!canTryNext) {
            throw cause;
          }
        }
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
          options.fetchImpl ?? fetch,
        ))
    )({
      lucid,
      rewardAddress: identity.rewardAddress,
      scriptHash: identity.scriptHash,
    });

    if (registered) {
      yield* Effect.logInfo(
        `PHAS membership reward account is already registered before submission: scriptHash=${identity.scriptHash},rewardAddress=${identity.rewardAddress}`,
      );
      return {
        status: "already_registered",
        rewardAddress: identity.rewardAddress,
        scriptHash: identity.scriptHash,
        txHash: null,
      } satisfies PhasMembershipRewardRegistrationResult;
    }

    const built = yield* (
      options.buildRegistrationTx ?? defaultBuildRegistrationTx
    )(lucid, { script });
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
        } satisfies PhasMembershipRewardRegistrationResult;
      }
      return yield* Effect.fail(submitted.left);
    }
    return {
      status: "registration_submitted",
      rewardAddress: built.rewardAddress,
      scriptHash: built.scriptHash,
      txHash: submitted.right,
    } satisfies PhasMembershipRewardRegistrationResult;
  });
