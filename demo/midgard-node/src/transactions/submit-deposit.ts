/**
 * Deposit submission flow for projecting deposit observations into Midgard
 * state.
 * This module owns node/API concerns and delegates production transaction
 * construction to the SDK user-event builders.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { normalizeHex as normalizeCoreHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data as LucidData,
  getAddressDetails,
  Lucid as makeLucid,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Duration, Option } from "effect";
import { Data as EffectData, Effect } from "effect";

import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@/asset-specs.js";
import { DepositsDB, DepositSubmissionAttemptsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { reconcileVisibleDepositUTxOs } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import {
  Database,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  awaitSubmittedTransactionConfirmation,
  type SignSubmitContext,
  type SubmitRecoveryOptions,
  submitSignedTxWithRecovery,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  depositDependenciesFromSignedTx,
  type DepositTxObservation,
  observeDepositSubmissionAttempt,
} from "@/transactions/deposit-submission-provider.js";

export type SubmitDepositReferenceScripts = SDK.SubmitDepositReferenceScripts;
export type SubmitDepositConfig = SDK.SubmitDepositConfig;

export type BuildDepositRequest = SubmitDepositConfig & {
  readonly fundingAddress: string;
  readonly fundingUtxos: readonly UTxO[];
};

export type BuiltUnsignedDepositTx = {
  readonly unsignedTxCbor: string;
};

export type DepositBuildMetadata = SDK.DepositBuildMetadata;

export type SubmittedDeposit = {
  readonly txHash: string;
  readonly metadata: DepositBuildMetadata;
  readonly confirmationStatus:
    | "confirmed"
    | "reconciled_after_timeout"
    | "ambiguous";
};

/**
 * A signed deposit checkpoint that is complete enough to persist before any
 * provider submission. Recovery submits only `signedTxCbor`; it never accepts
 * a builder or logical deposit config that could synthesize a different tx.
 */
export type PreparedDeposit = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly selectedInputOutRefs: readonly string[];
  readonly metadata: DepositBuildMetadata;
  readonly submissionAttempt: DepositSubmissionAttemptsDB.InsertPreparedInput;
};

export type PreparedDepositSubmission = SignSubmitContext & {
  readonly providerTxHash: string | null;
};

export class SubmitDepositError extends EffectData.TaggedError(
  "SubmitDepositError",
)<{
  message: string;
  cause: unknown;
}> {}

export class DepositConfirmationUnknownError extends EffectData.TaggedError(
  "DepositConfirmationUnknownError",
)<{
  message: string;
  txHash: string;
  depositEventId: string;
  expectedDepositOutRef: string;
  reconciliation: DepositSubmissionReconciliationResult;
  cause: unknown;
}> {}

const MAX_DEPOSIT_BUILD_FUNDING_UTXOS = 128;
const MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES = 64;
const MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS = 64;

const buildUnsignedDepositTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: DepositBuildMetadata;
  },
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.catchTag("UserEventBuildError", (error) =>
      Effect.fail(
        new SubmitDepositError({
          message: error.message,
          cause: error.cause,
        }),
      ),
    ),
  );

const sortedAssetEntries = (assets: Assets): [string, string][] =>
  Object.entries(assets)
    .map(([unit, quantity]) => [unit, quantity.toString()] as [string, string])
    .sort(([left], [right]) => left.localeCompare(right));

const serializeAssets = (
  assets: Assets,
): DepositSubmissionAttemptsDB.SerializedAssets =>
  Object.fromEntries(sortedAssetEntries(assets));

const sameSerializedAssets = (
  left: DepositSubmissionAttemptsDB.SerializedAssets,
  right: DepositSubmissionAttemptsDB.SerializedAssets,
): boolean => JSON.stringify(left) === JSON.stringify(right);

const inputOutRefsFromSignedTx = (tx: CML.Transaction): readonly string[] => {
  const inputs = tx.body().inputs();
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${Number(input.index()).toString()}`,
    );
  }
  return outRefs;
};

const decodeDepositDatumEventId = (datumCbor: string): string => {
  const datum = LucidData.from(datumCbor, SDK.DepositDatum) as SDK.DepositDatum;
  return LucidData.to(datum.event.id, SDK.OutputReference);
};

export const depositSubmissionAttemptFromSignedTx = ({
  txHash,
  signedTxCbor,
  metadata,
  config,
}: {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly metadata: DepositBuildMetadata;
  readonly config: SubmitDepositConfig;
}): DepositSubmissionAttemptsDB.InsertPreparedInput => {
  const tx = CML.Transaction.from_cbor_hex(signedTxCbor);
  const outputs = tx.body().outputs();
  const matches: Array<{
    readonly outputIndex: number;
    readonly assets: Assets;
  }> = [];

  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const output = coreToTxOutput(outputs.get(outputIndex));
    if (output.address !== metadata.depositAddress) {
      continue;
    }
    if ((output.assets[metadata.depositAuthUnit] ?? 0n) !== 1n) {
      continue;
    }
    if (output.datum === undefined || output.datum === null) {
      throw new Error(
        `Deposit output ${txHash}#${outputIndex.toString()} is missing the inline deposit datum`,
      );
    }
    const actualEventId = decodeDepositDatumEventId(output.datum);
    if (actualEventId !== metadata.depositEventId) {
      continue;
    }
    matches.push({ outputIndex, assets: output.assets });
  }

  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one deposit output for event ${metadata.depositEventId} in signed tx ${txHash}; found ${matches.length.toString()}`,
    );
  }

  const match = matches[0]!;
  const expectedAssets: Assets = { ...match.assets };
  delete expectedAssets[metadata.depositAuthUnit];
  const expectedSerializedAssets = serializeAssets(expectedAssets);
  const requestedSerializedAssets = serializeAssets({
    ...config.additionalAssets,
    lovelace: config.lovelace,
  });
  if (
    !sameSerializedAssets(expectedSerializedAssets, requestedSerializedAssets)
  ) {
    throw new Error(
      `Deposit output ${txHash}#${match.outputIndex.toString()} does not match requested projected assets`,
    );
  }

  return {
    [DepositSubmissionAttemptsDB.Columns.TX_HASH]: Buffer.from(txHash, "hex"),
    [DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR]: Buffer.from(
      signedTxCbor,
      "hex",
    ),
    [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: Buffer.from(
      metadata.depositEventId,
      "hex",
    ),
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]:
      `${txHash}#${match.outputIndex.toString()}`,
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS]: config.l2Address,
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]:
      config.lovelace.toString(),
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS]:
      expectedSerializedAssets,
    [DepositSubmissionAttemptsDB.Columns.METADATA]: {
      depositAddress: metadata.depositAddress,
      depositEventId: metadata.depositEventId,
      depositAssetName: metadata.depositAssetName,
      depositAuthUnit: metadata.depositAuthUnit,
      nonceInput: metadata.nonceInput,
      validTo: metadata.validTo,
      inclusionTime: metadata.inclusionTime,
    },
    [DepositSubmissionAttemptsDB.Columns.DEPENDENCY_OUT_REFS]:
      depositDependenciesFromSignedTx(tx),
  };
};

export type DepositSubmissionReconciliationResult = {
  readonly txHash: string;
  readonly depositEventId: string;
  readonly status:
    | "confirmed"
    | "reconciled_after_timeout"
    | "accepted"
    | "unseen"
    | "expired"
    | "ambiguous"
    | "missing_attempt";
  readonly expectedDepositOutRef?: string;
  readonly depositRowsFound: number;
  readonly reconciledCount: number;
  readonly nextSafeAction: string;
};

export type DepositSubmissionObservationReader = (
  attempt: DepositSubmissionAttemptsDB.Row,
) => Promise<DepositTxObservation>;

const findMatchingDepositEntry = (
  rows: readonly DepositsDB.Entry[],
  attempt: DepositSubmissionAttemptsDB.Row,
): DepositsDB.Entry | undefined =>
  rows.find((row) =>
    row[DepositsDB.Columns.ID].equals(
      attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID],
    ),
  );

export const reconcileDepositSubmissionAttemptProgram = (
  txHash: string,
  options: {
    readonly observe?: DepositSubmissionObservationReader;
  } = {},
): Effect.Effect<
  DepositSubmissionReconciliationResult,
  DatabaseError | SDK.LucidError | SubmitDepositError,
  Database | MidgardContracts | LucidService | NodeConfig
> =>
  Effect.gen(function* () {
    const txHashBuffer = Buffer.from(txHash, "hex");
    const attemptOption =
      yield* DepositSubmissionAttemptsDB.retrieveByTxHash(txHashBuffer);
    if (Option.isNone(attemptOption)) {
      return {
        txHash,
        depositEventId: "",
        status: "missing_attempt",
        depositRowsFound: 0,
        reconciledCount: 0,
        nextSafeAction:
          "The durable journal is missing, so claim history is unknown. Do not resume or rebuild from this result; require explicit operator provenance and positive provider evidence before any new operation.",
      } as const;
    }

    const attempt = attemptOption.value;
    const attemptStatus = attempt[DepositSubmissionAttemptsDB.Columns.STATUS];
    const depositEventId =
      attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID].toString(
        "hex",
      );
    const expectedDepositOutRef =
      attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF];
    if (
      attemptStatus === DepositSubmissionAttemptsDB.Status.Confirmed ||
      attemptStatus ===
        DepositSubmissionAttemptsDB.Status.ReconciledAfterTimeout ||
      attemptStatus === DepositSubmissionAttemptsDB.Status.Expired
    ) {
      return {
        txHash,
        depositEventId,
        status:
          attemptStatus === DepositSubmissionAttemptsDB.Status.Confirmed
            ? "confirmed"
            : attemptStatus ===
                DepositSubmissionAttemptsDB.Status.ReconciledAfterTimeout
              ? "reconciled_after_timeout"
              : "expired",
        expectedDepositOutRef,
        depositRowsFound: 0,
        reconciledCount: 0,
        nextSafeAction:
          attemptStatus === DepositSubmissionAttemptsDB.Status.Expired
            ? "The exact transaction is expired and terminal; never submit it."
            : "The durable attempt is terminal; never resubmit this transaction.",
      } as const;
    }
    const lucidService = yield* LucidService;
    const nodeConfig = yield* NodeConfig;
    const observation = yield* Effect.tryPromise({
      try: () =>
        options.observe === undefined
          ? observeDepositSubmissionAttempt({
              lucid: lucidService.api,
              attempt,
              ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
              timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
            })
          : options.observe(attempt),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Failed to observe durable deposit submission state",
          cause,
        }),
    });

    if (observation.kind === "committed") {
      const reconciliation = yield* Effect.either(
        reconcileVisibleDepositUTxOs(),
      );
      const reconciledCount =
        reconciliation._tag === "Right"
          ? reconciliation.right.reconciledCount
          : 0;
      const afterRows = yield* DepositsDB.retrieveByCardanoTxHash(txHashBuffer);
      const afterMatch = findMatchingDepositEntry(afterRows, attempt);
      if (afterMatch !== undefined) {
        yield* DepositSubmissionAttemptsDB.markReconciled(txHashBuffer);
      } else {
        yield* DepositSubmissionAttemptsDB.markConfirmed(txHashBuffer);
      }
      return {
        txHash,
        depositEventId,
        status:
          afterMatch === undefined ? "confirmed" : "reconciled_after_timeout",
        expectedDepositOutRef,
        depositRowsFound: afterRows.length,
        reconciledCount,
        nextSafeAction:
          "Historical Kupo evidence proves this exact deposit committed; never resubmit it.",
      } as const;
    }
    if (observation.kind === "accepted") {
      if (
        attemptStatus === DepositSubmissionAttemptsDB.Status.SubmissionUnknown
      ) {
        yield* DepositSubmissionAttemptsDB.markSubmitted(
          txHashBuffer,
          "ogmios_mempool",
        );
      }
      return {
        txHash,
        depositEventId,
        status: "accepted",
        expectedDepositOutRef,
        depositRowsFound: 0,
        reconciledCount: 0,
        nextSafeAction:
          "Ogmios has this exact transaction in its acquired mempool snapshot; wait for confirmation without resubmitting.",
      } as const;
    }
    if (observation.kind === "absent_safe") {
      const neverClaimed =
        attemptStatus === DepositSubmissionAttemptsDB.Status.Prepared;
      return {
        txHash,
        depositEventId,
        status: neverClaimed ? "unseen" : "ambiguous",
        expectedDepositOutRef,
        depositRowsFound: 0,
        reconciledCount: 0,
        nextSafeAction: neverClaimed
          ? "The durable row proves no provider call has been claimed; one initial exact-byte submission may be claimed."
          : "The transaction is not currently observed, but a provider call may already have occurred; never resubmit it automatically.",
      } as const;
    }
    if (observation.kind === "expired") {
      const reason = `Exact signed deposit expired absent from synchronized chain/mempool evidence: current_slot=${observation.currentSlot.toString()},invalid_hereafter=${observation.invalidHereafterSlot.toString()}`;
      if (attemptStatus === DepositSubmissionAttemptsDB.Status.Prepared) {
        yield* DepositSubmissionAttemptsDB.markExpired(txHashBuffer, reason);
      }
      return {
        txHash,
        depositEventId,
        status:
          attemptStatus === DepositSubmissionAttemptsDB.Status.Prepared
            ? "expired"
            : "ambiguous",
        expectedDepositOutRef,
        depositRowsFound: 0,
        reconciledCount: 0,
        nextSafeAction:
          attemptStatus === DepositSubmissionAttemptsDB.Status.Prepared
            ? "This never-submitted exact transaction is expired and terminal; create a new deposit operation explicitly if still desired."
            : "The exact transaction expired after a provider call may have occurred; inspect positive chain evidence and never resubmit it.",
      } as const;
    }

    return {
      txHash,
      depositEventId,
      status: "ambiguous",
      expectedDepositOutRef,
      depositRowsFound: 0,
      reconciledCount: 0,
      nextSafeAction:
        "Provider evidence is incomplete or inconsistent; do not resubmit.",
    } as const;
  });

export const buildUnsignedDepositTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  TxSignBuilder,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

const parsePreparedSignedTransaction = ({
  txHash,
  signedTxCbor,
}: Pick<PreparedDeposit, "txHash" | "signedTxCbor">): CML.Transaction => {
  const parsed = CML.Transaction.from_cbor_hex(signedTxCbor);
  const recomputedTxHash = CML.hash_transaction(parsed.body()).to_hex();
  if (recomputedTxHash !== txHash) {
    throw new Error(
      `Prepared deposit CBOR hash mismatch: expected=${txHash}, actual=${recomputedTxHash}`,
    );
  }
  return parsed;
};

/**
 * Builds and signs a deposit without submitting it. The returned exact signed
 * CBOR and its derived expected-output identity are the durable recovery
 * checkpoint.
 */
export const prepareDepositWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
  options: {
    readonly submissionAttemptBuilder?: typeof depositSubmissionAttemptFromSignedTx;
  } = {},
): Effect.Effect<
  PreparedDeposit,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
  | TxSignError
> =>
  Effect.gen(function* () {
    const { tx, metadata } = yield* buildUnsignedDepositTxWithMetadataProgram(
      lucid,
      contracts,
      config,
    );
    const txHash = tx.toHash();
    const signed = yield* tx.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.mapError(
          (cause) =>
            new TxSignError({
              message: "Failed to sign deposit transaction",
              cause,
              txHash,
            }),
        ),
      );
    const signedTxCbor = signed.toCBOR();
    const parsedSigned = yield* Effect.try({
      try: () => parsePreparedSignedTransaction({ txHash, signedTxCbor }),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Prepared signed deposit transaction failed identity checks",
          cause,
        }),
    });
    const submissionAttempt = yield* Effect.try({
      try: () =>
        (
          options.submissionAttemptBuilder ??
          depositSubmissionAttemptFromSignedTx
        )({
          txHash,
          signedTxCbor,
          metadata,
          config,
        }),
      catch: (cause) =>
        new SubmitDepositError({
          message:
            "Failed to derive expected deposit output from the prepared signed transaction",
          cause,
        }),
    });
    return {
      txHash,
      signedTxCbor,
      selectedInputOutRefs: inputOutRefsFromSignedTx(parsedSigned),
      metadata,
      submissionAttempt,
    };
  });

/**
 * Submits one exact persisted signed payload. Generic provider retries are
 * disabled. The durable one-way claim prevents every later provider call.
 */
export const submitPreparedDepositProgram = (
  lucid: LucidEvolution,
  prepared: Pick<PreparedDeposit, "txHash" | "signedTxCbor">,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  PreparedDepositSubmission,
  SubmitDepositError | TxSubmitError
> =>
  Effect.gen(function* () {
    yield* Effect.try({
      try: () => parsePreparedSignedTransaction(prepared),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Prepared signed deposit CBOR is invalid",
          cause,
        }),
    });
    const provider = lucid.config().provider;
    if (provider === undefined) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: "Cannot submit prepared deposit without a Lucid provider",
          cause: "provider-unavailable",
        }),
      );
    }
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: () => "<unknown>",
    }).pipe(Effect.catchAll(() => Effect.succeed("<unknown>")));
    let providerTxHash: string | null = null;
    const exactSigned = {
      toCBOR: () => prepared.signedTxCbor,
      submitProgram: () =>
        Effect.tryPromise({
          try: async () => {
            const returnedHash = await provider.submitTx(prepared.signedTxCbor);
            if (returnedHash !== prepared.txHash) {
              throw new Error(
                `Provider returned a mismatched deposit transaction hash: expected=${prepared.txHash}, actual=${returnedHash}`,
              );
            }
            providerTxHash = returnedHash;
            return returnedHash;
          },
          catch: (cause) => cause,
        }),
    } as unknown as Awaited<ReturnType<TxSignBuilder["complete"]>>;

    yield* submitSignedTxWithRecovery(lucid, exactSigned, prepared.txHash, {
      ...options,
      maxProviderRetryAttempts: 0,
      maxOutsideValidityRecoveryAttempts: 0,
    }).pipe(
      Effect.mapError(
        (cause) =>
          new TxSubmitError({
            message: "Failed to submit prepared deposit transaction",
            cause,
            txHash: prepared.txHash,
          }),
      ),
    );
    return {
      txHash: prepared.txHash,
      signedTxCbor: prepared.signedTxCbor,
      walletAddress,
      providerTxHash,
    };
  });

export const buildUnsignedDepositTxFromFundingContextProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  request: BuildDepositRequest,
): Effect.Effect<
  BuiltUnsignedDepositTx,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message:
            "Cardano network not found while preparing deposit transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const externalLucid = yield* Effect.tryPromise({
      try: () => makeLucid(lucid.config().provider, network),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to initialize external-wallet deposit builder",
          cause,
        }),
    });
    yield* Effect.sync(() =>
      externalLucid.selectWallet.fromAddress(request.fundingAddress, [
        ...request.fundingUtxos,
      ]),
    );

    const { tx } = yield* buildUnsignedDepositTxWithMetadataProgram(
      externalLucid,
      contracts,
      request,
    );
    return { unsignedTxCbor: tx.toCBOR() };
  });

const preparedIdentityFromRow = (
  row: DepositSubmissionAttemptsDB.Row,
): Pick<PreparedDeposit, "txHash" | "signedTxCbor"> => ({
  txHash: row[DepositSubmissionAttemptsDB.Columns.TX_HASH].toString("hex"),
  signedTxCbor:
    row[DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR].toString("hex"),
});

const retrieveRequiredDepositAttempt = (
  txHash: string,
): Effect.Effect<
  DepositSubmissionAttemptsDB.Row,
  DatabaseError | SubmitDepositError,
  Database
> =>
  Effect.gen(function* () {
    const row = yield* DepositSubmissionAttemptsDB.retrieveByTxHash(
      Buffer.from(txHash, "hex"),
    );
    if (Option.isNone(row)) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: `Durable deposit submission attempt ${txHash} does not exist`,
          cause: "missing-attempt",
        }),
      );
    }
    return row.value;
  });

const submitDurableDepositAttemptProgram = (
  lucid: LucidEvolution,
  txHash: string,
  eligibility: DepositSubmissionReconciliationResult,
  options: {
    readonly observe?: DepositSubmissionObservationReader;
    readonly submitRecovery?: SubmitRecoveryOptions;
  } = {},
): Effect.Effect<
  PreparedDepositSubmission,
  DatabaseError | SDK.LucidError | SubmitDepositError | TxSubmitError,
  Database | MidgardContracts | LucidService | NodeConfig
> =>
  Effect.gen(function* () {
    if (eligibility.txHash !== txHash || eligibility.status !== "unseen") {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: `Deposit ${txHash} is not eligible for exact-byte submission after status-first reconciliation`,
          cause: eligibility,
        }),
      );
    }

    const txHashBuffer = Buffer.from(txHash, "hex");
    const claimed =
      yield* DepositSubmissionAttemptsDB.beginSubmission(txHashBuffer);
    const persisted = preparedIdentityFromRow(claimed);
    const lucidService = yield* LucidService;
    const submitted = yield* Effect.either(
      submitPreparedDepositProgram(lucid, persisted, {
        slotSnapshot: lucidService.submitSlotSnapshot,
        requireSlotForBoundedTx: true,
        ...options.submitRecovery,
        maxProviderRetryAttempts: 0,
      }),
    );
    if (submitted._tag === "Left") {
      const reason = `Exact deposit provider submission outcome is unknown: ${formatUnknownError(
        submitted.left,
        { includeCause: true },
      )}`;
      yield* DepositSubmissionAttemptsDB.markAmbiguous(txHashBuffer, reason);
      return yield* Effect.fail(submitted.left);
    }
    yield* DepositSubmissionAttemptsDB.markSubmitted(
      txHashBuffer,
      submitted.right.providerTxHash ?? "already_included_confirmed",
    );
    return submitted.right;
  });

const confirmDurableDepositAttemptProgram = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
  attempt: DepositSubmissionAttemptsDB.Row,
  options: {
    readonly observe?: DepositSubmissionObservationReader;
  } = {},
): Effect.Effect<
  "confirmed" | "reconciled_after_timeout",
  | DatabaseError
  | SDK.LucidError
  | SubmitDepositError
  | DepositConfirmationUnknownError,
  Database | MidgardContracts | LucidService | NodeConfig
> =>
  awaitSubmittedTransactionConfirmation(lucid, submission).pipe(
    Effect.tap(() =>
      DepositSubmissionAttemptsDB.markConfirmed(
        Buffer.from(submission.txHash, "hex"),
      ),
    ),
    Effect.as("confirmed" as const),
    Effect.catchTag("TxConfirmError", (error) =>
      Effect.gen(function* () {
        yield* Effect.logWarning(
          `Deposit tx ${submission.txHash} confirmation timed out; reconciling historical chain and mempool evidence.`,
        );
        const reconciliation = yield* reconcileDepositSubmissionAttemptProgram(
          submission.txHash,
          {
            observe: options.observe,
          },
        );
        if (
          reconciliation.status === "reconciled_after_timeout" ||
          reconciliation.status === "confirmed"
        ) {
          return "reconciled_after_timeout" as const;
        }
        return yield* Effect.fail(
          new DepositConfirmationUnknownError({
            message:
              "Deposit confirmation remains nonterminal after historical chain and mempool reconciliation.",
            txHash: submission.txHash,
            depositEventId:
              attempt[
                DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID
              ].toString("hex"),
            expectedDepositOutRef:
              attempt[
                DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF
              ],
            reconciliation,
            cause: error,
          }),
        );
      }),
    ),
  );

export type ResumeDepositSubmissionResult = {
  readonly txHash: string;
  readonly status: "confirmed" | "reconciled_after_timeout" | "accepted";
};

/**
 * Recovers one durable attempt without a builder or signer. Status is observed
 * first, accepted/committed transactions are never submitted, and only a
 * never-claimed prepared row permits its one initial exact-CBOR provider call.
 */
export const resumeDepositSubmissionAttemptProgram = (
  txHash: string,
  options: {
    readonly observe?: DepositSubmissionObservationReader;
    readonly submitRecovery?: SubmitRecoveryOptions;
  } = {},
): Effect.Effect<
  ResumeDepositSubmissionResult,
  | DatabaseError
  | SDK.LucidError
  | SubmitDepositError
  | TxSubmitError
  | DepositConfirmationUnknownError,
  Database | MidgardContracts | LucidService | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* LucidService;
    const reconciled = yield* reconcileDepositSubmissionAttemptProgram(txHash, {
      observe: options.observe,
    });
    if (
      reconciled.status === "confirmed" ||
      reconciled.status === "reconciled_after_timeout"
    ) {
      return { txHash, status: reconciled.status } as const;
    }
    const attempt = yield* retrieveRequiredDepositAttempt(txHash);
    if (reconciled.status === "accepted") {
      const confirmationStatus = yield* confirmDurableDepositAttemptProgram(
        lucidService.api,
        {
          ...preparedIdentityFromRow(attempt),
          walletAddress: "<unknown>",
        },
        attempt,
        { observe: options.observe },
      );
      return { txHash, status: confirmationStatus } as const;
    }
    if (
      reconciled.status !== "unseen" ||
      attempt[DepositSubmissionAttemptsDB.Columns.STATUS] !==
        DepositSubmissionAttemptsDB.Status.Prepared
    ) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: `Deposit ${txHash} recovery is fail-closed in observed status ${reconciled.status} and durable status ${attempt[DepositSubmissionAttemptsDB.Columns.STATUS]}`,
          cause: reconciled,
        }),
      );
    }
    const submission = yield* submitDurableDepositAttemptProgram(
      lucidService.api,
      txHash,
      reconciled,
      options,
    );
    const currentAttempt = yield* retrieveRequiredDepositAttempt(txHash);
    const confirmationStatus = yield* confirmDurableDepositAttemptProgram(
      lucidService.api,
      submission,
      currentAttempt,
      { observe: options.observe },
    );
    return { txHash, status: confirmationStatus } as const;
  });

export const STARTUP_DEPOSIT_RECONCILIATION_LIMIT = 32;
export const STARTUP_DEPOSIT_RECONCILIATION_CONCURRENCY = 4;
export const STARTUP_DEPOSIT_RECONCILIATION_ATTEMPT_TIMEOUT_MS = 15_000;

/**
 * Best-effort startup observation. The caller runs this concurrently with the
 * server; this sweep is bounded and never submits a transaction.
 */
export const reconcileOpenDepositSubmissionAttemptsProgram = (
  options: {
    readonly limit?: number;
    readonly concurrency?: number;
    readonly attemptTimeoutMs?: number;
    readonly observe?: DepositSubmissionObservationReader;
  } = {},
) =>
  Effect.gen(function* () {
    const attempts = yield* DepositSubmissionAttemptsDB.retrieveOpenAttempts();
    const limit = Math.max(
      0,
      Math.floor(options.limit ?? STARTUP_DEPOSIT_RECONCILIATION_LIMIT),
    );
    const concurrency = Math.max(
      1,
      Math.floor(
        options.concurrency ?? STARTUP_DEPOSIT_RECONCILIATION_CONCURRENCY,
      ),
    );
    const attemptTimeoutMs = Math.max(
      1,
      Math.floor(
        options.attemptTimeoutMs ??
          STARTUP_DEPOSIT_RECONCILIATION_ATTEMPT_TIMEOUT_MS,
      ),
    );
    const selected = attempts.slice(0, limit);
    const results = yield* Effect.forEach(
      selected,
      (attempt) => {
        const txHash =
          attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH].toString("hex");
        return reconcileDepositSubmissionAttemptProgram(txHash, {
          observe: options.observe,
        }).pipe(
          Effect.timeoutOption(Duration.millis(attemptTimeoutMs)),
          Effect.either,
          Effect.map((result) => {
            if (result._tag === "Left") {
              return {
                txHash,
                status: "failed" as const,
                reason: formatUnknownError(result.left, {
                  includeCause: true,
                }),
              };
            }
            if (Option.isNone(result.right)) {
              return {
                txHash,
                status: "timed_out" as const,
                reason: `observation exceeded ${attemptTimeoutMs.toString()}ms`,
              };
            }
            return result.right.value;
          }),
        );
      },
      { concurrency },
    );
    return {
      open: attempts.length,
      inspected: selected.length,
      deferred: attempts.length - selected.length,
      results,
    } as const;
  });

export const submitDepositWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  SubmittedDeposit,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
  | TxSubmitError
  | TxSignError
  | DatabaseError
  | DepositConfirmationUnknownError,
  Database | MidgardContracts | LucidService | NodeConfig
> =>
  Effect.gen(function* () {
    const prepared = yield* prepareDepositWithMetadataProgram(
      lucid,
      contracts,
      config,
    );
    const inserted = yield* DepositSubmissionAttemptsDB.insertPrepared(
      prepared.submissionAttempt,
    );
    const eligibility = yield* reconcileDepositSubmissionAttemptProgram(
      prepared.txHash,
    );
    const submission = yield* submitDurableDepositAttemptProgram(
      lucid,
      prepared.txHash,
      eligibility,
    );
    const confirmationStatus = yield* confirmDurableDepositAttemptProgram(
      lucid,
      submission,
      inserted,
    );
    return {
      txHash: prepared.txHash,
      metadata: prepared.metadata,
      confirmationStatus,
    };
  });

type UnknownRecord = Record<string, unknown>;

const asObject = (value: unknown, field: string): UnknownRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object.`);
  }
  return value as UnknownRecord;
};

const parseRequiredString = (value: unknown, field: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string.`);
  }
  const normalized = value.trim();
  if (normalized.length === 0) {
    throw new Error(`${field} must not be empty.`);
  }
  return normalized;
};

const parseOptionalString = (value: unknown, field: string): string | null => {
  if (value === undefined || value === null) {
    return null;
  }
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string when provided.`);
  }
  const normalized = value.trim();
  return normalized.length === 0 ? null : normalized;
};

const parsePositiveIntegerString = (value: string, field: string): bigint => {
  const normalized = value.trim();
  if (!/^[1-9]\d*$/.test(normalized)) {
    throw new Error(`${field} must be a positive integer string.`);
  }
  return BigInt(normalized);
};

const parseNonNegativeInteger = (value: unknown, field: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${field} must be a non-negative integer.`);
  }
  return value;
};

const expectedNetworkIdForAddressValidation = (
  network: string | undefined,
): number | undefined => {
  if (network === undefined || network === "Custom") {
    return undefined;
  }
  return network === "Mainnet" ? 1 : 0;
};

const parseAddressString = ({
  value,
  field,
  expectedNetwork,
}: {
  readonly value: unknown;
  readonly field: string;
  readonly expectedNetwork?: string;
}): string => {
  const normalized = parseRequiredString(value, field);
  let details: ReturnType<typeof getAddressDetails>;
  try {
    details = getAddressDetails(normalized);
  } catch (cause) {
    throw new Error(`Invalid ${field} "${normalized}": ${String(cause)}`);
  }
  const expectedNetworkId =
    expectedNetworkIdForAddressValidation(expectedNetwork);
  if (
    expectedNetworkId !== undefined &&
    details.networkId !== expectedNetworkId
  ) {
    throw new Error(
      `${field} must target the configured ${expectedNetwork} network.`,
    );
  }
  return details.address.bech32;
};

const normalizeAssetUnit = (value: string, field: string): string => {
  const normalized = value.trim();
  const assetName = normalizeCoreHex(normalized.slice(56), {
    fieldName: `${field}.assetName`,
    allowEmpty: true,
  });
  if (assetName.length > 64) {
    throw new Error(
      `${field} must be a Cardano unit string (56 hex policy id plus optional asset-name hex).`,
    );
  }
  return `${normalizeCoreHex(normalized.slice(0, 56), {
    fieldName: `${field}.policyId`,
    byteLength: 28,
  })}${assetName}`;
};

const normalizeOptionalHexField = (
  value: unknown,
  field: string,
  byteLength?: number,
): string | null => {
  if (value === undefined || value === null) {
    return null;
  }
  if (typeof value !== "string") {
    throw new Error(`${field} must be a hex string when provided.`);
  }
  const normalized = value.trim();
  if (normalized.length === 0) {
    return null;
  }
  return normalizeCoreHex(normalized, { fieldName: field, byteLength });
};

const parseFundingAssets = (value: unknown, field: string): Assets => {
  const rawAssets = asObject(value, field);
  const entries = Object.entries(rawAssets);
  if (entries.length === 0) {
    throw new Error(`${field} must include at least lovelace.`);
  }
  if (entries.length > MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES) {
    throw new Error(
      `${field} exceeds the maximum asset entry count (${entries.length} > ${MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES}).`,
    );
  }

  const assets: Assets = {};
  for (const [unitKey, amountValue] of entries) {
    const unit =
      unitKey === "lovelace"
        ? "lovelace"
        : normalizeAssetUnit(unitKey, `${field}.${unitKey}`);
    if (assets[unit] !== undefined) {
      throw new Error(`Duplicate asset unit "${unit}" in ${field}.`);
    }
    assets[unit] = parsePositiveIntegerString(
      parseRequiredString(amountValue, `${field}.${unit}`),
      `${field}.${unit}`,
    );
  }
  if (assets.lovelace === undefined) {
    throw new Error(`${field} must include lovelace.`);
  }
  return assets;
};

const parseAdditionalAssetsFromRequest = (value: unknown): Readonly<Assets> => {
  if (value === undefined || value === null) {
    return {};
  }
  if (!Array.isArray(value)) {
    throw new Error("additionalAssets must be an array when provided.");
  }
  if (value.length > MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS) {
    throw new Error(
      `additionalAssets exceeds the maximum entry count (${value.length} > ${MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS}).`,
    );
  }

  const assets: Assets = {};
  for (const [index, entry] of value.entries()) {
    const field = `additionalAssets[${index.toString()}]`;
    const raw = asObject(entry, field);
    const unit = normalizeAssetUnit(
      parseRequiredString(raw.unit, `${field}.unit`),
      `${field}.unit`,
    );
    if (assets[unit] !== undefined) {
      throw new Error(`Duplicate additional asset "${unit}" provided.`);
    }
    assets[unit] = parsePositiveIntegerString(
      parseRequiredString(raw.amount, `${field}.amount`),
      `${field}.amount`,
    );
  }
  return assets;
};

const parseFundingUtxos = ({
  value,
  fundingAddress,
  expectedNetwork,
}: {
  readonly value: unknown;
  readonly fundingAddress: string;
  readonly expectedNetwork?: string;
}): readonly UTxO[] => {
  if (!Array.isArray(value)) {
    throw new Error("fundingUtxos must be an array.");
  }
  if (value.length === 0) {
    throw new Error("fundingUtxos must not be empty.");
  }
  if (value.length > MAX_DEPOSIT_BUILD_FUNDING_UTXOS) {
    throw new Error(
      `fundingUtxos exceeds the maximum count (${value.length} > ${MAX_DEPOSIT_BUILD_FUNDING_UTXOS}).`,
    );
  }

  const seenOutRefs = new Set<string>();
  return value.map((entry, index) => {
    const field = `fundingUtxos[${index.toString()}]`;
    const raw = asObject(entry, field);
    const txHash = normalizeCoreHex(
      parseRequiredString(raw.txHash, `${field}.txHash`),
      { fieldName: `${field}.txHash`, byteLength: 32 },
    );
    const outputIndex = parseNonNegativeInteger(
      raw.outputIndex,
      `${field}.outputIndex`,
    );
    const outRefKey = `${txHash}#${outputIndex.toString()}`;
    if (seenOutRefs.has(outRefKey)) {
      throw new Error(`Duplicate funding UTxO "${outRefKey}" provided.`);
    }
    seenOutRefs.add(outRefKey);

    const utxoAddress = parseAddressString({
      value: raw.address,
      field: `${field}.address`,
      expectedNetwork,
    });
    if (utxoAddress !== fundingAddress) {
      throw new Error(`${field}.address must match fundingAddress.`);
    }

    const datumHash = normalizeOptionalHexField(
      raw.datumHash,
      `${field}.datumHash`,
      32,
    );
    const datum = normalizeOptionalHexField(raw.datum, `${field}.datum`);
    if (parseOptionalString(raw.scriptRef, `${field}.scriptRef`) !== null) {
      throw new Error(
        `${field}.scriptRef is not supported for deposit build funding inputs.`,
      );
    }

    return {
      txHash,
      outputIndex,
      address: utxoAddress,
      assets: parseFundingAssets(raw.assets, `${field}.assets`),
      datumHash: datumHash ?? undefined,
      datum: datum ?? undefined,
      scriptRef: undefined,
    };
  });
};

const buildSubmitDepositConfig = ({
  l2Address,
  l2Datum,
  lovelace,
  additionalAssets,
  expectedNetwork,
}: {
  readonly l2Address: unknown;
  readonly l2Datum?: unknown;
  readonly lovelace: unknown;
  readonly additionalAssets: Readonly<Assets>;
  readonly expectedNetwork?: string;
}): SubmitDepositConfig => {
  const normalizedL2Address = parseAddressString({
    value: l2Address,
    field: "l2Address",
    expectedNetwork,
  });
  const l2DatumHex = parseOptionalString(l2Datum, "l2Datum");

  return {
    l2Address: normalizedL2Address,
    l2Datum:
      l2DatumHex === null
        ? null
        : normalizeCoreHex(l2DatumHex, {
            fieldName: "L2 datum",
            allowEmpty: true,
          }),
    lovelace: parseLovelaceAmount(
      parseRequiredString(lovelace, "lovelace"),
      "Deposit lovelace amount must be greater than zero.",
    ),
    additionalAssets,
  };
};

export const parseSubmitDepositConfig = ({
  l2Address,
  l2Datum,
  lovelace,
  assetSpecs,
}: {
  readonly l2Address: string;
  readonly l2Datum?: string;
  readonly lovelace: string;
  readonly assetSpecs: readonly string[];
}): SubmitDepositConfig =>
  buildSubmitDepositConfig({
    l2Address,
    l2Datum,
    lovelace,
    additionalAssets: parseAdditionalAssetSpecs(assetSpecs),
  });

export const parseBuildDepositRequest = (
  payload: unknown,
  options?: {
    readonly expectedNetwork?: string;
  },
): BuildDepositRequest => {
  const body = asObject(payload, "Deposit build request");
  const fundingAddress = parseAddressString({
    value: body.fundingAddress,
    field: "fundingAddress",
    expectedNetwork: options?.expectedNetwork,
  });
  const fundingUtxos = parseFundingUtxos({
    value: body.fundingUtxos,
    fundingAddress,
    expectedNetwork: options?.expectedNetwork,
  });

  return {
    ...buildSubmitDepositConfig({
      l2Address: body.l2Address,
      l2Datum: body.l2Datum,
      lovelace: body.lovelace,
      additionalAssets: parseAdditionalAssetsFromRequest(body.additionalAssets),
      expectedNetwork: options?.expectedNetwork,
    }),
    fundingAddress,
    fundingUtxos,
  };
};
