import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  getAddressDetails,
  type LucidEvolution,
  type Script,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { exactObjectKeys } from "@/exact-object-keys.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

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

export type PhasMembershipRegistrationStatus = "registered" | "unregistered";

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
  readonly inspectRegistrationTx?: (
    built: SDK.BuiltPhasMembershipRewardRegistrationTx,
    expected: { readonly rewardAddress: string; readonly scriptHash: string },
  ) => CapturedPhasMembershipRegistrationTransaction;
  readonly onSubmittedRegistrationTx?: (
    capture: CapturedPhasMembershipRegistrationTransaction,
  ) => void;
};

export const decodePhasMembershipRegistrationTransactionBodyEvidenceV1 = (
  value: unknown,
): PhasMembershipRegistrationTransactionBodyEvidence => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "txHash",
      "cborSha256",
      "cborSizeBytes",
      "certificate",
    ]) ||
    !exactObjectKeys(value.certificate, [
      "kind",
      "index",
      "count",
      "credentialType",
      "scriptHash",
    ])
  ) {
    throw new Error(
      "PHAS registration transaction-body evidence fields do not match the exact V1 schema",
    );
  }
  if (
    value.schemaVersion !== "midgard-phas-registration-transaction-body-v1" ||
    typeof value.txHash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.txHash) ||
    typeof value.cborSha256 !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.cborSha256) ||
    !Number.isSafeInteger(value.cborSizeBytes) ||
    (value.cborSizeBytes as number) <= 0 ||
    value.certificate.kind !== "stake_registration" ||
    value.certificate.index !== 0 ||
    value.certificate.count !== 1 ||
    value.certificate.credentialType !== "script" ||
    typeof value.certificate.scriptHash !== "string" ||
    !/^[a-f0-9]{56}$/u.test(value.certificate.scriptHash)
  ) {
    throw new Error(
      "PHAS registration transaction-body evidence contains a noncanonical V1 value",
    );
  }
  return value as PhasMembershipRegistrationTransactionBodyEvidence;
};

export const decodePhasMembershipRewardRegistrationResultV1 = (
  value: unknown,
): PhasMembershipRewardRegistrationResult => {
  if (
    !exactObjectKeys(value, [
      "status",
      "rewardAddress",
      "scriptHash",
      "txHash",
      "transactionBody",
    ])
  ) {
    throw new Error(
      "PHAS registration result fields do not match the exact V1 schema",
    );
  }
  if (
    typeof value.rewardAddress !== "string" ||
    !/^stake(?:_test)?1[0-9a-z]+$/u.test(value.rewardAddress) ||
    typeof value.scriptHash !== "string" ||
    !/^[a-f0-9]{56}$/u.test(value.scriptHash)
  ) {
    throw new Error("PHAS registration result identity is noncanonical");
  }
  let rewardAddressDetails: ReturnType<typeof getAddressDetails>;
  try {
    rewardAddressDetails = getAddressDetails(value.rewardAddress);
  } catch (cause) {
    throw new Error("PHAS registration result reward address is invalid", {
      cause,
    });
  }
  if (
    rewardAddressDetails.type !== "Reward" ||
    rewardAddressDetails.stakeCredential?.type !== "Script" ||
    rewardAddressDetails.stakeCredential.hash !== value.scriptHash
  ) {
    throw new Error(
      "PHAS registration result reward address is not bound to its script hash",
    );
  }
  if (value.status === "already_registered") {
    if (value.txHash !== null || value.transactionBody !== null) {
      throw new Error(
        "PHAS already-registered result must not contain transaction evidence",
      );
    }
    return value as PhasMembershipRewardRegistrationResult;
  }
  if (value.status !== "registration_submitted") {
    throw new Error("PHAS registration result status is not canonical V1");
  }
  const transactionBody =
    decodePhasMembershipRegistrationTransactionBodyEvidenceV1(
      value.transactionBody,
    );
  if (
    value.txHash !== transactionBody.txHash ||
    value.scriptHash !== transactionBody.certificate.scriptHash
  ) {
    throw new Error(
      "PHAS registration result is not bound to its transaction evidence",
    );
  }
  return value as PhasMembershipRewardRegistrationResult;
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
  const evidence = decodePhasMembershipRegistrationTransactionBodyEvidenceV1({
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
  });
  return {
    evidence,
    unsignedTransactionCborHex,
  };
};

export const isPhasMembershipAlreadyRegisteredError = (
  error: TxSubmitError,
  scriptHash: string,
): boolean => {
  const normalizedScriptHash = scriptHash.toLowerCase();
  const seen = new Set<unknown>();

  const hasKnownCredential = (value: unknown): boolean => {
    if (typeof value !== "object" || value === null || seen.has(value)) {
      return false;
    }
    seen.add(value);
    const record = value as Record<string, unknown>;
    if (
      typeof record.knownCredential === "string" &&
      record.knownCredential.toLowerCase() === normalizedScriptHash &&
      (record.from === undefined || record.from === "script")
    ) {
      return true;
    }
    return Object.values(record).some(hasKnownCredential);
  };

  const findTypedOgmiosError = (value: unknown): boolean => {
    if (typeof value !== "object" || value === null || seen.has(value)) {
      return false;
    }
    const record = value as Record<string, unknown>;
    if (record._tag === "OgmiosJsonRpcError") {
      seen.clear();
      return hasKnownCredential(record.data);
    }
    seen.add(value);
    return [record.cause, record.error, record.failure].some(
      findTypedOgmiosError,
    );
  };

  return findTypedOgmiosError(error);
};

export const queryPhasMembershipRewardAccountRegisteredProgram = (
  lucid: LucidEvolution,
  rewardAddress: string,
): Effect.Effect<PhasMembershipRegistrationStatus, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () =>
      (await lucid.rewardAccountAt(rewardAddress)).registered
        ? "registered"
        : "unregistered",
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
      return decodePhasMembershipRewardRegistrationResultV1({
        status: "already_registered",
        rewardAddress: identity.rewardAddress,
        scriptHash: identity.scriptHash,
        txHash: null,
        transactionBody: null,
      });
    }

    const built = yield* (
      options.buildRegistrationTx ?? defaultBuildRegistrationTx
    )(lucid, { script });
    const capturedTransaction = yield* Effect.try({
      try: () => {
        const captured = (
          options.inspectRegistrationTx ??
          inspectPhasMembershipRegistrationTransaction
        )(built, identity);
        decodePhasMembershipRegistrationTransactionBodyEvidenceV1(
          captured.evidence,
        );
        return captured;
      },
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
        return decodePhasMembershipRewardRegistrationResultV1({
          status: "already_registered",
          rewardAddress: built.rewardAddress,
          scriptHash: built.scriptHash,
          txHash: null,
          transactionBody: null,
        });
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
    return decodePhasMembershipRewardRegistrationResultV1({
      status: "registration_submitted",
      rewardAddress: built.rewardAddress,
      scriptHash: built.scriptHash,
      txHash: submitted.right,
      transactionBody: capturedTransaction.evidence,
    });
  });
