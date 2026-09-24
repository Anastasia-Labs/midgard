import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect, Option, Runtime } from "effect";

import * as Journal from "../database/eventHistorySubmissions.js";
import { Database } from "../services/database.js";
import {
  awaitSubmittedTransactionConfirmation,
  submitSignedTxWithRecovery,
} from "./utils.js";

export class HistorySubmissionError extends EffectData.TaggedError(
  "HistorySubmissionError",
)<{
  readonly message: string;
  readonly submissionId: string;
  readonly cause: unknown;
}> {}

type Prepared = {
  readonly request: SDK.EventHistorySubmissionRequest;
};

const encodeAssets = (assets: Assets) =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, amount.toString()]),
  );
const decodeAssets = (assets: Readonly<Record<string, string>>): Assets =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, BigInt(amount)]),
  );

export const encodeHistorySubmissionRequest = (
  request: SDK.EventHistorySubmissionRequest,
): Journal.StoredRequest => {
  if (
    request.nonce.datum != null ||
    request.nonce.datumHash != null ||
    request.nonce.scriptRef != null
  )
    throw new Error("History nonce must be a plain wallet output");
  if (request.payload !== undefined && request.payloadCbor !== undefined)
    throw new Error("History payload must have exactly one encoding source");
  const payloadCbor =
    request.payloadCbor ?? Data.to(request.payload, SDK.EventHistoryPayload);
  Data.from(payloadCbor, SDK.EventHistoryPayload);
  return {
    payloadCbor,
    reclaimAuthCbor: Data.to(request.reclaimAuth, SDK.CredentialD),
    assets: encodeAssets(request.assets),
    structuralLovelace: request.structuralLovelace.toString(),
    structuralRefundKey: request.structuralRefundKey,
    nonce: {
      txHash: request.nonce.txHash,
      outputIndex: request.nonce.outputIndex,
      address: request.nonce.address,
      assets: encodeAssets(request.nonce.assets),
    },
  };
};

export const decodeHistorySubmissionRequest = (
  request: Journal.StoredRequest,
): Extract<SDK.EventHistorySubmissionRequest, { payloadCbor: string }> => {
  Data.from(request.payloadCbor, SDK.EventHistoryPayload);
  return {
    payloadCbor: request.payloadCbor,
    reclaimAuth: Data.from(request.reclaimAuthCbor, SDK.CredentialD),
    assets: decodeAssets(request.assets),
    structuralLovelace: BigInt(request.structuralLovelace),
    structuralRefundKey: request.structuralRefundKey,
    nonce: { ...request.nonce, assets: decodeAssets(request.nonce.assets) },
  };
};

export const assertHistorySubmissionAttempt = (
  attempt: SDK.EventHistorySubmissionAttempt,
) => {
  const transaction = CML.Transaction.from_cbor_hex(attempt.transactionCbor);
  if (
    CML.hash_transaction(transaction.body()).to_hex() !== attempt.txHash ||
    !Number.isSafeInteger(attempt.outputIndex) ||
    attempt.outputIndex < 0 ||
    attempt.outputIndex >= transaction.body().outputs().len()
  )
    throw new Error("History attempt does not match its completed transaction");
};

export const historyAdmissionMetadata = (
  lucid: LucidEvolution,
  attempt: SDK.EventHistorySubmissionAttempt,
) => {
  assertHistorySubmissionAttempt(attempt);
  if (attempt.phase !== "Admission")
    throw new Error("Expected history admission attempt");
  const body = CML.Transaction.from_cbor_hex(attempt.transactionCbor).body();
  const output = coreToTxOutput(body.outputs().get(attempt.outputIndex));
  if (output.datum == null || body.ttl() === undefined)
    throw new Error("History admission lacks a datum or finite validity bound");
  const node = Data.from(output.datum, SDK.EventHistoryNode);
  if (
    node.position === "Root" ||
    node.payload === "RootContent" ||
    !("Order" in node.payload)
  )
    throw new Error("History admission output is not an Order");
  return {
    output,
    key: node.position.Key[0],
    facts: node.payload.Order.facts,
    validTo: lucid.slotToUnixTime(Number(body.ttl())),
  };
};

export const historyIntentOptionsData = (
  config: SDK.UserHistoryBuildOptions,
): Data => {
  if (config.externalData !== undefined || config.validity !== undefined)
    throw new Error(
      "Automatic durable submission owns publication and validity; use the staged unsigned API for explicit outputs or bounds",
    );
  return [
    config.nonceInput === undefined
      ? []
      : [Data.from(SDK.outputReferenceToPlutusDataCbor(config.nonceInput))],
    config.reclaimAuth === undefined
      ? []
      : [Data.from(Data.to(config.reclaimAuth, SDK.CredentialD))],
    config.structuralRefundKey === undefined
      ? []
      : [config.structuralRefundKey],
  ];
};

/** Exact-body transport shared by fresh submission and restart. Absence is
 * deliberately not a rejection: providers may lag or outputs may be spent. */
export const historySubmissionTransport = (
  lucid: LucidEvolution,
  walletAddress: string,
) => {
  const observe = async (
    attempt: SDK.EventHistorySubmissionAttempt,
  ): Promise<SDK.EventHistorySubmissionOutcome> => {
    const status = await lucid.transactionStatus(attempt.txHash);
    if (
      status.txHash !== attempt.txHash ||
      (status.status === "confirmed" &&
        status.confirmation.txHash !== attempt.txHash)
    )
      throw new Error(
        "Provider returned confirmation for a different transaction",
      );
    return { kind: status.status === "confirmed" ? "Confirmed" : "Pending" };
  };
  const submit = async (
    tx: ReturnType<LucidEvolution["fromTx"]>,
    attempt: SDK.EventHistorySubmissionAttempt,
  ): Promise<SDK.EventHistorySubmissionOutcome> => {
    assertHistorySubmissionAttempt(attempt);
    if (tx.toHash() !== attempt.txHash)
      throw new Error("Signing would change the persisted history transaction");
    const signed = await tx.sign.withWallet().complete();
    const signedTxCbor = signed.toCBOR();
    if (
      CML.hash_transaction(
        CML.Transaction.from_cbor_hex(signedTxCbor).body(),
      ).to_hex() !== attempt.txHash
    )
      throw new Error("Wallet changed the history transaction body");
    try {
      await Effect.runPromise(
        submitSignedTxWithRecovery(lucid, signed, attempt.txHash),
      );
      await Effect.runPromise(
        awaitSubmittedTransactionConfirmation(lucid, {
          txHash: attempt.txHash,
          signedTxCbor,
          walletAddress,
        }),
      );
      return { kind: "Confirmed" };
    } catch (cause) {
      // A BadInputs response can mean this exact transaction already landed.
      // Neither a submit error nor not_found authorizes another transaction.
      const outcome = await observe(attempt);
      if (outcome.kind !== "Confirmed") throw cause;
      return outcome;
    }
  };
  return { observe, submit };
};

/** Both event kinds resume from the stored request before selecting any nonce.
 * The journal is operational recovery state; every success still needs current
 * exact-hash provider confirmation. Call outside a surrounding SQL transaction:
 * each checkpoint must commit before the corresponding external action. */
export const submitDurableEventHistoryProgram = <E, F = never>({
  lucid,
  contracts,
  kind,
  submissionId,
  intentHash,
  prepare,
  beforeAdmission,
  nonceInput,
  scriptReference,
  timeoutMs = 180_000,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.UserHistoryContracts;
  readonly kind: "Deposit" | "Withdrawal";
  readonly submissionId: string;
  /** Hash of caller intent before nonce selection, using canonical schema encodings. */
  readonly intentHash: string;
  readonly prepare: (
    nonce: Pick<UTxO, "txHash" | "outputIndex">,
  ) => Effect.Effect<Prepared, E>;
  readonly beforeAdmission?: (
    attempt: SDK.EventHistorySubmissionAttempt,
    request: SDK.EventHistorySubmissionRequest,
  ) => Effect.Effect<void, F, Database>;
  readonly nonceInput?: Pick<UTxO, "txHash" | "outputIndex">;
  readonly scriptReference?: UTxO;
  readonly timeoutMs?: number;
}) =>
  Effect.gen(function* () {
    const runtime = yield* Effect.runtime<Database>();
    const run = Runtime.runPromise(runtime);
    const wrap = (cause: unknown) =>
      new HistorySubmissionError({
        message: `History submission ${submissionId} requires reconciliation: ${String(cause)}`,
        submissionId,
        cause,
      });
    if (
      !/^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$/u.test(submissionId) ||
      !/^[0-9a-f]{64}$/u.test(intentHash) ||
      !Number.isSafeInteger(timeoutMs) ||
      timeoutMs <= 0
    )
      return yield* Effect.fail(
        wrap("Invalid submission ID, intent hash or timeout"),
      );
    const pair = yield* Effect.try({
      try: () => SDK.requireEventHistoryContracts(contracts),
      catch: wrap,
    });
    const history = kind === "Deposit" ? pair.deposit : pair.withdrawal;
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: wrap,
    });
    const identity: Journal.Identity = {
      submission_id: submissionId,
      kind,
      policy_id: history.list.policyId,
      wallet_address: walletAddress,
      intent_hash: intentHash,
    };
    const existing = yield* Journal.retrieve(submissionId);
    let row: Journal.Row;
    if (Option.isSome(existing)) {
      if (!Journal.matchesIdentity(existing.value, identity))
        return yield* Effect.fail(
          wrap(
            "Submission ID belongs to a different intent, wallet or deployment",
          ),
        );
      row = existing.value;
    } else {
      const reserved = yield* Journal.reservedInputs(walletAddress);
      const candidates = yield* Effect.tryPromise({
        try: () => lucid.utxosAt(walletAddress),
        catch: wrap,
      });
      const nonce = candidates
        .filter(
          (utxo) =>
            !reserved.has(outRefLabel(utxo)) &&
            utxo.datum == null &&
            utxo.datumHash == null &&
            utxo.scriptRef == null &&
            !Object.keys(utxo.assets).some(
              (unit) =>
                unit.startsWith(pair.deposit.list.policyId) ||
                unit.startsWith(pair.withdrawal.list.policyId),
            ) &&
            (nonceInput === undefined ||
              outRefLabel(nonceInput) === outRefLabel(utxo)),
        )
        .sort(compareOutRefs)[0];
      if (nonce === undefined)
        return yield* Effect.fail(
          wrap("No unreserved plain wallet nonce is available"),
        );
      const prepared = yield* prepare(nonce);
      const stored = yield* Effect.try({
        try: () => ({
          ...identity,
          nonce_out_ref: outRefLabel(prepared.request.nonce),
          request: encodeHistorySubmissionRequest(prepared.request),
          checkpoint: {
            requestHash: SDK.eventHistorySubmissionRequestHash(
              history.list.policyId,
              prepared.request,
              history.recipe,
            ),
          },
        }),
        catch: wrap,
      });
      if (stored.nonce_out_ref !== outRefLabel(nonce))
        return yield* Effect.fail(
          wrap("Prepared request changed its reserved nonce"),
        );
      row = yield* Journal.reserve(stored);
    }
    const request = yield* Effect.try({
      try: () => decodeHistorySubmissionRequest(row.request),
      catch: wrap,
    });
    const hub = yield* SDK.fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
    const network = lucid.config().network;
    if (network === undefined)
      return yield* Effect.fail(wrap("Missing Cardano network"));
    const context: Omit<SDK.EventHistoryBuildContext, "fundingInputs"> = {
      lucid,
      recipe: history.recipe,
      hubReference: hub.utxo,
      scriptReference,
      applied: {
        validator: history.list.spendingScript,
        policyId: history.list.policyId,
        address: history.list.spendingScriptAddress,
        rewardAddress: validatorToRewardAddress(
          network,
          history.list.withdrawalScript,
        ),
        retention: {
          validator: history.retention.spendingScript,
          address: history.retention.spendingScriptAddress,
        },
      },
    };
    return yield* Effect.tryPromise({
      try: async () => {
        const save = async (
          checkpoint: SDK.EventHistorySubmissionCheckpoint,
        ) => {
          row = await run(Journal.saveCheckpoint(row, checkpoint));
        };
        const transport = historySubmissionTransport(lucid, walletAddress);
        const submit = async (
          tx: ReturnType<LucidEvolution["fromTx"]>,
          attempt: SDK.EventHistorySubmissionAttempt,
        ): Promise<SDK.EventHistorySubmissionOutcome> => {
          if (attempt.phase === "Admission" && beforeAdmission !== undefined)
            await run(beforeAdmission(attempt, request));
          return transport.submit(tx, attempt);
        };
        const checkpoint = await SDK.submitEventHistory({
          context,
          request,
          checkpoint: row.checkpoint,
          maxAttempts: 4,
          deadlineMs: Date.now() + timeoutMs,
          validityDurationMs: 180_000,
          outputVisibilityAttempts: 8,
          retryDelayMs: 1_000,
          driver: {
            save,
            submit,
            reconcile: async (attempt) => {
              assertHistorySubmissionAttempt(attempt);
              const status = await transport.observe(attempt);
              if (status.kind === "Confirmed") return status;
              // Re-sign/rebroadcast the identical completed body. This also recovers
              // a crash before the original signature, without allocating another ID.
              return submit(lucid.fromTx(attempt.transactionCbor), attempt);
            },
            funding: async () => {
              const reserved = await run(Journal.reservedInputs(walletAddress));
              return (await lucid.utxosAt(walletAddress))
                .filter(
                  (utxo) =>
                    !reserved.has(outRefLabel(utxo)) &&
                    utxo.datum == null &&
                    utxo.datumHash == null &&
                    utxo.scriptRef == null &&
                    !Object.keys(utxo.assets).some(
                      (unit) =>
                        unit.startsWith(pair.deposit.list.policyId) ||
                        unit.startsWith(pair.withdrawal.list.policyId),
                    ),
                )
                .sort(compareOutRefs);
            },
            now: Date.now,
            waitUntil: async (target) => {
              await new Promise((resolve) =>
                setTimeout(resolve, Math.max(0, target - Date.now())),
              );
            },
          },
        });
        if (
          checkpoint.admission === undefined ||
          !("transactionCbor" in checkpoint.admission)
        )
          throw new Error(
            "Confirmed admission has no completed transaction receipt",
          );
        return { request, checkpoint, admission: checkpoint.admission };
      },
      catch: wrap,
    });
  });
