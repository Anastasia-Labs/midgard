import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { DaPayloadsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { NodeConfig } from "../services/config.js";
import {
  availabilityParametersFromExplicitEnvironment,
  availabilityParametersFromManifest,
  ContractDeploymentIdentity,
  Database,
  Lucid,
  MidgardContracts,
} from "../services/index.js";
import { attestHeader } from "./da-attestation.attest-header.js";
import {
  type AttestStateQueueHeaderResult,
  type AttestStateQueueOnceOptions,
  fetchDaAttestationReferenceScripts,
  fetchDaParamsUtxo,
} from "./da-attestation.fetch-da-attestation-reference-scripts.js";
import {
  DA_ATTESTATION_POOL_SKIP_EVENT,
  daBondPoolBackingAttestationProgram,
  fetchUnattestedHeaders,
  isDaBondPoolAttestationSkip,
} from "./da-attestation.fetch-unattested-headers.js";
import { TxConfirmError, TxSignError, TxSubmitError } from "./utils.js";

export const attestStateQueueOnceProgram = (
  options: AttestStateQueueOnceOptions = {},
): Effect.Effect<
  readonly AttestStateQueueHeaderResult[],
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.DaAttestationBuildError
  | SDK.StateQueueError
  | DatabaseError
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  Lucid | MidgardContracts | NodeConfig | Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const daParams = yield* fetchDaParamsUtxo(lucid, contracts);
    const availabilityParameters =
      deploymentIdentity.manifest === undefined
        ? availabilityParametersFromExplicitEnvironment()
        : availabilityParametersFromManifest(
            deploymentIdentity.manifest.availabilityChallenge,
          );
    const referenceScripts = yield* fetchDaAttestationReferenceScripts(
      lucid,
      lucidService.referenceScriptsAddress,
      contracts,
    );
    const targets = yield* fetchUnattestedHeaders(
      lucid,
      contracts,
      options.headerHash,
    );
    yield* Effect.logInfo(
      `DA attestation targets selected: count=${targets.length.toString()},headers=${targets.map((target) => target.headerHash).join(",")}`,
    );
    const results: AttestStateQueueHeaderResult[] = [];
    for (const target of targets) {
      const payloadRow = yield* DaPayloadsDB.retrieveByHeaderHash(
        Buffer.from(target.headerHash, "hex"),
      );
      if (Option.isNone(payloadRow)) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Refusing to attest a state-queue header without its canonical retained DA payload",
            cause: `header=${target.headerHash}`,
          }),
        );
      }
      const payloadCbor = payloadRow.value[DaPayloadsDB.Columns.PAYLOAD_CBOR];
      const payloadHash = SDK.daPayloadHashHex(payloadCbor);
      const storedPayloadHash =
        payloadRow.value[DaPayloadsDB.Columns.PAYLOAD_SHA256].toString("hex");
      const payload = yield* Effect.tryPromise({
        try: () =>
          payloadRow.value[DaPayloadsDB.Columns.VERSION] !==
          Number(SDK.DA_PAYLOAD_VERSION)
            ? Promise.reject(
                new Error(
                  "Stored DA payload schema version must equal canonical V1",
                ),
              )
            : unwrapDaPayload(payloadCbor, {
                maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
              }).then((unwrapped) => SDK.decodeDaPayload(unwrapped.innerBytes)),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Retained DA payload is not canonical V1 CBOR",
            cause,
          }),
      });
      const payloadHeaderHash = yield* SDK.hashBlockHeader(
        payload.block_body.header,
      );
      if (
        payloadHash !== storedPayloadHash ||
        payload.block_body.header_hash !== target.headerHash ||
        payloadHeaderHash !== target.headerHash ||
        Data.to(payload.block_body.header, SDK.Header) !==
          Data.to(target.stateQueueNode.header, SDK.Header)
      ) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Refusing to attest stale or mismatched retained DA payload bytes",
            cause: `header=${target.headerHash},stored_payload_hash=${storedPayloadHash},computed_payload_hash=${payloadHash},payload_header_hash=${payloadHeaderHash}`,
          }),
        );
      }
      const availabilityCommitment = SDK.buildDaAvailabilityCommitment({
        deploymentIdentity: contracts.hubOracle.policyId,
        headerHash: target.headerHash,
        payload: payloadCbor,
        responseGeometry: availabilityParameters.response_geometry,
      });
      const outcome = yield* Effect.either(
        daBondPoolBackingAttestationProgram(
          lucid,
          contracts,
          availabilityParameters,
        ).pipe(
          Effect.zipRight(
            attestHeader({
              lucid,
              contracts,
              nodeConfig,
              daParamsUtxo: daParams.utxo,
              daParamsDatum: daParams.datum,
              target,
              referenceScripts,
              availabilityCommitment,
              availabilityParameters,
            }),
          ),
        ),
      );
      if (outcome._tag === "Right") {
        results.push(outcome.right);
        continue;
      }
      if (!isDaBondPoolAttestationSkip(outcome.left)) {
        return yield* Effect.fail(outcome.left);
      }
      // Decision E5: a short or Withdrawing pool never fails the attestation
      // loop. The pool backs every header alike, so the rest of the round is
      // skipped too; the next round retries once the pool is topped up or its
      // withdrawal is cancelled.
      yield* Effect.logWarning(
        `DA attestation skipped for this round at header ${target.headerHash}: ${outcome.left.message} (reason=${outcome.left.reason}, ${String(outcome.left.cause)}). Top up the DA bond pool or cancel its withdrawal; the next round retries.`,
      ).pipe(
        Effect.annotateLogs({
          event: DA_ATTESTATION_POOL_SKIP_EVENT,
          reason: outcome.left.reason,
          headerHash: target.headerHash,
          remainingTargets: String(targets.length - results.length - 1),
        }),
      );
      break;
    }
    return results;
  });
