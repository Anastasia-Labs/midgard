import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Option, Schedule } from "effect";

import { committeeSignerIndex, daLocalSigners } from "@/da/local-signers.js";
import { DaPayloadsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { NodeConfig } from "@/services/config.js";
import {
  availabilityParametersFromExplicitEnvironment,
  availabilityParametersFromManifestV1,
  ContractDeploymentIdentity,
  Database,
  Lucid,
  MidgardContracts,
} from "@/services/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "@/transactions/reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { outRefLabel } from "@/tx-context.js";

const UTXO_VISIBILITY_RETRY_DELAY = "2 seconds";
const UTXO_VISIBILITY_RETRY_COUNT = 12;

type CompleteOptions = {
  readonly localUPLCEval: boolean;
};

type CompletableTx = {
  readonly complete: (options?: CompleteOptions) => Promise<TxSignBuilder>;
};

type OperatorDaConfig = {
  readonly L1_OPERATOR_SEED_PHRASE: string;
  readonly NETWORK: Network;
  readonly DA_COSIGNER_SEED_PHRASE?: string;
};

export type AttestStateQueueOnceOptions = {
  readonly headerHash?: string;
};

export type AttestStateQueueHeaderResult = {
  readonly headerHash: string;
  readonly initTxHash: string | null;
  readonly addSignaturesTxHash: string | null;
  readonly applyTxHash: string;
  readonly appliedAttestationOutRef: string;
  readonly candidateCount: number;
};

const decodeDatum = <T>(
  utxo: UTxO,
  schema: Parameters<typeof Data.from>[1],
  label: string,
): Effect.Effect<T, SDK.StateQueueError> =>
  Effect.try({
    try: () => {
      if (utxo.datum == null) {
        throw new Error(`${label} UTxO has no inline datum`);
      }
      return Data.from(utxo.datum, schema) as T;
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: `Failed to decode ${label} datum`,
        cause,
      }),
  });

const completeWithLocalUplc = (
  tx: CompletableTx,
  label: string,
): Effect.Effect<TxSignBuilder, SDK.LucidError> =>
  Effect.tryPromise({
    try: () => tx.complete({ localUPLCEval: true }),
    catch: (cause) =>
      new SDK.LucidError({
        message: `Failed to build ${label} transaction with local UPLC evaluation: ${String(cause)}`,
        cause,
      }),
  });

const submitCompletedTx = (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Effect.Effect<string, TxConfirmError | TxSignError | TxSubmitError> =>
  handleSignSubmit(lucid, tx);

export const fetchDaParamsUtxo = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  {
    readonly utxo: UTxO;
    readonly datum: SDK.DaParamsDatum;
  },
  SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const daParamsUnit = SDK.daParamsUnit(contracts.daParamsGovernor);
    const daParamsUtxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.daParamsGovernor.spendingScriptAddress,
          daParamsUnit,
        ),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to fetch DA params UTxO",
          cause,
        }),
    });
    if (daParamsUtxos.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to resolve unique DA params UTxO",
          cause: `expected=1,found=${daParamsUtxos.length.toString()},unit=${daParamsUnit}`,
        }),
      );
    }
    const utxo = daParamsUtxos[0]!;
    const datum = yield* decodeDatum<SDK.DaParamsDatum>(
      utxo,
      SDK.DaParamsDatum as never,
      "DA params",
    );
    return { utxo, datum };
  });

const compareOutRefs = (left: UTxO, right: UTxO): number =>
  left.txHash.localeCompare(right.txHash) ||
  left.outputIndex - right.outputIndex;

const compareBigIntDesc = (left: bigint, right: bigint): number =>
  left === right ? 0 : left > right ? -1 : 1;

const daAttestationMatchesParams = (
  candidate: SDK.DaAttestationUtxo,
  daParamsDatum: SDK.DaParamsDatum,
): boolean =>
  candidate.datum.da_threshold === daParamsDatum.da_threshold &&
  candidate.datum.committee_signers_hash ===
    daParamsDatum.committee_signers_hash;

const daAttestationReachedThreshold = (
  candidate: SDK.DaAttestationUtxo,
): boolean => candidate.datum.attestation_count >= candidate.datum.da_threshold;

const selectDaAttestationCandidate = (
  candidates: readonly SDK.DaAttestationUtxo[],
  daParamsDatum: SDK.DaParamsDatum,
  label: string,
  predicate: (candidate: SDK.DaAttestationUtxo) => boolean = () => true,
): Effect.Effect<SDK.DaAttestationUtxo, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const matching = candidates
      .filter((candidate) =>
        daAttestationMatchesParams(candidate, daParamsDatum),
      )
      .filter(predicate)
      .sort(
        (left, right) =>
          Number(daAttestationReachedThreshold(right)) -
            Number(daAttestationReachedThreshold(left)) ||
          compareBigIntDesc(
            left.datum.attestation_count,
            right.datum.attestation_count,
          ) ||
          compareOutRefs(left.utxo, right.utxo),
      );
    if (matching.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Failed to select ${label} DA attestation UTxO`,
          cause: `candidate_count=${candidates.length.toString()}`,
        }),
      );
    }
    return matching[0]!;
  });

const fetchDaAttestationCandidates = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  headerHash: string,
): Effect.Effect<readonly SDK.DaAttestationUtxo[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const unit = SDK.daAttestationUnit(contracts.daAttestation, headerHash);
    const utxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.daAttestation.spendingScriptAddress,
          unit,
        ),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to fetch DA attestation UTxO",
          cause,
        }),
    });
    const decoded = yield* Effect.forEach(utxos, (utxo) =>
      Effect.gen(function* () {
        const datum = yield* decodeDatum<SDK.DaAttestationDatum>(
          utxo,
          SDK.DaAttestationDatum as never,
          "DA attestation",
        );
        return { utxo, datum };
      }).pipe(Effect.either),
    );
    const matching = decoded
      .filter((entry) => entry._tag === "Right")
      .map((entry) => entry.right)
      .filter((entry) => entry.datum.header_hash === headerHash)
      .sort((left, right) => compareOutRefs(left.utxo, right.utxo));
    return matching;
  });

const fetchVisibleDaAttestationCandidates = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  headerHash: string,
): Effect.Effect<readonly SDK.DaAttestationUtxo[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const candidates = yield* fetchDaAttestationCandidates(
      lucid,
      contracts,
      headerHash,
    );
    if (candidates.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to find visible DA attestation UTxO",
          cause: `header=${headerHash}`,
        }),
      );
    }
    return candidates;
  }).pipe(
    Effect.retry(
      Schedule.intersect(
        Schedule.fixed(UTXO_VISIBILITY_RETRY_DELAY),
        Schedule.recurs(UTXO_VISIBILITY_RETRY_COUNT),
      ),
    ),
  );

const fetchDaAttestationReferenceScripts = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.DaAttestationReferenceScripts, SDK.StateQueueError> =>
  fetchReferenceScriptUtxosProgram(
    lucid,
    referenceScriptsAddress,
    [
      {
        name: "da-attestation minting",
        script: contracts.daAttestation.mintingScript,
      },
      {
        name: "da-attestation spending",
        script: contracts.daAttestation.spendingScript,
      },
      {
        name: "state-queue minting",
        script: contracts.stateQueue.mintingScript,
      },
      {
        name: "state-queue spending",
        script: contracts.stateQueue.spendingScript,
      },
      {
        name: "availability-challenge minting",
        script: contracts.availabilityChallenge.mintingScript,
      },
    ],
    contracts.referenceScriptAuth,
  ).pipe(
    Effect.map((resolved) => ({
      daAttestationMinting: referenceScriptByName(
        resolved,
        "da-attestation minting",
      ),
      daAttestationSpending: referenceScriptByName(
        resolved,
        "da-attestation spending",
      ),
      stateQueueMinting: referenceScriptByName(resolved, "state-queue minting"),
      stateQueueSpending: referenceScriptByName(
        resolved,
        "state-queue spending",
      ),
      availabilityChallengeMinting: referenceScriptByName(
        resolved,
        "availability-challenge minting",
      ),
    })),
  );

const fetchUnattestedHeaders = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  headerHash?: string,
): Effect.Effect<
  readonly SDK.DaAttestationStateQueueTarget[],
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const stateQueueUtxos = yield* SDK.fetchSortedStateQueueUTxOsProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
    );
    const matches: SDK.DaAttestationStateQueueTarget[] = [];
    for (const stateQueueUtxo of stateQueueUtxos) {
      if (stateQueueUtxo.datum.key === "Empty") {
        continue;
      }
      const node = yield* SDK.getStateQueueNodeV1FromStateQueueDatum(
        stateQueueUtxo.datum,
      );
      if (node.da_attestation !== SDK.NO_DA_ATTESTATION) {
        continue;
      }
      const recomputedHeaderHash = yield* SDK.hashBlockHeaderV1(node.header);
      const datumHeaderHash = stateQueueUtxo.datum.key.Key.key;
      if (recomputedHeaderHash !== datumHeaderHash) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to select DA attestation target: state-queue key/hash mismatch",
            cause: `outRef=${outRefLabel(stateQueueUtxo.utxo)},datumKey=${datumHeaderHash},computed=${recomputedHeaderHash}`,
          }),
        );
      }
      if (headerHash !== undefined && recomputedHeaderHash !== headerHash) {
        continue;
      }
      matches.push({
        stateQueueUtxo,
        stateQueueNode: node,
        headerHash: recomputedHeaderHash,
      });
    }
    if (headerHash !== undefined && matches.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Requested state-queue header is not available for DA attestation",
          cause: `header=${headerHash}`,
        }),
      );
    }
    return matches;
  });

/**
 * Every attestation witness this process can produce for `headerHash`.
 *
 * Since Q63 the governed floor puts `da_threshold` at two or more, so a single
 * operator signature can never reach threshold alone. A node holding more than
 * one DA key (dev and emulator bootstrap, via `DA_COSIGNER_SEED_PHRASE`)
 * contributes one genuine Ed25519 signature per key here; a production node
 * holds one key and the remaining witnesses arrive from peers over libp2p.
 *
 * The signer index is looked up in the on-chain committee rather than assumed,
 * because the committee is emitted sorted-unique and the operator's key is not
 * necessarily first. Keys absent from the committee are skipped: they cannot be
 * indexed, and the attestation validator would reject them.
 */
const localDaSignatureWitnesses = (
  availabilityCommitment: SDK.DaAvailabilityCommitmentV1,
  nodeConfig: OperatorDaConfig,
  committeeHex: string,
): readonly SDK.DaAttestationSignatureWitness[] => {
  const message = Buffer.from(
    SDK.daAvailabilityAttestationMessageV1(availabilityCommitment),
  );
  return daLocalSigners(nodeConfig)
    .flatMap((signer) => {
      const signerIndex = committeeSignerIndex(
        committeeHex,
        signer.verificationKeyHex,
      );
      return signerIndex === null
        ? []
        : [{ signerIndex, signatureHex: signer.sign(message) }];
    })
    .sort((left, right) => left.signerIndex - right.signerIndex);
};

const attestHeader = ({
  lucid,
  contracts,
  nodeConfig,
  daParamsUtxo,
  daParamsDatum,
  target,
  referenceScripts,
  availabilityCommitment,
  availabilityParameters,
  hubOracleRefInput,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly nodeConfig: OperatorDaConfig;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly target: SDK.DaAttestationStateQueueTarget;
  readonly referenceScripts: SDK.DaAttestationReferenceScripts;
  readonly availabilityCommitment: SDK.DaAvailabilityCommitmentV1;
  readonly availabilityParameters: SDK.DaAvailabilityParametersV1;
  readonly hubOracleRefInput: UTxO;
}): Effect.Effect<
  AttestStateQueueHeaderResult,
  | SDK.LucidError
  | SDK.DaAttestationBuildError
  | SDK.StateQueueError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    let initTxHash: string | null = null;
    let addSignaturesTxHash: string | null = null;
    let candidates = yield* fetchDaAttestationCandidates(
      lucid,
      contracts,
      target.headerHash,
    );
    if (candidates.length === 0) {
      const rescueBeneficiaryAddress = yield* Effect.tryPromise({
        try: () => lucid.wallet().address(),
        catch: (cause) =>
          new SDK.LucidError({
            message: "Failed to resolve DA attestation rescue beneficiary",
            cause,
          }),
      });
      const rescueBeneficiary = yield* SDK.addressDataFromBech32(
        rescueBeneficiaryAddress,
      ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.LucidError({
              message: "Failed to encode DA attestation rescue beneficiary",
              cause,
            }),
        ),
      );
      const initTx = yield* SDK.incompleteInitDaAttestationTxProgram(
        lucid,
        contracts,
        {
          daParamsUtxo,
          daParamsDatum,
          target,
          referenceScripts,
          attestationOutputLovelace: availabilityParameters.da_bond_lovelace,
          rescueBeneficiary,
          availabilityCommitment,
        },
      );
      initTxHash = yield* submitCompletedTx(
        lucid,
        yield* completeWithLocalUplc(initTx, "DA attestation init"),
      );
      candidates = yield* fetchVisibleDaAttestationCandidates(
        lucid,
        contracts,
        target.headerHash,
      );
    } else {
      yield* Effect.logInfo(
        `Resuming DA attestation for header ${target.headerHash}: found ${candidates.length.toString()} existing candidate UTxO(s).`,
      );
    }

    const initializedAttestation = yield* selectDaAttestationCandidate(
      candidates,
      daParamsDatum,
      "initialized",
    );
    const localWitnesses = localDaSignatureWitnesses(
      initializedAttestation.datum.availability_commitment,
      nodeConfig,
      daParamsDatum.committee,
    );

    if (!daAttestationReachedThreshold(initializedAttestation)) {
      // Distinguish "this node is not on the committee at all" from "this node
      // has already contributed everything it can". They need different
      // operator responses, and conflating them sends whoever reads the log
      // after the wrong problem.
      if (localWitnesses.length === 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "No locally held DA key is a member of the on-chain DA committee",
            cause: `outRef=${outRefLabel(initializedAttestation.utxo)},committee_members=${(daParamsDatum.committee.length / 64).toString()},threshold=${initializedAttestation.datum.da_threshold.toString()}`,
          }),
        );
      }
      const pendingWitnesses = localWitnesses.filter(
        (witness) =>
          !SDK.signerIndexIsDaAttested(
            initializedAttestation.datum.attested_signers,
            witness.signerIndex,
          ),
      );
      if (pendingWitnesses.length === 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Selected DA attestation UTxO already includes every locally held DA signature but has not reached threshold",
            cause: `outRef=${outRefLabel(initializedAttestation.utxo)},local_signers=${localWitnesses.length.toString()},attestation_count=${initializedAttestation.datum.attestation_count.toString()},threshold=${initializedAttestation.datum.da_threshold.toString()}`,
          }),
        );
      }
      const addSignaturesTx =
        yield* SDK.incompleteAddDaAttestationSignaturesTxProgram(
          lucid,
          contracts,
          {
            daParamsUtxo,
            daParamsDatum,
            attestation: initializedAttestation,
            witnesses: pendingWitnesses,
            referenceScripts,
          },
        );
      addSignaturesTxHash = yield* submitCompletedTx(
        lucid,
        yield* completeWithLocalUplc(
          addSignaturesTx,
          "DA attestation add-signatures",
        ),
      );
      candidates = yield* fetchVisibleDaAttestationCandidates(
        lucid,
        contracts,
        target.headerHash,
      );
    }

    const signedAttestation = yield* selectDaAttestationCandidate(
      candidates,
      daParamsDatum,
      "threshold-signed",
      daAttestationReachedThreshold,
    );
    const applyValidFrom = BigInt(lucid.slotToUnixTime(lucid.currentSlot()));
    const applyDeadline =
      target.stateQueueNode.header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
    const applyValidTo =
      applyValidFrom + 120_000n < applyDeadline
        ? applyValidFrom + 120_000n
        : applyDeadline;
    const applyTx =
      yield* SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        contracts,
        {
          hubOracleRefInput,
          daParamsUtxo,
          daParamsDatum,
          target,
          attestation: signedAttestation,
          referenceScripts,
          validityRange: {
            validFrom: applyValidFrom,
            validTo: applyValidTo,
          },
        },
      );
    const applyTxHash = yield* submitCompletedTx(
      lucid,
      yield* completeWithLocalUplc(applyTx, "DA attestation apply"),
    );
    return {
      headerHash: target.headerHash,
      initTxHash,
      addSignaturesTxHash,
      applyTxHash,
      appliedAttestationOutRef: outRefLabel(signedAttestation.utxo),
      candidateCount: candidates.length,
    };
  });

export const attestStateQueueOnceProgram = (
  options: AttestStateQueueOnceOptions = {},
): Effect.Effect<
  readonly AttestStateQueueHeaderResult[],
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.DaAttestationBuildError
  | SDK.HubOracleError
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
        : availabilityParametersFromManifestV1(
            deploymentIdentity.manifest.availabilityChallenge,
          );
    const hubOracle = yield* SDK.fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
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
          Number(SDK.DA_PAYLOAD_V1_VERSION)
            ? Promise.reject(
                new Error(
                  "Stored DA payload schema version must equal canonical V1",
                ),
              )
            : unwrapDaPayloadV1(payloadCbor, {
                maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
              }).then((unwrapped) =>
                SDK.decodeDaPayloadV1(unwrapped.innerBytes),
              ),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Retained DA payload is not canonical V1 CBOR",
            cause,
          }),
      });
      const payloadHeaderHash = yield* SDK.hashBlockHeaderV1(
        payload.block_body.header,
      );
      if (
        payloadHash !== storedPayloadHash ||
        payload.block_body.header_hash !== target.headerHash ||
        payloadHeaderHash !== target.headerHash ||
        Data.to(payload.block_body.header, SDK.HeaderV1) !==
          Data.to(target.stateQueueNode.header, SDK.HeaderV1)
      ) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Refusing to attest stale or mismatched retained DA payload bytes",
            cause: `header=${target.headerHash},stored_payload_hash=${storedPayloadHash},computed_payload_hash=${payloadHash},payload_header_hash=${payloadHeaderHash}`,
          }),
        );
      }
      const walletAddress = yield* Effect.tryPromise({
        try: () => lucid.wallet().address(),
        catch: (cause) =>
          new SDK.LucidError({
            message: "Failed to resolve DA bond owner wallet address",
            cause,
          }),
      });
      const walletAddressData = yield* SDK.addressDataFromBech32(
        walletAddress,
      ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.LucidError({
              message: "Failed to decode DA bond owner wallet address",
              cause,
            }),
        ),
      );
      const paymentCredential = walletAddressData.paymentCredential;
      if (!("PublicKeyCredential" in paymentCredential)) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "DA bond owner must be a public-key wallet credential",
            cause: `address=${walletAddress}`,
          }),
        );
      }
      const availabilityCommitment = SDK.buildDaAvailabilityCommitmentV1({
        deploymentIdentity: contracts.hubOracle.policyId,
        headerHash: target.headerHash,
        payload: payloadCbor,
        bondOwner: paymentCredential.PublicKeyCredential[0],
        responseGeometry: availabilityParameters.response_geometry,
      });
      const result = yield* attestHeader({
        lucid,
        contracts,
        nodeConfig,
        daParamsUtxo: daParams.utxo,
        daParamsDatum: daParams.datum,
        target,
        referenceScripts,
        availabilityCommitment,
        availabilityParameters,
        hubOracleRefInput: hubOracle.utxo,
      });
      results.push(result);
    }
    return results;
  });
