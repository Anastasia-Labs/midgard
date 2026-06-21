import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { NodeConfig } from "@/services/config.js";
import { Lucid, MidgardContracts } from "@/services/index.js";
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

const DA_ATTESTATION_OUTPUT_LOVELACE = 5_000_000n;
const UTXO_VISIBILITY_RETRY_DELAY = "2 seconds";
const UTXO_VISIBILITY_RETRY_COUNT = 12;
const OPERATOR_DA_SIGNER_INDEX = 0;

type CompleteOptions = {
  readonly localUPLCEval: boolean;
};

type CompletableTx = {
  readonly complete: (options?: CompleteOptions) => Promise<TxSignBuilder>;
};

type OperatorDaConfig = {
  readonly L1_OPERATOR_SEED_PHRASE: string;
  readonly NETWORK: Network;
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
        message: `Failed to build ${label} transaction with local UPLC evaluation`,
        cause,
      }),
  });

const submitCompletedTx = (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Effect.Effect<string, TxConfirmError | TxSignError | TxSubmitError> =>
  handleSignSubmit(lucid, tx);

const fetchDaParamsUtxo = (
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
      const node = yield* SDK.getStateQueueNodeFromStateQueueDatum(
        stateQueueUtxo.datum,
      );
      if (node.da_attestation !== SDK.NO_DA_ATTESTATION) {
        continue;
      }
      const recomputedHeaderHash = yield* SDK.hashBlockHeader(node.header);
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

const indexedOperatorSignature = (
  headerHash: string,
  operatorSeedPhrase: string,
  network: Network,
): SDK.DaAttestationSignatureWitness => {
  const wallet = walletFromSeed(operatorSeedPhrase, { network });
  const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
  return {
    signerIndex: OPERATOR_DA_SIGNER_INDEX,
    signatureHex: privateKey
      .sign(SDK.daAttestationMessage(headerHash))
      .to_hex(),
  };
};

const attestHeader = ({
  lucid,
  contracts,
  nodeConfig,
  daParamsUtxo,
  daParamsDatum,
  target,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly nodeConfig: OperatorDaConfig;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly target: SDK.DaAttestationStateQueueTarget;
  readonly referenceScripts: SDK.DaAttestationReferenceScripts;
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
      const initTx = yield* SDK.incompleteInitDaAttestationTxProgram(
        lucid,
        contracts,
        {
          daParamsUtxo,
          daParamsDatum,
          target,
          referenceScripts,
          attestationOutputLovelace: DA_ATTESTATION_OUTPUT_LOVELACE,
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

    const signatureWitnesses = indexedOperatorSignature(
      target.headerHash,
      nodeConfig.L1_OPERATOR_SEED_PHRASE,
      nodeConfig.NETWORK,
    );
    const initializedAttestation = yield* selectDaAttestationCandidate(
      candidates,
      daParamsDatum,
      "initialized",
    );

    if (!daAttestationReachedThreshold(initializedAttestation)) {
      if (
        SDK.signerIndexIsDaAttested(
          initializedAttestation.datum.attested_signers,
          OPERATOR_DA_SIGNER_INDEX,
        )
      ) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Selected DA attestation UTxO already includes the local operator signature but has not reached threshold",
            cause: `outRef=${outRefLabel(initializedAttestation.utxo)},attestation_count=${initializedAttestation.datum.attestation_count.toString()},threshold=${initializedAttestation.datum.da_threshold.toString()}`,
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
            witnesses: [signatureWitnesses],
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
    const applyTx =
      yield* SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        contracts,
        {
          target,
          attestation: signedAttestation,
          referenceScripts,
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
  | SDK.StateQueueError
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const daParams = yield* fetchDaParamsUtxo(lucid, contracts);
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
      const result = yield* attestHeader({
        lucid,
        contracts,
        nodeConfig,
        daParamsUtxo: daParams.utxo,
        daParamsDatum: daParams.datum,
        target,
        referenceScripts,
      });
      results.push(result);
    }
    return results;
  });
