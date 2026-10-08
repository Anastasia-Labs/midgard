import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import type {
  IntentJournal,
  SubmissionIntent,
} from "../services/intent-journal.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "./reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";

const UTXO_VISIBILITY_RETRY_DELAY = "2 seconds";

export const UTXO_VISIBILITY_RETRY_COUNT = 12;

type CompleteOptions = {
  readonly localUPLCEval: boolean;
};

type CompletableTx = {
  readonly complete: (options?: CompleteOptions) => Promise<TxSignBuilder>;
};

export type OperatorDaConfig = {
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

export const completeWithLocalUplc = (
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

export const submitCompletedTx = (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
  intent: SubmissionIntent,
): Effect.Effect<
  string,
  TxConfirmError | TxSignError | TxSubmitError,
  IntentJournal
> => handleSignSubmit(lucid, tx, intent);

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

export const daAttestationReachedThreshold = (
  candidate: SDK.DaAttestationUtxo,
): boolean => candidate.datum.attestation_count >= candidate.datum.da_threshold;

export const selectDaAttestationCandidate = (
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

export const fetchDaAttestationCandidates = (
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

export const fetchVisibleDaAttestationCandidates = (
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

export const fetchDaAttestationReferenceScripts = (
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
