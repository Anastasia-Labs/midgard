import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type IntentJournal,
  journaledIntent,
} from "../services/intent-journal.js";
import { outRefLabel } from "../tx-context.js";
import {
  type AttestStateQueueHeaderResult,
  completeWithLocalUplc,
  daAttestationReachedThreshold,
  fetchDaAttestationCandidates,
  fetchVisibleDaAttestationCandidates,
  type OperatorDaConfig,
  selectDaAttestationCandidate,
  submitCompletedTx,
} from "./da-attestation.fetch-da-attestation-reference-scripts.js";
import {
  applyWithDaBondPoolChurnRetry,
  daAttestationInitOutputLovelace,
  daBondPoolBackingAttestationProgram,
  localDaSignatureWitnesses,
} from "./da-attestation.fetch-unattested-headers.js";
import { TxConfirmError, TxSignError, TxSubmitError } from "./utils.js";

/** One step of a header's DA attestation, journaled against the header. */
const attestIntent = (
  headerHash: string,
  step: "init" | "add_signatures" | "apply",
) =>
  journaledIntent(
    "attest",
    `attest:${headerHash}:${step}`,
    Buffer.from(headerHash, "hex"),
  );

export const attestHeader = ({
  lucid,
  contracts,
  nodeConfig,
  daParamsUtxo,
  daParamsDatum,
  target,
  referenceScripts,
  availabilityCommitment,
  availabilityParameters,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly nodeConfig: OperatorDaConfig;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly target: SDK.DaAttestationStateQueueTarget;
  readonly referenceScripts: SDK.DaAttestationReferenceScripts;
  readonly availabilityCommitment: SDK.DaAvailabilityCommitment;
  readonly availabilityParameters: SDK.DaAvailabilityParameters;
}): Effect.Effect<
  AttestStateQueueHeaderResult,
  | SDK.LucidError
  | SDK.DaAttestationBuildError
  | SDK.StateQueueError
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
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
          attestationOutputLovelace: yield* daAttestationInitOutputLovelace(
            lucid,
            {
              attestationAddress: contracts.daAttestation.spendingScriptAddress,
              attestationUnit: SDK.daAttestationUnit(
                contracts.daAttestation,
                target.headerHash,
              ),
              headerHash: target.headerHash,
              availabilityCommitment,
              daParamsDatum,
              rescueBeneficiary,
            },
          ),
          rescueBeneficiary,
          availabilityCommitment,
        },
      );
      initTxHash = yield* submitCompletedTx(
        lucid,
        yield* completeWithLocalUplc(lucid, initTx, "DA attestation init"),
        attestIntent(target.headerHash, "init"),
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
          lucid,
          addSignaturesTx,
          "DA attestation add-signatures",
        ),
        attestIntent(target.headerHash, "add_signatures"),
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
    const applyTxHash = yield* applyWithDaBondPoolChurnRetry({
      headerHash: target.headerHash,
      readPoolOutRef: daBondPoolBackingAttestationProgram(
        lucid,
        contracts,
        availabilityParameters,
      ),
      apply: Effect.gen(function* () {
        const validityRange = yield* SDK.daAttestationApplyValidityRangeProgram(
          {
            currentTime: BigInt(lucid.slotToUnixTime(lucid.currentSlot())),
            headerEndTime: target.stateQueueNode.header.endTime,
          },
        );
        // The SDK re-reads the pool immediately before assembling the
        // transaction and refuses a Withdrawing or under-backed pool with a
        // typed `DaAttestationBuildError`.
        const applyTx =
          yield* SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
            lucid,
            contracts,
            {
              daParamsUtxo,
              daParamsDatum,
              target,
              attestation: signedAttestation,
              referenceScripts,
              validityRange,
              availabilityParameters,
            },
          );
        return yield* submitCompletedTx(
          lucid,
          yield* completeWithLocalUplc(lucid, applyTx, "DA attestation apply"),
          attestIntent(target.headerHash, "apply"),
        );
      }),
    });
    return {
      headerHash: target.headerHash,
      initTxHash,
      addSignaturesTxHash,
      applyTxHash,
      appliedAttestationOutRef: outRefLabel(signedAttestation.utxo),
      candidateCount: candidates.length,
    };
  });
