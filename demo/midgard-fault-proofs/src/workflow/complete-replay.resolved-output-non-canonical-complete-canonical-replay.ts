import { encodeProofThreadForcedSourceKey } from "@al-ft/midgard-sdk";
import { type VerdictSubject } from "@al-ft/midgard-sdk";

import { detectCrossBlockDuplicateEvents } from "../cross-block-duplicate-event/replay.js";
import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { detectExecutionSourceScriptDecodingCanonicalViolations } from "../execution-source-script-decoding/authenticated-replay.js";
import { detectFieldItemWidthIllegalCompleteReplay } from "../field-item-width-illegal/workflow.js";
import { detectFieldPreimageLengthCompleteReplay } from "../field-preimage-length-mismatch/evidence.js";
import { detectInvalidSignatureWrongfulRejections } from "../invalid-signature/wrongful-rejection.js";
import { detectMinAdaForcedReplay } from "../min-ada/forced.js";
import { detectMinFeeForcedReplay } from "../min-fee-forced.js";
import { detectMintDeclaredAssetLimitForcedReplay } from "../mint-declared-asset-limit/replay.js";
import { detectNativeScriptDecodingReplay } from "../native-script-decoding/replay.js";
import { detectNativeScriptInvalidForcedReplay } from "../native-script-invalid/forced.js";
import { detectObserversForbiddenForcedReplay } from "../observers-forbidden-on-untagged-network/replay.js";
import { detectOutputReferenceScriptDecodingCanonicalViolations } from "../output-reference-script-decoding/output-reference-script-decoding.js";
import { detectProtectedOutputSignerMissingCompleteReplay } from "../protected-output-signer-missing/protected-output-signer-missing.js";
import { protectedOutputSignerEvidenceIdentity } from "../protected-output-signer-missing/workflow.js";
import {
  deriveResolvedOutputPriorLedgerReplay,
  detectResolvedOutputNonCanonicalCompleteReplay,
  resolvedOutputEvidenceIdentity,
} from "../resolved-output-non-canonical/resolved-output-non-canonical.js";
import { detectScriptIntegrityHashMissingFromCanonicalEvidence } from "../script-integrity-hash-missing/replay.js";
import { detectSpendInputSignerMissingCompleteReplay } from "../spend-input-signer-missing/spend-input-signer-missing.js";
import { spendInputSignerWorkflowEvidenceIdentity } from "../spend-input-signer-missing/workflow.js";
import { detectTransactionOutputNonCanonicalCompleteReplay } from "../transaction-output-non-canonical/workflow.js";
import { detectWitnessScriptDecodingCompleteReplay } from "../witness-script-decoding/workflow.js";
import { completeCanonicalReplayPredecessorEvidence } from "./complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  detectL2TxMistags,
  detectMinFees,
} from "./complete-replay.detect-accepted-transaction-faults.js";
import {
  completeReplayer,
  detectDoubleWithdraws,
  detectReferenceInputNoIdxViolations,
} from "./complete-replay.detect-double-withdraws.js";
import {
  detectInputNoIdxViolations,
  detectInputSetUniqueness,
  detectInvalidSignatures,
  detectMinAda,
  detectNativeScriptInvalid,
} from "./complete-replay.detect-min-ada.js";
import { type CompleteCanonicalReplay } from "./complete-replay.replay-context-identity.js";
import { subjectOf, verdictDetectionSubject } from "./detection-subject.js";
import {
  type FabricatedDepositEvidenceAuthority,
  requireFabricatedDepositEvidenceAuthority,
} from "./fabricated-deposit-evidence.js";
import {
  type FabricatedWithdrawalEvidenceAuthority,
  requireFabricatedWithdrawalEvidenceAuthority,
} from "./fabricated-withdrawal-evidence.js";

/** Complete Ed25519 verification of every committed address witness. */
export const INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["invalidSignature"],
  async (evidence) => [
    ...(await detectInvalidSignatures(evidence)),
    ...detectInvalidSignatureWrongfulRejections({ block: evidence }).map(
      (detection) => ({
        ...subjectOf(detection),
        detectionId: detection.detectionId,
        headerHash: detection.headerHash,
        violationId: detection.violationId,
        position: detection.position,
        diagnostic: `forced transaction ${detection.transactionId} has no invalid signature at its authenticated rejected coordinate`,
      }),
    ),
  ],
);

/**
 * Complete committed-deposit scan with each candidate classified against the
 * concrete public L1 authority. The returned replayer is opaque-admitted like
 * every fixed replay bundle; structural evidence-authority copies are refused.
 */
export const createFabricatedDepositCompleteCanonicalReplay = ({
  authority,
  owner,
}: {
  readonly authority: FabricatedDepositEvidenceAuthority;
  readonly owner: string;
}): CompleteCanonicalReplay => {
  const admitted = requireFabricatedDepositEvidenceAuthority(authority);
  return completeReplayer(["fabricatedDeposit"], async (evidence) =>
    (await admitted.detect(evidence, owner)).map(({ detection }) => detection),
  );
};

/** Complete committed-withdrawal scan against the concrete public L1 authority. */
export const createFabricatedWithdrawalCompleteCanonicalReplay = ({
  authority,
  owner,
}: {
  readonly authority: FabricatedWithdrawalEvidenceAuthority;
  readonly owner: string;
}): CompleteCanonicalReplay => {
  const admitted = requireFabricatedWithdrawalEvidenceAuthority(authority);
  return completeReplayer(["fabricatedWithdrawal"], async (evidence) =>
    (await admitted.detect(evidence, owner)).map(({ detection }) => detection),
  );
};

/** Complete evaluation of all accepted native script witnesses. */
export const CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["crossBlockDuplicateEvent"], async (evidence, context) => {
    if (context?.settlements === undefined)
      throw new Error(
        "cross-block duplicate replay requires authenticated live settlement context",
      );
    return detectCrossBlockDuplicateEvents({
      evidence,
      context: context.settlements,
    });
  });

export const NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(
    ["nativeScriptDecoding"],
    async (evidence, context) =>
      await detectNativeScriptDecodingReplay({
        block: evidence,
        predecessor: completeCanonicalReplayPredecessorEvidence({
          evidence,
          context,
        }),
      }),
  );

export const NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["nativeScriptInvalid"],
  async (evidence) => [
    ...(await detectNativeScriptInvalid(evidence)),
    ...detectNativeScriptInvalidForcedReplay(evidence),
  ],
);

/** Complete transaction-output and introducing post-state min-Ada scan. */
export const MIN_ADA_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["minAda"],
  async (evidence, context) => [
    ...(await detectMinAda(evidence, context)),
    ...detectMinAdaForcedReplay(evidence),
  ],
);

/** Complete same-block input/producer output-count scan. */
export const INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["nonExistentInputNoIndex"],
  detectInputNoIdxViolations,
);

/** Complete same-block reference-input/producer output-count scan. */
export const REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["referenceInputNoIdx"], async (evidence) =>
    detectReferenceInputNoIdxViolations(evidence),
  );

/** Complete scan of every committed withdrawal pair in the accused block. */
export const DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["doubleWithdraw"],
  async (evidence) => detectDoubleWithdraws(evidence),
);

/** Complete scan of every normal transactions-root leaf for code-1 mistags. */
export const L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["l2TxMistag"],
  detectL2TxMistags,
);

/** Complete exact-size/header-schedule fee scan of every transaction leaf. */
export const MIN_FEE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["minFee"],
  async (evidence) => [
    ...(await detectMinFees(evidence)),
    ...detectMinFeeForcedReplay(evidence),
  ],
);

/** Complete input-set scan for every accepted transaction leaf. */
export const INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["inputSetUniqueness"],
  detectInputSetUniqueness,
);

/** Complete scan of forced field-length wrongful-rejection contradictions. */
export const FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["fieldPreimageLengthMismatch"], async (evidence) =>
    detectFieldPreimageLengthCompleteReplay(evidence),
  );

/** Complete scan of every output and mint item for illegal committed width. */
export const FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["fieldItemWidthIllegal"], async (evidence) =>
    detectFieldItemWidthIllegalCompleteReplay(evidence),
  );

/** Complete accepted and forced scan for malformed field-6 native scripts. */
export const WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["witnessScriptDecoding"], async (evidence) =>
    detectWitnessScriptDecodingCompleteReplay(evidence),
  );

/** Complete accepted and forced scan for missing required integrity hashes. */
export const SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["scriptIntegrityHashMissing"], async (evidence) =>
    detectScriptIntegrityHashMissingFromCanonicalEvidence(evidence),
  );

/** Complete accepted and forced scan for non-canonical transaction outputs. */
export const TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["transactionOutputNonCanonical"], async (evidence) =>
    detectTransactionOutputNonCanonicalCompleteReplay(evidence),
  );

/** Complete resolved-input scan against the classifier-admitted predecessor ledger. */
export const RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(
    ["resolvedOutputNonCanonical"],
    async (evidence, context) => {
      const priorLedger = await deriveResolvedOutputPriorLedgerReplay({
        block: evidence,
        predecessor: completeCanonicalReplayPredecessorEvidence({
          evidence,
          context,
        }),
      });
      return detectResolvedOutputNonCanonicalCompleteReplay({
        block: evidence,
        priorLedger,
      }).map((finding) => ({
        ...verdictDetectionSubject(finding.subject),
        detectionId: `resolved-output-non-canonical:${resolvedOutputEvidenceIdentity(finding)}`,
        headerHash: evidence.headerHash,
        violationId: "resolved-output-non-canonical",
        position: verdictSubjectReplayPosition(evidence, finding.subject),
        diagnostic: "authenticated prior ledger output is non-canonical",
      }));
    },
  );

/** Complete exact forced-rejection scan; accepted crossings use the raw route. */
export const MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["mintDeclaredAssetLimit"], async (evidence) =>
    detectMintDeclaredAssetLimitForcedReplay(evidence),
  );

const verdictSubjectReplayPosition = (
  evidence: CanonicalBlockEvidence,
  subject: VerdictSubject,
): bigint => {
  const index =
    subject.source_kind === 0n
      ? evidence.transactions.findIndex(
          (transaction) => transaction.nodeTxId === subject.transaction_id,
        )
      : evidence.reconstruction.forcedTransactions.findIndex(
          (transaction) =>
            transaction.value.tx_id === subject.transaction_id &&
            encodeProofThreadForcedSourceKey(transaction.key).toString(
              "hex",
            ) === subject.source_key,
        );
  if (index < 0) {
    throw new Error(
      "signer replay finding does not belong to its authenticated transaction frontier",
    );
  }
  return BigInt(index);
};

/** Complete spend-signature scan against the classifier-admitted predecessor ledger. */
export const SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["spendInputSignerMissing"], async (evidence, context) => {
    const priorLedger = await deriveResolvedOutputPriorLedgerReplay({
      block: evidence,
      predecessor: completeCanonicalReplayPredecessorEvidence({
        evidence,
        context,
      }),
    });
    return detectSpendInputSignerMissingCompleteReplay({
      block: evidence,
      priorLedger,
    }).map((finding) => ({
      ...verdictDetectionSubject(finding.subject),
      detectionId: `spend-input-signer-missing:${spendInputSignerWorkflowEvidenceIdentity(finding)}`,
      headerHash: evidence.headerHash,
      violationId: "spend-input-signer-missing",
      position: verdictSubjectReplayPosition(evidence, finding.subject),
      diagnostic: "authenticated spend input has no valid matching key witness",
    }));
  });

/** Complete protected-output signature scan over accepted and exact forced subjects. */
export const PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["protectedOutputSignerMissing"], async (evidence) =>
    detectProtectedOutputSignerMissingCompleteReplay(evidence).map(
      (finding) => ({
        ...verdictDetectionSubject(finding.subject),
        detectionId: `protected-output-signer-missing:${protectedOutputSignerEvidenceIdentity(finding)}`,
        headerHash: evidence.headerHash,
        violationId: "protected-output-signer-missing",
        position: verdictSubjectReplayPosition(evidence, finding.subject),
        diagnostic:
          "authenticated protected output has no valid matching key witness",
      }),
    ),
  );

/** Forced half of the observer rule; accepted crossings use the raw route. */
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["observersForbiddenOnUntaggedNetwork"], async (evidence) =>
    detectObserversForbiddenForcedReplay(evidence),
  );

/** Complete accepted and forced scan for malformed output reference scripts. */
export const OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["outputReferenceScriptDecoding"], async (evidence) =>
    detectOutputReferenceScriptDecodingCanonicalViolations(evidence),
  );

/** Complete accepted and forced scan for malformed execution-source scripts. */
export const EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["executionSourceScriptDecoding"], async (evidence) =>
    detectExecutionSourceScriptDecodingCanonicalViolations(evidence),
  );
