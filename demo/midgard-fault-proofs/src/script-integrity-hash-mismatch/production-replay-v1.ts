import { deriveMidgardNativeTxFaultEvidenceMaterialV1 } from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  prepareScriptIntegrityHashMismatchEvidenceV1,
  SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID_V1,
  scriptIntegrityHashMismatchEvidenceClosesV1,
  type ScriptIntegrityHashMismatchEvidenceV1,
} from "./family-v1.js";
import type { ScriptIntegrityHashMismatchProductionArtifactV1 } from "./lucid-actuator-v1.js";
import { buildScriptIntegrityStageThreeAuthenticationFromRetainedDaV1 } from "./retained-stage-three-v1.js";

type CandidateSource = "accepted" | "forced";

/** Pure terminal-polarity detector shared by canonical replay and unit vectors. */
export const scriptIntegrityHashMismatchDetectionFromEvidenceV1 = ({
  headerHash,
  position,
  source,
  evidence,
}: {
  readonly headerHash: string;
  readonly position: bigint;
  readonly source: CandidateSource;
  readonly evidence: ScriptIntegrityHashMismatchEvidenceV1;
}): CanonicalViolationDetectionV1 | null => {
  if (!scriptIntegrityHashMismatchEvidenceClosesV1(evidence)) return null;
  const transactionId = evidence.finding.subject.transaction_id;
  return Object.freeze({
    detectionId: `${SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID_V1}:${source}:${position.toString()}:${transactionId}`,
    headerHash,
    violationId: SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID_V1,
    position,
    diagnostic:
      source === "accepted"
        ? `accepted transaction ${transactionId} committed a mismatched script integrity hash`
        : `forced transaction ${transactionId} was rejected despite an equal script integrity hash`,
  });
};

const evidenceFor = async ({
  block,
  subject,
  eventKey,
}: {
  readonly block: CanonicalBlockEvidenceV1;
  readonly subject: VerdictSubjectV1;
  readonly eventKey:
    | Readonly<{ L2TransactionEventKey: Readonly<{ tx_id: string }> }>
    | Readonly<{
        ForcedTransactionEventKey: Readonly<{
          tx_order_id: Readonly<{
            transactionId: string;
            outputIndex: bigint;
          }>;
        }>;
      }>;
}): Promise<
  Readonly<{
    evidence: ScriptIntegrityHashMismatchEvidenceV1;
    authentication: Awaited<
      ReturnType<
        typeof buildScriptIntegrityStageThreeAuthenticationFromRetainedDaV1
      >
    >;
  }>
> => {
  const body = block.reconstruction.payload.block_body;
  const authentication =
    await buildScriptIntegrityStageThreeAuthenticationFromRetainedDaV1({
      eventKey,
      authenticatedValidationTraceEntries: body.validation_traces.map(
        ([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        }),
      ),
      retainedValidationWitnessEntries: body.validation_trace_witnesses.map(
        ([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        }),
      ),
      expectedValidationTracesRoot: block.header.validationTracesRoot,
    });
  const bitmap = Number(authentication.control.language_bitmap);
  if (bitmap !== 0 && bitmap !== 1 && bitmap !== 2 && bitmap !== 3)
    throw new Error(
      "scriptIntegrityHashMismatch canonical replay language bitmap changed",
    );
  return Object.freeze({
    authentication,
    evidence: prepareScriptIntegrityHashMismatchEvidenceV1({
      finding: { subject },
      scriptIntegrityHash: authentication.scriptIntegrityHash,
      redeemerWitnessHash: authentication.redeemerWitnessHash,
      selectedLanguageBitmap: bitmap,
      executionCount: authentication.control.execution_count,
    }),
  });
};

/** Complete accepted/forced replay in deterministic canonical selection order. */
export const detectScriptIntegrityHashMismatchCanonicalViolationsV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const accepted = await Promise.all(
    block.transactions.map(async (transaction, position) => {
      const { evidence } = await evidenceFor({
        block,
        subject: acceptedVerdictSubjectV1(transaction.nodeTxId),
        eventKey: {
          L2TransactionEventKey: { tx_id: transaction.nodeTxId },
        },
      });
      return scriptIntegrityHashMismatchDetectionFromEvidenceV1({
        headerHash: block.headerHash,
        position: BigInt(position),
        source: "accepted",
        evidence,
      });
    }),
  );
  const forced = await Promise.all(
    block.reconstruction.forcedTransactions.map(
      async (transaction, position) => {
        const verdict = transaction.value.verdict;
        if (
          verdict === "ForcedTxValid" ||
          verdict.ForcedTxInvalid.reason !== "ScriptIntegrityHashMismatch"
        )
          return null;
        const eventKey = {
          ForcedTransactionEventKey: { tx_order_id: transaction.key },
        } as const;
        const { evidence } = await evidenceFor({
          block,
          subject: forcedVerdictSubjectV1({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: verdict.ForcedTxInvalid.reason,
          }),
          eventKey,
        });
        return scriptIntegrityHashMismatchDetectionFromEvidenceV1({
          headerHash: block.headerHash,
          position: BigInt(position),
          source: "forced",
          evidence,
        });
      },
    ),
  );
  return Object.freeze(
    [...accepted, ...forced]
      .filter(
        (candidate): candidate is CanonicalViolationDetectionV1 =>
          candidate !== null,
      )
      .sort(
        (left, right) =>
          Number(left.position - right.position) ||
          left.detectionId.localeCompare(right.detectionId),
      ),
  );
};

/** Selects the first canonical contradiction and constructs its exact actuator artifact. */
export const prepareProductionScriptIntegrityHashMismatchArtifactV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<ScriptIntegrityHashMismatchProductionArtifactV1> => {
  const selected = (
    await detectScriptIntegrityHashMismatchCanonicalViolationsV1(block)
  )[0];
  if (selected === undefined)
    throw new Error(
      "scriptIntegrityHashMismatch canonical replay yielded no contradiction",
    );
  const position = Number(selected.position);
  if (!Number.isSafeInteger(position) || position < 0)
    throw new Error("scriptIntegrityHashMismatch selected position changed");
  if (selected.detectionId.includes(":accepted:")) {
    const transaction = block.transactions[position];
    if (transaction === undefined)
      throw new Error(
        "scriptIntegrityHashMismatch accepted candidate disappeared",
      );
    const eventKey = {
      L2TransactionEventKey: { tx_id: transaction.nodeTxId },
    } as const;
    const { evidence, authentication } = await evidenceFor({
      block,
      subject: acceptedVerdictSubjectV1(transaction.nodeTxId),
      eventKey,
    });
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(transaction.txCbor, "hex"),
    );
    const trie = await buildTrieView(
      block.transactions.map((entry) => ({
        key: Buffer.from(entry.nodeTxId, "hex"),
        value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
      })),
    );
    return Object.freeze({
      headerHash: block.headerHash,
      header: block.header,
      evidence,
      authentication,
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: transaction.nodeTxId,
        nativeTx: nativeTxFromCoreCompact(material.compact),
        nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
        l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(transaction.nodeTxId, "hex"),
          "script-integrity-hash-mismatch transaction",
        ),
      }),
    });
  }
  const transaction = block.reconstruction.forcedTransactions[position];
  if (
    transaction === undefined ||
    transaction.value.verdict === "ForcedTxValid"
  )
    throw new Error("scriptIntegrityHashMismatch forced candidate disappeared");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const { evidence, authentication } = await evidenceFor({
    block,
    subject: forcedVerdictSubjectV1({
      transactionId: transaction.value.tx_id,
      sourceKey: transaction.key,
      rejectionReason: transaction.value.verdict.ForcedTxInvalid.reason,
    }),
    eventKey,
  });
  return Object.freeze({
    headerHash: block.headerHash,
    header: block.header,
    evidence,
    authentication,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
  });
};
