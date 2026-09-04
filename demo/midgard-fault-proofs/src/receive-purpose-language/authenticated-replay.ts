import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import {
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import type { ReceivePurposeLanguageArtifact } from "./actuator.js";
import {
  prepareReceivePurposeLanguageEvidence,
  RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID,
  receivePurposeLanguageEvidenceCloses,
} from "./family.js";
import {
  buildReceivePurposeLanguageAuthenticationFromRetainedDa,
  receivePurposeLanguageDescriptorFromAuthentication,
} from "./retained-witness.js";

const exact = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`receivePurposeLanguage ${label} changed`);
  return result;
};
const candidates = async (block: CanonicalBlockEvidence) => {
  const traces = block.reconstruction.payload.block_body.validation_traces.map(
    ([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }),
  );
  const witnesses =
    block.reconstruction.payload.block_body.validation_trace_witnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const coordinates = witnesses
    .map((entry) => ({
      entry,
      key: decodeRetainedValidationWitnessKey(entry.key),
      value: decodeRetainedValidationWitness(entry.value),
    }))
    .filter(
      ({ value }) =>
        typeof value.auxiliary === "object" &&
        "NativeExecutionDescriptorWitness" in value.auxiliary &&
        value.auxiliary.NativeExecutionDescriptorWitness.purpose_kind === 3n,
    );
  const trie = await buildTrieView(
    block.transactions.map((transaction) => ({
      key: Buffer.from(transaction.nodeTxId, "hex"),
      value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
    })),
  );
  const accepted = await Promise.all(
    coordinates
      .flatMap(({ key, value }) => {
        if (
          !("L2TransactionEventKey" in key.event_key) ||
          !(
            typeof value.auxiliary === "object" &&
            "NativeExecutionDescriptorWitness" in value.auxiliary
          ) ||
          value.auxiliary.NativeExecutionDescriptorWitness.language_tag !== 3n
        )
          return [];
        const acceptedEventKey = key.event_key;
        return [
          async () => {
            const transactionId = acceptedEventKey.L2TransactionEventKey.tx_id;
            const position = block.transactions.findIndex(
              (transaction) => transaction.nodeTxId === transactionId,
            );
            if (position < 0)
              throw new Error(
                "receivePurposeLanguage retained accepted transaction disappeared",
              );
            const transaction = block.transactions[position]!;
            const executionIndex = exact(
              key.execution_index,
              "execution coordinate",
            );
            const rebuilt =
              await buildReceivePurposeLanguageAuthenticationFromRetainedDa({
                eventKey: acceptedEventKey,
                executionIndex,
                authenticatedValidationTraceEntries: traces,
                retainedValidationWitnessEntries: witnesses,
                expectedValidationTracesRoot: block.header.validationTracesRoot,
                expectedLanguageTag: 3,
              });
            const evidence = prepareReceivePurposeLanguageEvidence({
              finding: {
                subject: {
                  version: 1n,
                  direction: 0n,
                  source_kind: 0n,
                  transaction_id: transactionId,
                  source_key: "",
                  rejection_reason: null,
                },
                executionIndex,
              },
              descriptor: receivePurposeLanguageDescriptorFromAuthentication(
                rebuilt.authentication,
                executionIndex,
              ),
            });
            if (!receivePurposeLanguageEvidenceCloses(evidence)) return null;
            const material = deriveMidgardNativeTxFaultEvidenceMaterial(
              Buffer.from(transaction.txCbor, "hex"),
            );
            const acceptedInclusion = parseSubmitStep01TxInclusion({
              nativeTxId: transactionId,
              nativeTx: nativeTxFromCoreCompact(material.compact),
              nativeTxCompactCbor:
                material.proofSource.compactCbor.toString("hex"),
              l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
              transactionsPhasRoot: trie.root,
              txMembershipProofCbor: requireProof(
                trie,
                Buffer.from(transactionId, "hex"),
                "receive-purpose transaction",
              ),
            });
            const detection: CanonicalViolationDetection = {
              detectionId: `${RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID}:accepted:${position.toString()}:${transactionId}:${executionIndex.toString()}`,
              headerHash: block.headerHash,
              violationId: RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID,
              position: BigInt(position),
              diagnostic: `accepted receive execution ${executionIndex.toString()} selected forbidden PlutusV3`,
            };
            return {
              detection,
              artifact: Object.freeze({
                headerHash: block.headerHash,
                header: block.header,
                evidence,
                authentication: rebuilt.authentication,
                acceptedInclusion,
              }) satisfies ReceivePurposeLanguageArtifact,
            };
          },
        ];
      })
      .map((build) => build()),
  );
  const forced = await Promise.all(
    block.reconstruction.forcedTransactions.map(
      async (transaction, position) => {
        const verdict = transaction.value.verdict;
        if (
          verdict === "ForcedTxValid" ||
          typeof verdict.ForcedTxInvalid.reason === "string" ||
          !("ReceivePurposePlutusV3Forbidden" in verdict.ForcedTxInvalid.reason)
        )
          return null;
        const executionIndex = exact(
          verdict.ForcedTxInvalid.reason.ReceivePurposePlutusV3Forbidden
            .execution_index,
          "forced reason coordinate",
        );
        const eventKey = {
          ForcedTransactionEventKey: { tx_order_id: transaction.key },
        } as const;
        const match = coordinates.find(
          ({ key }) =>
            "ForcedTransactionEventKey" in key.event_key &&
            key.execution_index === BigInt(executionIndex) &&
            key.event_key.ForcedTransactionEventKey.tx_order_id
              .transactionId === transaction.key.transactionId &&
            key.event_key.ForcedTransactionEventKey.tx_order_id.outputIndex ===
              transaction.key.outputIndex,
        );
        if (
          match === undefined ||
          !(
            typeof match.value.auxiliary === "object" &&
            "NativeExecutionDescriptorWitness" in match.value.auxiliary
          )
        )
          throw new Error(
            "receivePurposeLanguage retained forced witness disappeared",
          );
        const language = Number(
          match.value.auxiliary.NativeExecutionDescriptorWitness.language_tag,
        );
        if (language !== 0 && language !== 128) return null;
        const rebuilt =
          await buildReceivePurposeLanguageAuthenticationFromRetainedDa({
            eventKey,
            executionIndex,
            authenticatedValidationTraceEntries: traces,
            retainedValidationWitnessEntries: witnesses,
            expectedValidationTracesRoot: block.header.validationTracesRoot,
            expectedLanguageTag: language,
          });
        const evidence = prepareReceivePurposeLanguageEvidence({
          finding: {
            subject: forcedVerdictSubject({
              transactionId: transaction.value.tx_id,
              sourceKey: transaction.key,
              rejectionReason: verdict.ForcedTxInvalid.reason,
            }),
            executionIndex,
          },
          descriptor: receivePurposeLanguageDescriptorFromAuthentication(
            rebuilt.authentication,
            executionIndex,
          ),
        });
        if (!receivePurposeLanguageEvidenceCloses(evidence)) return null;
        const detection: CanonicalViolationDetection = {
          detectionId: `${RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID}:forced:${position.toString()}:${transaction.value.tx_id}:${executionIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID,
          position: BigInt(position),
          diagnostic: `forced receive execution ${executionIndex.toString()} used allowed language`,
        };
        return {
          detection,
          artifact: Object.freeze({
            headerHash: block.headerHash,
            header: block.header,
            evidence,
            authentication: rebuilt.authentication,
            forcedMembership: await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey,
            }),
          }) satisfies ReceivePurposeLanguageArtifact,
        };
      },
    ),
  );
  return [...accepted, ...forced]
    .filter(
      (candidate): candidate is NonNullable<typeof candidate> =>
        candidate !== null,
    )
    .sort(
      (left, right) =>
        Number(left.detection.position - right.detection.position) ||
        left.detection.detectionId.localeCompare(right.detection.detectionId),
    );
};

/** All exact accepted/forced ID34 detections in canonical selection order. */
export const detectReceivePurposeLanguageCanonicalViolations = async (
  block: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> =>
  Object.freeze((await candidates(block)).map(({ detection }) => detection));
export const prepareReceivePurposeLanguageArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<ReceivePurposeLanguageArtifact> => {
  const selected = (await candidates(block))[0];
  if (selected === undefined)
    throw new Error(
      "receivePurposeLanguage canonical replay yielded no contradiction",
    );
  return selected.artifact;
};
