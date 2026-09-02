import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
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
  missingScriptSourceEvidenceClosesV1,
  type MissingScriptSourceEvidenceV1,
  MissingScriptSourceResultClassesV1,
  missingScriptSourceViolationIdV1,
} from "./family-v1.js";
import type { MissingScriptSourceProductionArtifactV1 } from "./production-actuator-v1.js";
import {
  type MissingScriptSourceReplayFindingV1,
  selectMissingScriptSourceCanonicalFindingV1,
} from "./replay-v1.js";
import {
  buildRetainedMissingScriptSourceUniverseV1,
  discoverRetainedMissingScriptSourceCoordinatesV1,
  type RetainedMissingScriptSourceUniverseV1,
} from "./retained-script-universe-v1.js";

const exactIndex = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`missingScriptSource ${label} changed`);
  return result;
};

export const missingScriptSourceEvidenceFromUniverseV1 = ({
  subject,
  universe,
}: {
  subject: MissingScriptSourceEvidenceV1["finding"]["subject"];
  universe: RetainedMissingScriptSourceUniverseV1;
}): MissingScriptSourceEvidenceV1 => {
  const foundAtSourceIndex = universe.sources.findIndex(
    ({ scriptHashHex }) =>
      scriptHashHex === universe.purpose.requiredScriptHashHex,
  );
  const resultClass =
    foundAtSourceIndex < 0
      ? MissingScriptSourceResultClassesV1.Missing
      : MissingScriptSourceResultClassesV1.Present;
  const first = universe.sources[0];
  const descriptor =
    first === undefined
      ? {
          sourceIndex: 0,
          originKind: 0 as const,
          sourceKeyHex: "",
          languageTag: 0 as const,
          scriptHashHex: universe.purpose.requiredScriptHashHex,
          scriptItemHex: "",
          scriptTotalLength: 0,
          scriptItemCommitmentHex: "",
          purposeKind: universe.purpose.purposeKind,
          purposeIndex: universe.purpose.purposeIndex,
          purposeSubjectHex: universe.purpose.subjectHex,
          redeemerLeafHex: "" as const,
          purposeMembership: universe.purpose.membership,
          sourceMembership: universe.purpose.membership,
          executionMembership: universe.purpose.membership,
        }
      : {
          ...first,
          // The selected target is a purpose, not an existing source.
          scriptHashHex: universe.purpose.requiredScriptHashHex,
          purposeMembership: universe.purpose.membership,
        };
  return Object.freeze({
    finding: Object.freeze({
      subject,
      purposeKind: universe.purpose.purposeKind,
      purposeIndex: universe.purpose.purposeIndex,
      executionIndex: universe.purpose.absoluteIndex,
      accusedClass:
        subject.direction === 0n
          ? MissingScriptSourceResultClassesV1.Pending
          : MissingScriptSourceResultClassesV1.Missing,
    }),
    descriptor,
    itemCommitmentHex: first?.scriptItemCommitmentHex ?? "",
    itemLength: first?.scriptTotalLength ?? 0,
    sourceLeafHex: first?.sourceMembership.leafHash.toString("hex") ?? "",
    purposeLeafHex: universe.purpose.membership.leafHash.toString("hex"),
    executionLeafHex: "",
    resultClass,
    initialControlCbor: "",
    chunkProofCount: 0,
    sourceCount: universe.sources.length,
    foundAtSourceIndex: foundAtSourceIndex < 0 ? null : foundAtSourceIndex,
    sources: universe.sources,
  });
};

const replayCandidates = async (block: CanonicalBlockEvidenceV1) => {
  const validationTraceEntries =
    block.reconstruction.payload.block_body.validation_traces.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const retainedWitnessEntries =
    block.reconstruction.payload.block_body.validation_trace_witnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const accepted = await Promise.all(
    block.transactions.map(async (transaction, position) => {
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        Buffer.from(transaction.txCbor, "hex"),
      );
      const eventKey = {
        L2TransactionEventKey: { tx_id: transaction.nodeTxId },
      } as const;
      const terminals = discoverRetainedMissingScriptSourceCoordinatesV1({
        eventKey,
        retainedValidationWitnessEntries: retainedWitnessEntries,
      });
      const universes = await Promise.all(
        terminals.map((coordinate) =>
          buildRetainedMissingScriptSourceUniverseV1({
            eventKey,
            ...coordinate,
            authenticatedValidationTraceEntries: validationTraceEntries,
            retainedValidationWitnessEntries: retainedWitnessEntries,
            expectedValidationTracesRoot: block.header.validationTracesRoot,
          }),
        ),
      );
      if (universes.length === 0) return [];
      const entries = block.reconstruction.transactions.map((entry) => ({
        key: entry.keyBytes,
        value: entry.valueBytes,
      }));
      const trie = await buildTrieView(entries);
      const proofCbor = requireProof(
        trie,
        material.transactionId,
        "accepted missing-script-source transaction",
      );
      const inclusion = parseSubmitStep01TxInclusion({
        nativeTxId: transaction.nodeTxId,
        nativeTx: nativeTxFromCoreCompact(material.compact),
        nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
        l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: proofCbor,
      });
      return universes.flatMap((universe) => {
        const evidence = missingScriptSourceEvidenceFromUniverseV1({
          subject: acceptedVerdictSubjectV1(transaction.nodeTxId),
          universe,
        });
        if (!missingScriptSourceEvidenceClosesV1(evidence)) return [];
        const violationId = missingScriptSourceViolationIdV1();
        const finding: MissingScriptSourceReplayFindingV1 = {
          evidence,
          detection: {
            detectionId: `${violationId}:accepted:${position.toString()}:${transaction.nodeTxId}:${universe.purpose.absoluteIndex.toString()}`,
            headerHash: block.headerHash,
            violationId,
            position: BigInt(position),
            diagnostic: `accepted transaction ${transaction.nodeTxId} omitted purpose script source`,
          },
        };
        const artifact: MissingScriptSourceProductionArtifactV1 = Object.freeze(
          {
            headerHash: block.headerHash,
            header: block.header,
            evidence,
            authentication: universe.authentication,
            acceptedInclusion: inclusion,
          },
        );
        return [{ finding, artifact }];
      });
    }),
  );
  const forced = await Promise.all(
    block.reconstruction.forcedTransactions.map(
      async (transaction, position) => {
        const verdict = transaction.value.verdict;
        if (
          verdict === "ForcedTxValid" ||
          typeof verdict.ForcedTxInvalid.reason === "string" ||
          !("ScriptSourceMissing" in verdict.ForcedTxInvalid.reason)
        )
          return null;
        const reason = verdict.ForcedTxInvalid.reason;
        const coordinate = reason.ScriptSourceMissing;
        const purposeKind = exactIndex(coordinate.purpose_kind, "purpose kind");
        if (purposeKind > 3)
          throw new Error("missingScriptSource purpose kind changed");
        const purposeIndex = exactIndex(
          coordinate.purpose_index,
          "purpose index",
        );
        const eventKey = {
          ForcedTransactionEventKey: { tx_order_id: transaction.key },
        } as const;
        const universe = await buildRetainedMissingScriptSourceUniverseV1({
          eventKey,
          purposeKind: purposeKind as 0 | 1 | 2 | 3,
          purposeIndex,
          authenticatedValidationTraceEntries: validationTraceEntries,
          retainedValidationWitnessEntries: retainedWitnessEntries,
          expectedValidationTracesRoot: block.header.validationTracesRoot,
          expectedPresence: true,
        });
        const adjudicated = encodeMidgardNativeTxCanonicalV1(
          adjudicateMidgardNativeTxFullV1Validity(
            decodeMidgardNativeTxFullV1FromCanonicalCbor(
              transaction.fullTransactionCbor,
            ),
            "TxIsInvalid",
          ),
        );
        const material =
          deriveMidgardNativeTxFaultEvidenceMaterialV1(adjudicated);
        if (
          material.transactionId.toString("hex") !== transaction.value.tx_id ||
          material.proofSource.compactCbor.toString("hex") !==
            transaction.value.source.compact_cbor ||
          material.proofSource.witnessSetCompactCbor.toString("hex") !==
            transaction.value.source.witness_set_compact_cbor ||
          material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
            transaction.value.source.field_preimage_lengths_cbor
        )
          throw new Error(
            "missingScriptSource forced source changed authenticated leaf",
          );
        const evidence = missingScriptSourceEvidenceFromUniverseV1({
          subject: forcedVerdictSubjectV1({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: reason,
          }),
          universe,
        });
        if (!missingScriptSourceEvidenceClosesV1(evidence)) return null;
        const violationId = missingScriptSourceViolationIdV1();
        const finding: MissingScriptSourceReplayFindingV1 = {
          evidence,
          detection: {
            detectionId: `${violationId}:forced:${position.toString()}:${transaction.value.tx_id}:${universe.purpose.absoluteIndex.toString()}`,
            headerHash: block.headerHash,
            violationId,
            position: BigInt(position),
            diagnostic: `forced transaction ${transaction.value.tx_id} rejected despite retained purpose source`,
          },
        };
        const artifact: MissingScriptSourceProductionArtifactV1 = Object.freeze(
          {
            headerHash: block.headerHash,
            header: block.header,
            evidence,
            authentication: universe.authentication,
            forcedMembership: await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey,
            }),
          },
        );
        return { finding, artifact };
      },
    ),
  );
  return [...accepted.flat(), ...forced].filter(
    (value): value is NonNullable<typeof value> => value !== null,
  );
};

export const detectMissingScriptSourceCanonicalViolationsV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> =>
  Object.freeze(
    (await replayCandidates(block))
      .map(({ finding }) => finding.detection)
      .sort(
        (a, b) =>
          Number(a.position - b.position) ||
          a.detectionId.localeCompare(b.detectionId),
      ),
  );

export const prepareProductionMissingScriptSourceArtifactV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<MissingScriptSourceProductionArtifactV1> => {
  const candidates = await replayCandidates(block);
  const selected = selectMissingScriptSourceCanonicalFindingV1(
    candidates.map(({ finding }) => finding),
  );
  const exact = candidates.find(
    ({ finding }) =>
      finding.detection.detectionId === selected.detection.detectionId,
  );
  if (exact === undefined)
    throw new Error("missingScriptSource canonical finding disappeared");
  return exact.artifact;
};
