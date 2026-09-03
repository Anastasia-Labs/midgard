import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  EventKeySchema,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  type AuthenticatedScriptPurpose,
  MISSING_REDEEMER_VIOLATION_ID,
  type MissingRedeemerEvidence,
  missingRedeemerEvidenceCloses,
  type MissingRedeemerPurposeKind,
  prepareMissingRedeemerEvidence,
} from "./family-v1.js";
import {
  buildMissingRedeemerStageTenAuthenticationFromRetainedDa,
  decodeMissingRedeemerStageTenControl,
  type MissingRedeemerStageTenAuthentication,
} from "./retained-stage-ten-v1.js";

export type MissingRedeemerArtifact = Readonly<{
  schemaVersion: "midgard-missing-redeemer-production-artifact-v1";
  headerHash: string;
  header: CanonicalBlockEvidence["header"];
  nativeTxCompactCbor: string;
  evidence: MissingRedeemerEvidence;
  authentication: MissingRedeemerStageTenAuthentication;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: NonNullable<
    Awaited<ReturnType<typeof buildForcedTransactionLeafMembershipProof>>
  >;
}>;
export type MissingRedeemerCandidate = Readonly<{
  detection: Readonly<{
    detectionId: string;
    headerHash: string;
    violationId: typeof MISSING_REDEEMER_VIOLATION_ID;
    position: bigint;
    diagnostic: string;
  }>;
  artifact: MissingRedeemerArtifact;
}>;

const exact = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`missingRedeemer ${label} changed`);
  return result;
};
export const admitMissingRedeemerArtifact = (
  value: unknown,
): MissingRedeemerArtifact => {
  const artifact = value as MissingRedeemerArtifact;
  if (
    typeof artifact !== "object" ||
    artifact === null ||
    artifact.schemaVersion !==
      "midgard-missing-redeemer-production-artifact-v1" ||
    !/^[0-9a-f]{64}$/u.test(artifact.headerHash) ||
    artifact.header.validationTracesRoot !==
      artifact.authentication.validationTracesRoot ||
    artifact.header.validationTraceCount !==
      artifact.authentication.validationTraceCount ||
    artifact.evidence.subject.transaction_id !==
      artifact.authentication.machineState.transaction_id ||
    artifact.evidence.purposeKind !==
      Number(artifact.authentication.control.discovery.current_purpose_kind) ||
    artifact.evidence.purposeIndex !==
      Number(artifact.authentication.control.discovery.current_purpose_index) ||
    artifact.evidence.purpose.sourceLeafHashHex !==
      artifact.authentication.control.discovery.matched_source_leaf ||
    artifact.evidence.purpose.sourceLanguageTag !==
      Number(artifact.authentication.sourceLanguageTag) ||
    (artifact.acceptedInclusion === undefined) ===
      (artifact.forcedMembership === undefined) ||
    !missingRedeemerEvidenceCloses(artifact.evidence)
  )
    throw new Error("missingRedeemer production artifact is not admitted");
  return artifact;
};

const terminalCoordinates = (
  retainedEntries: readonly Readonly<{ key: Buffer; value: Buffer }>[],
  eventKeyCbor: string,
) => {
  const coordinates = new Map<
    string,
    { purposeKind: MissingRedeemerPurposeKind; purposeIndex: number }
  >();
  for (const entry of retainedEntries) {
    const key = decodeRetainedValidationWitnessKey(entry.key);
    if (Data.to(key.event_key as never, EventKeySchema) !== eventKeyCbor)
      continue;
    const retained = decodeRetainedValidationWitness(entry.value);
    if (
      retained.phase !== 8n ||
      retained.machine_state.phase !== "ScriptSources"
    )
      continue;
    try {
      const control = decodeMissingRedeemerStageTenControl(
        Buffer.from(retained.witness_cbor, "hex"),
      );
      const kind = exact(
        control.discovery.current_purpose_kind,
        "purpose kind",
      );
      if (kind > 3) continue;
      const index = exact(
        control.discovery.current_purpose_index,
        "purpose index",
      );
      coordinates.set(`${kind.toString()}:${index.toString()}`, {
        purposeKind: kind as MissingRedeemerPurposeKind,
        purposeIndex: index,
      });
    } catch {
      // Other ScriptSources stages are not candidates for this category.
    }
  }
  return [...coordinates.values()];
};

const retainedPurpose = (
  authentication: MissingRedeemerStageTenAuthentication,
): AuthenticatedScriptPurpose => ({
  purposeKind: Number(
    authentication.control.discovery.current_purpose_kind,
  ) as MissingRedeemerPurposeKind,
  purposeIndex: exact(
    authentication.control.discovery.current_purpose_index,
    "purpose index",
  ),
  scriptHashHex: authentication.sourceScriptHash,
  subjectHex: authentication.control.discovery.current_subject,
  source:
    authentication.sourceOriginKind === 0n ? "witness" : "resolved-reference",
  sourceIndex: exact(
    authentication.control.discovery.matched_source_index,
    "source index",
  ),
  sourceOriginKind: Number(authentication.sourceOriginKind) as 0 | 1,
  sourceKeyHex: authentication.sourceKey,
  sourceLanguageTag: Number(authentication.sourceLanguageTag) as 3 | 128,
  sourceTotalLength: exact(authentication.sourceTotalLength, "source length"),
  sourceItemCommitmentHex: authentication.sourceItemCommitment,
  sourceLeafHashHex: authentication.control.discovery.matched_source_leaf,
  traceStateHashHex: authentication.traceProof.state_hash,
  workRootHex: authentication.machineState.work_root,
});

/** Complete accepted/forced replay over every retained terminal stage-10 coordinate. */
export const replayMissingRedeemer = async (
  block: CanonicalBlockEvidence,
): Promise<readonly MissingRedeemerCandidate[]> => {
  const descriptorEntries =
    block.reconstruction.payload.block_body.validation_traces.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const retainedEntries =
    block.reconstruction.payload.block_body.validation_trace_witnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const trie = await buildTrieView(
    block.transactions.map((transaction) => ({
      key: Buffer.from(transaction.nodeTxId, "hex"),
      value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
    })),
  );
  const accepted = await Promise.all(
    block.transactions.flatMap((transaction, position) => {
      const eventKey = {
        L2TransactionEventKey: { tx_id: transaction.nodeTxId },
      } as const;
      const eventKeyCbor = Data.to(eventKey as never, EventKeySchema);
      return terminalCoordinates(retainedEntries, eventKeyCbor).map(
        async (coordinate) => {
          const authentication =
            await buildMissingRedeemerStageTenAuthenticationFromRetainedDa({
              eventKey,
              transactionId: transaction.nodeTxId,
              ...coordinate,
              authenticatedValidationTraceEntries: descriptorEntries,
              retainedValidationWitnessEntries: retainedEntries,
              expectedValidationTracesRoot: block.header.validationTracesRoot,
            });
          const material = deriveMidgardNativeTxFaultEvidenceMaterial(
            Buffer.from(transaction.txCbor, "hex"),
          );
          const field = material.fieldPreimages[8];
          if (field === undefined)
            throw new Error(
              "missingRedeemer retained transaction omitted field 8",
            );
          const evidence = prepareMissingRedeemerEvidence({
            finding: {
              subject: acceptedVerdictSubject(transaction.nodeTxId),
              ...coordinate,
            },
            authenticatedPurpose: retainedPurpose(authentication),
            redeemerFieldPreimage: field,
            committedFieldHashHex:
              midgardFieldCommitment(field).toString("hex"),
          });
          if (!missingRedeemerEvidenceCloses(evidence)) return null;
          const proofCbor = requireProof(
            trie,
            Buffer.from(transaction.nodeTxId, "hex"),
            "missingRedeemer transaction",
          );
          const acceptedInclusion = parseSubmitStep01TxInclusion({
            nativeTxId: transaction.nodeTxId,
            nativeTx: nativeTxFromCoreCompact(material.compact),
            nativeTxCompactCbor:
              material.proofSource.compactCbor.toString("hex"),
            l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
            transactionsPhasRoot: trie.root,
            txMembershipProofCbor: proofCbor,
          });
          return {
            detection: {
              detectionId: `${MISSING_REDEEMER_VIOLATION_ID}:accepted:${position.toString()}:${transaction.nodeTxId}:${coordinate.purposeKind.toString()}:${coordinate.purposeIndex.toString()}`,
              headerHash: block.headerHash,
              violationId: MISSING_REDEEMER_VIOLATION_ID,
              position: BigInt(position),
              diagnostic: `accepted transaction lacks redeemer ${coordinate.purposeKind.toString()}:${coordinate.purposeIndex.toString()}`,
            },
            artifact: {
              schemaVersion: "midgard-missing-redeemer-production-artifact-v1",
              headerHash: block.headerHash,
              header: block.header,
              nativeTxCompactCbor:
                material.proofSource.compactCbor.toString("hex"),
              evidence,
              authentication,
              acceptedInclusion,
            },
          } satisfies MissingRedeemerCandidate;
        },
      );
    }),
  );
  const forced = await Promise.all(
    block.reconstruction.forcedTransactions.map(
      async (transaction, position) => {
        const verdict = transaction.value.verdict;
        if (
          verdict === "ForcedTxValid" ||
          typeof verdict.ForcedTxInvalid.reason === "string" ||
          !("RedeemerMissing" in verdict.ForcedTxInvalid.reason)
        )
          return null;
        const reason = verdict.ForcedTxInvalid.reason;
        const coordinate = {
          purposeKind: exact(
            reason.RedeemerMissing.purpose_kind,
            "forced purpose kind",
          ) as MissingRedeemerPurposeKind,
          purposeIndex: exact(
            reason.RedeemerMissing.purpose_index,
            "forced purpose index",
          ),
        };
        if (coordinate.purposeKind > 3)
          throw new Error("missingRedeemer forced purpose kind changed");
        const eventKey = {
          ForcedTransactionEventKey: { tx_order_id: transaction.key },
        } as const;
        const authentication =
          await buildMissingRedeemerStageTenAuthenticationFromRetainedDa({
            eventKey,
            transactionId: transaction.value.tx_id,
            ...coordinate,
            authenticatedValidationTraceEntries: descriptorEntries,
            retainedValidationWitnessEntries: retainedEntries,
            expectedValidationTracesRoot: block.header.validationTracesRoot,
          });
        const material = deriveMidgardNativeTxFaultEvidenceMaterial(
          encodeMidgardNativeTxCanonical(
            adjudicateMidgardNativeTxFullValidity(
              decodeMidgardNativeTxFullFromCanonicalCbor(
                transaction.fullTransactionCbor,
              ),
              "TxIsInvalid",
            ),
          ),
        );
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
            "missingRedeemer forced transaction differs from authenticated source",
          );
        const field = material.fieldPreimages[8];
        if (field === undefined)
          throw new Error("missingRedeemer forced transaction omitted field 8");
        const evidence = prepareMissingRedeemerEvidence({
          finding: {
            subject: forcedVerdictSubject({
              transactionId: transaction.value.tx_id,
              sourceKey: transaction.key,
              rejectionReason: reason,
            }),
            ...coordinate,
          },
          authenticatedPurpose: retainedPurpose(authentication),
          redeemerFieldPreimage: field,
          committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
        });
        if (!missingRedeemerEvidenceCloses(evidence)) return null;
        return {
          detection: {
            detectionId: `${MISSING_REDEEMER_VIOLATION_ID}:forced:${position.toString()}:${transaction.value.tx_id}:${coordinate.purposeKind.toString()}:${coordinate.purposeIndex.toString()}`,
            headerHash: block.headerHash,
            violationId: MISSING_REDEEMER_VIOLATION_ID,
            position: BigInt(position),
            diagnostic: `forced transaction has redeemer ${coordinate.purposeKind.toString()}:${coordinate.purposeIndex.toString()}`,
          },
          artifact: {
            schemaVersion: "midgard-missing-redeemer-production-artifact-v1",
            headerHash: block.headerHash,
            header: block.header,
            nativeTxCompactCbor:
              material.proofSource.compactCbor.toString("hex"),
            evidence,
            authentication,
            forcedMembership: (await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey,
            }))!,
          },
        } satisfies MissingRedeemerCandidate;
      },
    ),
  );
  const available: MissingRedeemerCandidate[] = [];
  for (const candidate of [...accepted, ...forced])
    if (candidate !== null) available.push(candidate);
  return available.sort((left, right) =>
    left.detection.detectionId.localeCompare(right.detection.detectionId),
  );
};

/** Closed complete-replay adapter consumed by the central replay token. */
export const detectMissingRedeemerCanonicalViolations = async (
  block: CanonicalBlockEvidence,
) =>
  Object.freeze(
    (await replayMissingRedeemer(block)).map(({ detection }) => detection),
  );
