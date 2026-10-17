import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { forcedTransactionSubject } from "../workflow/detection-subject.js";
import { minAdaOutputFacts } from "./scan.js";

/**
 * Forced leaves whose minimum-Ada verdict contradicts the committed source:
 * a `ForcedTxValid` leaf with an output below the floor (its outputs entered
 * the ledger), or an `OutputBelowMinAda` rejection naming an output that
 * meets it.
 */
export const detectMinAdaForcedReplay = (block: CanonicalBlockEvidence) =>
  block.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      const accepted = verdict === "ForcedTxValid";
      if (
        !accepted &&
        (typeof verdict.ForcedTxInvalid.reason === "string" ||
          !("OutputBelowMinAda" in verdict.ForcedTxInvalid.reason))
      )
        return [];
      const material = deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      const source = transaction.value.submitted_source;
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          source.field_preimage_lengths_cbor
      )
        throw new Error(
          "min-ada: forced preimage differs from authenticated leaf",
        );
      const outputs = decodeMidgardFieldPreimage(material.fieldPreimages[2]!);
      const outputMaterial = (outputIndex: bigint) => {
        const item =
          outputIndex >= 0n && outputIndex <= BigInt(Number.MAX_SAFE_INTEGER)
            ? outputs[Number(outputIndex)]
            : undefined;
        if (item === undefined) return undefined;
        const output = minAdaOutputFacts(item);
        if (output === null) return undefined;
        return {
          output,
          meets: outputMeetsMinAda(
            MIDGARD_COINS_PER_UTXO_BYTE,
            BigInt(output.totalLength),
            output.lovelace,
          ),
        };
      };
      // An accepted leaf is wrong at every output below the floor; a
      // rejection is wrong only when its named output meets the floor.
      const contradicted = accepted
        ? outputs.flatMap((_, index) => {
            const opened = outputMaterial(BigInt(index));
            return opened === undefined || opened.meets
              ? []
              : [{ outputIndex: BigInt(index), output: opened.output }];
          })
        : (() => {
            const outputIndex = (
              verdict.ForcedTxInvalid.reason as {
                OutputBelowMinAda: { output_index: bigint };
              }
            ).OutputBelowMinAda.output_index;
            const opened = outputMaterial(outputIndex);
            return opened === undefined || !opened.meets
              ? []
              : [{ outputIndex, output: opened.output }];
          })();
      const direction = accepted ? 0n : 1n;
      const subject = forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: accepted
          ? null
          : (verdict.ForcedTxInvalid.reason as Exclude<
              typeof verdict.ForcedTxInvalid.reason,
              string
            >),
      });
      return contradicted.map(({ outputIndex }) => ({
        ...forcedTransactionSubject(transaction.key),
        detectionId: `min-ada:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${outputIndex.toString()}`,
        violationId: "min-ada" as const,
        headerHash: block.headerHash,
        position: BigInt(forcedIndex),
        forcedIndex,
        transactionId: transaction.value.tx_id,
        evidence: {
          subject,
          state: {
            grammar_checkpoint_hash: "",
            grammar_complete: false,
            walk_checkpoint_hash: "",
            direction,
            source_kind: 1n,
            bad_tx_id: transaction.value.tx_id,
            fault: { output_index: outputIndex },
          },
          nativeTxCompactCbor: source.compact_cbor,
          nativeTxCanonicalCbor: Buffer.from(
            transaction.fullTransactionCbor,
          ).toString("hex"),
          badTxId: transaction.value.tx_id,
          badOutputIndex: outputIndex,
          outputItemCbors: outputs.map((item) =>
            Buffer.from(item).toString("hex"),
          ),
          fault: { output_index: outputIndex },
          kind: "min-ada-forced" as const,
          headerHash: block.headerHash,
        },
      }));
    },
  );

export const prepareMinAdaForcedPlan = async ({
  block,
  detectionId,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly detectionId?: string;
}) => {
  const findings = detectMinAdaForcedReplay(block);
  const finding =
    detectionId === undefined
      ? findings[0]
      : findings.find((value) => value.detectionId === detectionId);
  if (finding === undefined)
    throw new Error("min-ada: no authenticated forced verdict contradiction");
  const transaction =
    block.reconstruction.forcedTransactions[finding.forcedIndex]!;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: transaction.key } },
  });
  return {
    ...finding,
    forcedSource: {
      header: block.header,
      membership,
      direction: finding.evidence.state.direction,
    },
  };
};
