import {
  decodeMidgardFieldPreimage,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompact,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { nativeScriptInvalidSignerSet } from "./evidence-machine.js";

export const detectNativeScriptInvalidForcedReplay = (
  block: CanonicalBlockEvidence,
) =>
  block.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (
        verdict === "ForcedTxValid" ||
        typeof verdict.ForcedTxInvalid.reason === "string" ||
        !("WitnessNativeScriptFalse" in verdict.ForcedTxInvalid.reason)
      )
        return [];
      const reason = verdict.ForcedTxInvalid.reason;
      const scriptIndex = reason.WitnessNativeScriptFalse.script_index;
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
          "native-script-invalid: forced preimage differs from authenticated leaf",
        );
      const scripts = decodeMidgardFieldPreimage(material.fieldPreimages[6]!);
      const item =
        scriptIndex >= 0n && scriptIndex <= BigInt(Number.MAX_SAFE_INTEGER)
          ? scripts[Number(scriptIndex)]
          : undefined;
      if (item === undefined) return [];
      const script = decodeMidgardVersionedScript(item);
      if (script.language !== "NativeCardano") return [];
      const witnesses = decodeMidgardFieldPreimage(material.fieldPreimages[7]!);
      const signers = nativeScriptInvalidSignerSet(
        witnesses,
        transaction.value.tx_id,
      );
      const body = material.canonical.body;
      if (
        !verifyMidgardNativeScript(script.nativeScript, {
          witnessSigners: new Set(
            signers.hashes.map((hash) => hash.toString("hex")),
          ),
          validityIntervalStart:
            body.validityIntervalStart === MIDGARD_POSIX_TIME_NONE
              ? undefined
              : body.validityIntervalStart,
          validityIntervalEnd:
            body.validityIntervalEnd === MIDGARD_POSIX_TIME_NONE
              ? undefined
              : body.validityIntervalEnd,
        })
      )
        return [];
      const witness = deriveMidgardNativeTxWitnessSetCompact(
        material.canonical.witnessSet,
      );
      const subject = forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: reason,
      });
      return [
        {
          detectionId: `native-script-invalid:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${scriptIndex.toString()}`,
          violationId: "native-script-invalid" as const,
          headerHash: block.headerHash,
          position: BigInt(forcedIndex),
          forcedIndex,
          transactionId: transaction.value.tx_id,
          evidence: {
            subject,
            state: {
              subject,
              bad_tx_id: transaction.value.tx_id,
              bad_tx_witness_set_hash:
                material.compact.transactionWitnessSetHash.toString("hex"),
              validity_interval_start: body.validityIntervalStart,
              validity_interval_end: body.validityIntervalEnd,
              grammar_checkpoint_hash: "",
              grammar_complete: false,
              script_checkpoint_hash: "",
            },
            nativeTxCompactCbor: source.compact_cbor,
            nativeTxCanonicalCbor: Buffer.from(
              transaction.fullTransactionCbor,
            ).toString("hex"),
            scriptIndex,
            scriptItemCbor: Buffer.from(item).toString("hex"),
            addrWitnessItemCbors: witnesses.map((value) =>
              Buffer.from(value).toString("hex"),
            ),
            scriptWitnessItemCbors: scripts.map((value) =>
              Buffer.from(value).toString("hex"),
            ),
            witnessSet: {
              addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
              script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
              redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
            },
          },
        },
      ];
    },
  );

export const prepareNativeScriptInvalidForcedPlan = async ({
  block,
  detectionId,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly detectionId?: string;
}) => {
  const findings = detectNativeScriptInvalidForcedReplay(block);
  const finding =
    detectionId === undefined
      ? findings[0]
      : findings.find((value) => value.detectionId === detectionId);
  if (finding === undefined)
    throw new Error(
      "native-script-invalid: no authenticated wrongful rejection",
    );
  const transaction =
    block.reconstruction.forcedTransactions[finding.forcedIndex]!;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: transaction.key } },
  });
  return {
    ...finding,
    forcedSource: { header: block.header, membership, direction: 1n },
  };
};
