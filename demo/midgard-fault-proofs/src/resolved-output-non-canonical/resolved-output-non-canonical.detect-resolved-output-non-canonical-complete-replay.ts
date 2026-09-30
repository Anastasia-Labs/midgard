import {
  decodeMidgardInputFieldPreimage,
  decodeMidgardSpendInputItem,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  EMPTY_MERKLE_TREE_ROOT,
  forcedVerdictSubject,
  GENESIS_HEADER_HASH,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import type { HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import { requireHistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import {
  type AuthenticatedPriorLedgerOutput,
  fail,
  index,
  outRefKey,
  prepareResolvedOutputNonCanonicalEvidence,
  type ResolvedOutputCoordinate,
  type ResolvedOutputNonCanonicalEvidence,
  type ResolvedOutputPriorLedgerReplay,
} from "./resolved-output-non-canonical.prepare-resolved-output-non-canonical-evidence.js";

/**
 * Reconstructs the complete predecessor view from the already-admitted public
 * retained-DA history authority. No caller-provided output or proof can cross
 * this boundary: descriptor membership and every raw output are re-derived.
 */
export const deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus =
  async ({
    block,
    corpus,
  }: {
    readonly block: CanonicalBlockEvidence;
    readonly corpus: HistoricalNativeScriptCorpus;
  }): Promise<ResolvedOutputPriorLedgerReplay> => {
    const admitted = requireHistoricalNativeScriptCorpus(corpus);
    if (admitted.currentEvidence !== block)
      return fail("historical corpus belongs to another challenged block");
    const predecessor = admitted.reconstructions.at(-2);
    if (
      predecessor === undefined &&
      block.header.prevHeaderHash === GENESIS_HEADER_HASH
    ) {
      if (block.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT)
        return fail(
          "genesis predecessor does not commit the canonical empty ledger",
        );
      return Object.freeze({
        priorRoot: EMPTY_MERKLE_TREE_ROOT,
        outputs: new Map(),
      });
    }
    if (
      predecessor === undefined ||
      predecessor.headerHash !== block.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== block.header.prevUtxosRoot
    )
      return fail("authenticated predecessor history is absent or substituted");

    // Retained UTxOs carry raw output bytes; the authenticated ledger root
    // commits their descriptors. Use the already reconstructed descriptor view
    // and pair it with the exact retained output by its canonical ledger key.
    const descriptorEntries = predecessor.rootData.utxos.entries;
    const outputPreimages = new Map(
      predecessor.utxos.map((entry) => [
        entry.key.toString("hex"),
        entry.value.toString("hex"),
      ]),
    );
    const trie = await buildTrieView(descriptorEntries);
    if (trie.root !== block.header.prevUtxosRoot)
      return fail("reconstructed predecessor trie root changed");
    const outputs = new Map<
      string,
      Omit<AuthenticatedPriorLedgerOutput, "priorRoot">
    >();
    for (const entry of descriptorEntries) {
      const decoded = decodeMidgardSpendInputItem(entry.key);
      const transactionId = Buffer.from(decoded.txId).toString("hex");
      const outputIndex = decoded.outputIndex;
      const key = outRefKey(transactionId, outputIndex);
      const outputCborHex = outputPreimages.get(entry.key.toString("hex"));
      if (outputCborHex === undefined)
        return fail("historical retained DA omitted a live output preimage");
      const proof = await keyValuePhasProof(
        {
          root: trie.root,
          count: BigInt(descriptorEntries.length),
          entries: descriptorEntries,
        },
        entry.key,
        entry.value,
      );
      outputs.set(key, {
        transactionId,
        outputIndex,
        descriptorCborHex: entry.value.toString("hex"),
        outputCborHex,
        membershipProofCborHex: requireProof(
          trie,
          entry.key,
          `resolved output ${key}`,
        ),
        membershipProof: proof,
      });
    }
    return Object.freeze({
      priorRoot: block.header.prevUtxosRoot,
      outputs,
    });
  };

/**
 * Package-owned complete replay route. It visits every spend and reference
 * input of every accepted transaction, plus every forced rejection carrying
 * this exact reason. The supplied predecessor view is not an operator hint:
 * callers must pass the complete view reconstructed from retained predecessor
 * DA whose root is the challenged header's authenticated `prevUtxosRoot`.
 */
export const detectResolvedOutputNonCanonicalCompleteReplay = ({
  block,
  priorLedger,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly priorLedger: ResolvedOutputPriorLedgerReplay;
}): readonly ResolvedOutputNonCanonicalEvidence[] => {
  if (priorLedger.priorRoot !== block.header.prevUtxosRoot)
    return fail("predecessor replay root differs from authenticated header");
  const detections: ResolvedOutputNonCanonicalEvidence[] = [];
  const inspect = (
    bytes: Uint8Array,
    subject: VerdictSubject,
    coordinate: ResolvedOutputCoordinate,
  ): void => {
    const material = (
      subject.source_kind === 1n
        ? deriveMidgardForcedTxFaultEvidenceMaterial
        : deriveMidgardNativeTxFaultEvidenceMaterial
    )(bytes);
    const selected = decodeMidgardInputFieldPreimage(
      material.fieldPreimages[coordinate.sourceKind]!,
    )[coordinate.inputIndex];
    if (selected === undefined)
      return fail("forced reason input coordinate is absent");
    const transactionId = Buffer.from(selected.txId).toString("hex");
    const resolved = priorLedger.outputs.get(
      outRefKey(transactionId, selected.outputIndex),
    );
    if (resolved === undefined && subject.source_kind === 0n) return;
    if (resolved === undefined)
      return fail("complete predecessor replay omitted a resolved input");
    try {
      detections.push(
        prepareResolvedOutputNonCanonicalEvidence({
          subject,
          coordinate,
          canonicalTransactionCbor: bytes,
          resolved: { ...resolved, priorRoot: priorLedger.priorRoot },
        }),
      );
    } catch (cause) {
      if (
        cause instanceof Error &&
        cause.message.endsWith(
          "authenticated output agrees with the operator verdict",
        )
      )
        return;
      throw cause;
    }
  };
  block.transactions.forEach((transaction) => {
    const bytes = Buffer.from(transaction.txCbor, "hex");
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
    const subject = acceptedVerdictSubject(
      material.transactionId.toString("hex"),
    );
    ([0, 1] as const).forEach((sourceKind) => {
      const inputs = decodeMidgardInputFieldPreimage(
        material.fieldPreimages[sourceKind]!,
      );
      inputs.forEach((_input, inputIndex) =>
        inspect(bytes, subject, { sourceKind, inputIndex }),
      );
    });
  });
  block.reconstruction.forcedTransactions.forEach((transaction) => {
    if (transaction.value.verdict === "ForcedTxValid") return;
    const reason = transaction.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !("InputSpentOutputNonCanonical" in reason)
    )
      return;
    const coordinate = reason.InputSpentOutputNonCanonical;
    const sourceKind = Number(coordinate.source_kind);
    if (sourceKind !== 0 && sourceKind !== 1)
      return fail("forced reason source kind is invalid");
    inspect(
      transaction.fullTransactionCbor,
      forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: reason,
      }),
      {
        sourceKind,
        inputIndex: index(
          Number(coordinate.input_index),
          "forced reason input index",
        ),
      },
    );
  });
  return Object.freeze(detections);
};
