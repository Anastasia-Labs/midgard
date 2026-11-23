import {
  decodeMidgardInputFieldPreimage,
  decodeMidgardSpendInputItem,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { EMPTY_MERKLE_TREE_ROOT, Proof } from "@al-ft/midgard-sdk/core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import { checkedLedgerAsLedger } from "../checked-ledger-store.js";
import { readingLedger } from "../event-ledger.ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { claimedIndex } from "../evidence/claimed-coordinate.js";
import { type CheckedLedger, ledgerEntryLeaf } from "../ledger-at.js";
import type { Ledger } from "../ledger-store/index.js";
import {
  type AuthenticatedPriorLedgerOutput,
  fail,
  outRefKey,
  prepareResolvedOutputAbsentInputEvidence,
  prepareResolvedOutputNonCanonicalEvidence,
  type ResolvedOutputCoordinate,
  type ResolvedOutputPriorLedgerReplay,
  type ResolvedOutputWorkflowEvidence,
} from "./resolved-output-non-canonical.prepare-resolved-output-non-canonical-evidence.js";

const PRIOR_LEDGER = "resolved-output prior ledger";

/**
 * The ledger keys of the inputs in both input fields (spend and reference) of
 * a transaction's `fieldPreimages`. A field that does not decode names none:
 * a family that reads it decodes the same bytes and refuses them itself, and
 * a refusal here would also stop the family that never reads that field.
 * (Every transaction of authenticated evidence derives its field preimages.)
 */
const inputKeys = (fieldPreimages: readonly Uint8Array[]): Buffer[] =>
  [0, 1].flatMap((field) => {
    try {
      return decodeMidgardInputFieldPreimage(fieldPreimages[field]!).map(
        encodeMidgardSpendInputItem,
      );
    } catch {
      return [];
    }
  });

/**
 * Every out-ref whose output resolved-output-non-canonical or
 * spend-input-signer-missing reads from the prior ledger: each spend and
 * reference input of each accepted transaction, and each input of each forced
 * transaction rejected for one of those two reasons.
 */
const resolvedInputKeys = (block: CanonicalBlockEvidence): Buffer[] => [
  ...block.transactions.flatMap((transaction) =>
    inputKeys(
      deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(transaction.txCbor, "hex"),
      ).fieldPreimages,
    ),
  ),
  ...block.reconstruction.forcedTransactions.flatMap((transaction) => {
    if (transaction.value.verdict === "ForcedTxValid") return [];
    const reason = transaction.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !(
        "InputSpentOutputNonCanonical" in reason ||
        "SpendInputSignerMissing" in reason
      )
    )
      return [];
    return inputKeys(
      deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      ).fieldPreimages,
    );
  }),
];

/**
 * The prior view of `block` from its predecessor ledger: the output, leaf
 * descriptor and membership proof of every resolved input the two families
 * read (`resolvedInputKeys`) that the ledger holds, each proof against the
 * ledger's root. A header committing the empty previous ledger has an empty
 * prior view and needs no predecessor. A ledger the store can no longer read
 * is this process's own fault (`readingLedger`).
 */
export const readResolvedOutputPriorLedger = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: Ledger | undefined;
}): Promise<ResolvedOutputPriorLedgerReplay> => {
  if (predecessor === undefined) {
    if (block.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT)
      return fail("authenticated predecessor history is absent");
    return Object.freeze({
      priorRoot: EMPTY_MERKLE_TREE_ROOT,
      outputs: new Map(),
    });
  }
  if (predecessor.root !== block.header.prevUtxosRoot)
    return fail("authenticated predecessor history is substituted");
  const keys = new Map(
    resolvedInputKeys(block).map((key) => [key.toString("hex"), key]),
  );
  const outputs = new Map<
    string,
    Omit<AuthenticatedPriorLedgerOutput, "priorRoot">
  >();
  await readingLedger(PRIOR_LEDGER, async () => {
    const read = await predecessor.read([...keys.values()]);
    for (const [hex, outRef] of keys) {
      const { output, proof } = read.get(hex)!;
      if (output === undefined) continue;
      const { leaf } = ledgerEntryLeaf({ outRef, output }, PRIOR_LEDGER);
      const membershipProofCborHex = (await proof()).toString("hex");
      const { txId, outputIndex } = decodeMidgardSpendInputItem(outRef);
      const transactionId = Buffer.from(txId).toString("hex");
      outputs.set(outRefKey(transactionId, outputIndex), {
        transactionId,
        outputIndex,
        descriptorCborHex: leaf.toString("hex"),
        outputCborHex: output.toString("hex"),
        membershipProofCborHex,
        membershipProof: Data.from(membershipProofCborHex, Proof),
      });
    }
  });
  return Object.freeze({
    priorRoot: block.header.prevUtxosRoot,
    outputs,
  });
};

/**
 * `readResolvedOutputPriorLedger` from a predecessor still held as a
 * `CheckedLedger`.
 */
export const deriveResolvedOutputPriorLedgerReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: CheckedLedger | undefined;
}): Promise<ResolvedOutputPriorLedgerReplay> =>
  await readResolvedOutputPriorLedger({
    block,
    predecessor:
      predecessor === undefined
        ? undefined
        : await checkedLedgerAsLedger(predecessor),
  });

/**
 * Package-owned complete replay route. It visits every spend and reference
 * input of every accepted transaction, plus every forced rejection carrying
 * this exact reason. The supplied predecessor view is not an operator hint:
 * callers must pass the view `readResolvedOutputPriorLedger` reads from the
 * predecessor ledger whose root is the challenged header's authenticated
 * `prevUtxosRoot`.
 */
export const detectResolvedOutputNonCanonicalCompleteReplay = ({
  block,
  priorLedger,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly priorLedger: ResolvedOutputPriorLedgerReplay;
}): readonly ResolvedOutputWorkflowEvidence[] => {
  if (priorLedger.priorRoot !== block.header.prevUtxosRoot)
    return fail("predecessor replay root differs from authenticated header");
  const detections: ResolvedOutputWorkflowEvidence[] = [];
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
    if (selected === undefined) return;
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
    const subject = forcedVerdictSubject({
      transactionId: transaction.value.tx_id,
      sourceKey: transaction.key,
      rejectionReason: reason,
    });
    const sourceKind = coordinate.source_kind;
    const inputIndex =
      sourceKind === 0n || sourceKind === 1n
        ? claimedIndex(
            coordinate.input_index,
            decodeMidgardInputFieldPreimage(
              deriveMidgardForcedTxFaultEvidenceMaterial(
                transaction.fullTransactionCbor,
              ).fieldPreimages[Number(sourceKind)]!,
            ).length,
          )
        : undefined;
    // A coordinate naming no input of the transaction resolves no output,
    // so the rejection is contradicted outright.
    if (inputIndex === undefined) {
      detections.push(
        prepareResolvedOutputAbsentInputEvidence({
          subject,
          canonicalTransactionCbor: transaction.fullTransactionCbor,
          priorRoot: priorLedger.priorRoot,
        }),
      );
      return;
    }
    inspect(transaction.fullTransactionCbor, subject, {
      sourceKind: sourceKind === 0n ? 0 : 1,
      inputIndex,
    });
  });
  return Object.freeze(detections);
};
