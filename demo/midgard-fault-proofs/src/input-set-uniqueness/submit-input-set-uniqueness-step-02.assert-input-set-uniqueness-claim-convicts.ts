import { type UTxO } from "@lucid-evolution/lucid";

import type { InputSetUniquenessClaim } from "./scan.js";
import {
  inputSetUniquenessStepLabel,
  inputSetUniquenessSubmitError,
} from "./submit-common.js";

export const STEP_LABEL = inputSetUniquenessStepLabel(1);

export type SubmitInputSetUniquenessStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  /** The anchored transaction the conviction opened. */
  readonly badTxId: string;
  readonly claim: InputSetUniquenessClaim;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type Step02SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export const uniqueUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) {
      return false;
    }
    seen.add(key);
    return true;
  });
};

const normalizedItems = (
  items: readonly string[],
  fieldLabel: string,
): readonly string[] =>
  items.map((item, index) => {
    const lowered = item.toLowerCase();
    if (!/^([0-9a-f]{2})+$/u.test(lowered)) {
      throw inputSetUniquenessSubmitError(
        `${STEP_LABEL} ${fieldLabel} item ${index.toString()} is not hexadecimal.`,
      );
    }
    return lowered;
  });

const requireItemAt = (
  items: readonly string[],
  index: bigint,
  fieldLabel: string,
): string => {
  if (index < 0n || index >= BigInt(items.length)) {
    throw inputSetUniquenessSubmitError(
      `${STEP_LABEL} ${fieldLabel} index ${index.toString()} is outside the field's ${items.length.toString()} committed items.`,
    );
  }
  return items[Number(index)] as string;
};

/** Twin of the validator's per-arm conviction predicate, fail-closed. */
export const assertInputSetUniquenessClaimConvicts = ({
  claim,
  spendInputItemCbors,
  referenceInputItemCbors,
}: {
  readonly claim: InputSetUniquenessClaim;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
}): void => {
  const spends = normalizedItems(spendInputItemCbors, "spend-input");
  const references = normalizedItems(
    referenceInputItemCbors,
    "reference-input",
  );
  if (claim.kind === "spendReferenceOverlap") {
    const spendItem = requireItemAt(spends, claim.spendIndex, "spend-input");
    const referenceItem = requireItemAt(
      references,
      claim.referenceIndex,
      "reference-input",
    );
    if (spendItem !== referenceItem) {
      throw inputSetUniquenessSubmitError(
        `${STEP_LABEL} spend input ${claim.spendIndex.toString()} and reference input ${claim.referenceIndex.toString()} name different out-refs; the sets are disjoint at the claimed positions.`,
      );
    }
    return;
  }
  const fieldLabel =
    claim.kind === "duplicateSpendInputs" ? "spend-input" : "reference-input";
  const items = claim.kind === "duplicateSpendInputs" ? spends : references;
  if (claim.firstIndex >= claim.secondIndex) {
    throw inputSetUniquenessSubmitError(
      `${STEP_LABEL} duplicate claim needs first_index < second_index; got ${claim.firstIndex.toString()} and ${claim.secondIndex.toString()}.`,
    );
  }
  const first = requireItemAt(items, claim.firstIndex, fieldLabel);
  const second = requireItemAt(items, claim.secondIndex, fieldLabel);
  if (first !== second) {
    throw inputSetUniquenessSubmitError(
      `${STEP_LABEL} ${fieldLabel} items ${claim.firstIndex.toString()} and ${claim.secondIndex.toString()} name different out-refs; there is no duplicate at the claimed positions.`,
    );
  }
};
