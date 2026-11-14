import { type FaultProofContracts } from "@al-ft/midgard-sdk";

import { type SupportedFaultProofCategoryName } from "./runtime.require-fault-proof-step-reference-script.js";

/** Categories whose label is not the kebab-case spelling of their name. */
const CATEGORY_LABEL_OVERRIDES: Readonly<
  Partial<Record<SupportedFaultProofCategoryName, string>>
> = Object.freeze({ nonExistentInputNoIndex: "input-no-idx" });

/** The kebab-case label a category's messages name it by. */
export const categoryLabel = (
  categoryName: SupportedFaultProofCategoryName,
): string =>
  CATEGORY_LABEL_OVERRIDES[categoryName] ??
  categoryName.replace(/[A-Z]/gu, (letter) => `-${letter.toLowerCase()}`);

export type OneCategoryFaultProofContracts = Pick<
  FaultProofContracts,
  "computationThread" | "fraudProof"
> &
  Partial<{
    readonly [CategoryName in SupportedFaultProofCategoryName]: FaultProofContracts[CategoryName];
  }>;
