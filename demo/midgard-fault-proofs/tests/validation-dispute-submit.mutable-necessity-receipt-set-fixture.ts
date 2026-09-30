import { type CekProgramMaterialNecessityReceiptSet } from "@al-ft/midgard-validation";

type MutableNecessityReceiptSetFixture = {
  validatorIdentities: Array<{
    title: string;
    generatedHash: string;
    appliedHash: string;
  }>;
  targetProtocolParameters: Record<string, unknown>;
  routeAttempts: Array<{
    route: string;
    transactions: Array<Record<string, unknown>>;
    dataAvailabilityFetchMilliseconds: number;
    evidenceConstructionMilliseconds: number;
    retryMilliseconds: number;
    rollbackAllowanceMilliseconds: number;
    settlementMilliseconds: number;
    removalMilliseconds: number;
    maturityWindowMarginMilliseconds: number;
    fit: boolean;
    limitingConstraint: Record<string, unknown> | null;
    minimumMultiOutputCount: number | null;
  }>;
} & Record<string, unknown>;

export const mutateNecessityReceiptSet = (
  receiptSet: CekProgramMaterialNecessityReceiptSet,
  mutate: (draft: MutableNecessityReceiptSetFixture) => void,
): CekProgramMaterialNecessityReceiptSet => {
  const draft = JSON.parse(
    JSON.stringify(receiptSet),
  ) as MutableNecessityReceiptSetFixture;
  mutate(draft);
  return draft as unknown as CekProgramMaterialNecessityReceiptSet;
};
