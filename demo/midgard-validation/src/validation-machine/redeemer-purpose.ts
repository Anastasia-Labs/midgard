/**
 * Purpose kinds, redeemer purpose tags, and the pointer/purpose match.
 */

export type MidgardPurposeKind = 0 | 1 | 2 | 3;

export type MidgardRedeemerPurposeTag = 0 | 1 | 3 | 6;

export function redeemerTagForPurposeKind(
  purposeKind: MidgardPurposeKind,
): MidgardRedeemerPurposeTag;

export function redeemerTagForPurposeKind(
  purposeKind: number,
): MidgardRedeemerPurposeTag | null;

export function redeemerTagForPurposeKind(
  purposeKind: number,
): MidgardRedeemerPurposeTag | null {
  switch (purposeKind) {
    case 0:
      return 0;
    case 1:
      return 1;
    case 2:
      return 3;
    case 3:
      return 6;
    default:
      return null;
  }
}

export const purposeKindForRedeemerTag = (
  redeemerTag: number,
): MidgardPurposeKind | null => {
  switch (redeemerTag) {
    case 0:
      return 0;
    case 1:
      return 1;
    case 3:
      return 2;
    case 6:
      return 3;
    default:
      return null;
  }
};

export const redeemerPointerMatchesPurpose = (input: {
  readonly purposeKind: number;
  readonly purposeIndex: bigint;
  readonly redeemerTag: number;
  readonly redeemerIndex: bigint;
}): boolean => {
  const expectedTag = redeemerTagForPurposeKind(input.purposeKind);
  return (
    expectedTag !== null &&
    input.redeemerTag === expectedTag &&
    input.redeemerIndex === input.purposeIndex
  );
};
