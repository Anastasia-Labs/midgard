/**
 * Purpose kinds, redeemer purpose tags, and the pointer/purpose match.
 */

export type MidgardPurposeKindV1 = 0 | 1 | 2 | 3;

export type MidgardRedeemerPurposeTagV1 = 0 | 1 | 3 | 6;

export function redeemerTagForPurposeKindV1(
  purposeKind: MidgardPurposeKindV1,
): MidgardRedeemerPurposeTagV1;

export function redeemerTagForPurposeKindV1(
  purposeKind: number,
): MidgardRedeemerPurposeTagV1 | null;

export function redeemerTagForPurposeKindV1(
  purposeKind: number,
): MidgardRedeemerPurposeTagV1 | null {
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

export const purposeKindForRedeemerTagV1 = (
  redeemerTag: number,
): MidgardPurposeKindV1 | null => {
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

export const redeemerPointerMatchesPurposeV1 = (input: {
  readonly purposeKind: number;
  readonly purposeIndex: bigint;
  readonly redeemerTag: number;
  readonly redeemerIndex: bigint;
}): boolean => {
  const expectedTag = redeemerTagForPurposeKindV1(input.purposeKind);
  return (
    expectedTag !== null &&
    input.redeemerTag === expectedTag &&
    input.redeemerIndex === input.purposeIndex
  );
};
