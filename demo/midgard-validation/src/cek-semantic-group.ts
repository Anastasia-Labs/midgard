/**
 * The semantics group of an arm 31 step for builtin `tag`, as the budget hop
 * routes it (`semantic_group`, cek-semantic-chain), or null for a tag no
 * semantics group takes.
 */
export const midgardCekSemanticGroup = (tag: bigint): number | null => {
  if (tag < 0n || tag > 255n) return null;
  const t = Number(tag);
  if (t === 29 || t === 30) return 0;
  if (t === 31 || t === 32 || t === 35) return 1;
  if (t === 33 || t === 34) return 2;
  if (t === 36) return 3;
  if (t === 37 || t === 42) return 4;
  if (t >= 47 && t <= 50) return 6;
  if (t === 51) return 10;
  if ([39, 40, 41, 44, 45, 46].includes(t)) return 5;
  if (t >= 2 && t <= 6) return 9;
  if (t >= 0 && t <= 9) return 8;
  if (
    (t >= 10 && t <= 25) ||
    t === 53 ||
    t === 71 ||
    t === 72 ||
    (t >= 75 && t <= 79) ||
    t === 84 ||
    t === 85 ||
    t === 86
  ) {
    return 7;
  }
  if (t === 73 || t === 74 || t === 82 || t === 83) return 11;
  if (t === 80) return 12;
  return null;
};
