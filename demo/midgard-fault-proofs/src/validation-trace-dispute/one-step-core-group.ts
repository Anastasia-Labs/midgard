/** The CEK core group a core step's mode and arm witness select
 * (cek-semantic-chain). */

const MODE = {
  compute: 0n,
  return: 1n,
  lookup: 2n,
  builtin: 3n,
  caseSelect: 6n,
  caseApply: 7n,
  semanticBuiltin: 8n,
} as const;

/**
 * `core_group(mode, witness)` (cek-semantic-chain): the arm chain a core
 * step of `mode` with the witness of constructor `arm` runs, or undefined
 * for a pair the chain refuses.
 */
export const cekCoreGroup = (
  mode: bigint,
  arm: number,
  builtinTag?: bigint,
): number | undefined => {
  switch (mode) {
    case MODE.compute:
      return (arm >= 0 && arm <= 10) || arm === 40 ? 0 : undefined;
    case MODE.lookup:
      return arm === 11 || arm === 12 ? 1 : undefined;
    case MODE.return:
      return arm === 41 ? 11 : arm >= 13 && arm <= 27 ? 1 : undefined;
    case MODE.caseSelect:
      return arm === 28 ? 1 : undefined;
    case MODE.caseApply:
      return arm === 29 ? 1 : undefined;
    case MODE.semanticBuiltin:
      return arm >= 33 && arm <= 35 ? 2 : undefined;
    case MODE.builtin: {
      if (arm === 38)
        return builtinTag === undefined
          ? undefined
          : builtinTag === 59n || builtinTag === 66n
            ? 12
            : 9;
      if (arm === 30)
        return builtinTag === undefined
          ? undefined
          : builtinTag >= 29n &&
              builtinTag <= 69n &&
              builtinTag !== 52n &&
              builtinTag !== 53n
            ? 4
            : 3;
      const groups: Readonly<Record<number, number>> = {
        31: 5,
        32: 6,
        36: 7,
        37: 8,
        39: 10,
      };
      return groups[arm];
    }
    default:
      return undefined;
  }
};
