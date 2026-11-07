export const CODE_ROOTS: readonly string[];
export const DOC: RegExp;
export const HISTORICAL_MARKER: string;
export const DELETED: readonly Readonly<{ name: string; pattern: RegExp }>[];
export function trackedFiles(root: string): string[];
export function exempt(path: string): boolean;
export function linesToCheck(
  path: string,
  text: string,
): { line: string; number: number }[];
export function namesDeleted(path: string, text: string): boolean;
export function deletedNameReaders(
  root: string,
  files: readonly string[],
): string[];
