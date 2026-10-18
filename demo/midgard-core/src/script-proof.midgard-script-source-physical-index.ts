/** Physical membership index for a logical reference-first source scan.
 * inlineSourceCount is authenticated by the field-6 door and retained through
 * resolved-input replay. This coordinate is separate from raw source fields.
 */
export const midgardScriptSourcePhysicalIndex = (
  inlineSourceCount: number,
  sourceCount: number,
  sourceCursor: number,
): number => {
  if (
    ![inlineSourceCount, sourceCount, sourceCursor].every(
      Number.isSafeInteger,
    ) ||
    inlineSourceCount < 0 ||
    inlineSourceCount > sourceCount ||
    sourceCursor < 0 ||
    sourceCursor >= sourceCount
  ) {
    throw new Error(
      "script source scan has invalid authenticated count/cursor",
    );
  }
  const referenceCount = sourceCount - inlineSourceCount;
  return sourceCursor < referenceCount
    ? inlineSourceCount + sourceCursor
    : sourceCursor - referenceCount;
};
