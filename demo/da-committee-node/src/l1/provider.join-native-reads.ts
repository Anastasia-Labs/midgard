/** Keep ownership of every started read and preserve the first observed failure. */
export function joinNativeReads<T extends readonly unknown[] | []>(
  reads: T,
): Promise<{ -readonly [K in keyof T]: Awaited<T[K]> }>;
export async function joinNativeReads(
  reads: readonly unknown[],
): Promise<unknown[]> {
  // Native promises retain identity; other thenables are adopted only once.
  const pending = reads.map((read) => Promise.resolve(read));
  try {
    return await Promise.all(pending);
  } catch (error) {
    await Promise.allSettled(pending);
    throw error;
  }
}
