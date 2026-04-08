import fs from 'fs/promises';

/**
 * Returns whether a file-system path is currently accessible.
 *
 * The CLI uses this in setup flows where non-existence is expected and should
 * not surface as a noisy stack trace.
 */
export const exists = async (path: string): Promise<boolean> => {
  try {
    await fs.access(path);
    return true;
  } catch (error) {
    return false;
  }
};
