import fs from 'fs/promises';

/**
 * Check if a file or directory exists
 */
export const exists = async (path: string): Promise<boolean> => {
  try {
    await fs.access(path);
    return true;
  } catch (error) {
    return false;
  }
};
