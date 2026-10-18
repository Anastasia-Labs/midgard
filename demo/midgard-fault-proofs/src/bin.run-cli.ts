import {
  formatUnknownError,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";

import { main } from "./bin.main.js";

/**
 * Runs the CLI and returns its exit code. Fault-proof preparation verifies
 * Ed25519 signatures with libsodium, which answers only once it has
 * initialised, so the run waits for it first; that wait is bounded where it
 * is defined. A failure of the wait or of `main` is written as one line, and
 * the code is 1.
 */
export const runFaultProofsCli = async (
  write: (line: string) => void = (line) => {
    process.stderr.write(line);
  },
): Promise<number> => {
  try {
    await ledgerEd25519SignatureReady();
    await main();
    return 0;
  } catch (error) {
    write(`midgard-fault-proofs: ${formatUnknownError(error)}\n`);
    return 1;
  }
};
