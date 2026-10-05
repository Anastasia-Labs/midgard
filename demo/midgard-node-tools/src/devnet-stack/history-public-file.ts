import {
  closeSync,
  constants,
  fstatSync,
  lstatSync,
  openSync,
  readSync,
  realpathSync,
  type Stats,
} from "node:fs";

import { HistoryAdmissionExpired } from "./history-admission-expired.js";
import {
  HistoryConfigurationRefusal,
  HistoryEvidenceContradiction,
} from "./history-configuration-refusal.js";
import { historyProofRemaining } from "./history-proof-deadline.js";

const refuse = (message: string): never => {
  throw new HistoryConfigurationRefusal(message);
};
const metadata = (stat: Stats) =>
  `${stat.dev}:${stat.ino}:${stat.size}:${stat.mtimeMs}:${stat.ctimeMs}`;
export const historyPublicBudget = (deadline?: number): void => {
  if (deadline !== undefined && !Number.isFinite(deadline))
    throw new Error("history admission deadline is invalid");
  if (deadline !== undefined && historyProofRemaining(deadline) === 0)
    throw new HistoryAdmissionExpired(deadline);
};
/** Bounded public evidence only: callers supply an explicit public allowlist. */
export const historyPublicFile = (path: string, deadline?: number) => {
  historyPublicBudget(deadline);
  let failure: unknown;
  const refuseObserved = (message: string): never => {
    throw new HistoryEvidenceContradiction(message);
  };
  try {
    if (realpathSync(path) !== path)
      refuseObserved("history public evidence traverses a symlink");
    const fd = openSync(path, constants.O_RDONLY | constants.O_NOFOLLOW);
    try {
      const stat = fstatSync(fd);
      if (!stat.isFile() || stat.size === 0 || stat.size > 16 * 1024 * 1024)
        refuseObserved("history public evidence is nonregular or oversized");
      const bytes = Buffer.alloc(stat.size + 1);
      let size = 0;
      while (size < bytes.length) {
        historyPublicBudget(deadline);
        const n = readSync(fd, bytes, size, bytes.length - size, size);
        if (n === 0) break;
        size += n;
      }
      const opened = metadata(stat);
      if (
        size !== stat.size ||
        metadata(fstatSync(fd)) !== opened ||
        realpathSync(path) !== path ||
        metadata(lstatSync(path)) !== opened
      )
        refuseObserved("history public evidence changed while reading");
      let text: string;
      try {
        text = new TextDecoder("utf8", { fatal: true }).decode(
          bytes.subarray(0, size),
        );
      } catch {
        return refuse("history public evidence is malformed UTF8");
      }
      historyPublicBudget(deadline);
      return {
        bytes: bytes.subarray(0, size),
        text,
        identity: `${stat.dev}:${stat.ino}`,
      };
    } finally {
      closeSync(fd);
    }
  } catch (error) {
    failure = error;
    if (
      error instanceof Error &&
      "code" in error &&
      ["ENOENT", "ENOTDIR", "ELOOP"].some((code) => error.code === code)
    )
      failure = new HistoryEvidenceContradiction(
        "history recorded public evidence is missing; restore exact state",
      );
    throw failure;
  } finally {
    // A positive file/identity contradiction remains known after its deadline.
    // Decoder uncertainty may still become an ordinary attempt expiry.
    if (
      failure === undefined ||
      (failure instanceof HistoryConfigurationRefusal &&
        !(failure instanceof HistoryEvidenceContradiction))
    )
      historyPublicBudget(deadline);
  }
};
