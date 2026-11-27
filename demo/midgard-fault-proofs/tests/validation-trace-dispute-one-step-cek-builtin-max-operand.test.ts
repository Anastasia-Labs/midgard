/** The validation-trace dispute runner at a CEK builtin step of core group 3
 * whose operand is a byte string near the largest admission takes, driven
 * through the binder and the group's arm chain to the award
 * (support/validation-trace-dispute-one-step-cek-builtin-drive.ts): every
 * hop carries the operand in its arm witness, and each lands within the
 * transaction limits. */
import { beforeAll, describe, expect, it } from "vitest";

import {
  cekBuiltinDrive,
  flatScript,
} from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";

/** The operand's length in bytes. */
const OPERAND_BYTES = 8_900;

/** `(program 1.1.0 (lam ctx [(lam _ (error)) [(builtin sha2_256)
 * (con bytestring #abab..ab)]]))`, the byte string `OPERAND_BYTES` long, in
 * Flat (its byte string in chunks of at most 255 bytes). */
const LARGEST_OPERAND_SCRIPT = (() => {
  const chunks: string[] = [];
  for (let rest = OPERAND_BYTES; rest > 0; rest -= 255) {
    const length = Math.min(255, rest);
    chunks.push(length.toString(16).padStart(2, "0"), "ab".repeat(length));
  }
  return flatScript(`010100232637249101${chunks.join("")}0001`);
})();

const ARMS = [
  "directScalarRoots",
  "directScalarBudget",
  "directScalar",
] as const;

/** The moves that carry the step's arm witness, by their fit labels. */
const CARRIERS = [
  "PrepareSelected core",
  "VerifyCoreStep",
  ...ARMS.map((holder) => `VerifyArm ${holder}`),
  "Settle core",
];

const drive = cekBuiltinDrive({
  group: 3,
  arm: 30,
  arms: ARMS,
  script: LARGEST_OPERAND_SCRIPT,
});

describe("validation-trace dispute runner: driving a thread at a CEK builtin step of core group 3 over an operand near the largest admission takes", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "plans the step through the binder to the group's arm chain, and leaves it unresolved where the chain is not installed",
    drive.plans,
    300_000,
  );

  it("moves the thread through the rest of the chain to the award within the transaction limits, each move carrying the operand, and closes it", async () => {
    await drive.awards();
    for (const label of CARRIERS) {
      expect(drive.bytesOf(label)).toBeGreaterThan(OPERAND_BYTES);
      console.info(
        `fee ${label}: ${drive.feeOf(label).toString()} lovelace, ${drive.bytesOf(label).toString()} bytes`,
      );
    }
  }, 1_200_000);

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
