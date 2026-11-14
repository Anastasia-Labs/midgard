import { type MidgardCekProgramMaterialEntry } from "./cek-proof.program-material.js";
import {
  type MidgardCekProgramMaterialVerification,
  type MidgardCekProgramMaterialVerificationOptions,
  normalizeProgramMaterial,
} from "./cek-proof.program-material-task.js";
import { verifyOneProgramMaterial } from "./cek-proof.verify-one-program-material.js";

/** Verifies locally derived term/value/blob material before building a dispute. */
export const verifyMidgardCekProgramMaterial = (
  termRoot: Uint8Array,
  entries: Iterable<MidgardCekProgramMaterialEntry>,
  options: MidgardCekProgramMaterialVerificationOptions = {},
): MidgardCekProgramMaterialVerification => {
  const material = normalizeProgramMaterial(entries);
  const verified = verifyOneProgramMaterial(
    termRoot,
    material,
    { materializedBlobs: new Map(), validatedConstants: new Map() },
    {
      includeConstants: true,
      onBlobMaterialized: options.onBlobMaterialized,
      onConstantMaterialized: options.onConstantMaterialized,
    },
  );
  if (
    options.allowUnreachable !== true &&
    verified.reachableRoots.size !== material.size
  ) {
    throw new Error("CEK program material contains unreachable nodes");
  }
  return verified;
};
