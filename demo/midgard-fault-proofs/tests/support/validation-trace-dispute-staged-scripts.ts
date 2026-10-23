/** Failing PlutusV3 scripts whose run takes a staged builtin step (arm 31,
 * semantics groups 7 and 8), `(program 1.1.0 (lam ctx [(lam _ (error))
 * STEP]))` in Flat, and the step's witness in a run of each. */
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  evaluateMidgardCekScript,
  type MidgardCekCoreStepWitness,
} from "@al-ft/midgard-validation";
import { plutusV3ScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";

/** STEP: `[(builtin sha2_256) B]`, B the 16,384 bytes of fourteen
 * doublings of `#00` by `(lam x [(builtin appendByteString) x x])`. */
export const STAGED_SHA2_SCRIPT =
  "010100232637246466e28004004c8cdc50008009919b8a00100132337140020026466e28004004c8cdc50008009919b8a00100132337140020026466e28004004c8cdc50008009919b8a00100132337140020026466e28004004c8cdc5000800a44101000001";

/** STEP: `[(lam x [(builtin addInteger) x x]) N]`, N the 4,163-byte integer
 * of nine squarings of 2^63 + 1 by `(lam x [(builtin multiplyInteger) x x])`. */
export const STAGED_ADD_SCRIPT =
  "010100232632337000020026466e08004004c8cdc10008009919b8200100132337040020026466e08004004c8cdc10008009919b8200100132337040020026466e08004005208280808080808080800201";

/** The script `hex` as a PlutusV3 script witness. */
export const stagedScript = (hex: string) =>
  plutusV3ScriptWitness(encodeCbor(Buffer.from(hex, "hex")));

export type StagedWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinStaged" }
>;

/** The staged step of builtin `tag` in the run of the failing script
 * `hex`. */
export const stagedStep = (hex: string, tag: bigint): StagedWitness => {
  const run = evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(Buffer.from(hex, "hex")),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from("d87980", "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: { cpu: 10_000_000_000n, memory: 16_500_000n },
    },
    "proof",
  );
  const witness = run.execution?.steps
    .map((step) => step.witness)
    .find(
      (candidate): candidate is StagedWitness =>
        candidate.kind === "executeBuiltinStaged" && candidate.tag === tag,
    );
  if (witness === undefined)
    throw new Error(`the run has no staged step of ${tag.toString()}`);
  return witness;
};
