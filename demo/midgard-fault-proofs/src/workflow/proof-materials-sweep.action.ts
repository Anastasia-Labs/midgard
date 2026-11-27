/** A proof-materials sweep's identity, and the one output it pays
 * (`proof-materials-sweep.ts`). */
import { createHash } from "node:crypto";

import { coreToTxOutput, type TxSigned } from "@lucid-evolution/lucid";

import type { FraudProofWorkflowAction } from "./orchestrator.js";

export const PROOF_MATERIALS_SWEEP =
  "midgard-proof-materials-sweep-v1" as const;
export const PROOF_MATERIALS_SWEEP_STAGE = "proof_materials_sweep" as const;
/** What every sweep's action id starts with. */
export const PROOF_MATERIALS_SWEEP_ACTION_PREFIX = "proof-materials-sweep:";

/**
 * The most materials one sweep spends. An input adds about 36 bytes to the
 * body (a 32-byte transaction id, its index and CBOR framing), so a page of
 * 100, with its one output, fee, declared signer (about 31 bytes), key
 * witness (about 100) and the materials' one `sig(key)` lock (about 35),
 * stays within a quarter of the 16,384-byte maxTxSize. Further materials go
 * in further pages.
 */
export const PROOF_MATERIALS_SWEEP_PAGE_INPUTS = 100;

const OUT_REF = /^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u;

const byOutRef = (left: string, right: string) => {
  const [leftHash, leftIndex] = left.split("#") as [string, string];
  const [rightHash, rightIndex] = right.split("#") as [string, string];
  return leftHash === rightHash
    ? Number(leftIndex) - Number(rightIndex)
    : leftHash < rightHash
      ? -1
      : 1;
};

/** The sweep of exactly the materials `outRefs`. */
export const proofMaterialsSweepAction = ({
  category,
  outRefs,
}: {
  readonly category: string;
  readonly outRefs: readonly string[];
}): FraudProofWorkflowAction => {
  const inputs = [...new Set(outRefs)].sort(byOutRef);
  return Object.freeze({
    actionId: `${PROOF_MATERIALS_SWEEP_ACTION_PREFIX}${createHash("sha256")
      .update(inputs.join(","))
      .digest("hex")}`,
    input: Object.freeze({
      schemaVersion: PROOF_MATERIALS_SWEEP,
      category,
      stage: PROOF_MATERIALS_SWEEP_STAGE,
      inputs: Object.freeze(inputs),
    }),
  });
};

export const isProofMaterialsSweepAction = (
  action: FraudProofWorkflowAction,
): boolean => action.input.stage === PROOF_MATERIALS_SWEEP_STAGE;

/** The materials the sweep `action` spends, checked against its identity. */
export const proofMaterialsSweepInputs = (
  action: FraudProofWorkflowAction,
): readonly string[] => {
  const { schemaVersion, category, inputs } = action.input;
  if (
    schemaVersion !== PROOF_MATERIALS_SWEEP ||
    typeof category !== "string" ||
    !Array.isArray(inputs) ||
    inputs.length === 0 ||
    inputs.length > PROOF_MATERIALS_SWEEP_PAGE_INPUTS ||
    !inputs.every(
      (outRef) => typeof outRef === "string" && OUT_REF.test(outRef),
    ) ||
    Object.keys(action.input).length !== 4
  )
    throw new Error("proof-materials sweep action is not canonical");
  const page = inputs as string[];
  if (
    proofMaterialsSweepAction({ category, outRefs: page }).actionId !==
      action.actionId ||
    [...new Set(page)].sort(byOutRef).join(",") !== page.join(",")
  )
    throw new Error("proof-materials sweep changed its identity");
  return page;
};

/** The single output a sweep pays: a plain coin at the funding address. */
export const assertProofMaterialsSweepOutput = (
  signed: TxSigned,
  fundingAddress: string,
): void => {
  const outputs = signed.toTransaction().body().outputs();
  const output = outputs.len() === 1 ? coreToTxOutput(outputs.get(0)) : null;
  if (
    output === null ||
    output.address !== fundingAddress ||
    output.datum != null ||
    output.datumHash != null ||
    output.scriptRef != null ||
    Object.keys(output.assets).some((unit) => unit !== "lovelace")
  )
    throw new Error(
      "a proof-materials sweep pays one plain coin to the funding address",
    );
};
