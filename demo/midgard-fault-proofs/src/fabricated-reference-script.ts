import { type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import { outRefLabel, requireMatchingScriptHash } from "./runtime.js";

export const requireFabricatedReferenceScript = ({
  utxo,
  expectedScriptHash,
  categoryLabel,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly categoryLabel: string;
  readonly stepIndex: 0 | 1 | 2 | 3;
}): UTxO => {
  const stepLabel = `${categoryLabel} step 0${(stepIndex + 1).toString()}`;
  if (utxo.scriptRef == null) {
    throw new Error(
      `${stepLabel} reference UTxO ${outRefLabel(utxo)} carries no reference script.`,
    );
  }
  requireMatchingScriptHash({
    label: `${stepLabel} reference script at ${outRefLabel(utxo)}`,
    deployed: expectedScriptHash,
    derived: validatorToScriptHash(utxo.scriptRef),
  });
  return utxo;
};
