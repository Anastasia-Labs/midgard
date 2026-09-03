import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { ZeroInputContracts } from "./contracts-v1.js";

export const submitZeroInputCancel = async (args: {
  readonly lucid: LucidEvolution;
  readonly contracts: ZeroInputContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) =>
  await submitLinearFaultCancel({
    ...args,
    family: "zero-input",
    steps: args.contracts.steps,
    computationThread: args.contracts.computationThread,
  });
