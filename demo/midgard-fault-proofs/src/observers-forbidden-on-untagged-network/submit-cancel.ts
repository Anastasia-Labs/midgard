import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { ObserversForbiddenContracts } from "./contracts.js";

export const submitObserversForbiddenCancel = async (args: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) =>
  await submitLinearFaultCancel({
    ...args,
    family: "observers-forbidden-on-untagged-network",
    steps: args.contracts.steps,
    computationThread: args.contracts.computationThread,
  });
