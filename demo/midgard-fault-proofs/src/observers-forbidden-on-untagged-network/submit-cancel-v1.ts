import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { ObserversForbiddenContractsV1 } from "./contracts-v1.js";

export const submitObserversForbiddenCancelV1 = async (args: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
}) =>
  await submitLinearFaultCancelV1({
    ...args,
    family: "observers-forbidden-on-untagged-network",
    steps: args.contracts.steps,
    computationThread: args.contracts.computationThread,
  });
