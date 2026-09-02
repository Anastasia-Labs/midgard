import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { ZeroInputContractsV1 } from "./contracts-v1.js";

export const submitZeroInputCancelV1 = async (args: {
  readonly lucid: LucidEvolution;
  readonly contracts: ZeroInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
}) =>
  await submitLinearFaultCancelV1({
    ...args,
    family: "zero-input",
    steps: args.contracts.steps,
    computationThread: args.contracts.computationThread,
  });
