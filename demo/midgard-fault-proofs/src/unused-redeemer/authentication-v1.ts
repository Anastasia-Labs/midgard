import type { ValidationAuxiliaryWitnessV1 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  UnusedRedeemerAuthenticatedControlV1Schema,
  UnusedRedeemerAuthenticatedDescriptorV1Schema,
  UnusedRedeemerAuthenticatedItemHeaderV1Schema,
  UnusedRedeemerAuthenticatedV1Schema,
  UnusedRedeemerScriptSourcesControlV1Schema,
  UnusedRedeemerStep02aRedeemerV1Schema,
  UnusedRedeemerStep02RedeemerV1Schema,
} from "./schemas-v1.js";

type Step02aContinue = Extract<
  Data.Static<typeof UnusedRedeemerStep02aRedeemerV1Schema>,
  { Continue: unknown }
>["Continue"][0];

export type UnusedRedeemerAuthenticationV1 = Readonly<{
  traceMembership: Omit<
    Extract<
      Data.Static<typeof UnusedRedeemerStep02RedeemerV1Schema>,
      { Continue: unknown }
    >["Continue"][0],
    "input_index" | "output_index"
  >["trace_membership"];
  machineState: Step02aContinue["machine_state"];
  traceProof: Step02aContinue["trace_proof"];
  control: Data.Static<typeof UnusedRedeemerScriptSourcesControlV1Schema>;
  itemControl: Extract<
    ValidationAuxiliaryWitnessV1,
    { RedeemerItemStepWitness: unknown }
  >["RedeemerItemStepWitness"]["control"];
  headerChunkProof: unknown;
  headerNextChunkProof: unknown | null;
  tailChunkProof: unknown;
  tailNextChunkProof: unknown | null;
  descriptorState: Data.Static<
    typeof UnusedRedeemerAuthenticatedDescriptorV1Schema
  >;
  controlState: Data.Static<typeof UnusedRedeemerAuthenticatedControlV1Schema>;
  headerState: Data.Static<
    typeof UnusedRedeemerAuthenticatedItemHeaderV1Schema
  >;
  authenticatedState: Data.Static<typeof UnusedRedeemerAuthenticatedV1Schema>;
}>;
