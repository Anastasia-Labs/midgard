import type { ValidationAuxiliaryWitness } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  UnusedRedeemerAuthenticatedControlSchema,
  UnusedRedeemerAuthenticatedDescriptorSchema,
  UnusedRedeemerAuthenticatedItemHeaderSchema,
  UnusedRedeemerAuthenticatedSchema,
  UnusedRedeemerScriptSourcesControlSchema,
  UnusedRedeemerStep02aRedeemerSchema,
  UnusedRedeemerStep02RedeemerSchema,
} from "./schemas-v1.js";

type Step02aContinue = Extract<
  Data.Static<typeof UnusedRedeemerStep02aRedeemerSchema>,
  { Continue: unknown }
>["Continue"][0];

export type UnusedRedeemerAuthentication = Readonly<{
  traceMembership: Omit<
    Extract<
      Data.Static<typeof UnusedRedeemerStep02RedeemerSchema>,
      { Continue: unknown }
    >["Continue"][0],
    "input_index" | "output_index"
  >["trace_membership"];
  machineState: Step02aContinue["machine_state"];
  traceProof: Step02aContinue["trace_proof"];
  control: Data.Static<typeof UnusedRedeemerScriptSourcesControlSchema>;
  itemControl: Extract<
    ValidationAuxiliaryWitness,
    { RedeemerItemStepWitness: unknown }
  >["RedeemerItemStepWitness"]["control"];
  headerChunkProof: unknown;
  headerNextChunkProof: unknown | null;
  tailChunkProof: unknown;
  tailNextChunkProof: unknown | null;
  descriptorState: Data.Static<
    typeof UnusedRedeemerAuthenticatedDescriptorSchema
  >;
  controlState: Data.Static<typeof UnusedRedeemerAuthenticatedControlSchema>;
  headerState: Data.Static<typeof UnusedRedeemerAuthenticatedItemHeaderSchema>;
  authenticatedState: Data.Static<typeof UnusedRedeemerAuthenticatedSchema>;
}>;
