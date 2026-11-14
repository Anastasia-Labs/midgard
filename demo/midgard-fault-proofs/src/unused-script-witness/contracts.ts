/** The `unusedScriptWitness` direct walk family the submitters and prover
 * read. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  DirectPurposeContracts,
  DirectPurposeCustodyFamily,
  DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL = "unused-script-witness";

/** Named roles; fails closed when the chain is not the 24-role graph. */
export const unusedScriptWitnessContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.UnusedScriptWitnessRoles =>
  SDK.unusedScriptWitnessRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const UNUSED_SCRIPT_WITNESS_FAMILY: DirectPurposeCustodyFamily = {
  label: UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
  roleOrder: SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER,
  roles: unusedScriptWitnessContractRoles,
};

/** Twin of `terminal.convicts_v1` over the completed witness walk and the
 * selection the terminal is given. */
const unusedScriptWitnessTerminalConvicts = (
  state: SDK.UnusedScriptWitnessWalkState,
  selection: SDK.UnusedScriptWitnessSelection | null,
): boolean => {
  const hash = state.witness_hash;
  const unused = SDK.unusedScriptWitnessIsUnused(
    hash === null || selection === null
      ? null
      : {
          selected:
            selection.source.field_index === 6n &&
            selection.source.item_index === state.script_index,
          reached:
            state.result.facts.probe === hash &&
            state.result.facts.probe_reached,
        },
  );
  return SDK.unusedScriptWitnessConvicts(
    state.result.bound.subject,
    state.result.bound.network_order_context,
    state.script_index,
    unused,
  );
};

/** The family as the kit's terminal submits it with `selection`: the
 * completed walk convicts against the selected witness, and the finalize
 * carries the selection. */
export const unusedScriptWitnessTerminalFamily = (
  selection: SDK.UnusedScriptWitnessSelection | null,
): DirectPurposeFamily<SDK.UnusedScriptWitnessWalkState> => ({
  ...UNUSED_SCRIPT_WITNESS_FAMILY,
  verdict: {
    schema: SDK.UnusedScriptWitnessWalkDatum,
    convicts: (state) => unusedScriptWitnessTerminalConvicts(state, selection),
    finalize: (indices) =>
      Data.to(
        { Continue: [{ ...indices, selection }] },
        SDK.UnusedScriptWitnessFinalizeRedeemer,
      ),
  },
});

export type UnusedScriptWitnessRole = SDK.UnusedScriptWitnessRole;
