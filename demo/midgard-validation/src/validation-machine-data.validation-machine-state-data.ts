import type { MidgardValidationMachineState } from "@al-ft/midgard-core";
import { Constr } from "@lucid-evolution/lucid";

import { type ValidationMachineWorkWitness } from "./validation-machine/index.js";
import {
  bytes,
  type ConstructorData,
  int,
  record,
} from "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";

export const validationMachineStateData = (
  state: MidgardValidationMachineState,
): ConstructorData =>
  record([
    int(state.machineVersion),
    bytes(state.eventKeyHash),
    bytes(state.transactionId),
    bytes(state.transactionCommitment),
    bytes(state.validationContextHash),
    new Constr(state.sourceKind === "normal" ? 0 : 1, []),
    bytes(state.priorLedgerRoot),
    new Constr(
      {
        canonicalDecode: 0,
        compactBinding: 1,
        staticLedgerRules: 2,
        inputSets: 3,
        signatures: 4,
        phaseANativeScripts: 5,
        phaseAScriptPreconditions: 6,
        resolveInputs: 7,
        scriptSources: 8,
        nativeScripts: 9,
        scriptIntegrity: 10,
        cek: 11,
        valueAndMint: 12,
        terminal: 13,
      }[state.phase],
      [],
    ),
    int(state.programCounter),
    bytes(state.workRoot),
    state.executionCpu,
    state.executionMemory,
    new Constr(
      state.verdict === "pending" ? 0 : state.verdict === "accepted" ? 1 : 2,
      [],
    ),
    bytes(state.rejectionCodeHash),
  ]);

export const validationOneStepWitnessData = ({
  witness,
  claimedSuccessor,
}: {
  readonly witness: ValidationMachineWorkWitness;
  readonly claimedSuccessor: MidgardValidationMachineState;
}): ConstructorData =>
  record([bytes(witness.cbor), validationMachineStateData(claimedSuccessor)]);
