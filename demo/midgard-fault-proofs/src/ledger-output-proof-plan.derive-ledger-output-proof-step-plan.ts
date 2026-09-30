import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import { ValidationOneStepWitness } from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  controlData,
  controlFromCarrier,
  datumTraverseItems,
  deriveLedgerOutputProofStepClaims,
  integer,
  items,
  ledgerOutputProofAttestationRoles,
  ledgerOutputProofDatumRoleIndex,
  type LedgerOutputProofStepPlan,
  none,
  optionInner,
} from "./ledger-output-proof-plan.ledger-output-proof-datum-role-index.js";

export const deriveLedgerOutputProofStepPlan = ({
  resolverIndex,
  semanticResolverIndex,
  transitionCbor,
  auxiliaryCbor,
  ledgerOutputProofSuccessorWorkWitnessCbor,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: Uint8Array;
  readonly auxiliaryCbor: Uint8Array;
  readonly ledgerOutputProofSuccessorWorkWitnessCbor?: Uint8Array;
}): LedgerOutputProofStepPlan => {
  if (
    !(
      (resolverIndex === 7 && semanticResolverIndex === 3) ||
      (resolverIndex === 8 && semanticResolverIndex === 2)
    )
  )
    throw new Error(
      "Ledger output proof plan requires its step semantic resolver",
    );
  const transition = Data.from(
    Buffer.from(transitionCbor).toString("hex"),
    ValidationOneStepWitness,
  );
  const controlCbor = controlFromCarrier(
    resolverIndex,
    transition.work_witness_cbor,
  );
  const control = items(controlData(controlCbor), 17, "output proof control");
  const stage = integer(control[1], "output proof stage");
  const auxiliary = Data.from(Buffer.from(auxiliaryCbor).toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 32 ||
    auxiliary.fields.length !== 1
  )
    throw new Error("Ledger output proof requires the exact step auxiliary");
  const witness = auxiliary.fields[0];
  if (!(witness instanceof Constr))
    throw new Error("Output proof witness is malformed");
  let roleIndex: number;
  if (stage === 0n) {
    const scan = items(control[5]!, 23, "output scan");
    const scanStage = integer(scan[1], "scan stage");
    const finished =
      scanStage === 7n ||
      (scanStage === 4n &&
        integer(scan[2], "scan cursor") ===
          integer(control[3], "output length") &&
        integer(scan[4], "optional fields") + 2n ===
          integer(scan[3], "map entries") &&
        integer(scan[18], "payload remaining") === 0n);
    roleIndex = finished ? 13 : scanStage <= 1n ? 0 : scanStage <= 3n ? 11 : 12;
  } else if (stage === 1n) roleIndex = 1;
  else if (stage === 2n || stage === 3n || stage === 4n) {
    // `SpanAttach` routes first at the content stages: the span-attach step
    // (role 23) records the window commitment every later consumer binds to.
    if (witness.index === 5 && witness.fields.length === 4) roleIndex = 23;
    else if (stage === 3n) roleIndex = 8;
    else if (stage === 4n) roleIndex = 9;
    // `NoWitness` at the datum stage is the terminal finish hand-off
    // (`ledger_output_proof_datum.finish`, role 19).
    else if (witness.index === 0 && witness.fields.length === 0) roleIndex = 19;
    else {
      if (
        witness.index !== 3 ||
        witness.fields.length !== 2 ||
        !(witness.fields[0] instanceof Constr)
      )
        throw new Error("Datum output proof witness is malformed");
      roleIndex = ledgerOutputProofDatumRoleIndex(control, witness.fields[0]);
    }
  } else if (stage === 5n) roleIndex = 10;
  else throw new Error("Terminal output proof cannot take a step");
  const claims = deriveLedgerOutputProofStepClaims({ control, roleIndex });
  const attestationRoles = ledgerOutputProofAttestationRoles(roleIndex);
  const successor = transition.claimed_successor;
  if (successor.phase === "Terminal") {
    if (ledgerOutputProofSuccessorWorkWitnessCbor !== undefined)
      throw new Error(
        "Rejected output proof cannot carry successor control bytes",
      );
    return {
      controlCbor,
      nextControlCbor: "",
      roleIndex,
      ...claims,
      attestationRoles,
    };
  }
  const expectedPhase = resolverIndex === 7 ? "ResolveInputs" : "ScriptSources";
  const adjacent = ledgerOutputProofSuccessorWorkWitnessCbor;
  const pc = Number(successor.program_counter);
  if (
    successor.phase !== expectedPhase ||
    adjacent === undefined ||
    !Number.isSafeInteger(pc) ||
    pc < 0 ||
    hashMidgardValidationWorkWitness({
      phase: resolverIndex === 7 ? "resolveInputs" : "scriptSources",
      programCounter: pc,
      witnessCbor: adjacent,
    }).toString("hex") !== successor.work_root
  )
    throw new Error(
      "Output proof successor bytes do not match the frozen successor work root",
    );
  return {
    controlCbor,
    roleIndex,
    ...claims,
    attestationRoles,
    nextControlCbor: controlFromCarrier(
      resolverIndex,
      Buffer.from(adjacent).toString("hex"),
    ),
  };
};

export type LedgerOutputProofFinalizePlan = {
  readonly controlCbor: string;
  /** `DataSummaryV1` on the wire, pinned by the value-summary yield. */
  readonly claimedValueSummary: Data;
  /** `Option<DataSummaryV1>` on the wire, pinned by the datum-summary yield. */
  readonly claimedDatumSummary: Data;
  /**
   * Descriptor roles this finalize-shaped step attaches, exactly
   * `ledger_output_proof_v1.fact_attach_groups_v1`'s first all-missing group;
   * empty exactly at the thin terminal (all four facts recorded).
   */
  readonly attachRoles: readonly number[];
};

/**
 * Fact-commitment slots live at control items 13-16 in role order (role 0 ->
 * item 13). The attach order is `[[2, 3], [0], [1]]`; the first group whose
 * facts are all missing is the group this step attaches.
 */
export const ledgerOutputProofFactAttachRoles = (
  control: readonly Data[],
): readonly number[] => {
  const missing = (role: number): boolean =>
    optionInner(control[13 + role], "fact commitment") === null;
  for (const group of [[2, 3], [0], [1]])
    if (group.every(missing)) return group;
  return [];
};

/**
 * The terminal control and leaf-summary claims of one LOP finalize step. The
 * value claim is the terminal value sub-control's own folded result and the
 * datum claim the terminal datum traversal's result (absent exactly when the
 * output scan pins no datum), so the descriptor yields' pinning checks hold
 * by construction on an honest trace.
 */
export const deriveLedgerOutputProofFinalizePlan = ({
  resolverIndex,
  semanticResolverIndex,
  transitionCbor,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: Uint8Array;
}): LedgerOutputProofFinalizePlan => {
  if (
    !(
      (resolverIndex === 7 && semanticResolverIndex === 4) ||
      (resolverIndex === 8 && semanticResolverIndex === 3)
    )
  )
    throw new Error(
      "Ledger output proof finalize plan requires its finalize semantic resolver",
    );
  const transition = Data.from(
    Buffer.from(transitionCbor).toString("hex"),
    ValidationOneStepWitness,
  );
  const controlCbor = controlFromCarrier(
    resolverIndex,
    transition.work_witness_cbor,
  );
  const control = items(controlData(controlCbor), 17, "output proof control");
  return {
    controlCbor,
    ...deriveLedgerOutputProofFinalizeClaims(control),
    attachRoles: ledgerOutputProofFactAttachRoles(control),
  };
};

/**
 * The leaf-summary claims of a terminal LOP control: the value sub-control's
 * folded result and the datum traversal's result (absent exactly when the
 * output scan pins no datum).
 */
export const deriveLedgerOutputProofFinalizeClaims = (
  control: readonly Data[],
): {
  readonly claimedValueSummary: Data;
  readonly claimedDatumSummary: Data;
} => {
  if (integer(control[1], "output proof stage") !== 6n)
    throw new Error("Finalize requires the terminal output proof control");
  const scan = items(control[5]!, 23, "output scan");
  const valueInner = optionInner(control[6], "value control");
  if (valueInner === null)
    throw new Error("Terminal output proof control must carry a value control");
  const valueResult = optionInner(
    items(valueInner, 7, "value control")[6],
    "value result",
  );
  if (valueResult === null)
    throw new Error("Terminal value control must carry its folded summary");
  const claimedValueSummary: Data = new Constr(0, [
    ...items(valueResult, 3, "value summary"),
  ]);
  const datumOffset = integer(scan[16], "datum offset");
  let claimedDatumSummary: Data;
  if (datumOffset === -1n) claimedDatumSummary = none();
  else {
    const datumResult = optionInner(
      datumTraverseItems(control)[9],
      "datum result",
    );
    if (datumResult === null)
      throw new Error("Terminal datum traversal must carry its folded summary");
    claimedDatumSummary = new Constr(0, [
      new Constr(0, [...items(datumResult, 3, "datum summary")]),
    ]);
  }
  return { claimedValueSummary, claimedDatumSummary };
};
