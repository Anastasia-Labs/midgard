import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import { ValidationOneStepWitness } from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

const items = (value: Data, count: number, label: string): Data[] => {
  if (!Array.isArray(value) || value.length !== count)
    throw new Error(`${label} must contain exactly ${count.toString()} fields`);
  return value;
};
const bytes = (value: Data | undefined, label: string): string => {
  if (typeof value !== "string") throw new Error(`${label} must be bytes`);
  return value;
};
const integer = (value: Data | undefined, label: string): bigint => {
  if (typeof value !== "bigint") throw new Error(`${label} must be an integer`);
  return value;
};
const controlData = (cbor: string): Data =>
  Data.from(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor));
const controlFromCarrier = (
  resolverIndex: number,
  workCbor: string,
): string => {
  const carrier = items(
    controlData(workCbor),
    resolverIndex === 7 ? 11 : 31,
    "LOP carrier",
  );
  if (resolverIndex === 8) return bytes(carrier[30], "output proof");
  const pending = items(
    controlData(bytes(carrier[9], "pending input")),
    5,
    "pending input",
  );
  return bytes(pending[4], "input output proof");
};

/**
 * Traversal stage of the datum sub-control carried at LOP control item 7, as
 * `scalar_control` in `lib/midgard/ledger-output-proof-datum.ak` reads it: the
 * item is `Constr(0, [[version, stage, ..8 more]])` and the on-chain scalar
 * actions pin `stage` against `cek_data_traverse_v1.stage_integer` (1) or
 * `stage_bytes` (2). The action constructor alone does not discriminate the
 * two, so the planner must read the same field the validators pin.
 */
const datumTraverseStage = (control: readonly Data[]): bigint => {
  const wrapper = control[7];
  if (
    !(wrapper instanceof Constr) ||
    wrapper.index !== 0 ||
    wrapper.fields.length !== 1
  )
    throw new Error("Datum traversal sub-control is malformed");
  return integer(
    items(wrapper.fields[0]!, 10, "datum traversal control")[1],
    "datum traversal stage",
  );
};

/**
 * Published role index for one datum-traversal step. Attach (action 5) and
 * advance (action 0) each split across an integer and a bytes validator; every
 * other action has a single physical role.
 */
export const ledgerOutputProofDatumRoleIndex = (
  control: readonly Data[],
  action: Constr<Data>,
): number => {
  const scalarRole = (integerRole: number, bytesRole: number): number => {
    const traverseStage = datumTraverseStage(control);
    if (traverseStage === 1n) return integerRole;
    if (traverseStage === 2n) return bytesRole;
    throw new Error(
      "Scalar datum action requires the integer or bytes traversal stage",
    );
  };
  switch (action.index) {
    case 7:
      return 2;
    case 8:
      return 3;
    case 1:
      return 4;
    case 2:
      return 14;
    case 3:
      return 15;
    case 4:
      return 16;
    case 6:
      return 6;
    case 5:
      return scalarRole(5, 17);
    case 0:
      return scalarRole(7, 18);
    default:
      throw new Error("Unknown datum action");
  }
};

export type LedgerOutputProofStepPlan = {
  readonly controlCbor: string;
  readonly nextControlCbor: string;
  readonly roleIndex: number;
};

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
  const control = items(controlData(controlCbor), 12, "output proof control");
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
  else if (stage === 2n) {
    if (witness.index === 0 && witness.fields.length === 0) roleIndex = 7;
    else {
      if (
        witness.index !== 3 ||
        witness.fields.length !== 3 ||
        !(witness.fields[0] instanceof Constr)
      )
        throw new Error("Datum output proof witness is malformed");
      roleIndex = ledgerOutputProofDatumRoleIndex(control, witness.fields[0]);
    }
  } else if (stage === 3n) roleIndex = 8;
  else if (stage === 4n) roleIndex = 9;
  else if (stage === 5n) roleIndex = 10;
  else throw new Error("Terminal output proof cannot take a step");
  const successor = transition.claimed_successor;
  if (successor.phase === "Terminal") {
    if (ledgerOutputProofSuccessorWorkWitnessCbor !== undefined)
      throw new Error(
        "Rejected output proof cannot carry successor control bytes",
      );
    return { controlCbor, nextControlCbor: "", roleIndex };
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
    nextControlCbor: controlFromCarrier(
      resolverIndex,
      Buffer.from(adjacent).toString("hex"),
    ),
  };
};
