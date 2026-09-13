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
 * Published role index for one datum-traversal step. Attach (action 5) splits
 * across an integer and a bytes validator; advance (action 0) splits across
 * the integer, bytes, large-constructor, large-fields and close validators —
 * all pinned against the datum traversal sub-control stage the validators
 * read. Every other action has a single physical role.
 */
export const ledgerOutputProofDatumRoleIndex = (
  control: readonly Data[],
  action: Constr<Data>,
): number => {
  const attachRole = (integerRole: number, bytesRole: number): number => {
    const traverseStage = datumTraverseStage(control);
    if (traverseStage === 1n) return integerRole;
    if (traverseStage === 2n) return bytesRole;
    throw new Error(
      "Scalar datum action requires the integer or bytes traversal stage",
    );
  };
  const advanceRole = (): number => {
    const traverseStage = datumTraverseStage(control);
    if (traverseStage === 1n) return 7;
    if (traverseStage === 2n) return 18;
    if (traverseStage === 3n) return 20;
    if (traverseStage === 4n) return 21;
    if (traverseStage === 5n) return 22;
    throw new Error("NoAction datum step requires an active traversal stage");
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
      return attachRole(5, 17);
    case 0:
      return advanceRole();
    default:
      throw new Error("Unknown datum action");
  }
};

const none = (): Data => new Constr(1, []);
const constr = (value: Data | undefined, label: string): Constr<Data> => {
  if (!(value instanceof Constr)) throw new Error(`${label} must be a constr`);
  return value;
};
/** `constr 1 []` -> null, `constr 0 [inner]` -> inner. */
const optionInner = (value: Data | undefined, label: string): Data | null => {
  const wrapper = constr(value, label);
  if (wrapper.index === 1 && wrapper.fields.length === 0) return null;
  if (wrapper.index !== 0 || wrapper.fields.length !== 1)
    throw new Error(`${label} must be an option`);
  return wrapper.fields[0]!;
};
const datumTraverseItems = (control: readonly Data[]): Data[] => {
  const wrapper = constr(control[7], "datum traversal item");
  if (wrapper.index !== 0 || wrapper.fields.length !== 1)
    throw new Error("Datum traversal sub-control is malformed");
  return items(wrapper.fields[0]!, 10, "datum traversal control");
};

// ---------------------------------------------------------------------------
// Scalar-claim transcoding. The claim carries the active scalar sub-control
// as the Aiken-typed upcast (single-constructor records, `constr 0 [...]`),
// while the control wire stores the canonical `control_data_v1` list
// encoding; the scalar attestation yields compare the claim against the
// typed upcast of the pinned control's decode, so the planner transcodes
// list wire -> constr claim without touching any scalar value.
// ---------------------------------------------------------------------------

const mapOption = (
  value: Data | undefined,
  label: string,
  transform: (inner: Data) => Data,
): Data => {
  const inner = optionInner(value, label);
  return inner === null ? none() : new Constr(0, [transform(inner)]);
};

const blake256ClaimData = (wire: Data): Data =>
  new Constr(0, [...items(wire, 9, "blake2b-256 control")]);

const frontierClaimData = (wire: Data): Data => {
  const fields = items(wire, 4, "blob frontier");
  if (integer(fields[0], "blob frontier version") !== 1n)
    throw new Error("Blob frontier version must be 1");
  const peaksData = fields[3];
  if (!Array.isArray(peaksData))
    throw new Error("Blob frontier peaks must be a list");
  return new Constr(0, [
    fields[1]!,
    fields[2]!,
    peaksData.map(
      (peak) => new Constr(0, [...items(peak, 3, "blob frontier peak")]),
    ),
  ]);
};

const blobClaimData = (wire: Data): Data => {
  const fields = items(wire, 6, "source blob control");
  return new Constr(0, [
    fields[0]!,
    fields[1]!,
    fields[2]!,
    fields[3]!,
    frontierClaimData(fields[4]!),
    mapOption(fields[5], "source blob active hash", blake256ClaimData),
  ]);
};

const scalarSubControlClaimData = (wire: Data, label: string): Data => {
  const fields = items(wire, 6, label);
  return new Constr(0, [
    fields[0]!,
    fields[1]!,
    fields[2]!,
    fields[3]!,
    fields[4]!,
    mapOption(fields[5], `${label} blob`, blobClaimData),
  ]);
};

/**
 * The scalar claim of the shared step action: `constr 0 [source_start,
 * source_length, offset, frame_root, scalar]`, exactly
 * `ledger_output_proof_datum.scalar_claim_data` on the pinned control's
 * active scalar sub-control.
 */
const scalarClaim = (
  control: readonly Data[],
  family: "integer" | "bytes",
): Data => {
  const fields = datumTraverseItems(control);
  const slot = family === "integer" ? 7 : 8;
  const inner = optionInner(fields[slot], `traversal ${family} control`);
  if (inner === null)
    throw new Error(
      `Scalar claim requires an active ${family} sub-control in the datum traversal`,
    );
  return new Constr(0, [
    fields[2]!,
    fields[3]!,
    fields[4]!,
    fields[5]!,
    scalarSubControlClaimData(inner, `traversal ${family} control`),
  ]);
};

export type LedgerOutputProofAttestation = "scalarInteger" | "scalarBytes";

/**
 * The attestation yields one stage role requires, in the exact order of
 * `ledger_output_proof_roles.stage_attestation_roles`; the dispatcher's
 * `yield_ref_input_indices` lists the stage yield first and then these.
 */
export const ledgerOutputProofAttestationRoles = (
  roleIndex: number,
): readonly LedgerOutputProofAttestation[] => {
  if (roleIndex === 5 || roleIndex === 7) return ["scalarInteger"];
  if (roleIndex === 17 || roleIndex === 18) return ["scalarBytes"];
  if (Number.isInteger(roleIndex) && roleIndex >= 0 && roleIndex < 24)
    return [];
  throw new Error("Unknown ledger output proof stage role");
};

/**
 * The scalar claim of one shared LOP step, exactly the value the scalar
 * attestation yield of `roleIndex` verifies. Span claims are gone: the span
 * window is attested once by the span-attach step (role 23) and every later
 * consumer binds its redeemer bytes to the recorded window commitment
 * inline.
 */
export const deriveLedgerOutputProofStepClaims = ({
  control,
  roleIndex,
}: {
  readonly control: readonly Data[];
  readonly roleIndex: number;
}): { readonly claimedScalar: Data } => {
  switch (roleIndex) {
    case 5:
    case 7:
      return { claimedScalar: scalarClaim(control, "integer") };
    case 17:
    case 18:
      return { claimedScalar: scalarClaim(control, "bytes") };
    default:
      if (!(Number.isInteger(roleIndex) && roleIndex >= 0 && roleIndex < 24))
        throw new Error("Unknown ledger output proof stage role");
      return { claimedScalar: none() };
  }
};

export type LedgerOutputProofStepPlan = {
  readonly controlCbor: string;
  readonly nextControlCbor: string;
  readonly roleIndex: number;
  readonly claimedScalar: Data;
  readonly attestationRoles: readonly LedgerOutputProofAttestation[];
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
