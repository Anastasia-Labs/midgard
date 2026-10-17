import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

export const items = (value: Data, count: number, label: string): Data[] => {
  if (!Array.isArray(value) || value.length !== count)
    throw new Error(`${label} must contain exactly ${count.toString()} fields`);
  return value;
};

export const bytes = (value: Data | undefined, label: string): string => {
  if (typeof value !== "string") throw new Error(`${label} must be bytes`);
  return value;
};

export const integer = (value: Data | undefined, label: string): bigint => {
  if (typeof value !== "bigint") throw new Error(`${label} must be an integer`);
  return value;
};

export const controlData = (cbor: string): Data =>
  Data.from(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor));

export const controlFromCarrier = (
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
 * item is `Constr(0, [[version, stage, ..7 more]])` and the on-chain scalar
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
    items(wrapper.fields[0]!, 9, "datum traversal control")[1],
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

export const none = (): Data => new Constr(1, []);

const constr = (value: Data | undefined, label: string): Constr<Data> => {
  if (!(value instanceof Constr)) throw new Error(`${label} must be a constr`);
  return value;
};

/** `constr 1 []` -> null, `constr 0 [inner]` -> inner. */
export const optionInner = (
  value: Data | undefined,
  label: string,
): Data | null => {
  const wrapper = constr(value, label);
  if (wrapper.index === 1 && wrapper.fields.length === 0) return null;
  if (wrapper.index !== 0 || wrapper.fields.length !== 1)
    throw new Error(`${label} must be an option`);
  return wrapper.fields[0]!;
};

export const datumTraverseItems = (control: readonly Data[]): Data[] => {
  const wrapper = constr(control[7], "datum traversal item");
  if (wrapper.index !== 0 || wrapper.fields.length !== 1)
    throw new Error("Datum traversal sub-control is malformed");
  return items(wrapper.fields[0]!, 9, "datum traversal control");
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
  const slot = family === "integer" ? 6 : 7;
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
