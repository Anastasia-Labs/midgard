import {
  type MidgardBlake2b256TraceControl,
  type MidgardCekBlobFrontier,
  type MidgardCekDataBytesControl,
  type MidgardCekDataIntegerControl,
  type MidgardCekDataSummary,
  type MidgardCekDataTraverseControl,
  type MidgardCekSourceBlobControl,
  nextMidgardCekDataTraverseSpan,
} from "@al-ft/midgard-core";
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
const count = (value: Data | undefined, label: string): number => {
  const raw = integer(value, label);
  const exact = Number(raw);
  if (!Number.isSafeInteger(exact))
    throw new Error(`${label} must be a safe integer`);
  return exact;
};
const buffer = (value: Data | undefined, label: string): Buffer =>
  Buffer.from(bytes(value, label), "hex");

// ---------------------------------------------------------------------------
// Wire decoders for the datum-traversal sub-controls. The wire layouts are
// the canonical `control_data_v1` list encodings of the on-chain machines,
// byte-identical to the `encodeMidgard*Control` producers in midgard-core;
// the typed records feed midgard-core's own `next*Span` planners so the
// planner claims exactly the span the stage yield's control demands.
// ---------------------------------------------------------------------------

const blake256FromWire = (
  value: Data,
  label: string,
): MidgardBlake2b256TraceControl => {
  const fields = items(value, 9, label);
  if (integer(fields[0], `${label} version`) !== 1n)
    throw new Error(`${label} version must be 1`);
  return {
    version: 1,
    stage: count(
      fields[1],
      `${label} stage`,
    ) as MidgardBlake2b256TraceControl["stage"],
    cursor: count(fields[2], `${label} cursor`),
    totalLength: count(fields[3], `${label} total length`),
    chainingValue: buffer(fields[4], `${label} chaining value`),
    activeBlock: buffer(fields[5], `${label} active block`),
    activeBlockLength: count(fields[6], `${label} active block length`),
    workingValue: buffer(fields[7], `${label} working value`),
    round: count(fields[8], `${label} round`),
  };
};

const frontierFromWire = (
  value: Data,
  label: string,
): MidgardCekBlobFrontier => {
  const fields = items(value, 4, label);
  if (integer(fields[0], `${label} version`) !== 1n)
    throw new Error(`${label} version must be 1`);
  const peaksData = fields[3];
  if (!Array.isArray(peaksData))
    throw new Error(`${label} peaks must be a list`);
  return {
    count: count(fields[1], `${label} count`),
    byteLength: integer(fields[2], `${label} byte length`),
    peaks: peaksData.map((peak) => {
      const parts = items(peak, 3, `${label} peak`);
      return {
        height: count(parts[0], `${label} peak height`),
        root: buffer(parts[1], `${label} peak root`),
        byteLength: integer(parts[2], `${label} peak byte length`),
      };
    }),
  };
};

const blobFromWire = (
  value: Data,
  label: string,
): MidgardCekSourceBlobControl => {
  const fields = items(value, 6, label);
  if (integer(fields[0], `${label} version`) !== 1n)
    throw new Error(`${label} version must be 1`);
  const activeHash = optionInner(fields[5], `${label} active hash`);
  return {
    version: 1,
    stage: count(
      fields[1],
      `${label} stage`,
    ) as MidgardCekSourceBlobControl["stage"],
    sourceStart: count(fields[2], `${label} source start`),
    sourceLength: count(fields[3], `${label} source length`),
    frontier: frontierFromWire(fields[4]!, `${label} frontier`),
    activeHash:
      activeHash === null
        ? null
        : blake256FromWire(activeHash, `${label} active hash`),
  };
};

const integerControlFromWire = (
  value: Data,
  label: string,
): MidgardCekDataIntegerControl => {
  const fields = items(value, 6, label);
  if (integer(fields[0], `${label} version`) !== 1n)
    throw new Error(`${label} version must be 1`);
  const blob = optionInner(fields[5], `${label} blob`);
  return {
    version: 1,
    stage: count(
      fields[1],
      `${label} stage`,
    ) as MidgardCekDataIntegerControl["stage"],
    sourceStart: count(fields[2], `${label} source start`),
    sourceLength: count(fields[3], `${label} source length`),
    memory: integer(fields[4], `${label} memory`),
    blob: blob === null ? null : blobFromWire(blob, `${label} blob`),
  };
};

const bytesControlFromWire = (
  value: Data,
  label: string,
): MidgardCekDataBytesControl => {
  const fields = items(value, 6, label);
  if (integer(fields[0], `${label} version`) !== 1n)
    throw new Error(`${label} version must be 1`);
  const blob = optionInner(fields[5], `${label} blob`);
  return {
    version: 1,
    stage: count(
      fields[1],
      `${label} stage`,
    ) as MidgardCekDataBytesControl["stage"],
    sourceStart: count(fields[2], `${label} source start`),
    sourceLength: count(fields[3], `${label} source length`),
    bytesLength: count(fields[4], `${label} bytes length`),
    blob: blob === null ? null : blobFromWire(blob, `${label} blob`),
  };
};

const summaryFromWire = (value: Data, label: string): MidgardCekDataSummary => {
  const fields = items(value, 3, label);
  return {
    root: buffer(fields[0], `${label} root`),
    cborLength: integer(fields[1], `${label} cbor length`),
    memory: integer(fields[2], `${label} memory`),
  };
};

const datumTraverseItems = (control: readonly Data[]): Data[] => {
  const wrapper = constr(control[7], "datum traversal item");
  if (wrapper.index !== 0 || wrapper.fields.length !== 1)
    throw new Error("Datum traversal sub-control is malformed");
  return items(wrapper.fields[0]!, 10, "datum traversal control");
};

const traverseControlFromWire = (
  control: readonly Data[],
): MidgardCekDataTraverseControl => {
  const fields = datumTraverseItems(control);
  if (integer(fields[0], "traversal version") !== 1n)
    throw new Error("Datum traversal control version must be 1");
  const pending = optionInner(fields[6], "traversal pending children");
  const integerControl = optionInner(fields[7], "traversal integer control");
  const bytesControl = optionInner(fields[8], "traversal bytes control");
  const result = optionInner(fields[9], "traversal result");
  return {
    version: 1,
    stage: count(
      fields[1],
      "traversal stage",
    ) as MidgardCekDataTraverseControl["stage"],
    sourceStart: count(fields[2], "traversal source start"),
    sourceLength: count(fields[3], "traversal source length"),
    offset: count(fields[4], "traversal offset"),
    frameRoot: buffer(fields[5], "traversal frame root"),
    pendingLargeExpectedChildren:
      pending === null ? null : count(pending, "traversal pending children"),
    integer:
      integerControl === null
        ? null
        : integerControlFromWire(integerControl, "traversal integer control"),
    bytes:
      bytesControl === null
        ? null
        : bytesControlFromWire(bytesControl, "traversal bytes control"),
    result:
      result === null ? null : summaryFromWire(result, "traversal result"),
  };
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

// ---------------------------------------------------------------------------
// Span claims. Sliced mode (`constr 0 [start, length]`) for the chunk-cursor
// stages; content mode (`constr 2 [start, length, bytes]`) for the
// datum-traversal stages, whose bytes the planner slices out of the shared
// witness chunk proofs exactly as `output_span_slice_raw_v1` does.
// ---------------------------------------------------------------------------

const CHUNK_BYTES = 4095n;

const chunkProofBytes = (
  value: Data | undefined,
  label: string,
): { readonly chunkIndex: bigint; readonly chunk: string } => {
  const proof = constr(value, label);
  if (proof.index !== 0 || proof.fields.length !== 8)
    throw new Error(`${label} must be a chunk proof`);
  return {
    chunkIndex: integer(proof.fields[4], `${label} chunk index`),
    chunk: bytes(proof.fields[5], `${label} chunk`),
  };
};

const witnessChunks = (
  witness: Constr<Data>,
): {
  readonly first: {
    readonly chunkIndex: bigint;
    readonly chunk: string;
  } | null;
  readonly next: { readonly chunkIndex: bigint; readonly chunk: string } | null;
} => {
  if (witness.index === 1 && witness.fields.length === 2) {
    const nextInner = optionInner(witness.fields[1], "next chunk proof");
    return {
      first: chunkProofBytes(witness.fields[0], "chunk proof"),
      next:
        nextInner === null
          ? null
          : chunkProofBytes(nextInner, "next chunk proof"),
    };
  }
  if (witness.index === 3 && witness.fields.length === 3) {
    const firstInner = optionInner(witness.fields[1], "datum chunk proof");
    const nextInner = optionInner(witness.fields[2], "next datum chunk proof");
    return {
      first:
        firstInner === null
          ? null
          : chunkProofBytes(firstInner, "datum chunk proof"),
      next:
        nextInner === null
          ? null
          : chunkProofBytes(nextInner, "next datum chunk proof"),
    };
  }
  return { first: null, next: null };
};

/** `output_span_slice_raw_v1` over the witness chunk proofs, as hex. */
const sliceWitnessSpan = (
  totalLength: bigint,
  start: bigint,
  length: bigint,
  witness: Constr<Data>,
): string => {
  if (length <= 0n || length > CHUNK_BYTES)
    throw new Error("Output span length is out of range");
  if (start < 0n || start + length > totalLength)
    throw new Error("Output span exceeds the output bytes");
  const firstIndex = start / CHUNK_BYTES;
  const lastIndex = (start + length - 1n) / CHUNK_BYTES;
  if (lastIndex > firstIndex + 1n)
    throw new Error("Output span crosses more than two chunks");
  const { first, next } = witnessChunks(witness);
  if (first === null)
    throw new Error("Output span requires a witness chunk proof");
  if (first.chunkIndex !== firstIndex)
    throw new Error("Witness chunk proof does not cover the span start");
  const localStart = Number(start - firstIndex * CHUNK_BYTES);
  const spanLength = Number(length);
  if (lastIndex === firstIndex) {
    if (next !== null)
      throw new Error("Single-chunk span must carry exactly one chunk proof");
    return first.chunk.slice(localStart * 2, (localStart + spanLength) * 2);
  }
  if (next === null)
    throw new Error("Chunk-crossing span requires the next chunk proof");
  if (next.chunkIndex !== firstIndex + 1n)
    throw new Error("Witness chunk proofs are not adjacent");
  return (first.chunk + next.chunk).slice(
    localStart * 2,
    (localStart + spanLength) * 2,
  );
};

/**
 * Content-mode span claim for a datum-traversal stage: the exact source span
 * midgard-core's traverse planner demands next, with its bytes sliced out of
 * the shared witness chunk proofs. `required` marks the roles whose stage
 * always reads a span; the streaming roles claim no span on the steps that
 * consume no source bytes.
 */
const contentSpanClaim = (
  control: readonly Data[],
  witness: Constr<Data>,
  required: boolean,
): Data => {
  const traverse = traverseControlFromWire(control);
  const span = nextMidgardCekDataTraverseSpan(traverse);
  if (span === null) {
    if (required)
      throw new Error("Datum traversal stage demands a source span");
    return none();
  }
  return new Constr(2, [
    BigInt(span.absoluteStart),
    BigInt(span.length),
    sliceWitnessSpan(
      integer(control[3], "output length"),
      BigInt(span.absoluteStart),
      BigInt(span.length),
      witness,
    ),
  ]);
};

/** Sliced-mode span claim of the reference-script commitment stage. */
const referenceScriptSpanClaim = (control: readonly Data[]): Data => {
  const scan = items(control[5]!, 23, "output scan");
  const totalLength = integer(control[3], "output length");
  const itemOffset = integer(scan[20], "reference script item offset");
  const itemLength = totalLength - itemOffset;
  if (itemLength <= 0n)
    throw new Error("Reference script item must be non-empty");
  const chunkCount = (itemLength + CHUNK_BYTES - 1n) / CHUNK_BYTES;
  const chunkIndex = integer(control[8], "reference script chunk index");
  if (chunkIndex < 0n || chunkIndex > chunkCount)
    throw new Error("Reference script chunk index is out of range");
  if (chunkIndex === chunkCount) return none();
  const length =
    chunkIndex + 1n < chunkCount
      ? CHUNK_BYTES
      : itemLength - chunkIndex * CHUNK_BYTES;
  return new Constr(0, [itemOffset + chunkIndex * CHUNK_BYTES, length]);
};

/** Sliced-mode span claim of the script-hash trace stage. */
const scriptHashSpanClaim = (control: readonly Data[]): Data => {
  const scan = items(control[5]!, 23, "output scan");
  const inner = optionInner(control[10], "script hash control");
  if (inner === null)
    throw new Error("Script hash stage requires an active trace control");
  const trace = items(inner, 9, "blake2b-224 control");
  if (integer(trace[1], "script hash stage") !== 0n) return none();
  const cursor = integer(trace[2], "script hash cursor");
  const totalLength = integer(trace[3], "script hash total length");
  const remaining = totalLength - cursor;
  const expected = remaining < 128n ? remaining : 128n;
  const includesLanguage = cursor === 0n;
  const contentLength = expected - (includesLanguage ? 1n : 0n);
  if (contentLength === 0n) return none();
  const start =
    integer(scan[21], "reference script offset") +
    cursor -
    (includesLanguage ? 0n : 1n);
  return new Constr(0, [start, contentLength]);
};

export type LedgerOutputProofAttestation =
  | "span"
  | "scalarInteger"
  | "scalarBytes";

/**
 * The attestation yields one stage role requires, in the exact order of
 * `ledger_output_proof_roles.stage_attestation_roles`; the dispatcher's
 * `yield_ref_input_indices` lists the stage yield first and then these.
 */
export const ledgerOutputProofAttestationRoles = (
  roleIndex: number,
): readonly LedgerOutputProofAttestation[] => {
  if (
    roleIndex === 4 ||
    roleIndex === 8 ||
    roleIndex === 9 ||
    (roleIndex >= 14 && roleIndex <= 16) ||
    (roleIndex >= 20 && roleIndex <= 22)
  )
    return ["span"];
  if (roleIndex === 5) return ["scalarInteger"];
  if (roleIndex === 17) return ["scalarBytes"];
  if (roleIndex === 7) return ["span", "scalarInteger"];
  if (roleIndex === 18) return ["span", "scalarBytes"];
  if (roleIndex >= 0 && roleIndex < 23) return [];
  throw new Error("Unknown ledger output proof stage role");
};

/**
 * The span and scalar claims of one shared LOP step, exactly the values the
 * stage yield of `roleIndex` demands and its attestation yields verify.
 */
export const deriveLedgerOutputProofStepClaims = ({
  control,
  witness,
  roleIndex,
}: {
  readonly control: readonly Data[];
  readonly witness: Constr<Data>;
  readonly roleIndex: number;
}): { readonly claimedSpan: Data; readonly claimedScalar: Data } => {
  switch (roleIndex) {
    case 4:
    case 14:
    case 15:
    case 16:
    case 21:
    case 22:
      return {
        claimedSpan: contentSpanClaim(control, witness, true),
        claimedScalar: none(),
      };
    case 20:
      return {
        claimedSpan: contentSpanClaim(control, witness, false),
        claimedScalar: none(),
      };
    case 5:
      return {
        claimedSpan: none(),
        claimedScalar: scalarClaim(control, "integer"),
      };
    case 17:
      return {
        claimedSpan: none(),
        claimedScalar: scalarClaim(control, "bytes"),
      };
    case 7:
      return {
        claimedSpan: contentSpanClaim(control, witness, false),
        claimedScalar: scalarClaim(control, "integer"),
      };
    case 18:
      return {
        claimedSpan: contentSpanClaim(control, witness, false),
        claimedScalar: scalarClaim(control, "bytes"),
      };
    case 8:
      return {
        claimedSpan: referenceScriptSpanClaim(control),
        claimedScalar: none(),
      };
    case 9:
      return {
        claimedSpan: scriptHashSpanClaim(control),
        claimedScalar: none(),
      };
    default:
      if (!(Number.isInteger(roleIndex) && roleIndex >= 0 && roleIndex < 23))
        throw new Error("Unknown ledger output proof stage role");
      return { claimedSpan: none(), claimedScalar: none() };
  }
};

export type LedgerOutputProofStepPlan = {
  readonly controlCbor: string;
  readonly nextControlCbor: string;
  readonly roleIndex: number;
  readonly claimedSpan: Data;
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
    // `NoWitness` at the datum stage is the terminal finish hand-off
    // (`ledger_output_proof_datum.finish`, role 19).
    if (witness.index === 0 && witness.fields.length === 0) roleIndex = 19;
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
  const claims = deriveLedgerOutputProofStepClaims({
    control,
    witness,
    roleIndex,
  });
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
  const control = items(controlData(controlCbor), 12, "output proof control");
  return { controlCbor, ...deriveLedgerOutputProofFinalizeClaims(control) };
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
