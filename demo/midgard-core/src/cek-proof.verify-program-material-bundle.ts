import {
  encodeMidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialEntry,
} from "./cek-proof.decode-midgard-cek-program-envelope.js";
import {
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekProgramMaterialDaValue,
} from "./cek-proof.decode-midgard-cek-program-material-da-entry.js";
import { type MidgardCekProgramEnvelope } from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import {
  exactHash,
  MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK,
  MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import {
  canonicalProgramEnvelope,
  MidgardCekProgramMaterialMissingRootError,
  type MidgardCekProgramMaterialVerification,
  type MidgardCekProgramMaterialVerificationOptions,
  normalizeProgramMaterial,
  type ProgramMaterialBundleCache,
} from "./cek-proof.program-material-task.js";
import { verifyOneProgramMaterial } from "./cek-proof.verify-one-program-material.js";
import {
  compareBytes,
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";

/**
 * Verifies exact content hashes, node syntax, typed graph edges, sequence
 * lengths, canonical blob shape, acyclicity, and envelope counts. By default
 * a one-program sidecar may not contain unreachable material.
 */
export const verifyMidgardCekProgramMaterial = (
  envelope: MidgardCekProgramEnvelope,
  entries: Iterable<MidgardCekProgramMaterialEntry>,
  options: MidgardCekProgramMaterialVerificationOptions = {},
): MidgardCekProgramMaterialVerification => {
  const exactEnvelope = canonicalProgramEnvelope(envelope).envelope;
  const material = normalizeProgramMaterial(entries);
  const verified = verifyOneProgramMaterial(
    exactEnvelope,
    material,
    { materializedBlobs: new Map(), validatedConstants: new Map() },
    {
      includeConstants: true,
      onBlobMaterialized: options.onBlobMaterialized,
      onConstantMaterialized: options.onConstantMaterialized,
    },
  );
  if (
    options.allowUnreachable !== true &&
    verified.reachableRoots.size !== material.size
  ) {
    throw new Error("CEK program material contains unreachable nodes");
  }
  return verified;
};

const verifyProgramMaterialBundle = (
  envelopes: readonly MidgardCekProgramEnvelope[],
  entries: Iterable<MidgardCekProgramMaterialEntry>,
  options: MidgardCekProgramMaterialVerificationOptions,
  includeResults: boolean,
): readonly MidgardCekProgramMaterialVerification[] => {
  const envelopeIdentities: string[] = [];
  const uniqueEnvelopes = new Map<string, MidgardCekProgramEnvelope>();
  let aggregateNodeVisits = 0n;
  let aggregateByteWork = 0n;
  for (const envelope of envelopes) {
    const canonical = canonicalProgramEnvelope(envelope);
    envelopeIdentities.push(canonical.identity);
    if (uniqueEnvelopes.has(canonical.identity)) continue;
    aggregateNodeVisits += canonical.envelope.nodeCount;
    if (aggregateNodeVisits > MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS) {
      throw new Error(
        `CEK program material bundle declares ${aggregateNodeVisits.toString()} aggregate unique-envelope node visits, exceeding ${MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS.toString()}`,
      );
    }
    aggregateByteWork += canonical.envelope.materialByteLength;
    if (aggregateByteWork > MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK) {
      throw new Error(
        `CEK program material bundle declares ${aggregateByteWork.toString()} aggregate unique-envelope byte work/result, exceeding ${MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK.toString()}`,
      );
    }
    uniqueEnvelopes.set(canonical.identity, canonical.envelope);
  }

  const material = normalizeProgramMaterial(entries);
  if (envelopes.length === 0) {
    if (material.size !== 0 && options.allowUnreachable !== true) {
      throw new Error(
        "CEK program material is present without a program envelope",
      );
    }
    return Object.freeze([]);
  }
  const reached = new Set<string>();
  const verifiedByIdentity = new Map<
    string,
    MidgardCekProgramMaterialVerification
  >();
  const cache: ProgramMaterialBundleCache = {
    materializedBlobs: new Map(),
    validatedConstants: new Map(),
  };
  let missingRoot: MidgardCekProgramMaterialMissingRootError | undefined;
  for (const [identity, envelope] of uniqueEnvelopes) {
    let result: MidgardCekProgramMaterialVerification | undefined;
    try {
      result = verifyOneProgramMaterial(envelope, material, cache, {
        includeConstants: includeResults,
        onBlobMaterialized: options.onBlobMaterialized,
        onConstantMaterialized: options.onConstantMaterialized,
      });
    } catch (cause) {
      if (cause instanceof MidgardCekProgramMaterialMissingRootError) {
        missingRoot ??= cause;
        continue;
      }
      throw cause;
    }
    if (result === undefined) continue;
    for (const key of result.reachableRoots) reached.add(key);
    if (includeResults) verifiedByIdentity.set(identity, result);
  }
  if (options.allowUnreachable !== true && reached.size !== material.size) {
    throw new Error(
      "CEK program material bundle contains nodes unreachable from every envelope",
    );
  }
  if (missingRoot !== undefined) {
    throw missingRoot;
  }
  return includeResults
    ? Object.freeze(
        envelopeIdentities.map((identity) => verifiedByIdentity.get(identity)!),
      )
    : Object.freeze([]);
};

/**
 * Verifies a DA block's deduplicated material against every referenced
 * program. Every supplied node must be reachable from at least one envelope.
 */
export const verifyMidgardCekProgramMaterialBundle = (
  envelopes: readonly MidgardCekProgramEnvelope[],
  entries: Iterable<MidgardCekProgramMaterialEntry>,
  options: MidgardCekProgramMaterialVerificationOptions = {},
): readonly MidgardCekProgramMaterialVerification[] =>
  verifyProgramMaterialBundle(envelopes, entries, options, true);

/**
 * Strict coverage-only form for DA admission. It performs the same validation
 * but does not retain per-envelope constant buffers after verification.
 */
export const assertMidgardCekProgramMaterialBundle = (
  envelopes: readonly MidgardCekProgramEnvelope[],
  entries: Iterable<MidgardCekProgramMaterialEntry>,
  options: MidgardCekProgramMaterialVerificationOptions = {},
): void => {
  verifyProgramMaterialBundle(envelopes, entries, options, false);
};

export const MIDGARD_PROOF_SUBMISSION_ENVELOPE_VERSION = 1n;

export const MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_VERSION = 1n;

export type MidgardCekProgramMaterialSidecar =
  readonly MidgardCekProgramMaterialEntry[];

export type MidgardProofSubmission = {
  readonly transactionCbor: Buffer;
  readonly programMaterial: MidgardCekProgramMaterialSidecar;
};

const canonicalizeMidgardCekProgramMaterialEntries = (
  entries: readonly MidgardCekProgramMaterialEntry[],
  label: string,
): readonly MidgardCekProgramMaterialEntry[] => {
  const material = [...entries]
    .map((entry) =>
      decodeMidgardCekProgramMaterialEntry(
        encodeMidgardCekProgramMaterialEntry(entry),
      ),
    )
    .sort((left, right) => compareBytes(left.root, right.root));
  for (let index = 1; index < material.length; index += 1) {
    if (Buffer.from(material[index - 1]!.root).equals(material[index]!.root)) {
      throw new Error(`${label} has duplicate material roots`);
    }
  }
  return Object.freeze(material);
};

export const encodeMidgardCekProgramMaterialEntryList = (
  entries: readonly MidgardCekProgramMaterialEntry[],
  label: string,
): readonly (readonly [Buffer, Buffer])[] =>
  canonicalizeMidgardCekProgramMaterialEntries(entries, label).map(
    (entry) =>
      Object.freeze([
        Buffer.from(entry.root),
        encodeMidgardCekProgramMaterialDaValue(entry),
      ]) as readonly [Buffer, Buffer],
  );

export const decodeMidgardCekProgramMaterialEntryList = (
  source: Buffer,
  offset: number,
  label: string,
): {
  readonly entries: readonly MidgardCekProgramMaterialEntry[];
  readonly nextOffset: number;
} => {
  const materialHeader = readCborArrayHeader(source, offset, label);
  const programMaterial: MidgardCekProgramMaterialEntry[] = [];
  let cursor = materialHeader.nextOffset;
  let previousRoot: Buffer | undefined;
  for (let index = 0; index < materialHeader.length; index += 1) {
    const entryLabel = `${label}[${index.toString()}]`;
    const entryHeader = readCborArrayHeader(source, cursor, entryLabel);
    if (entryHeader.length !== 2) {
      throw new Error(
        "V1 program material entry must contain exactly two fields",
      );
    }
    const root = readCborBytes(
      source,
      entryHeader.nextOffset,
      `${entryLabel}.root`,
    );
    const exactRoot = exactHash(root.value, `${entryLabel}.root`);
    if (
      previousRoot !== undefined &&
      compareBytes(previousRoot, exactRoot) >= 0
    ) {
      throw new Error("V1 program material roots must be strictly sorted");
    }
    const value = readCborBytes(source, root.nextOffset, `${entryLabel}.value`);
    programMaterial.push(
      decodeMidgardCekProgramMaterialDaEntry(exactRoot, value.value),
    );
    previousRoot = exactRoot;
    cursor = value.nextOffset;
  }
  return Object.freeze({
    entries: Object.freeze(programMaterial),
    nextOffset: cursor,
  });
};

/**
 * Canonical storage/transport sidecar independent of the HTTP submission
 * wrapper. Keeping the version in the stored bytes makes replay and migration
 * fail closed if a future material encoding changes.
 */
export const encodeMidgardCekProgramMaterialSidecar = (
  entries: MidgardCekProgramMaterialSidecar,
): Buffer =>
  encodeCbor([
    MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_VERSION,
    encodeMidgardCekProgramMaterialEntryList(
      entries,
      "V1 program material sidecar",
    ),
  ]);

export const decodeMidgardCekProgramMaterialSidecar = (
  bytes: Uint8Array,
): MidgardCekProgramMaterialSidecar => {
  const source = Buffer.from(bytes);
  const header = readCborArrayHeader(source, 0, "program_material_sidecar");
  if (header.length !== 2) {
    throw new Error(
      "V1 program material sidecar must contain exactly two fields",
    );
  }
  const version = readCborUnsigned(
    source,
    header.nextOffset,
    "program_material_sidecar.version",
  );
  if (version.value !== MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_VERSION) {
    throw new Error(
      `unsupported V1 program material sidecar version ${version.value.toString()}`,
    );
  }
  const material = decodeMidgardCekProgramMaterialEntryList(
    source,
    version.nextOffset,
    "program_material_sidecar.entries",
  );
  if (material.nextOffset !== source.length) {
    throw new Error("V1 program material sidecar has trailing bytes");
  }
  if (
    !encodeMidgardCekProgramMaterialSidecar(material.entries).equals(source)
  ) {
    throw new Error("V1 program material sidecar CBOR is not canonical");
  }
  return material.entries;
};
