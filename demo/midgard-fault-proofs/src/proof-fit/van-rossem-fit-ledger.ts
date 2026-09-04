import { createHash, randomUUID } from "node:crypto";
import { mkdir, open, rename } from "node:fs/promises";
import { dirname } from "node:path";

export const VAN_ROSSEM_MAX_SIGNED_TX_BYTES = 16_384;
export const VAN_ROSSEM_PUBLICATION_RESERVE_BYTES = 512;
export const VAN_ROSSEM_PUBLICATION_TARGET_BYTES =
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES - VAN_ROSSEM_PUBLICATION_RESERVE_BYTES;
export const VAN_ROSSEM_MAX_MEMORY_UNITS = 16_500_000n;
export const VAN_ROSSEM_MAX_CPU_UNITS = 10_000_000_000n;
export const VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION =
  "midgard-van-rossem-fit-ledger-v1" as const;

export type VanRossemFitMeasurement = {
  readonly name: string;
  readonly kind: "publication" | "lifecycle";
  readonly maximumShape: string;
  readonly signedBytes: number;
  readonly memoryUnits: bigint;
  readonly cpuUnits: bigint;
};

export type VanRossemFitLedgerEntry = {
  readonly name: string;
  readonly kind: "publication" | "lifecycle";
  readonly maximumShape: string;
  readonly signedBytes: number;
  readonly memoryUnits: string;
  readonly cpuUnits: string;
  readonly signedByteMargin: number;
  readonly memoryUnitMargin: string;
  readonly cpuUnitMargin: string;
  readonly publicationReserveMargin: number | null;
};

export type VanRossemFitLedger = {
  readonly schemaVersion: typeof VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION;
  readonly category: string;
  readonly blueprintSha256: string;
  readonly compilerVersion: string;
  readonly entries: readonly VanRossemFitLedgerEntry[];
  readonly ledgerSha256: string;
};

const canonicalBody = (
  ledger: Omit<VanRossemFitLedger, "ledgerSha256">,
): string => `${JSON.stringify(ledger, null, 2)}\n`;

const requireCanonicalLabel = (value: string, field: string): string => {
  if (value.length === 0 || value.trim() !== value) {
    throw new Error(`${field} must be a non-empty canonical string`);
  }
  return value;
};

const entryFromMeasurement = (
  measurement: VanRossemFitMeasurement,
): VanRossemFitLedgerEntry => {
  requireCanonicalLabel(measurement.name, "fit measurement name");
  requireCanonicalLabel(measurement.maximumShape, "maximum evidence shape");
  if (
    !Number.isSafeInteger(measurement.signedBytes) ||
    measurement.signedBytes < 0 ||
    measurement.memoryUnits < 0n ||
    measurement.cpuUnits < 0n
  ) {
    throw new Error(
      `fit measurement ${measurement.name} has a negative or unsafe value`,
    );
  }
  const signedByteMargin =
    VAN_ROSSEM_MAX_SIGNED_TX_BYTES - measurement.signedBytes;
  const memoryUnitMargin =
    VAN_ROSSEM_MAX_MEMORY_UNITS - measurement.memoryUnits;
  const cpuUnitMargin = VAN_ROSSEM_MAX_CPU_UNITS - measurement.cpuUnits;
  const publicationReserveMargin =
    measurement.kind === "publication"
      ? VAN_ROSSEM_PUBLICATION_TARGET_BYTES - measurement.signedBytes
      : null;
  if (signedByteMargin <= 0 || memoryUnitMargin <= 0n || cpuUnitMargin <= 0n) {
    throw new Error(
      `fit measurement ${measurement.name} has no positive Van Rossem margin`,
    );
  }
  if (publicationReserveMargin !== null && publicationReserveMargin < 0) {
    throw new Error(
      `publication ${measurement.name} exceeds the 15,872-byte reliable target`,
    );
  }
  return Object.freeze({
    name: measurement.name,
    kind: measurement.kind,
    maximumShape: measurement.maximumShape,
    signedBytes: measurement.signedBytes,
    memoryUnits: measurement.memoryUnits.toString(),
    cpuUnits: measurement.cpuUnits.toString(),
    signedByteMargin,
    memoryUnitMargin: memoryUnitMargin.toString(),
    cpuUnitMargin: cpuUnitMargin.toString(),
    publicationReserveMargin,
  });
};

export const buildVanRossemFitLedger = ({
  category,
  blueprintSha256,
  compilerVersion,
  measurements,
}: {
  readonly category: string;
  readonly blueprintSha256: string;
  readonly compilerVersion: string;
  readonly measurements: readonly VanRossemFitMeasurement[];
}): VanRossemFitLedger => {
  requireCanonicalLabel(category, "fit ledger category");
  requireCanonicalLabel(compilerVersion, "fit ledger compiler version");
  if (!/^[0-9a-f]{64}$/u.test(blueprintSha256)) {
    throw new Error("fit ledger blueprintSha256 must be 32-byte lowercase hex");
  }
  if (measurements.length === 0) {
    throw new Error("fit ledger must contain at least one measurement");
  }
  const entries = measurements
    .map(entryFromMeasurement)
    .sort((left, right) =>
      left.kind === right.kind
        ? left.name < right.name
          ? -1
          : left.name > right.name
            ? 1
            : 0
        : left.kind < right.kind
          ? -1
          : 1,
    );
  const names = new Set(entries.map((entry) => entry.name));
  if (names.size !== entries.length) {
    throw new Error("fit ledger measurement names must be unique");
  }
  const body = Object.freeze({
    schemaVersion: VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION,
    category,
    blueprintSha256,
    compilerVersion,
    entries: Object.freeze(entries),
  });
  return Object.freeze({
    ...body,
    ledgerSha256: createHash("sha256")
      .update(canonicalBody(body))
      .digest("hex"),
  });
};

export const writeVanRossemFitLedger = async (
  path: string,
  ledger: VanRossemFitLedger,
): Promise<void> => {
  const { ledgerSha256: suppliedDigest, ...body } = ledger;
  const serializedBody = canonicalBody(body);
  const actualDigest = createHash("sha256")
    .update(serializedBody)
    .digest("hex");
  if (actualDigest !== suppliedDigest) {
    throw new Error("fit ledger digest does not match its canonical body");
  }
  const directory = dirname(path);
  await mkdir(directory, { recursive: true });
  const temporaryPath = `${path}.${randomUUID()}.tmp`;
  const handle = await open(temporaryPath, "wx", 0o600);
  try {
    await handle.writeFile(`${JSON.stringify(ledger, null, 2)}\n`, "utf8");
    await handle.sync();
  } finally {
    await handle.close();
  }
  await rename(temporaryPath, path);
};
