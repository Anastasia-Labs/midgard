import { createHash, randomUUID } from "node:crypto";
import { link, mkdir, open, readFile, unlink } from "node:fs/promises";
import { basename, extname, join } from "node:path";

import { afterAll } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./emulator/blueprints.js";

/**
 * One checked-in fit ledger measured by a suite whose cases are split across
 * several test files ("parts"). Every case has an ordinal in the suite's
 * complete case table, and a row's name ends with its position among all rows
 * in ordinal order, so the ledger reads exactly as one file running every case
 * in order wrote it.
 */
export type SplitFitLedger = {
  readonly path: string;
  readonly category: string;
  readonly compilerVersion: string;
  readonly caseCount: number;
  readonly parts: readonly string[];
  /**
   * The module holding the case table and its bodies. Parts measured against
   * different versions of it never merge, so a retry after an edit cannot
   * combine old rows with new ones under a reused run token.
   */
  readonly source: string;
};

/** A row before numbering; its name becomes `${stem}:${position}`. */
export type SplitFitRow = Omit<VanRossemFitMeasurement, "name"> & {
  readonly stem: string;
};

const PART_SCHEMA_VERSION = "midgard-split-fit-ledger-part-v1";

export type StoredRow = Omit<SplitFitRow, "memoryUnits" | "cpuUnits"> & {
  readonly memoryUnits: string;
  readonly cpuUnits: string;
};

/** One part's rows as stored for the merge. */
export type StoredSplitFitPart = {
  readonly schemaVersion: typeof PART_SCHEMA_VERSION;
  readonly ledger: string;
  readonly category: string;
  readonly compilerVersion: string;
  readonly blueprintSha256: string;
  readonly sourceSha256: string;
  readonly caseCount: number;
  readonly part: string;
  readonly cases: readonly {
    readonly ordinal: number;
    readonly rows: readonly StoredRow[];
  }[];
};

const fileSha256 = async (path: string): Promise<string> =>
  createHash("sha256")
    .update(await readFile(path))
    .digest("hex");

const blueprintSha256 = (): Promise<string> => fileSha256(realBlueprintPath);

const ledgerName = (ledger: SplitFitLedger): string =>
  basename(ledger.path, extname(ledger.path));

/** The same fresh-run contract as `measured-fit-ledger.ts`. */
const partDirectory = (ledger: SplitFitLedger): string => {
  const directory = process.env.MIDGARD_FIT_FRAGMENT_DIR;
  const run = process.env.MIDGARD_FIT_MEASUREMENT_RUN;
  if (!directory || !run || !/^[a-zA-Z0-9_-]+$/u.test(run))
    throw new Error(
      `Regenerating ${basename(ledger.path)} requires MIDGARD_FIT_FRAGMENT_DIR and a fresh MIDGARD_FIT_MEASUREMENT_RUN token: its rows come from ${ledger.parts.length.toString()} test files, merged only when all of them pass in that run`,
    );
  return join(directory, run, ledgerName(ledger));
};

/** Publish a part without ever replacing one: a token names one fresh run. */
const writePart = async (
  path: string,
  part: StoredSplitFitPart,
): Promise<void> => {
  const temporaryPath = `${path}.${randomUUID()}.tmp`;
  const handle = await open(temporaryPath, "wx", 0o600);
  try {
    await handle.writeFile(`${JSON.stringify(part, null, 2)}\n`, "utf8");
    await handle.sync();
  } finally {
    await handle.close();
  }
  try {
    await link(temporaryPath, path);
  } catch (cause) {
    throw new Error(
      `fit ledger part ${path} already exists; use a fresh MIDGARD_FIT_MEASUREMENT_RUN token`,
      { cause },
    );
  } finally {
    await unlink(temporaryPath);
  }
};

const readPart = async (
  path: string,
): Promise<StoredSplitFitPart | undefined> => {
  let text: string;
  try {
    text = await readFile(path, "utf8");
  } catch (cause) {
    if ((cause as NodeJS.ErrnoException).code === "ENOENT") return undefined;
    throw cause;
  }
  return JSON.parse(text) as StoredSplitFitPart;
};

/**
 * Numbers every row of every case in ordinal order. Refuses unless the parts
 * agree on the ledger, build and case table and together hold each case
 * exactly once, so a partial run can never produce a ledger.
 */
export const mergeSplitFitLedgerParts = (
  ledger: SplitFitLedger,
  parts: readonly StoredSplitFitPart[],
) => {
  const [first] = parts;
  if (first === undefined || parts.length !== ledger.parts.length)
    throw new Error(`${basename(ledger.path)} needs every part to merge`);
  const rowsByCase = new Map<number, readonly StoredRow[]>();
  parts.forEach((part, index) => {
    if (
      part.schemaVersion !== PART_SCHEMA_VERSION ||
      part.ledger !== ledgerName(ledger) ||
      part.category !== ledger.category ||
      part.compilerVersion !== ledger.compilerVersion ||
      part.caseCount !== ledger.caseCount ||
      part.part !== ledger.parts[index] ||
      part.blueprintSha256 !== first.blueprintSha256 ||
      part.sourceSha256 !== first.sourceSha256
    )
      throw new Error(
        `fit ledger part ${part.part} does not belong to this ${basename(ledger.path)} run`,
      );
    for (const { ordinal, rows } of part.cases) {
      if (rowsByCase.has(ordinal))
        throw new Error(`fit ledger case ${ordinal.toString()} is duplicated`);
      rowsByCase.set(ordinal, rows);
    }
  });
  const measurements: VanRossemFitMeasurement[] = [];
  for (let ordinal = 0; ordinal < ledger.caseCount; ordinal++) {
    const rows = rowsByCase.get(ordinal);
    if (rows === undefined)
      throw new Error(`fit ledger case ${ordinal.toString()} is missing`);
    for (const { stem, memoryUnits, cpuUnits, ...row } of rows)
      measurements.push({
        ...row,
        name: `${stem}:${measurements.length.toString()}`,
        memoryUnits: BigInt(memoryUnits),
        cpuUnits: BigInt(cpuUnits),
      });
  }
  if (rowsByCase.size !== ledger.caseCount)
    throw new Error(`${basename(ledger.path)} parts hold unknown cases`);
  return buildVanRossemFitLedger({
    category: ledger.category,
    blueprintSha256: first.blueprintSha256,
    compilerVersion: ledger.compilerVersion,
    measurements,
  });
};

/**
 * Records one part's rows. Under `MIDGARD_WRITE_FIT_LEDGER=1` the part stores
 * its rows once every case it owns has passed, then writes the ledger if every
 * other part of the same run has stored its rows too; the last part to finish
 * writes it. A part with a failed or filtered-out case fails without storing
 * anything, and a run missing a part leaves the checked-in ledger untouched.
 */
export const createSplitFitLedgerPart = (
  ledger: SplitFitLedger,
  part: string,
  ordinals: readonly number[],
) => {
  if (!ledger.parts.includes(part))
    throw new Error(`${part} is not a part of ${basename(ledger.path)}`);
  if (
    new Set(ordinals).size !== ordinals.length ||
    ordinals.some(
      (ordinal) =>
        !Number.isSafeInteger(ordinal) ||
        ordinal < 0 ||
        ordinal >= ledger.caseCount,
    )
  )
    throw new Error(`${part} names an invalid case`);
  const rows = new Map<number, SplitFitRow[]>();
  const passed = new Set<number>();
  const writing = process.env.MIDGARD_WRITE_FIT_LEDGER === "1";
  // Read at registration so a blueprint rebuilt mid-measurement is refused.
  const initialBlueprint = writing ? blueprintSha256() : undefined;
  const initialSource = writing ? fileSha256(ledger.source) : undefined;
  afterAll(async () => {
    if (!initialBlueprint || !initialSource) return;
    const directory = partDirectory(ledger);
    const unpassed = ordinals.filter((ordinal) => !passed.has(ordinal));
    if (unpassed.length > 0)
      throw new Error(
        `fit ledger part ${part} is incomplete (cases ${unpassed.join(", ")} did not pass); ${basename(ledger.path)} was not written`,
      );
    const blueprint = await initialBlueprint;
    if ((await blueprintSha256()) !== blueprint)
      throw new Error("The blueprint changed while its fit was measured");
    const source = await initialSource;
    if ((await fileSha256(ledger.source)) !== source)
      throw new Error(`${ledger.source} changed while its fit was measured`);
    await mkdir(directory, { recursive: true });
    await writePart(join(directory, `${part}.json`), {
      schemaVersion: PART_SCHEMA_VERSION,
      ledger: ledgerName(ledger),
      category: ledger.category,
      compilerVersion: ledger.compilerVersion,
      blueprintSha256: blueprint,
      sourceSha256: source,
      caseCount: ledger.caseCount,
      part,
      cases: [...ordinals]
        .sort((left, right) => left - right)
        .map((ordinal) => ({
          ordinal,
          rows: (rows.get(ordinal) ?? []).map((row) => ({
            ...row,
            memoryUnits: row.memoryUnits.toString(),
            cpuUnits: row.cpuUnits.toString(),
          })),
        })),
    });
    const stored = await Promise.all(
      ledger.parts.map((name) => readPart(join(directory, `${name}.json`))),
    );
    const missing = ledger.parts.filter((_, index) => !stored[index]);
    if (missing.length > 0) {
      console.warn(
        `${basename(ledger.path)} waits for parts ${missing.join(", ")} of this run; it is written by the last part to pass`,
      );
      return;
    }
    await writeVanRossemFitLedger(
      ledger.path,
      mergeSplitFitLedgerParts(ledger, stored as StoredSplitFitPart[]),
    );
  });
  return {
    /** Starts (or restarts, on a retry) a case's rows. */
    begin: (ordinal: number): void => {
      rows.set(ordinal, []);
      passed.delete(ordinal);
    },
    record: (ordinal: number, row: SplitFitRow): void => {
      const caseRows = rows.get(ordinal);
      if (caseRows === undefined)
        throw new Error(`fit ledger case ${ordinal.toString()} has not begun`);
      caseRows.push(row);
    },
    /** Marks a case passed; call it as the last statement of the case. */
    pass: (ordinal: number): void => {
      passed.add(ordinal);
    },
  };
};
