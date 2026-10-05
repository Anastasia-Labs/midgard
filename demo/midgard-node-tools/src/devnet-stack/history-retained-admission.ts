import { existsSync, lstatSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";

import { HistoryConfigurationRefusal } from "./history-configuration-refusal.js";
import type { BlockPoint } from "./watcher-history-chain.js";

const HEX32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(0|[1-9][0-9]{0,19})$/u;
const object = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);
const exact = (
  value: unknown,
  keys: readonly string[],
): value is Record<string, unknown> =>
  object(value) && Object.keys(value).sort().join() === [...keys].sort().join();
const natural = (value: unknown): value is string =>
  typeof value === "string" &&
  NATURAL.test(value) &&
  BigInt(value) <= 18446744073709551615n;
const hex = (value: unknown): value is string =>
  typeof value === "string" && HEX32.test(value);

const fail = (): never => {
  throw new HistoryConfigurationRefusal(
    "retained history checkpoint is missing or unproven; restore exact retained evidence before recovery",
  );
};
const entries = (directory: string): string[] => {
  if (!existsSync(directory)) return [];
  if (!lstatSync(directory).isDirectory()) fail();
  return readdirSync(directory);
};
const json = (path: string): unknown => {
  if (!lstatSync(path).isFile()) fail();
  const bytes = readFileSync(path, "utf8");
  try {
    return JSON.parse(bytes);
  } catch (error) {
    if (error instanceof SyntaxError) return fail();
    throw error;
  }
};
const parsePoint = (value: unknown, canonical: boolean): BlockPoint => {
  if (
    !exact(
      value,
      canonical
        ? ["blockHash", "blockNo", "slot", "pointId"]
        : ["blockHash", "blockNo", "slot"],
    ) ||
    !hex(value.blockHash) ||
    !natural(value.blockNo) ||
    !natural(value.slot)
  )
    return fail();
  const point = {
    blockHash: value.blockHash,
    blockNo: value.blockNo,
    slot: value.slot,
  };
  if (canonical && value.pointId !== computeFraudProofRawL1PointId(point))
    fail();
  return point;
};
type Row = {
  readonly point: BlockPoint;
  readonly prevHash: string | null;
  readonly bytes: string;
};
const same = (left: BlockPoint, right: BlockPoint) =>
  left.blockHash === right.blockHash &&
  left.blockNo === right.blockNo &&
  left.slot === right.slot;

/** Only durable final record namespaces count; sibling identities/logs and atomic .tmp files do not. */
export const retainedHistoryPresent = (
  directories: readonly string[],
): boolean => {
  let held = false;
  for (const directory of directories) {
    if (!lstatSync(directory).isDirectory()) fail();
    const finalRecords = (subdirectory: string, pattern: RegExp) => {
      const parent = join(directory, subdirectory);
      for (const name of entries(parent).filter((entry) =>
        pattern.test(entry),
      )) {
        if (!lstatSync(join(parent, name)).isFile()) fail();
        held = true;
      }
    };
    finalRecords("canonical", /^[0-9]+\.json$/u);
    finalRecords("records", /^[0-9a-f]{56}\.json$/u);
    for (const script of entries(join(directory, "native-scripts")).filter(
      (name) => /^[0-9a-f]{56}$/u.test(name),
    ))
      finalRecords(
        join("native-scripts", script),
        /^[0-9a-f]{64}_[0-9]+_[0-9a-f]{64}\.json$/u,
      );
  }
  return held;
};

/** Read-only admission before a native acknowledgement is allowed to prune retained suffixes. */
export const admitRetainedHistory = (input: {
  readonly directories: readonly string[];
  readonly commitsDirectory: string;
  readonly resume: () => readonly BlockPoint[];
}) => {
  if (input.directories.length === 0) fail();
  const held = retainedHistoryPresent(input.directories);
  const rows = input.directories.map((directory) =>
    entries(join(directory, "canonical"))
      .filter((name) => /^[0-9]+\.json$/u.test(name))
      .map((name): Row => {
        const path = join(directory, "canonical", name);
        const value = json(path);
        if (!exact(value, ["point", "prevHash"])) return fail();
        const point = parsePoint(value.point, true);
        // Match the native source contract: only genesis block0 may have no
        // parent hash. Do not normalize retained bytes or other heights.
        if (
          value.prevHash !== null &&
          !hex(value.prevHash) &&
          !(point.blockNo === "0" && value.prevHash === "")
        )
          return fail();
        if (name !== `${point.blockNo}.json`) fail();
        return {
          point,
          prevHash: value.prevHash,
          bytes: readFileSync(path, "utf8"),
        };
      })
      .sort((a, b) =>
        BigInt(a.point.blockNo) < BigInt(b.point.blockNo) ? -1 : 1,
      ),
  );
  if (held && !existsSync(input.commitsDirectory)) fail();
  const index = entries(input.commitsDirectory)
    .filter((name) => /^[0-9a-f]{56}\.json$/u.test(name))
    .map((name) => parsePoint(json(join(input.commitsDirectory, name)), false));
  if (!held && index.length !== 0) fail();
  for (const point of index) {
    const retained = rows
      .flat()
      .filter((row) => row.point.blockNo === point.blockNo);
    if (retained.length > 0) {
      if (retained.some((row) => !same(row.point, point))) fail();
    } else {
      // Index publication precedes the archive append. One pending next-block
      // entry is replayable after the node authenticates a retained intersection.
      const all = rows.flat();
      const latest = all.reduce<Row | undefined>(
        (last, row) =>
          last === undefined ||
          BigInt(row.point.blockNo) > BigInt(last.point.blockNo)
            ? row
            : last,
        undefined,
      );
      if (
        latest === undefined ||
        BigInt(point.blockNo) !== BigInt(latest.point.blockNo) + 1n ||
        BigInt(point.slot) <= BigInt(latest.point.slot)
      )
        fail();
    }
  }
  // Validate owned rows/index before resume's existing reader can follow any path.
  const resume = held ? [...input.resume()] : [];
  if (held && resume.length === 0) fail();
  for (const checkpoint of resume) {
    const prefixes = rows.map((provider) =>
      provider.filter(
        (row) => BigInt(row.point.blockNo) <= BigInt(checkpoint.blockNo),
      ),
    );
    const first = prefixes[0];
    if (
      first === undefined ||
      first.length === 0 ||
      !same(first[first.length - 1]?.point ?? fail(), checkpoint)
    )
      fail();
    for (let i = 0; i < first.length; i += 1) {
      const row = first[i] ?? fail();
      const previous = first[i - 1];
      if (
        previous !== undefined &&
        (BigInt(row.point.blockNo) !== BigInt(previous.point.blockNo) + 1n ||
          row.prevHash !== previous.point.blockHash ||
          BigInt(row.point.slot) <= BigInt(previous.point.slot))
      )
        fail();
      if (
        prefixes.some(
          (provider) =>
            provider.length !== first.length ||
            provider[i]?.bytes !== row.bytes,
        )
      )
        fail();
    }
  }
  return { held, resume };
};

/** A live rollback must name a retained point shared by every provider. */
export const knownRetainedRollback = (
  directories: readonly string[],
  rollback: { readonly blockHash: string; readonly slot: string },
): boolean => {
  const first = directories[0];
  if (first === undefined) return false;
  for (const name of entries(join(first, "canonical")).filter((name) =>
    /^[0-9]+\.json$/u.test(name),
  )) {
    const value = json(join(first, "canonical", name));
    if (!exact(value, ["point", "prevHash"])) return fail();
    const point = parsePoint(value.point, true);
    if (point.blockHash !== rollback.blockHash || point.slot !== rollback.slot)
      continue;
    const reference = readFileSync(join(first, "canonical", name), "utf8");
    return directories.every((directory) => {
      const path = join(directory, "canonical", name);
      return (
        existsSync(path) &&
        lstatSync(path).isFile() &&
        readFileSync(path, "utf8") === reference
      );
    });
  }
  return false;
};
