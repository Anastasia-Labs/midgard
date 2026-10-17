/**
 * Deeply nested Plutus Data, as CBOR hex, for the suites that decode an
 * event's datum: a chain of one-item lists (`81`) or of constructor-0
 * one-field definite constructors (`d879 81`), around the integer 0.
 *
 * The two `fits` depths fit in a 16 KB L1 transaction (about 15 KB and
 * 11.7 KB of datum); the `margin` depths are well above them.
 */
export const DEEP_PLUTUS_DATA_CASES = [
  { shape: "list", depth: 15_000, bound: "fits" },
  { shape: "list", depth: 30_000, bound: "margin" },
  { shape: "constr", depth: 3_900, bound: "fits" },
  { shape: "constr", depth: 8_000, bound: "margin" },
] as const;

export type DeepPlutusDataCase = (typeof DEEP_PLUTUS_DATA_CASES)[number];

/** `depth` nested containers of `shape` around the integer 0. */
export const deepPlutusDataCbor = ({
  shape,
  depth,
}: {
  readonly shape: DeepPlutusDataCase["shape"];
  readonly depth: number;
}): string => `${(shape === "list" ? "81" : "d87981").repeat(depth)}00`;

/**
 * `cbor` with its one occurrence of the item `from` replaced by `to`, by
 * bytes alone: a fixture built this way never runs the decoder it tests.
 * Throws unless `from` occurs exactly once, on a byte boundary.
 */
export const replaceCborItemOnce = (
  cbor: string,
  from: string,
  to: string,
): string => {
  const at = cbor.indexOf(from);
  if (at < 0 || at % 2 !== 0 || cbor.indexOf(from, at + 1) !== -1)
    throw new Error("the replaced item must occur exactly once");
  return cbor.slice(0, at) + to + cbor.slice(at + from.length);
};

/** The Plutus Data node bound deployments set on an event payload. */
export const ADMISSION_PAYLOAD_NODES = 512;

/**
 * The Plutus Data nodes of `cbor`: every constructor, map, list, integer
 * and bytestring, map keys included (a constructor's field list is not a
 * node of its own). By bytes alone, in one pass with no recursion.
 */
export const plutusDataNodeCount = (cbor: string): number => {
  const bytes = Buffer.from(cbor, "hex");
  let at = 0;
  const head = () => {
    const initial = bytes[at];
    if (initial === undefined) throw new Error("CBOR ends inside an item");
    at += 1;
    const info = initial & 0x1f;
    if (info > 27 && info !== 31) throw new Error("reserved CBOR head");
    const width = info < 24 ? 0 : info === 31 ? -1 : 2 ** (info - 24);
    let arg = info;
    if (width === -1) arg = -1;
    else if (width > 0) {
      arg = Number(
        BigInt(`0x${bytes.subarray(at, at + width).toString("hex")}`),
      );
      at += width;
    }
    return { major: initial >> 5, arg };
  };
  const skipBytes = (length: number) => {
    if (length >= 0) {
      at += length;
      return;
    }
    while (bytes[at] !== 0xff) at += head().arg;
    at += 1;
  };
  // Items still to read in each open container; -1 until a break.
  const pending = [1];
  let nodes = 0;
  while (pending.length > 0) {
    const top = pending.length - 1;
    if (pending[top] === 0 || (pending[top] === -1 && bytes[at] === 0xff)) {
      if (pending[top] === -1) at += 1;
      pending.pop();
      continue;
    }
    if (pending[top]! > 0) pending[top]! -= 1;
    nodes += 1;
    const { major, arg } = head();
    if (major === 2) skipBytes(arg);
    else if (major === 4) pending.push(arg);
    else if (major === 5) pending.push(arg < 0 ? -1 : arg * 2);
    else if (major === 6 && (arg === 2 || arg === 3)) skipBytes(head().arg);
    else if (major === 6) {
      // Tag 102 is `[alternative, fields]`; the others hold the fields.
      if (arg === 102 && head().major === 4) head();
      const fields = head();
      if (fields.major !== 4) throw new Error("constructor fields not a list");
      pending.push(fields.arg);
    } else if (major > 1) throw new Error(`not Plutus Data: major ${major}`);
  }
  if (at !== bytes.length) throw new Error("bytes follow the item");
  return nodes;
};

/**
 * The depth of `deepPlutusDataCbor` that, swapped for one integer item of
 * `payloadCbor`, gives a payload of exactly `ADMISSION_PAYLOAD_NODES` nodes:
 * each level adds one node.
 */
export const depthAtAdmissionBound = (payloadCbor: string): number =>
  ADMISSION_PAYLOAD_NODES - plutusDataNodeCount(payloadCbor);
