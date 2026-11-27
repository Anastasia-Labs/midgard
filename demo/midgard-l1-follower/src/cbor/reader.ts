/**
 * A lenient, span-preserving CBOR reader for Cardano L1 blocks.
 *
 * L1 blocks are not Midgard-canonical: wallets emit indefinite-length arrays
 * and byte strings, and non-minimal heads. This reader accepts every
 * well-formed RFC 8949 item and reports byte offsets, so callers can take the
 * exact slice of any item. Skipping is iterative, so a deeply nested datum or
 * redeemer cannot exhaust the call stack.
 */

export class CborReadError extends Error {
  constructor(message: string, offset: number) {
    super(`${message} at offset ${offset}`);
    this.name = "CborReadError";
  }
}

export type CborHead = Readonly<{
  major: number;
  /** The argument; for an indefinite head, -1. */
  value: bigint;
  indefinite: boolean;
  /** Offset just after the head (and after a definite string's argument). */
  next: number;
}>;

const BREAK = 0xff;

export const readHead = (bytes: Uint8Array, offset: number): CborHead => {
  if (offset >= bytes.length) throw new CborReadError("unexpected end", offset);
  const initial = bytes[offset] as number;
  const major = initial >> 5;
  const info = initial & 0x1f;
  if (info < 24)
    return { major, value: BigInt(info), indefinite: false, next: offset + 1 };
  if (info === 31) {
    if (major === 0 || major === 1 || major === 6)
      throw new CborReadError("indefinite length on a non-container", offset);
    return { major, value: -1n, indefinite: true, next: offset + 1 };
  }
  if (info > 27) throw new CborReadError("reserved additional info", offset);
  const size = info === 24 ? 1 : info === 25 ? 2 : info === 26 ? 4 : 8;
  if (offset + 1 + size > bytes.length)
    throw new CborReadError("truncated head", offset);
  let value = 0n;
  for (let index = 1; index <= size; index += 1)
    value = (value << 8n) | BigInt(bytes[offset + index] as number);
  return { major, value, indefinite: false, next: offset + 1 + size };
};

const safeLength = (value: bigint, offset: number): number => {
  if (value > BigInt(Number.MAX_SAFE_INTEGER))
    throw new CborReadError("length too large", offset);
  return Number(value);
};

/** The offset just after the item that starts at `offset`. */
export const skipItem = (bytes: Uint8Array, offset: number): number => {
  // Remaining item counts of the open containers; -1 marks an indefinite one.
  const remaining: number[] = [1];
  let position = offset;
  while (remaining.length > 0) {
    const top = remaining.length - 1;
    const count = remaining[top] as number;
    if (count === 0) {
      remaining.pop();
      continue;
    }
    if (count === -1 && bytes[position] === BREAK) {
      position += 1;
      remaining.pop();
      continue;
    }
    const head = readHead(bytes, position);
    if (count > 0) remaining[top] = count - 1;
    position = head.next;
    switch (head.major) {
      case 0:
      case 1:
        break;
      case 2:
      case 3:
        if (head.indefinite) remaining.push(-1);
        else position += safeLength(head.value, position);
        break;
      case 4:
        if (head.indefinite) remaining.push(-1);
        else if (head.value > 0n)
          remaining.push(safeLength(head.value, position));
        break;
      case 5:
        if (head.indefinite) remaining.push(-1);
        else if (head.value > 0n)
          remaining.push(safeLength(head.value, position) * 2);
        break;
      case 6:
        remaining.push(1);
        break;
      default:
        if (head.indefinite)
          throw new CborReadError("unexpected break", position - 1);
        break;
    }
    if (position > bytes.length)
      throw new CborReadError("truncated item", position);
  }
  return position;
};

/** Offsets of each element of the array at `offset`, and its end. */
export const readArray = (
  bytes: Uint8Array,
  offset: number,
): { items: number[]; end: number } => {
  const head = readHead(bytes, offset);
  if (head.major !== 4) throw new CborReadError("expected an array", offset);
  const items: number[] = [];
  let position = head.next;
  if (head.indefinite) {
    while (bytes[position] !== BREAK) {
      if (position >= bytes.length)
        throw new CborReadError("unterminated array", position);
      items.push(position);
      position = skipItem(bytes, position);
    }
    return { items, end: position + 1 };
  }
  const length = safeLength(head.value, offset);
  for (let index = 0; index < length; index += 1) {
    items.push(position);
    position = skipItem(bytes, position);
  }
  return { items, end: position };
};

/** Key and value offsets of each entry of the map at `offset`. */
export const readMap = (
  bytes: Uint8Array,
  offset: number,
): { entries: { key: number; value: number }[]; end: number } => {
  const head = readHead(bytes, offset);
  if (head.major !== 5) throw new CborReadError("expected a map", offset);
  const entries: { key: number; value: number }[] = [];
  let position = head.next;
  const readEntry = () => {
    const key = position;
    const value = skipItem(bytes, key);
    position = skipItem(bytes, value);
    entries.push({ key, value });
  };
  if (head.indefinite) {
    while (bytes[position] !== BREAK) {
      if (position >= bytes.length)
        throw new CborReadError("unterminated map", position);
      readEntry();
    }
    return { entries, end: position + 1 };
  }
  const length = safeLength(head.value, offset);
  for (let index = 0; index < length; index += 1) readEntry();
  return { entries, end: position };
};

export const readUint = (bytes: Uint8Array, offset: number): bigint => {
  const head = readHead(bytes, offset);
  if (head.major !== 0) throw new CborReadError("expected an unsigned", offset);
  return head.value;
};

export const readSmallUint = (bytes: Uint8Array, offset: number): number =>
  safeLength(readUint(bytes, offset), offset);

export const readInt = (bytes: Uint8Array, offset: number): bigint => {
  const head = readHead(bytes, offset);
  if (head.major === 0) return head.value;
  if (head.major === 1) return -1n - head.value;
  throw new CborReadError("expected an integer", offset);
};

/** A byte string's content; indefinite strings are concatenated. */
export const readBytes = (bytes: Uint8Array, offset: number): Buffer => {
  const head = readHead(bytes, offset);
  if (head.major !== 2) throw new CborReadError("expected bytes", offset);
  if (!head.indefinite) {
    const end = head.next + safeLength(head.value, offset);
    if (end > bytes.length) throw new CborReadError("truncated bytes", offset);
    return Buffer.from(bytes.subarray(head.next, end));
  }
  const chunks: Buffer[] = [];
  let position = head.next;
  while (bytes[position] !== BREAK) {
    if (position >= bytes.length)
      throw new CborReadError("unterminated bytes", position);
    const chunk = readHead(bytes, position);
    if (chunk.major !== 2 || chunk.indefinite)
      throw new CborReadError("bad indefinite bytes chunk", position);
    chunks.push(readBytes(bytes, position));
    position = skipItem(bytes, position);
  }
  return Buffer.concat(chunks);
};

/** A tag's number and the offset of the tagged item. */
export const readTag = (
  bytes: Uint8Array,
  offset: number,
): { tag: bigint; item: number } => {
  const head = readHead(bytes, offset);
  if (head.major !== 6) throw new CborReadError("expected a tag", offset);
  return { tag: head.value, item: head.next };
};

/** The offset of the item, skipping one optional tag `tag`. */
export const untag = (
  bytes: Uint8Array,
  offset: number,
  tag: bigint,
): number => {
  const head = readHead(bytes, offset);
  return head.major === 6 && head.value === tag ? head.next : offset;
};

export const isNull = (bytes: Uint8Array, offset: number): boolean =>
  bytes[offset] === 0xf6;

export const slice = (bytes: Uint8Array, start: number, end: number): Buffer =>
  Buffer.from(bytes.subarray(start, end));
