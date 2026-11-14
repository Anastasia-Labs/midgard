/** A SHA-256 digest of a value's content, for telling whether a check's
 * inputs are exactly the inputs it already passed. Two values have the same
 * digest only when they hold the same data: every string, number, bigint,
 * boolean, null and undefined in the same place, the same bytes in each byte
 * view, and the same keys on each object, array, map and set, each tagged
 * with its kind and, for an object, its constructor's name. A value that is
 * not data (a function, a symbol, a cycle, a sparse array, or an object that
 * is none of a plain object, an array, a map, a set, a date or a byte view)
 * has no digest. */
import { createHash, type Hash } from "node:crypto";

const update = (hash: Hash, tag: string, text: string) => {
  hash.update(`${tag}${text.length.toString()}:`);
  // UTF-16 code units, so no two strings encode alike.
  hash.update(Buffer.from(text, "utf16le"));
};

export const validationInputsDigest = (value: unknown): string => {
  const hash = createHash("sha256");
  const open = new Set<object>();
  const walk = (item: unknown): void => {
    switch (typeof item) {
      case "undefined":
        hash.update("u");
        return;
      case "boolean":
        hash.update(item ? "t" : "f");
        return;
      case "number":
        update(hash, "n", Object.is(item, -0) ? "-0" : String(item));
        return;
      case "bigint":
        update(hash, "i", item.toString(16));
        return;
      case "string":
        update(hash, "s", item);
        return;
      case "object":
        break;
      case "function":
      case "symbol":
        throw new Error(`a ${typeof item} has no content digest`);
    }
    if (item === null) {
      hash.update("z");
      return;
    }
    if (open.has(item)) throw new Error("a cyclic value has no content digest");
    open.add(item);
    const kind = Object.getPrototypeOf(item) as {
      readonly constructor?: { readonly name?: string };
    } | null;
    update(hash, "c", kind?.constructor?.name ?? "");
    if (ArrayBuffer.isView(item)) {
      const bytes = new Uint8Array(
        item.buffer,
        item.byteOffset,
        item.byteLength,
      );
      hash.update(`b${bytes.length.toString()}:`);
      hash.update(bytes);
    } else if (kind === Array.prototype) {
      const elements = item as readonly unknown[];
      hash.update(`a${elements.length.toString()}:`);
      for (let index = 0; index < elements.length; index += 1) {
        if (!(index in elements))
          throw new Error("a sparse array has no content digest");
        walk(elements[index]);
      }
    } else if (kind === Map.prototype) {
      const entries = item as ReadonlyMap<unknown, unknown>;
      hash.update(`m${entries.size.toString()}:`);
      for (const [key, element] of entries) {
        walk(key);
        walk(element);
      }
    } else if (kind === Set.prototype) {
      const elements = item as ReadonlySet<unknown>;
      hash.update(`e${elements.size.toString()}:`);
      for (const element of elements) walk(element);
    } else if (kind === Date.prototype) {
      update(hash, "d", String((item as Date).getTime()));
    } else if (kind === Object.prototype || kind === null) {
      if (Object.getOwnPropertySymbols(item).length > 0)
        throw new Error("a symbol-keyed object has no content digest");
      const keys = Object.keys(item).sort();
      hash.update(`o${keys.length.toString()}:`);
      for (const key of keys) {
        update(hash, "k", key);
        walk((item as Readonly<Record<string, unknown>>)[key]);
      }
    } else {
      throw new Error(
        `a ${kind.constructor?.name ?? "foreign"} object has no content digest`,
      );
    }
    open.delete(item);
  };
  walk(value);
  return hash.digest("hex");
};
