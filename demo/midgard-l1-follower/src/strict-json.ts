/**
 * JSON admission for files whose bytes become an identity or a configuration:
 * the grammar `JSON.parse` accepts, except that an object repeating a key is
 * refused rather than silently keeping the last value, and a number that is
 * not finite is refused. Objects are built without invoking inherited setters,
 * so a `__proto__` key is an ordinary field.
 *
 * Every refusal goes through `refuse` with the refused property path (`$` is
 * the document), so a caller reports it in its own error vocabulary.
 */
export type StrictJsonRefusal =
  | "malformed_json"
  | "duplicate_field"
  | "unsafe_value";

export type StrictJsonRefuse = (
  refusal: StrictJsonRefusal,
  path: string,
) => never;

class StrictJsonReader {
  readonly #source: string;
  readonly #refuse: StrictJsonRefuse;
  #position = 0;

  constructor(source: string, refuse: StrictJsonRefuse) {
    this.#source = source;
    this.#refuse = refuse;
  }

  parse(): unknown {
    this.#skipWhitespace();
    const value = this.#parseValue("$");
    this.#skipWhitespace();
    if (this.#position !== this.#source.length) {
      this.#refuse("malformed_json", "$");
    }
    return value;
  }

  #parseValue(path: string): unknown {
    this.#skipWhitespace();
    const character = this.#source[this.#position];
    if (character === "{") {
      return this.#parseObject(path);
    }
    if (character === "[") {
      return this.#parseArray(path);
    }
    if (character === '"') {
      return this.#parseString(path);
    }
    if (character === "t") {
      return this.#parseLiteral("true", true, path);
    }
    if (character === "f") {
      return this.#parseLiteral("false", false, path);
    }
    if (character === "n") {
      return this.#parseLiteral("null", null, path);
    }
    if (
      character === "-" ||
      (character !== undefined && /\d/u.test(character))
    ) {
      return this.#parseNumber(path);
    }
    this.#refuse("malformed_json", path);
  }

  #parseObject(path: string): Record<string, unknown> {
    this.#position += 1;
    this.#skipWhitespace();
    const result: Record<string, unknown> = {};
    const keys = new Set<string>();
    if (this.#source[this.#position] === "}") {
      this.#position += 1;
      return result;
    }
    while (this.#position < this.#source.length) {
      if (this.#source[this.#position] !== '"') {
        this.#refuse("malformed_json", path);
      }
      const key = this.#parseString(path);
      if (keys.has(key)) {
        this.#refuse("duplicate_field", path);
      }
      keys.add(key);
      this.#skipWhitespace();
      if (this.#source[this.#position] !== ":") {
        this.#refuse("malformed_json", path);
      }
      this.#position += 1;
      // Match JSON.parse's ordinary data objects without invoking inherited
      // setters (in particular __proto__) for untrusted field names.
      Object.defineProperty(result, key, {
        value: this.#parseValue(path),
        enumerable: true,
        writable: true,
        configurable: true,
      });
      this.#skipWhitespace();
      const delimiter = this.#source[this.#position];
      if (delimiter === "}") {
        this.#position += 1;
        return result;
      }
      if (delimiter !== ",") {
        this.#refuse("malformed_json", path);
      }
      this.#position += 1;
      this.#skipWhitespace();
    }
    this.#refuse("malformed_json", path);
  }

  #parseArray(path: string): readonly unknown[] {
    this.#position += 1;
    this.#skipWhitespace();
    const result: unknown[] = [];
    if (this.#source[this.#position] === "]") {
      this.#position += 1;
      return result;
    }
    while (this.#position < this.#source.length) {
      result.push(this.#parseValue(`${path}[${result.length.toString()}]`));
      this.#skipWhitespace();
      const delimiter = this.#source[this.#position];
      if (delimiter === "]") {
        this.#position += 1;
        return result;
      }
      if (delimiter !== ",") {
        this.#refuse("malformed_json", path);
      }
      this.#position += 1;
      this.#skipWhitespace();
    }
    this.#refuse("malformed_json", path);
  }

  #parseString(path: string): string {
    const start = this.#position;
    this.#position += 1;
    let escaped = false;
    while (this.#position < this.#source.length) {
      const character = this.#source[this.#position]!;
      if (!escaped && character === '"') {
        this.#position += 1;
        try {
          return JSON.parse(
            this.#source.slice(start, this.#position),
          ) as string;
        } catch {
          this.#refuse("malformed_json", path);
        }
      }
      if (!escaped && character.charCodeAt(0) < 0x20) {
        this.#refuse("malformed_json", path);
      }
      if (!escaped && character === "\\") {
        escaped = true;
      } else {
        escaped = false;
      }
      this.#position += 1;
    }
    this.#refuse("malformed_json", path);
  }

  #parseNumber(path: string): number {
    const match = /^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?/u.exec(
      this.#source.slice(this.#position),
    );
    if (match === null) {
      this.#refuse("malformed_json", path);
    }
    this.#position += match[0].length;
    const number = Number(match[0]);
    if (!Number.isFinite(number)) {
      this.#refuse("unsafe_value", path);
    }
    return number;
  }

  #parseLiteral<T>(literal: string, value: T, path: string): T {
    if (!this.#source.startsWith(literal, this.#position)) {
      this.#refuse("malformed_json", path);
    }
    this.#position += literal.length;
    return value;
  }

  #skipWhitespace(): void {
    while (
      this.#position < this.#source.length &&
      /\s/u.test(this.#source[this.#position]!)
    ) {
      this.#position += 1;
    }
  }
}

/** Parses `source` as strict JSON, refusing through `refuse`. */
export const parseStrictJson = (
  source: string,
  refuse: StrictJsonRefuse,
): unknown => new StrictJsonReader(source, refuse).parse();
