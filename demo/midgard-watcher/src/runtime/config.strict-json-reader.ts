import "./config.parse-providers.js";

import { parseWatcherConfig } from "./config.parse-watcher-config.js";
import {
  fail,
  WATCHER_CONFIG_BOUNDS,
  type WatcherConfig,
} from "./config.watcher-config.js";

class StrictJsonReader {
  readonly #source: string;
  #position = 0;

  constructor(source: string) {
    this.#source = source;
  }

  parse(): unknown {
    this.#skipWhitespace();
    const value = this.#parseValue("$");
    this.#skipWhitespace();
    if (this.#position !== this.#source.length) {
      fail("malformed_json", "$");
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
    fail("malformed_json", path);
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
        fail("malformed_json", path);
      }
      const key = this.#parseString(path);
      if (keys.has(key)) {
        fail("duplicate_field", path);
      }
      keys.add(key);
      this.#skipWhitespace();
      if (this.#source[this.#position] !== ":") {
        fail("malformed_json", path);
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
        fail("malformed_json", path);
      }
      this.#position += 1;
      this.#skipWhitespace();
    }
    fail("malformed_json", path);
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
        fail("malformed_json", path);
      }
      this.#position += 1;
      this.#skipWhitespace();
    }
    fail("malformed_json", path);
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
          fail("malformed_json", path);
        }
      }
      if (!escaped && character.charCodeAt(0) < 0x20) {
        fail("malformed_json", path);
      }
      if (!escaped && character === "\\") {
        escaped = true;
      } else {
        escaped = false;
      }
      this.#position += 1;
    }
    fail("malformed_json", path);
  }

  #parseNumber(path: string): number {
    const match = /^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?/u.exec(
      this.#source.slice(this.#position),
    );
    if (match === null) {
      fail("malformed_json", path);
    }
    this.#position += match[0].length;
    const number = Number(match[0]);
    if (!Number.isFinite(number)) {
      fail("unsafe_value", path);
    }
    return number;
  }

  #parseLiteral<T>(literal: string, value: T, path: string): T {
    if (!this.#source.startsWith(literal, this.#position)) {
      fail("malformed_json", path);
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

export const parseWatcherConfigJson = (source: string): WatcherConfig => {
  if (
    typeof source !== "string" ||
    source.length < WATCHER_CONFIG_BOUNDS.configJsonBytes.min ||
    Buffer.byteLength(source, "utf8") >
      WATCHER_CONFIG_BOUNDS.configJsonBytes.max
  ) {
    fail("out_of_bounds", "$");
  }
  return parseWatcherConfig(new StrictJsonReader(source).parse());
};

/** Duplicate-key rejecting JSON admission shared by production identity files. */
export const parseWatcherStrictJsonValue = (source: string): unknown =>
  new StrictJsonReader(source).parse();
