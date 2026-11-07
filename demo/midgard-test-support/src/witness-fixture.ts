import { createHash } from "node:crypto";

export const witnessFixture = <T>(
  value: T,
  encode: (value: T) => Uint8Array,
) => {
  // Invoke the production encoder before entering a behavior assertion.
  const encoded = Buffer.from(encode(value));
  if (encoded.length === 0)
    throw new Error("canonical witness encoder returned no bytes");
  const identity = createHash("sha256").update(encoded).digest("hex");
  return {
    value,
    identity,
    bytes: (): Buffer => Buffer.from(encoded),
    /** Explicitly malformed bytes, with the canonical basis retained. */
    mutate: (
      change: (bytes: Buffer) => void,
    ): {
      readonly malformed: true;
      readonly basis: string;
      readonly bytes: Buffer;
    } => {
      const bytes = Buffer.from(encoded);
      change(bytes);
      if (bytes.equals(encoded))
        throw new Error("malformed witness mutation did not change any byte");
      return { malformed: true, basis: identity, bytes };
    },
  };
};
