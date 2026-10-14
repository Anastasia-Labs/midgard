import { Data } from "effect";

export class PublicHttpReadLimitError extends Data.TaggedError(
  "PublicHttpReadLimitError",
)<{
  readonly message: "E_HTTP_READ_DATA_LIMIT";
}> {}

/** A null SQL payload is a byte barrier, never a missing or spendable row. */
export const requirePublicReadPayload = (payload: Buffer | null): Buffer => {
  if (payload === null)
    throw new PublicHttpReadLimitError({ message: "E_HTTP_READ_DATA_LIMIT" });
  return payload;
};
