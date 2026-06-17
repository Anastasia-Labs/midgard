import { CML } from "@lucid-evolution/lucid";

export const toLocalCmlValue = (value: CML.Value): CML.Value =>
  value instanceof CML.Value
    ? value
    : CML.Value.from_cbor_bytes(
        (
          value as unknown as { readonly to_cbor_bytes: () => Uint8Array }
        ).to_cbor_bytes(),
      );
