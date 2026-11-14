type DeepTermShape = "case" | "constr" | "application";

// Construct Flat directly, independently of the canonicalizer and its encoder.
// Cases nest their scrutinee, constrs their sole field, applications their
// function (with an error argument). Flat lists end with a zero bit.
export const deepFlatProgram = (
  shape: DeepTermShape,
  depth: number,
  oversizedConstant = false,
): Buffer => {
  const prefix = {
    case: "1001",
    constr: "1000000000001",
    application: "0011",
  }[shape];
  let bits = "000000010000000100000000" + prefix.repeat(depth);
  const pad = (): void => {
    bits += "0".repeat(7 - (bits.length % 8)) + "1";
  };
  if (oversizedConstant) {
    // Constant tag, singleton bytestring type list, then byte-aligned chunks.
    bits += "0100100010";
    pad();
    for (let remaining = 9_216; remaining > 0; ) {
      const length = Math.min(remaining, 255);
      bits += length.toString(2).padStart(8, "0") + "00000000".repeat(length);
      remaining -= length;
    }
    bits += "00000000";
  } else {
    bits += "0110"; // Error is a legal UPLC term.
  }
  bits += (shape === "application" ? "0110" : "0").repeat(depth);
  pad();
  return Buffer.from(
    Array.from({ length: bits.length / 8 }, (_, index) =>
      Number.parseInt(bits.slice(index * 8, index * 8 + 8), 2),
    ),
  );
};
