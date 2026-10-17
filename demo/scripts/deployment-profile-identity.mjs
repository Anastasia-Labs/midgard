import { createHash } from "node:crypto";

// Sorted object keys; arrays keep their order (midgard-core canonicalJson).
export const canonicalJson = (value) =>
  JSON.stringify(
    Array.isArray(value)
      ? value.map((entry) => JSON.parse(canonicalJson(entry)))
      : value !== null && typeof value === "object"
        ? Object.fromEntries(
            Object.keys(value)
              .sort()
              .map((key) => [key, JSON.parse(canonicalJson(value[key]))]),
          )
        : value,
  );
export const profileDigest = (profile) =>
  createHash("sha256").update(canonicalJson(profile)).digest("hex");

