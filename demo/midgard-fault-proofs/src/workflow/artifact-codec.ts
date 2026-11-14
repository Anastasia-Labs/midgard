import {
  journalJsonDigest,
  type JournalJsonObject,
  type JournalJsonValue,
  normalizeJournalJson,
} from "./journal.js";

const ENCODING = "midgard-workflow-artifact-v1";

/** Lossless, deterministic persistence for package-built proof material.
 * Tags cover every node, so ordinary record keys cannot impersonate bytes or integers. */
export const encodeWorkflowArtifact = (value: unknown): JournalJsonObject => {
  const active = new Set<object>();
  const encode = (node: unknown): JournalJsonValue => {
    if (node === null) return ["null"];
    if (typeof node === "string" || typeof node === "boolean")
      return [typeof node, node];
    if (typeof node === "number") {
      if (!Number.isSafeInteger(node) || Object.is(node, -0))
        throw new Error(
          "workflow artifact number must be a canonical safe integer",
        );
      return ["number", node];
    }
    if (typeof node === "bigint") return ["bigint", node.toString()];
    if (Buffer.isBuffer(node)) return ["bytes", node.toString("hex")];
    if (typeof node !== "object" || node === null)
      throw new Error("workflow artifact contains an unsupported value");
    if (active.has(node)) throw new Error("workflow artifact contains a cycle");
    active.add(node);
    try {
      if (Array.isArray(node)) {
        if (Reflect.ownKeys(node).length !== node.length + 1)
          throw new Error(
            "workflow artifact array is sparse or has extra fields",
          );
        const children: JournalJsonValue[] = [];
        for (let index = 0; index < node.length; index += 1) {
          const descriptor = Object.getOwnPropertyDescriptor(
            node,
            index.toString(),
          );
          if (
            descriptor === undefined ||
            !("value" in descriptor) ||
            !descriptor.enumerable
          )
            throw new Error(
              "workflow artifact array contains a missing or accessor element",
            );
          children.push(encode(descriptor.value));
        }
        return ["array", children];
      }
      const prototype = Object.getPrototypeOf(node) as unknown;
      if (prototype !== Object.prototype && prototype !== null)
        throw new Error("workflow artifact must contain plain records");
      const keys = Reflect.ownKeys(node);
      if (keys.some((key) => typeof key !== "string"))
        throw new Error("workflow artifact contains symbol fields");
      const entries = Object.keys(node)
        .sort()
        .map((key) => {
          const descriptor = Object.getOwnPropertyDescriptor(node, key);
          if (
            descriptor === undefined ||
            !("value" in descriptor) ||
            !descriptor.enumerable
          )
            throw new Error(
              "workflow artifact contains an accessor or hidden field",
            );
          return [key, encode(descriptor.value)] as const;
        });
      if (entries.length !== keys.length)
        throw new Error("workflow artifact contains hidden fields");
      return ["record", entries];
    } finally {
      active.delete(node);
    }
  };
  return normalizeJournalJson({
    encoding: ENCODING,
    value: encode(value),
  }) as JournalJsonObject;
};

/** Persisted material never becomes a typed object by assertion. Return only
 * fresh, independently reconstructed material after checking its exact encoding. */
export const requireWorkflowArtifactMatches = <T>(
  artifact: JournalJsonObject,
  fresh: T,
): T => {
  if (
    journalJsonDigest(artifact) !==
    journalJsonDigest(encodeWorkflowArtifact(fresh))
  )
    throw new Error(
      "prepared workflow artifact differs from freshly authenticated material",
    );
  return fresh;
};
