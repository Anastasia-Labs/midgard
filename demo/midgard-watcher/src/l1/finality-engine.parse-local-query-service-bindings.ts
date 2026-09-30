import { isExactEndpoint } from "./finality-engine.clone-external-providers.js";
import {
  exactArray,
  exactPlainRecord,
  isHex32,
  type LocalQueryServiceBinding,
  SOURCE_AUTHORITY_ID,
} from "./finality-engine.watcher-finality-reason-codes.js";

export const parseLocalQueryServiceBindings = (
  value: unknown,
): readonly LocalQueryServiceBinding[] | null => {
  const inputs = exactArray(value);
  if (inputs === null || inputs.length > 8) {
    return null;
  }
  const bindings: LocalQueryServiceBinding[] = [];
  for (const input of inputs) {
    const binding = exactPlainRecord(input, [
      "kind",
      "providerId",
      "endpoint",
      "observationStatus",
      "observationDigest",
    ]);
    if (
      binding === null ||
      !["ogmios", "kupo", "kupmios", "db_sync"].includes(
        binding.kind as string,
      ) ||
      typeof binding.providerId !== "string" ||
      !SOURCE_AUTHORITY_ID.test(binding.providerId) ||
      !isExactEndpoint(
        binding.endpoint,
        binding.kind === "ogmios"
          ? ["http:", "https:", "ws:", "wss:"]
          : binding.kind === "kupo"
            ? ["http:", "https:"]
            : binding.kind === "kupmios"
              ? ["http:", "https:", "ws:", "wss:"]
              : ["postgresql:"],
      ) ||
      ![
        "aligned",
        "unavailable",
        "stale",
        "forked",
        "rollback_not_propagated",
        "content_mismatch",
      ].includes(binding.observationStatus as string) ||
      !(
        binding.observationDigest === null || isHex32(binding.observationDigest)
      ) ||
      (binding.observationStatus === "unavailable") !==
        (binding.observationDigest === null)
    ) {
      return null;
    }
    bindings.push(
      Object.freeze({
        kind: binding.kind as LocalQueryServiceBinding["kind"],
        providerId: binding.providerId,
        endpoint: binding.endpoint,
        observationStatus:
          binding.observationStatus as LocalQueryServiceBinding["observationStatus"],
        observationDigest: binding.observationDigest as string | null,
      }),
    );
  }
  if (
    bindings.some(
      (binding, index) =>
        index > 0 &&
        binding.providerId <=
          (bindings[index - 1] as LocalQueryServiceBinding).providerId,
    )
  ) {
    return null;
  }
  return Object.freeze(bindings);
};
